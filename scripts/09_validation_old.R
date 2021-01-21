# Validate the results of the classification

library(caret)
library(dplyr)
library(gdalUtils)
library(raster)

prodes_file <- "./data/prodes/2019/PDigital2000_2019_AMZ_gtif_077095.tif"
gfw_file    <- "./data/global_forest_watch/Hansen_GFC-2019-v1.7_lossyear_10S_070W_077095.tif"
class_file  <- "./results/paper_defor/bmv_map_w3_nu1_covFALSE.tif"
stopifnot(file.exists(class_file))
stopifnot(file.exists(prodes_file))
stopifnot(file.exists(gfw_file))

# Pre-process classification map.
class_vrt <- tempfile(pattern = "class_",
                      fileext = ".vrt")
gdalUtils::gdalbuildvrt(gdalfile = class_file,
                        output.vrt = class_vrt)



#---- Compare classification to PRODES -----

# Pre-process PRODES.
prodes_trans <- tempfile(pattern = "prodes_wgs84_",
                         fileext = ".tif")
gdalUtils::gdalwarp(srcfile = prodes_file,
                    dstfile = prodes_trans,
                    t_srs = "EPSG:4326",
                    co = c("COMPRESS=LZW", "BIGTIFF=YES"),
                    tr = c(0.000268987056828,-0.000269018801881),
                    te = c(-65.23432383171814308, -10.92505295837335844,
                           -63.71562290886634372, -10.00312552432653312),
                    r = "near",
                    ot = "Int16")

class_trans <- tempfile(pattern = "class_wgs84_",
                        fileext = ".tif")
gdalUtils::gdalwarp(srcfile = class_vrt,
                    dstfile = class_trans,
                    t_srs = "EPSG:4326",
                    co = c("COMPRESS=LZW", "BIGTIFF=YES"),
                    tr = c(0.000268987056828,-0.000269018801881),
                    te = c(-65.23432383171814308, -10.92505295837335844,
                           -63.71562290886634372, -10.00312552432653312),
                    r = "near",
                    ot = "Int16")

class_prodes_overlay <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor/overlay_class_prodes.tif"

cmd <- sprintf("gdal_calc.py -A %s -B %s --outfile=%s --calc='(A.astype(numpy.int16) * 100) + B.astype(numpy.int16)' --type=Int16 --NoDataValue=-9999 --quiet --co COMPRESS=LZW --co BIGTIFF=YES",
               class_trans, prodes_trans, class_prodes_overlay)

# NOTE: This throws an error. I ran it localy!
system(cmd)
# ERROR 1: TIFFScanlineSize64:Computed scanline size is zero
# ERROR 1: TIFFReadDirectory:Cannot handle zero scanline size
# Traceback (most recent call last):
#     File "/usr/bin/gdal_calc.py", line 483, in <module>
#     main()
# File "/usr/bin/gdal_calc.py", line 476, in main
# doit(opts, args)
# File "/usr/bin/gdal_calc.py", line 201, in doit
# if [myOut.RasterXSize, myOut.RasterYSize] != DimensionsCheck:
#     AttributeError: 'NoneType' object has no attribute 'RasterXSize'

stopifnot(file.exists(class_prodes_overlay))

class_labels <- c(`1` = "deforestation",
                  `2` = "forest",
                  `3` = "natNonForest",
                  `4` = "pasture")

class_prodes <- c(`1`  = "FLORESTA", `2`  = "HIDROGRAFIA", `3`  = "NAO_FLORESTA",
                  `4`  = "NAO_FLORESTA2", `5`  = "NUVEM", `6`  = "d2007",
                  `7`  = "d2008", `8`  = "d2009", `9`  = "d2010", `10` = "d2011",
                  `11` = "d2012", `12` = "d2013", `13` = "d2014", `14` = "d2015",
                  `15` = "d2016", `16` = "d2017", `17` = "d2018", `18` = "r2010",
                  `19` = "r2011", `20` = "r2012", `21` = "r2013", `22` = "r2014",
                  `23` = "r2015", `24` = "r2016", `25` = "r2017", `26` = "r2018",
                  `27` = "d2019", `28` = "NUVEM2019")

overlay_r <- raster::raster(class_prodes_overlay)
overlay_table <- table(overlay_r[])
count_tb <- overlay_table %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(value= as.integer(as.character(Var1)),
                  predicted = as.integer(floor(value/100)),
                  reference = as.integer(value - (predicted * 100))) %>%
    ensurer::ensure_that(all(.$value > 100),
                         all(.$value < 1000),
                         err_desc = "Found malformed class ids") %>%
    dplyr::rename(frequency = Freq) %>%
    dplyr::select(predicted, reference, frequency) %>%
    dplyr::mutate(predicted = dplyr::recode(predicted, !!!class_labels),
                  reference = dplyr::recode(reference, !!!class_prodes)) %>%
    dplyr::mutate(predicted = dplyr::recode(predicted,
                                            "deforestation" = "deforestation",
                                            "forest"        = "forest",
                                            #"natNonForest",
                                            "pasture"       = "deforestation",
                                            .default = NA_character_)) %>%
    dplyr::mutate(reference = dplyr::recode(reference,
                                            "FLORESTA" = "forest",
                                            #"HIDROGRAFIA", #"NAO_FLORESTA",
                                            #"NAO_FLORESTA2", #"NUVEM",
                                            #"d2007", #"d2009", #"d2009",
                                            #"d2010", #"d2011", #"d2012",
                                            #"d2013", #"d2014", #"d2015",
                                            #"d2016", #"d2017", #"d2018",
                                            #"r2010", #"r2011", #"r2012",
                                            #"r2013", #"r2014", #"r2015",
                                            #"r2016", #"r2017", #"r2018",
                                            "d2019" = "deforestation_2019",
                                            #"NUVEM2019"
                                            .default = NA_character_)) %>%
    dplyr::mutate(predicted = factor(predicted),
                  reference = factor(reference)) %>%
    tidyr::drop_na()

cont_table <- xtabs(frequency ~ predicted + reference , data = data.frame(count_tb))
cont_table
#                  reference
# predicted       deforestation_p forest_p
# deforestation          162725   311269
# forest                  15564  8576621

print("Producer's accuracy")
diag(cont_table) / colSums(cont_table)
# deforestation_p        forest_p
# 0.9127035       0.9649783

print("User's accuracy")
diag(cont_table) / rowSums(cont_table)
# deforestation        forest
# 0.3433060     0.9981886
