# Validate the results of the classification
library(caret)
library(dplyr)
library(gdalUtils)
library(raster)
library(sf)



#---- Configuration ----

prodes2019_file <- "./data/prodes/2019/PDigital2000_2019_AMZ_gtif_077095.tif"
prodes2020_file <- "./data/prodes/2020/PDigital2000_2020_AMZ_077095.tif"
gfw_file    <- "./data/global_forest_watch/Hansen_GFC-2019-v1.7_lossyear_10S_070W_077095.tif"

# Classification result files
# - without post-processing
#class_file  <- "./results/paper_defor2/S2_10_16D_STK_077095_probs_2018_7_which_max.tif"
# - bayesian smoothing with a 3x3 window and no assuming no covariance among classes.
class_file  <- "./results/paper_defor/bmv_map_w3_nu1_covFALSE.tif"

stopifnot(file.exists(class_file))
stopifnot(file.exists(prodes2019_file))
stopifnot(file.exists(prodes2020_file))
stopifnot(file.exists(gfw_file))

source("./scripts/00_util.R")

class_labels <- c(`1` = "Deforestation",
                  `2` = "Forest",
                  `3` = "NatNonForest",
                  `4` = "Pasture")
# class_labels <- c(`1` = "Abandoned", `2` = "Deforestation", `3` = "Forest",
#                   `4` = "NatNonForest", `5` = "Pasture")



#---- Validation using Olofson's method ----

validation_shp <- "./data/validation/validation.shp"
stopifnot(file.exists(validation_shp))

validation_sf <- validation_shp %>%
    sf::read_sf() %>%
    ensurer::ensure_that(all(class_labels %in% unique(.$class)),
                         err_desc = "Label missmatch!") %>%
    dplyr::filter(class %in% class_labels) #dplyr::filter(class != "Abandoned")

# Print the number of samples.
validation_sf %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    (function(x) {
        x %>%
            dplyr::count(class) %>%
            print()
        print(nrow(x))
        invisible(x)
    })

class_r <- class_file %>%
    raster::raster()

# Get the predicted label for the validation points.
validation_tb <- raster::extract(class_r,
                                 as(validation_sf, "Spatial"),
                                 cellnumberes = FALSE,
                                 sp = TRUE) %>%
    sf::st_as_sf() %>%
    add_coords() %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::last_col(offset = 0:3)) %>%
    dplyr::rename(reference = class) %>%
    dplyr::select(longitude,  latitude,  reference,  tidyselect::everything()) %>%
    magrittr::set_names(c("longitude", "latitude", "reference", "predicted")) %>%
    dplyr::mutate(predicted = dplyr::recode(predicted,
                                            !!!class_labels,
                                            .default = NA_character_)) %>%
    ensurer::ensure_that(!all(is.na(.$predicted)),
                         !all(is.na(.$reference)),
                         err_desc = "NAs are not allowed!") %>%
    ensurer::ensure_that(all(unique(.$reference) %in% unique(.$predicted)),
                         all(unique(.$predicted) %in% unique(.$reference)),
                         err_desc = "Label mismatch!") %>%
    dplyr::mutate(reference = factor(reference,
                                     levels = class_labels),
                  predicted = factor(predicted,
                                     levels = class_labels))

# Compute the confusion matrices.
con_mat <- caret::confusionMatrix(data      = validation_tb$predicted,
                                  reference = validation_tb$reference)
cm_mt <-  as.matrix(con_mat$table)
print(con_mat)

# Overall accuracy
sum(diag(cm_mt)) / sum(colSums(cm_mt))

# Producer's accuracy
diag(cm_mt) / colSums(cm_mt)

# User's accuracy
diag(cm_mt) / rowSums(cm_mt)

# Save the missclassified points to a shapefile.
# validation_tb %>%
#     dplyr::filter(reference == "Forest",
#                   predicted == "Abandoned") %>%
#     sf::st_as_sf(coords = c("longitude", "latitude")) %>%
#     sf::st_set_crs(raster::crs(class_r)) %>%
#     sf::write_sf("./data/validation/validation_error.shp")



#---- Compare our classification to PRODES -----

# Pre-process PRODES.
prodes2019_trans <- tempfile(pattern = "prodes2019_wgs84_", fileext = ".tif")
prodes2020_trans <- tempfile(pattern = "prodes2020_wgs84_", fileext = ".tif")

# Project PRODES to WGS84.
gdalUtils::gdalwarp(srcfile = prodes2019_file,
                    dstfile = prodes2019_trans,
                    t_srs = "EPSG:4326",
                    co = c("COMPRESS=LZW", "BIGTIFF=YES"),
                    tr = c(0.000268987056828,-0.000269018801881),
                    te = c(-65.23432383171814308, -10.92505295837335844,
                           -63.71562290886634372, -10.00312552432653312),
                    r = "near",
                    ot = "Int16")
gdalUtils::gdalwarp(srcfile = prodes2020_file,
                    dstfile = prodes2020_trans,
                    t_srs = "EPSG:4326",
                    co = c("COMPRESS=LZW", "BIGTIFF=YES"),
                    tr = c(0.000268987056828,-0.000269018801881),
                    te = c(-65.23432383171814308, -10.92505295837335844,
                           -63.71562290886634372, -10.00312552432653312),
                    r = "near",
                    ot = "Int16")

# Project our classification results to WGS84.
class_trans <- tempfile(pattern = "class_wgs84_",
                        fileext = ".tif")
class_vrt <- tempfile(pattern = "class_",
                      fileext = ".vrt")
gdalUtils::gdalbuildvrt(gdalfile = class_file,
                        output.vrt = class_vrt)
# NOTE: We're re-sampling our 10m classification to 30m PRODES. We're
#       resampling the finer to the coarser resolution using the mode as
#       resampling strategy.
gdalUtils::gdalwarp(srcfile = class_vrt,
                    dstfile = class_trans,
                    t_srs = "EPSG:4326",
                    co = c("COMPRESS=LZW", "BIGTIFF=YES"),
                    tr = c(0.000268987056828,-0.000269018801881),
                    te = c(-65.23432383171814308, -10.92505295837335844,
                           -63.71562290886634372, -10.00312552432653312),
                    r = "mode",
                    ot = "Int16")

# Ouput overlay rasters (before recoding labels).
class_prodes2019_overlay <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor/overlay_class_prodes2019.tif"
class_prodes2020_overlay <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor/overlay_class_prodes2020.tif"
class_prodes_19_20_overlay <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor/overlay_class_prodes_19_20.tif"

# Overlay our classification results with PRODES
cmd_prodes2019 <- sprintf("gdal_calc.py -A %s -B %s --outfile=%s --calc='(A.astype(numpy.int16) * 100) + B.astype(numpy.int16)' --type=Int16 --NoDataValue=-9999 --quiet --co COMPRESS=LZW --co BIGTIFF=YES",
                          class_trans, prodes2019_trans, class_prodes2019_overlay)
cmd_prodes2020 <- sprintf("gdal_calc.py -A %s -B %s --outfile=%s --calc='(A.astype(numpy.int16) * 100) + B.astype(numpy.int16)' --type=Int16 --NoDataValue=-9999 --quiet --co COMPRESS=LZW --co BIGTIFF=YES",
                          class_trans, prodes2020_trans, class_prodes2020_overlay)
cmd_prodes_19_20 <- sprintf("gdal_calc.py -A %s -B %s -C %s --outfile=%s --calc='(A.astype(numpy.int16) * 10000) + (B.astype(numpy.int16) * 100) + C.astype(numpy.int16)' --type=Int16 --NoDataValue=-9999 --quiet --co COMPRESS=LZW --co BIGTIFF=YES",
                          class_trans, prodes2019_trans, prodes2020_trans, class_prodes_19_20_overlay)
system(cmd_prodes2019)
system(cmd_prodes2020)
system(cmd_prodes_19_20)
stopifnot(file.exists(class_prodes2019_overlay))
stopifnot(file.exists(class_prodes2020_overlay))
stopifnot(file.exists(class_prodes_19_20_overlay))

class_prodes2019 <- c(`1`  = "FLORESTA", `2`  = "HIDROGRAFIA", `3`  = "NAO_FLORESTA",
                      `4`  = "NAO_FLORESTA2", `5`  = "NUVEM", `6`  = "d2007",
                      `7`  = "d2008", `8`  = "d2009", `9`  = "d2010", `10` = "d2011",
                      `11` = "d2012", `12` = "d2013", `13` = "d2014", `14` = "d2015",
                      `15` = "d2016", `16` = "d2017", `17` = "d2018", `18` = "r2010",
                      `19` = "r2011", `20` = "r2012", `21` = "r2013", `22` = "r2014",
                      `23` = "r2015", `24` = "r2016", `25` = "r2017", `26` = "r2018",
                      `27` = "d2019", `28` = "NUVEM2019")
class_prodes2020 <- c(`1`  = "FLORESTA", `2`  = "HIDROGRAFIA", `3`  = "NAO FLORESTA" ,
                      `4`  = "NAO FLORESTA2", `6`  = "d2007", `7`  = "d2008",
                      `8`  = "d2009", `9`  = "d2010", `10` = "d2011", `11` = "d2012",
                      `12` = "d2013", `13` = "d2014", `14` = "d2015", `15` = "d2016",
                      `16` = "d2017", `17` = "d2018", `18` = "r2010", `19` = "r2011",
                      `20` = "r2012", `21` = "r2013", `22` = "r2014", `23` = "r2015",
                      `24` = "r2016", `25` = "r2017", `26` = "r2018", `27` = "d2019",
                      `28` = "r2019", `29` = "d2020", `30` = "NUVEM")

overlay2019_r <- raster::raster(class_prodes2019_overlay)
overlay2020_r <- raster::raster(class_prodes2020_overlay)

overlay2019_table <- table(overlay2019_r[])
overlay2020_table <- table(overlay2020_r[])

count2019_tb <- overlay2019_table %>%
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
                  reference = dplyr::recode(reference, !!!class_prodes2019)) %>%
    dplyr::mutate(predicted = dplyr::recode(predicted,
                                            "Deforestation" = "Deforestation",
                                            "Forest"        = "Forest",
                                            #"natNonForest",
                                            "Pasture"       = "Deforestation",
                                            .default = NA_character_)) %>%
    dplyr::mutate(reference = dplyr::recode(reference,
                                            "FLORESTA" = "Forest",
                                            "d2019" = "Deforestation",
                                            .default = NA_character_)) %>%
    dplyr::mutate(predicted = factor(predicted),
                  reference = factor(reference)) %>%
    tidyr::drop_na()
count2020_tb <- overlay2020_table %>%
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
                  reference = dplyr::recode(reference, !!!class_prodes2020)) %>%
    dplyr::mutate(predicted = dplyr::recode(predicted,
                                            "Deforestation" = "Deforestation",
                                            "Forest"        = "Forest",
                                            #"natNonForest",
                                            "Pasture"       = "Deforestation",
                                            .default = NA_character_)) %>%
    dplyr::mutate(reference = dplyr::recode(reference,
                                            "FLORESTA" = "Forest",
                                            "d2020" = "Deforestation",
                                            .default = NA_character_)) %>%
    dplyr::mutate(predicted = factor(predicted),
                  reference = factor(reference)) %>%
    tidyr::drop_na()

cont_table2019 <- xtabs(frequency ~ predicted + reference , data = data.frame(count2019_tb))
cont_table2020 <- xtabs(frequency ~ predicted + reference , data = data.frame(count2020_tb))
cont_table2019
#                              reference
# predicted       deforestation_2019  forest
# deforestation               163492  313346
# forest                       15561 8597166
cont_table2020
#                              reference
# predicted       Deforestation  Forest
# Deforestation           41378  272768
# Forest                  98026 8499622


print("Overall accuracy")
sum(diag(cont_table2019)) / sum(colSums(cont_table2019))
# 0.9638149
print("Producer's accuracy")
diag(cont_table2019) / colSums(cont_table2019)
# deforestation_2019             forest
#          0.9130928          0.9648341
print("User's accuracy")
diag(cont_table2019) / rowSums(cont_table2019)
# deforestation        forest
#     0.3428670     0.9981933

print("Overall accuracy")
sum(diag(cont_table2020)) / sum(colSums(cont_table2020))
# 0.9583929
print("Producer's accuracy")
diag(cont_table2020) / colSums(cont_table2020)
# Deforestation_2020        Forest
# 0.2968208                 0.9689061
print("User's accuracy")
diag(cont_table2020) / rowSums(cont_table2020)
# Deforestation_2020        Forest
# 0.1317158                 0.9885985

# Compare classification to only the  deforestation of 2020

# Estimate the deforestation difference between our classification and PRODES 2019

# Find the difference in PRODES 2020