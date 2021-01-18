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

class_labels <- c(`1` = "deforestation",
            `2` = "forest",
            `3` = "natNonForest",
            `4` = "pasture")

# Pre-process classification map.
class_vrt <- tempfile(pattern = "class_",
                      fileext = ".vrt")
gdalUtils::gdalbuildvrt(gdalfile = class_file,
                        output.vrt = class_vrt)
class_trans <- tempfile(pattern = "class_trans_",
                            fileext = ".tif")
gdalUtils::gdalwarp(srcfile = class_vrt,
                    dstfile = class_trans,
                    t_srs = "EPSG:4326",
                    co = c("COMPRESS=LZW", "BIGTIFF=YES"))



#---- Compare classification to PRODES -----

# Build a mask using PRODES' deforestation.
prodes_vrt <- tempfile(pattern = "prodes_",
                       fileext = ".vrt")
gdalUtils::gdalbuildvrt(gdalfile = prodes_file,
                        output.vrt = prodes_vrt)
prodes_mask <- tempfile(pattern = "prodes_mask_",
                        fileext = ".tif")
cmd <- sprintf("gdal_calc.py -A %s --outfile=%s --calc='A==27' --NoDataValue=-9999 --quiet --co COMPRESS=LZW BIGTIFF=YES --type=Byte",
               prodes_vrt, prodes_mask)
system(cmd)
prodes_trans <- tempfile(pattern = "prodes_wgs84_",
                         fileext = ".tif")
gdalUtils::gdalwarp(srcfile = prodes_mask,
                    dstfile = prodes_trans,
                    t_srs = "EPSG:4326",
                    co = c("COMPRESS=LZW", "BIGTIFF=YES"))
# Mask classification map.
result_masked_file <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor/class_masked_by_prodes.tif"
vrt_file <- tempfile(pattern = "class_masked_by_prodes_",
                       fileext = ".vrt")
gdalUtils::gdalbuildvrt(gdalfile = c(class_trans, prodes_trans),
                        output.vrt = vrt_file,
                        resolution = "highest",
                        allow_projection_difference = TRUE,
                        separate = TRUE,
                        q = TRUE)
cmd <- sprintf("gdal_calc.py -A %s --A_band=1 -B %s --B_band=2 --outfile=%s --calc='A * B' --NoDataValue=-0 --quiet --co COMPRESS=LZW BIGTIFF=YES --type=",
                   vrt_file, vrt_file, result_masked_file)
system(cmd)
result_masked_r <- raster::raster(result_masked_file)
result_masked_tb <- result_masked_r[] %>%
    na.omit() %>%
    as.vector() %>%
    tibble::as_tibble() %>%
    dplyr::rename(predicted = value) %>%
    dplyr::mutate(predicted = dplyr::recode(predicted,
                                             !!!labels,
                                             .default = NA_character_)) %>%
    dplyr::mutate(reference = "deforestation")
cm <- caret::confusionMatrix(data = factor(result_masked_tb$predicted,
                                     levels = labels),
                       reference = factor(result_masked_tb$reference,
                                          levels = labels))
cm$table
total <- sum(colSums(cm$table))
cm$table/total
#> cm$table
# Reference
# Prediction      deforestation   foret natNonForest pasture
# deforestation       1290207       0            0       0
# foret                149803       0            0       0
# natNonForest          67288       0            0       0
# pasture              308115       0            0       0
# > total <- sum(colSums(cm$table))
# > cm$table/total
# Reference
# Prediction      deforestation      foret natNonForest    pasture
# deforestation    0.71069613 0.00000000   0.00000000 0.00000000
# foret            0.08251731 0.00000000   0.00000000 0.00000000
# natNonForest     0.03706484 0.00000000   0.00000000 0.00000000
# pasture          0.16972171 0.00000000   0.00000000 0.00000000



#---- Compare classification to GFW -----

# Build a mask using GLOBAL FOREST WATCH deforestation.
gfw_vrt <- tempfile(pattern = "gfw_",
                    fileext = ".vrt")
gdalUtils::gdalbuildvrt(gdalfile = gfw_file,
                        output.vrt = gfw_vrt)
gfw_mask <- tempfile(pattern = "gfw_mask_",
                     fileext = ".tif")
#cmd <- sprintf("gdal_calc.py -A %s --outfile=%s --calc='numpy.logical_or(A==18, A==19)' --NoDataValue=-9999 --quiet --co COMPRESS=LZW BIGTIFF=YES --type=Byte",
cmd <- sprintf("gdal_calc.py -A %s --outfile=%s --calc='A==19' --NoDataValue=-9999 --quiet --co COMPRESS=LZW BIGTIFF=YES --type=Byte",
               gfw_vrt, gfw_mask)
system(cmd)
result_masked_gfw_file <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor/class_masked_by_gfw.tif"
vrt_file <- tempfile(pattern = "class_masked_by_gfw_",
                       fileext = ".vrt")
gdalUtils::gdalbuildvrt(gdalfile = c(class_trans, gfw_mask),
                        output.vrt = vrt_file,
                        resolution = "highest",
                        allow_projection_difference = TRUE,
                        separate = TRUE,
                        q = TRUE)
cmd <- sprintf("gdal_calc.py -A %s --A_band=1 -B %s --B_band=2 --outfile=%s --calc='A * B' --NoDataValue=-0 --quiet --co COMPRESS=LZW BIGTIFF=YES --type=",
                   vrt_file, vrt_file, result_masked_gfw_file)
system(cmd)
result_masked_r <- raster::raster(result_masked_gfw_file)
result_masked_tb <- result_masked_r[] %>%
    na.omit() %>%
    as.vector() %>%
    tibble::as_tibble() %>%
    dplyr::rename(predicted = value) %>%
    dplyr::mutate(predicted = dplyr::recode(predicted,
                                             !!!labels,
                                             .default = NA_character_)) %>%
    dplyr::mutate(reference = "deforestation")
cm <- caret::confusionMatrix(data = factor(result_masked_tb$predicted,
                                     levels = labels),
                       reference = factor(result_masked_tb$reference,
                                          levels = labels))

cm$table
total <- sum(colSums(cm$table))
cm$table/total


# GFW 2018-2019
# > cm$table
# Reference
# Prediction      deforestation  forest natNonForest pasture
# deforestation       1816241       0            0       0
# forest               746363       0            0       0
# natNonForest         213768       0            0       0
# pasture             1066778       0            0       0
# > total <- sum(colSums(cm$table))
# > cm$table/total
# Reference
# Prediction      deforestation     forest natNonForest    pasture
# deforestation    0.47259175 0.00000000   0.00000000 0.00000000
# forest           0.19420605 0.00000000   0.00000000 0.00000000
# natNonForest     0.05562312 0.00000000   0.00000000 0.00000000
# pasture          0.27757907 0.00000000   0.00000000 0.00000000
#




# GFW 2019
# Prediction      deforestation  forest natNonForest pasture
# deforestation       1041035       0            0       0
# forest               465141       0            0       0
# natNonForest          71178       0            0       0
# pasture              143999       0            0       0
# > total <- sum(colSums(cm$table))
# > cm$table/total
# Reference
# Prediction      deforestation     forest natNonForest    pasture
# deforestation    0.60477717 0.00000000   0.00000000 0.00000000
# forest           0.27021825 0.00000000   0.00000000 0.00000000
# natNonForest     0.04135003 0.00000000   0.00000000 0.00000000
# pasture          0.08365454 0.00000000   0.00000000 0.00000000





# #---- Util ----
#
# # Helper for building a VRTs of classification files
# build_my_vrt <- function(file_path){
#     out_file <- tempfile(pattern = "class_file_",
#                          fileext = ".vrt")
#     gdalUtils::gdalbuildvrt(gdalfile = file_path,
#                             output.vrt = out_file,
#                             resolution = "highest")
#     invisible(out_file)
# }
#
# # Helper for projecting the classified images to PRODES' srs
# proj_class <- function(vrt_file){
#     out_file <- tempfile(pattern = "proj_class_",
#                          fileext = ".vrt")
#     gdalUtils::gdalwarp(srcfile = vrt_file,
#                         dstfile = out_file,
#                         t_srs = "EPSG:4674",
#                         of = "VRT")
#     return(out_file)
# }
#
#
#
# #---- Prepare PRODES ----
#
# prodes_file <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/prodes/2019/PDigital2000_2019_AMZ_gtif_077095.tif"
# stopifnot(file.exists(prodes_file))
#
# # Create a VRT with the required properties
# prodes_vrt <- tempfile(pattern = "prodes_",
#                        fileext = ".vrt")
# gdalUtils::gdalbuildvrt(gdalfile = prodes_file,
#                         tr = c(0.000090794469162, 0.000090794469162),
#                         output.vrt = prodes_vrt,
#                         vrtnodata = as.character(c(1:26, 28)),
#                         te = c(-64.8942687, -10.7006729,
#                                -64.7979358, -10.6082441))
#
# prodes_mask <- tempfile(pattern = "prodes_mask_",
#                         fileext = ".tif")
# cmd <- sprintf("gdal_calc.py -A %s --outfile=%s --calc='A==27' --NoDataValue=-9999",
#         prodes_vrt, prodes_mask)
# system(cmd)
# prodes_tif <- tempfile(pattern = "prodes_",
#                        fileext = ".tif")
# gdalUtils::gdalwarp(srcfile = prodes_mask,
#                     dstfile = prodes_tif,
#                     of = "GTiff",
#                     s_srs = "EPSG:4674",
#                     t_srs = "EPSG:4674",
#                     co = c("COMPRESS=LZW", "BIGTIFF=YES"),
#                     ot = "Int16",
#                     q = TRUE,
#                     srcnodata = -9999,
#                     dstnodata = -9999)
# r <- raster::raster(prodes_tif)
# plot(r)
#
# #----- Prepare classification results -----
#
# # Classification files
# files_tb <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results" %>%
#     list.files(pattern = "tif$",
#                full.names = TRUE,
#                recursive = TRUE) %>%
#     tibble::as_tibble() %>%
#     dplyr:::rename(file_path = value) %>%
#     dplyr::mutate(file_name = file_path %>%
#                       basename() %>%
#                       tools::file_path_sans_ext()) %>%
#     dplyr::filter(stringr::str_detect(file_name,
#                                       pattern = "_probs_class_")) %>%
#     tidyr::separate(col = file_name,
#                     into = c("mission", "sp_res", "tm_res", "cube", "tile",
#                              "map", "type", "start_year", "start_month",
#                              "end_year", "end_month", "version"),
#                     sep = "_") %>%
#     dplyr::filter(tile == "077095",
#                   map  == "probs",
#                   type == "class",
#                   start_year == "2018",
#                   end_year   == "2019",
#                   start_month == "7",
#                   end_month   == "7") %>%
#     dplyr::mutate(class_vrt = purrr::map_chr(file_path, build_my_vrt))
#
# vrt_to_tif <- function(vrt_file){
#     class_tif <- tempfile(pattern = "class_",
#                            fileext = ".tif")
#     gdalUtils::gdalwarp(srcfile = vrt_file,
#                         dstfile = class_tif,
#                         of = "GTiff",
#                         t_srs = "EPSG:4674",
#                         co = c("COMPRESS=LZW", "BIGTIFF=YES"),
#                         ot = "Int16",
#                         dstnodata = -9999,
#                         q = TRUE)
#     return(class_tif)
# }
#
# res <- files_tb %>%
#     dplyr::mutate(proj_vrt = purrr::map(class_vrt, proj_class)) %>%
#     dplyr::mutate(class_tif = purrr::map(proj_vrt, vrt_to_tif))
#
#
