# Validate the results of the classification

library(dplyr)
library(gdalUtils)
library(purrr)
library(raster)
library(stringr)


#---- Util ----

# Helper for building a VRTs of classification files
build_my_vrt <- function(file_path){
    out_file <- tempfile(pattern = "class_file_",
                         fileext = ".vrt")
    gdalUtils::gdalbuildvrt(gdalfile = file_path,
                            output.vrt = out_file,
                            resolution = "highest")
    invisible(out_file)
}

# Helper for projecting the classified images to PRODES' srs
proj_class <- function(vrt_file){
    out_file <- tempfile(pattern = "proj_class_",
                         fileext = ".vrt")
    gdalUtils::gdalwarp(srcfile = vrt_file,
                        dstfile = out_file,
                        t_srs = "EPSG:4674",
                        of = "VRT")
    return(out_file)
}



#---- Prepare PRODES ----

prodes_file <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/prodes/2019/PDigital2000_2019_AMZ_gtif_077095.tif"
stopifnot(file.exists(prodes_file))

# Create a VRT with the required properties
prodes_vrt <- tempfile(pattern = "prodes_",
                       fileext = ".vrt")
gdalUtils::gdalbuildvrt(gdalfile = prodes_file,
                        tr = c(0.000090794469162, 0.000090794469162),
                        output.vrt = prodes_vrt,
                        vrtnodata = as.character(c(1:26, 28)),
                        te = c(-64.8942687, -10.7006729,
                               -64.7979358, -10.6082441))

prodes_mask <- tempfile(pattern = "prodes_mask_",
                        fileext = ".tif")
cmd <- sprintf("gdal_calc.py -A %s --outfile=%s --calc='A==27' --NoDataValue=-9999",
        prodes_vrt, prodes_mask)
system(cmd)
prodes_tif <- tempfile(pattern = "prodes_",
                       fileext = ".tif")
gdalUtils::gdalwarp(srcfile = prodes_mask,
                    dstfile = prodes_tif,
                    of = "GTiff",
                    s_srs = "EPSG:4674",
                    t_srs = "EPSG:4674",
                    co = c("COMPRESS=LZW", "BIGTIFF=YES"),
                    ot = "Int16",
                    q = TRUE,
                    srcnodata = -9999,
                    dstnodata = -9999)
r <- raster::raster(prodes_tif)
plot(r)

#----- Prepare classification results -----

# Classification files
files_tb <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results" %>%
    list.files(pattern = "tif$",
               full.names = TRUE,
               recursive = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr:::rename(file_path = value) %>%
    dplyr::mutate(file_name = file_path %>%
                      basename() %>%
                      tools::file_path_sans_ext()) %>%
    dplyr::filter(stringr::str_detect(file_name,
                                      pattern = "_probs_class_")) %>%
    tidyr::separate(col = file_name,
                    into = c("mission", "sp_res", "tm_res", "cube", "tile",
                             "map", "type", "start_year", "start_month",
                             "end_year", "end_month", "version"),
                    sep = "_") %>%
    dplyr::filter(tile == "077095",
                  map  == "probs",
                  type == "class",
                  start_year == "2018",
                  end_year   == "2019",
                  start_month == "7",
                  end_month   == "7") %>%
    dplyr::mutate(class_vrt = purrr::map_chr(file_path, build_my_vrt))

vrt_to_tif <- function(vrt_file){
    class_tif <- tempfile(pattern = "class_",
                           fileext = ".tif")
    gdalUtils::gdalwarp(srcfile = vrt_file,
                        dstfile = class_tif,
                        of = "GTiff",
                        t_srs = "EPSG:4674",
                        co = c("COMPRESS=LZW", "BIGTIFF=YES"),
                        ot = "Int16",
                        dstnodata = -9999,
                        q = TRUE)
    return(class_tif)
}

res <- files_tb %>%
    dplyr::mutate(proj_vrt = purrr::map(class_vrt, proj_class)) %>%
    dplyr::mutate(class_tif = purrr::map(proj_vrt, vrt_to_tif))


