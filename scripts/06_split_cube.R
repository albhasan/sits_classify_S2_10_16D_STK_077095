# split cube into minicubes

library(magrittr)
library(dplyr)
library(raster)
library(snow)

source("./scripts/00_util.R")


#---- Helper function -----


#' Build VRT files for the input files
#'
#'@param cl          An snow cluster object.
#'@param input_files A character of path to image files.
#'@out_dir           A path to a directory.
#'@return            NULL
construct_vrt <- function(cl, input_files, v_size, h_size, out_dir){
    if (any(grepl("/vsicurl", input_files))) {
        BDC_ACCESS_KEY <- Sys.getenv("BDC_ACCESS_KEY")
        stopifnot(BDC_ACCESS_KEY != "")
        input_files <- paste0(input_files, "?access_token=", BDC_ACCESS_KEY)
    }
    lin_col <- dim(raster::raster(input_files[1]))

    grid <- build_grid(n_row = lin_col[[1]],
                       n_col = lin_col[[2]],
                       v_size = v_size,
                       h_size = h_size)

    snow::clusterExport(cl, list("build_vrt", "grid"))

    stopifnot(length(dir(out_dir)) == 0)

    vrt_files <- snow::clusterApplyLB(cl, x = input_files, fun = function(file_path,
                                                                          out_dir){
        vrt_files <- build_vrt(grid = grid,
                               image_path = file_path,
                               out_dir = out_dir,
                               out_file = paste0(tools::file_path_sans_ext(basename(file_path)),
                                                 ".vrt"))
        return(vrt_files)
    },
    out_dir = out_dir)

    stopifnot(length(input_files) == length(vrt_files))
    stopifnot(all(vapply(vrt_files, length, integer(1)) == nrow(grid)))
    saveRDS(grid, paste0(out_dir, "/grid.rds"))
}

#---- Query RSTAC ----

# source("~/Documents/bdc_access_key.R")
# BDC_ACCESS_KEY <- Sys.getenv("BDC_ACCESS_KEY")
# stopifnot(BDC_ACCESS_KEY != "")
# # Query the STAC service.
# stac_obj <- "http://brazildatacube.dpi.inpe.br/stac/" %>%
#     rstac::stac() %>%
#     rstac::stac_search(collections = "S2_10_16D_STK-1",
#                 bbox = c(xmin = -1 * (65 + (16/60) + (31.01/3600)),
#                          ymin = -1 * (10 + (55/60) + (30.19/3600)),
#                          xmax = -1 * (63 + (40/60) + (49.16/3600)),
#                          ymax = -1 * (10 + ( 0/60) + (11.25/3600)))) %>%
#     rstac::post_request() %>%
#     ensurer::ensure_that(.$context$returned > 0,
#                          err_desc = "No cubes found!")
# # Retrive the records.
# items <- stac_obj %>%
#     rstac::items_fetch()
# # Select a set of bands.
# my_bands <- setdiff(unlist(unique(rstac::items_bands(items))),
#                     c("CLEAROB", "TOTALOB", "PROVENANCE", "thumbnail"))
# file_vec <- rstac::assets_list(items = items,
#                                assets_names = my_bands) %>%
#     as.data.frame() %>%
#     magrittr::extract2("path")


#---- Query local directory ----

file_vec <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/077095" %>%
    list.files(pattern = ".tif$",
               full.names = TRUE,
               recursive = FALSE)


#---- Script ----

out_dir <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/077095_split"

my_cluster <- snow::makeSOCKcluster(24)
construct_vrt(cl = my_cluster,
              input_files = file_vec,
              h_size = 512,
              v_size = 10986,
              out_dir = out_dir)
snow::stopCluster(my_cluster)
