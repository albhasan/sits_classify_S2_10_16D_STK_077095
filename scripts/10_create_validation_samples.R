library(dplyr)
library(ensurer)
library(sf)
library(terra)

resnet_file  <- "/home/alber-d005/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor_resnet/amazon_S2_10_16D_STK_class_2018-07-12_2019-07-28_v1_class_bayes_v1.tif"
tempcnn_file <- "/home/alber-d005/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor_rf/amazon_S2_10_16D_STK_class_2018-07-12_2019-07-28_v1_class_bayes_v1.tif"
rf_file      <- "/home/alber-d005/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor_tempcnn/amazon_S2_10_16D_STK_class_2018-07-12_2019-07-28_v1_class_bayes_v1.tif"
out_dir      <- "/home/alber-d005/Documents/sits_classify_S2_10_16D_STK_077095/data/validation/"

stopifnot(dir.exists(out_dir))

# Helper function for saving a sf object (x) using a template file path (y) and
# and output directory (out_dir).
sf_to_shp <- function(x, y, out_dir){
    # Get the parent directory of the template because it includes the algorithm.
    my_dir <- y %>%
        dirname() %>%
        basename()
    # Get the template file name and change its extension to shp.
    out_name <- y %>%
        basename() %>%
        tools::file_path_sans_ext() %>%
        paste0(".shp")
    # Bild a new out name using the new out_name, the parent directory and the
    # output directory.
    out_name <- file.path(out_dir,
                          my_dir,
                          out_name)
    if (!dir.exists(dirname(out_name)))
        dir.create(dirname(out_name))
    sf::write_sf(x, out_name)
    return(TRUE)
}

set.seed(123)
data_tb <- tibble::tibble(file_path = c(resnet_file, tempcnn_file, rf_file)) %>%
    ensurer::ensure_that(all(file.exists(.$file_path)),
                         err_desc = "File not found!") %>%
    dplyr::mutate(r = purrr::map(file_path, terra::rast)) %>%
    dplyr::mutate(samples = purrr::map(r, terra::spatSample,
                                       size = 10000,
                                       method = "random",
                                       as.points = TRUE )) %>%
    dplyr::mutate(samples_sf = purrr::map(samples, sf::st_as_sf)) %>%
    dplyr::mutate(sub_samples = purrr::map(samples_sf, function(x) {
        x %>%
            dplyr::group_by(lyr1) %>%
            dplyr::sample_n(size = 150) %>%
            return()
    })) %>%
    dplyr::mutate(n_samples = purrr::map_int(sub_samples, nrow)) %>%
    ensurer::ensure_that(all(.$n_samples == 600),
                         err_desc = "Missing samples") %>%
    dplyr::mutate(export2shp = purrr::map2_lgl(sub_samples, file_path,
                                               sf_to_shp, out_dir = out_dir))
