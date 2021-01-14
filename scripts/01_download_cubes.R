# Download images from the Brazil Data Cubes website.

library(dplyr)
library(readr)
library(sits)
library(tidyr)



#---- Setup ----

#Sys.setenv( "BDC_SECRET_ACCESS_KEY" = <your_secret_access_key>)
access_key <- Sys.getenv("BDC_SECRET_ACCESS_KEY")
stopifnot(access_key != "")



#---- Script ----

image_tb <- "./scripts/S2_10_16D_STK-1.txt" %>%
    readr::read_delim(delim = " ",
                      col_names = FALSE,
                      col_types = "c") %>%
    magrittr::set_names("img_url") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(file_name = tools::file_path_sans_ext(basename(img_url))) %>%
    tidyr::separate(file_name, sep = "_",
                    into = c("mission", "sp_resolution", "time_resolution",
                             "type", "version", "tile", "start_date",
                             "end_date", "band")) %>%
    dplyr::filter(band %in% c("band1", "band11", "band12", "band2", "band3",
                              "band4", "band5", "band6", "band7", "band8",
                              "band8a", "EVI", "Fmask4", "NDVI")) %>%
    dplyr::mutate(url = stringr::str_c(img_url, "?access_token=",
                                           access_key),
                  destfile = stringr::str_c("/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/",
                                               tile, "/", basename(img_url)))
image_tb <- image_tb %>%
    dplyr::mutate(dowloaded = purrr::map2_int(url, destfile, download.file,
                                         method = "auto", quiet = TRUE))
image_tb <- image_tb %>%
    ensurer::ensure_that(all(.$dowloaded == 1),
                         err_desc = "Not all images were downloaded!")