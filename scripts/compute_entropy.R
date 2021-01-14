library(dplyr)
library(stringr)

source("./scripts/00_util.R")

prob_files_tb <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results" %>%
    list.files(pattern = "[.]tif$", full.names = TRUE, recursive = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(file_name = tools::file_path_sans_ext(basename(file_path))) %>%
    tidyr::separate(file_name,
                    into = c("mission", "sp_res", "tm_res", "cube", "tile",
                             "type", "syear", "smonth", "eyear", "emonth", "v")) %>%
    dplyr::filter(v == "v1") %>%
    dplyr::mutate(out_file = stringr::str_replace(file_path,
                                                  pattern = "_v1.tif",
                                                  replacement = "_entropy_v1.tif")) %>%
    dplyr::mutate(res = purrr::map2_chr(file_path, out_file, compute_entropy))



