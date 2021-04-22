.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")
source("~/Documents/bdc_access_key.R")

library(dplyr)
library(readr)
library(sits)



#---- Configuration ----

# Level model (this is for the BIOME)
my_tiles            <- "077095"

# Level data (for list of tiles in the BIOME)
my_cube       <- "amazon_S2_10_16D_STK"
samples_file  <- "./data/samples/samples_tb.rds"

model_file    <- "./results/paper_defor_rf/ml_model.rds"
out_dir       <- dirname(model_file)

stopifnot(file.exists(model_file))
stopifnot(dir.exists(out_dir))
stopifnot(file.exists(samples_file))

samples_time <- samples_file %>%
    readRDS() %>%
    dplyr::select(start_date, end_date) %>%
    purrr::map(.f = unique) %>%
    unlist(recursive = FALSE) %>%
    lubridate::as_date() %>%
    sort() %>%
    ensurer::ensure_that(length(.) == 2,
                         err_desc = "Inconsistent number of dates!")

data_cube <- sits::sits_cube(source = "BDC",
                             name = my_cube,
                             url = "http://datacube-005.dpi.inpe.br:8010/stac/",
                             collection = "S2_10_16D_STK-1",
                             tiles = my_tiles,
                             start_date = dplyr::first(samples_time),
                             end_date   = dplyr::last(samples_time))

ml_model <- readRDS(model_file)
my_labels <- ml_model %>%
    environment() %>%
    magrittr::extract2("data") %>%
    dplyr::pull(label) %>%
    unique() %>%
    sort()

my_labels %>%
    readr::write_lines(file = file.path(out_dir, "labels.txt"))

cube_date_range <- data_cube %>%
    sits::sits_timeline() %>%
    range()

prob_files <- out_dir %>%
    list.files(pattern = paste0("^", my_cube, ".+tif$"),
               full.names = TRUE) %>%
    ensurer::ensure_that(length(.) == length(my_tiles),
                         err_desc = "Probability file not found!")

for (my_file in prob_files) {
    probs_cube <- sits::sits_cube(source = "PROBS",
                                  name = my_file %>%
                                      basename() %>%
                                      tools::file_path_sans_ext(),
                                  satellite = "SENTINEL-2",
                                  sensor = "MSI",
                                  start_date = cube_date_range[1],
                                  end_date   = cube_date_range[2],
                                  probs_labels = my_labels,
                                  probs_files = my_file)

    bayesian <- sits::sits_smooth(probs_cube,
                                  type = "bayes",
                                  window_size = 5,
                                  multicores = 10,
                                  memsize = 2,
                                  output_dir = out_dir)

    sits::sits_label_classification(bayesian,
                                    multicores = 10,
                                    memsize = 2,
                                    output_dir = out_dir)
}
