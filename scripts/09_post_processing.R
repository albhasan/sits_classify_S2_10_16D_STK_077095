.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")
source("~/Documents/bdc_access_key.R")

library(dplyr)
library(readr)
library(sits)
library(stringr)



#---- Configuration ----

# Level model (this is for the BIOME)
my_tiles            <- "077095"

# Level data (for list of tiles in the BIOME)
my_cube       <- "amazon_S2_10_16D_STK"
samples_file  <- "./data/samples/samples_tb.rds"

my_model_files <- c("./results/paper_defor_rf/ml_model.rds",
                    "./results/paper_defor_resnet/ml_model.rds",
                    "./results/paper_defor_tempcnn/ml_model.rds")
stopifnot(file.exists(samples_file))
stopifnot(all(file.exists(my_model_files)))

source("./scripts/00_util.R")

#---- Helper functions ----

# Helper function for processin sits's probability files.
process_probs <- function(model_file, prob_files, data_cube) {
    out_dir <- model_file %>%
        dirname()
    # Validate if processing is already done.
    labels_file <- file.path(out_dir, "labels.txt")
    if (file.exists(labels_file)) {
        warning(sprintf("This file was already processed: %s", model_file))
        return(FALSE)
    }
    # Get the classification's labels from the sits' model file.
    model_labels <- model_file %>%
        readRDS() %>%
        environment() %>%
        magrittr::extract2("data") %>%
        dplyr::pull(label) %>%
        unique() %>%
        sort() %>%
        return()
    # Get the start dates of the images in the sits's cube.
    cube_date_range <- data_cube %>%
        sits::sits_timeline() %>%
        range()
    # Process the sits's probability files of produced with the given model file.
    for (my_file in prob_files) {
        probs_cube <- sits::sits_cube(source = "PROBS",
                                      name = my_file %>%
                                          basename() %>%
                                          tools::file_path_sans_ext(),
                                      satellite = "SENTINEL-2",
                                      sensor = "MSI",
                                      start_date = cube_date_range[1],
                                      end_date   = cube_date_range[2],
                                      probs_labels = model_labels,
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
    # Write labels to a text file.
    model_labels %>%
        readr::write_lines(file = labels_file)
    return(TRUE)
}



#---- Script ----

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

model_tb <- my_model_files %>%
    tibble::as_tibble() %>%
    dplyr::rename(model_file = value) %>%
    # NOTE: Assume the results are hosted along with the model.
    dplyr::mutate(out_dir = purrr::map(model_file, dirname),
                  model_type = purrr::map_chr(model_file, get_model_type),
                  out_dir = purrr::map_chr(model_file, dirname),
                  # NOTE: There could be more than one probability file in each dir.
                  prob_files = purrr::map(out_dir, get_prob_files,
                                          cube_name = my_cube),
                  processed = purrr::map2_lgl(model_file, prob_files,
                                              process_probs,
                                              data_cube = data_cube))
