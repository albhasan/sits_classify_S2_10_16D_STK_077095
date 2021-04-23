.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")
source("~/Documents/bdc_access_key.R")

Sys.setenv("__SITS_DEBUG__" = TRUE)
Sys.setenv("__SITS_RESUME__" = TRUE)

library(sits)
library(dplyr)



#---- Configuration ----

my_cube         <- "amazon_S2_10_16D_STK"
my_tiles        <- "077095"
my_samples_file <- "./data/samples/samples_tb.rds"

my_model_files <- c("./results/paper_defor_rf/ml_model.rds",
                    "./results/paper_defor_resnet/ml_model.rds",
                    "./results/paper_defor_tempcnn/ml_model.rds")

stopifnot(file.exists(my_samples_file))
stopifnot(all(file.exists(my_model_files)))
stopifnot(all(dir.exists(dirname(my_model_files))))



#---- Classify ----

samples_time <- my_samples_file %>%
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

for (model_file in my_model_files) {
    start_time = Sys.time()
    out_dir <- dirname(model_file)
    probs <- sits::sits_classify(data_cube,
                                 ml_model = readRDS(model_file),
                                 memsize = 15,
                                 multicores = 10,
                                 output_dir = out_dir)
    probs <- dplyr::mutate(probs,
                           processing = tibble::tibble(start_time = start_time,
                                                       end_time = Sys.time()))
    saveRDS(probs, file = file.path(out_dir, paste0(my_cube, "_results.rds")))
    rm(probs)
    gc()
}
