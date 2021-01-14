library(magrittr)
library(dplyr)
library(sf)
library(sits)

setwd("/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095")

source("./scripts/00_util.R")

num_of_trees <- 1000
my_bands <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")
#my_tile <-
my_version <- "v013b"
data_cube <- get_cube("mini_2")

my_model <- paste("rf", num_of_trees, sep = "-")

samples_tb <- "./data/samples/samples_paper.rds" %>%
    readRDS() %>%
    sits_select(my_bands) %>%
    dplyr::mutate(start_date = dplyr::first(sits::sits_timeline(data_cube)),
                  end_date   = dplyr::last(sits::sits_timeline(data_cube)))
    # dplyr::mutate(label = dplyr::recode(label,
    #                                     "Deforestation" = "Deforestation",
    #                                     "Forest"        = "Forest",
    #                                     "NatNonForest"  = "Pasture",
    #                                     "Pasture"       = "Pasture"))

rfor_model <- sits::sits_train(samples_tb,
                               ml_method = sits::sits_rfor(num_trees = num_of_trees))

dest_dir <- file.path(getwd(), "results",
                      my_model,
                      paste(sort(my_bands), collapse = "-"),
                      my_version)
dir.create(dest_dir, recursive = TRUE)

sits::sits_timeline(samples_tb)
sits::sits_timeline(data_cube)

s2_probs <- sits::sits_classify(data_cube,
                                ml_model = rfor_model,
                                memsize = 2,
                                multicores = 1,
                                output_dir = dest_dir)
label_tb <- sits::sits_labels(s2_probs) %>%
    tibble::as_tibble() %>%
    dplyr::rename(label = "value") %>%
    dplyr::mutate(id = 1:nrow(.)) %>%
    dplyr::select(id, label)
label_tb %>%
    write.csv(file.path(dest_dir, "sits_labels.csv"))
s2_bayes <- sits::sits_smooth_bayes(s2_probs,
                                    output_dir = dest_dir)
s2_label <- sits::sits_label_classification(s2_bayes,
                                            output_dir = dest_dir)

prob_file <- s2_probs$file_info[[1]]$path
entropy_file <- prob_file %>%
    stringr::str_replace(pattern = "_v1.tif",
                         replacement = "_entropy_v1.tif") %>%
    ensurer::ensure_that(. != prob_file)

compute_entropy(prob_file,
                out_file = entropy_file)
