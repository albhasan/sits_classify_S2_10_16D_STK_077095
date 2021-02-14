.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")

library(sits)
library(dplyr)

source("./scripts/00_util.R")

## Level model (this is for the BIOME)
classification_name <- "paper_bayes"
satellite           <- "SENTINEL-2"
sensor              <- "MSI"
my_bands            <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")
ml_method           <- sits::sits_rfor(num_trees = 2000)

## Level data (for list of tiles in the BIOME)
project_dir   <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095"
parse_info    <- c("mission", "sp_resolution",
                   "time_resolution", "type",
                   "version", "tile", "date",
                   "end_date", "band")
merge_out_dir <- paste0(project_dir, "/results/", classification_name)

# NOTE: These variables must match the number of tiles for classification.
split_dirs     <- paste0(project_dir,
                         c("/data/cube/077095_split_bayes"))
split_out_dirs <- paste0(project_dir, "/results/", classification_name,
                         c("/077095_split"))
tile_names     <- "S2_10_16D_STK_077095"

stopifnot(length(split_dirs) == length(split_out_dirs))
stopifnot(length(split_dirs) == length(tile_names))


#---- set up level tile ----

split_dir     <- split_dirs[[1]]
split_out_dir <- split_out_dirs[[1]]
cube_name     <- tile_names[[1]]
model_file    <- paste0("./results/paper_defor/ml_model.rds")

stopifnot(dir.exists(merge_out_dir))
if (!dir.exists(split_dir))
    dir.create(split_dir)
if (!dir.exists(split_out_dir))
    dir.create(split_out_dir)
stopifnot(dir.exists(split_dir))
stopifnot(dir.exists(split_out_dir))
stopifnot(file.exists(model_file))


#---- run level classification ----


if (file.exists(model_file)) {
    if (!requireNamespace("randomForest", quietly = TRUE)) {
        stop("randomForest required for this function to work.
             Please install it.", call. = FALSE)
    }
    ml_model <- readRDS(model_file)
} else {
    stop("Model file not found!")
}



#---- run level tile ----

samples_tb <- readRDS("~/Documents/sits_classify_S2_10_16D_STK_077095/data/samples/samples.rds") %>%
    sits::sits_select(my_bands)
ml_model <- sits::sits_rfor(data = samples_tb,
                            num_trees = 2000)

cube <- get_cube(cube = "mini_bayes")

probs <- sits::sits_classify(data = cube,
                             ml_model = ml_model,
                             output_dir = merge_out_dir,
                             version = "v1",
                             multicores = 23,
                             memory = 40)



#---- Post processing ----

bayes <- sits::sits_smooth(cube = probs,
                           type = "bayes",
                           memory = 0.1,
                           multicores = 20,
                           output_dir = merge_out_dir)

