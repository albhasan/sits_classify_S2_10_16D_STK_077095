.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")
Sys.setenv(SITS_USER_CONFIG_FILE = "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/config.yml")

library(dplyr)
library(sits)

#TODO: Is there a way not to repeat so much code from 08_classify.R?

#---- set up level classification ----

## Level model (this is for the BIOME)
classification_name <- "paper_defor"
satellite           <- "SENTINEL-2"
sensor              <- "MSI"
my_bands            <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")

## Level data (for list of tiles in the BIOME)
project_dir   <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095"
parse_info    <- c("mission", "sp_resolution",
                   "time_resolution", "type",
                   "version", "tile", "date",
                   "end_date", "band")
merge_out_dir <- paste0(project_dir, "/results/", classification_name)

# NOTE: These variables must match the number of tiles for classification.
split_dirs     <- paste0(project_dir,
                         c("/data/cube/077095_split"))
split_out_dirs <- paste0(project_dir, "/results/", classification_name,
                         c("/077095_split"))
tile_names     <- "S2_10_16D_STK_077095"

stopifnot(length(split_dirs) == length(split_out_dirs))
stopifnot(length(split_dirs) == length(tile_names))


#---- set up level tile ----

split_dir     <- split_dirs[[1]]
split_out_dir <- split_out_dirs[[1]]
cube_name     <- tile_names[[1]]
model_file    <- paste0("./results/",
                        classification_name,
                        "/ml_model.rds")

if (file.exists(model_file)) {
    if (!requireNamespace("randomForest", quietly = TRUE)) {
        stop("randomForest required for this function to work.
             Please install it.", call. = FALSE)
    }
    ml_model <- readRDS(model_file)
} else {
    stop("Model file not found!")
}

class_cube <- sits::sits_cube(type = "STACK",
                              name = cube_name,
                              satellite = satellite,
                              sensor = sensor,
                              # NOTE: Use here the cube instead of the VRTs!"
                              data_dir = "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/077095",
                              delim = "_",
                              parse_info = parse_info)

probs_file <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor/S2_10_16D_STK_077095_probs_2018_7.tif"
stopifnot(file.exists(probs_file))

probs_cube <- sits::sits_cube(type = "PROBS",
                              names = classification_name,
                              satellite = satellite,
                              sensor = sensor,
                              timeline = sits::sits_timeline(class_cube),
                              labels = sort(unique(environment(ml_model)$data$label)),
                              files = probs_file)



#smoother_type <- "bayes"
smoother_type <- "bilinear"
for (i in c(5, 9, 15, 19, 25)) {
    print(sprintf("%s Processing window size %s using smoother...",
                  Sys.time(), i, smoother_type))
    my_out_dir <- paste0(merge_out_dir, "/", paste0(i, "x", i), "_",
                         smoother_type)
    if (!dir.exists(my_out_dir))
        dir.create(my_out_dir)

    smoother <- sits::sits_smooth(probs_cube,
                                  type = smoother_type,
                                  window_size = i,
                                  multicores = 12,
                                  memsize = 36,
                                  output_dir = my_out_dir)
    print(smoother)
    print(Sys.time())
}
print(sprintf("%s Processing finished", Sys.time()))