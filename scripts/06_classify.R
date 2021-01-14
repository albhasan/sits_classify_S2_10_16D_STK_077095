.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")

library(sits)
library(dplyr)
library(snow)

source("./scripts/00_util.R")


#----- helper function ------

classify_subtile <- function(vrt_dir, out_dir, model_file, cube_name, satellite,
                             sensor, parse_info){
    start_time <- Sys.time()
    stopifnot(dir.exists(vrt_dir))
    stopifnot(dir.exists(out_dir))
    stopifnot(file.exists(model_file))

    if (!requireNamespace("randomForest", quietly = TRUE)) {
        stop("randomForest required for this function to work.
             Please install it.", call. = FALSE)
    }
    rfor_model <- readRDS(model_file)

    cube_name <- paste0(cube_name, "_", basename(vrt_dir))
    results_file <- paste0(out_dir, "/", cube_name, ".rda")
    if (file.exists(results_file))
        return(NULL)

    data_cube <- sits::sits_cube(type = "STACK",
                                 name = cube_name,
                                 satellite = satellite,
                                 sensor = sensor,
                                 data_dir = vrt_dir,
                                 delim = "_",
                                 bands = my_bands,
                                 parse_info = parse_info)

    s2_probs <- sits::sits_classify(data_cube,
                                    ml_model = rfor_model,
                                    memsize = 2,
                                    multicores = 1,
                                    output_dir = out_dir)

    s2_bayes <- sits::sits_smooth(type = "bayes",
                                  cube =  s2_probs,
                                  output_dir = out_dir)

    s2_labels <- sits::sits_label_classification(s2_bayes,
                                                 output_dir = out_dir)

    save(s2_probs, s2_bayes, s2_labels, file = results_file)

    return(c(Sys.time(), start_time))
}



#---- set up ----

num_of_trees <- 1000
my_bands <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")

samples_tb <- "./data/samples/samples.rds" %>%
    readRDS() %>%
    sits_select(my_bands)

#---- train model ----
model_file <- "./data/cube/077095_split_results/rfor_model.rds"
if (file.exists(model_file)) {
    if (!requireNamespace("randomForest", quietly = TRUE)) {
        stop("randomForest required for this function to work.
             Please install it.", call. = FALSE)
    }
    rfor_model <- readRDS(model_file)
} else {
    rfor_model <- sits::sits_train(samples_tb,
                                   ml_method = sits::sits_rfor(num_trees = num_of_trees))
    saveRDS(rfor_model, model_file)
}



#---- run in parallel on subtiles ----

vrt_dirs <- dir("/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/077095_split",
                full.names = TRUE)

out_dir <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/077095_split_results"
cube_name <- "S2_10_16D_STK_077095"
satellite <- "SENTINEL-2"
sensor <-  "MSI"
parse_info <- c("mission", "sp_resolution",
                "time_resolution", "type",
                "version", "tile", "date",
                "end_date", "band")

my_cluster <- snow::makeSOCKcluster(10)
snow::clusterEvalQ(my_cluster,
                   .libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0"))
time_start_end <- snow::clusterApplyLB(cl = my_cluster, x = vrt_dirs, fun = classify_subtile,
                                       out_dir = out_dir,
                                       model_file = model_file,
                                       cube_name = cube_name,
                                       satellite = satellite,
                                       sensor = sensor,
                                       parse_info = parse_info)
snow::stopCluster(my_cluster)

saveRDS(time_start_end, file = paste0(out_dir, "/processing_time.rds"))