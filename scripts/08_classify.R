.libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0")
Sys.setenv(SITS_USER_CONFIG_FILE = "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/config.yml")

library(sits)
library(dplyr)
library(snow)

source("./scripts/00_util.R")


#----- helper function ------

merge_subtiles <- function(cube_name, split_out_dir, grid){
    require(dplyr)

    merged_tb <- list.files(split_out_dir,
                            pattern = paste0(cube_name, ".*\\.rds$"),
                            full.names = TRUE) %>%
        lapply(readRDS) %>%
        dplyr::bind_rows() %>%
        dplyr::pull(file_info) %>%
        dplyr::bind_rows() %>%
        dplyr::select(-band) %>%
        tidyr::nest(paths = path) %>%
        dplyr::mutate(band = paste0(merge_out_dir,
                                    "/",
                                    cube_name, "_probs_",
                                    lubridate::year(date),
                                    "_",
                                    lubridate::month(date))) %>%
        dplyr::mutate(merged = paste0(band, ".tif"))

    purrr::map2(.x = merged_tb$paths,
                .y = merged_tb$merged,
                .f = function(paths, merged){
                    stopifnot(nrow(grid) == nrow(paths))
                    gdalUtils::gdalwarp(srcfile = paths$path,
                                        dstfile = merged,
                                        co = c("COMPRESS=LZW", "BIGTIFF=YES"))
                })

    return(merged_tb)
}


classify_subtile <- function(vrt_dir, out_dir, model_file, cube_name, satellite,
                             sensor, parse_info){
    start_time <- Sys.time()
    stopifnot(dir.exists(vrt_dir))
    stopifnot(dir.exists(out_dir))
    stopifnot(file.exists(model_file))
    cube_name <- paste0(cube_name, "_", basename(vrt_dir))
    results_file <- paste0(out_dir, "/", cube_name, ".rds")
    if (file.exists(results_file))
        return(NULL)

    if (!requireNamespace("randomForest", quietly = TRUE)) {
        stop("randomForest required for this function to work.
             Please install it.", call. = FALSE)
    }
    ml_model <- readRDS(model_file)

    data_cube <- sits::sits_cube(type = "STACK",
                                 name = cube_name,
                                 satellite = satellite,
                                 sensor = sensor,
                                 data_dir = vrt_dir,
                                 delim = "_",
                                 parse_info = parse_info)

    s2_probs <- sits::sits_classify(data_cube,
                                    ml_model = ml_model,
                                    memsize = 2,
                                    multicores = 1,
                                    output_dir = out_dir)
    s2_probs <- dplyr::mutate(s2_probs,
                              processing =
                                  tibble::tibble(start_time = start_time,
                                                 end_time = Sys.time()))
    saveRDS(s2_probs, file = results_file)

    return(NULL)
}

split_clusterR <- function(x, n_tiles, pad_rows, fun,
                           args = NULL, export = NULL, cl = NULL, ...) {

    stopifnot(n_tiles > 0)
    stopifnot(pad_rows >= 0)

    breaks <- ceiling(seq(1, nrow(x) + 1, length.out = n_tiles + 1))
    breaks <- mapply(list,
                     r1 = ifelse(breaks - pad_rows <= 0, 1,
                                 breaks - pad_rows)[seq_len(n_tiles)],
                     r2 = ifelse(breaks + pad_rows - 1 > nrow(x), nrow(x),
                                 breaks + pad_rows - 1)[-1:0], SIMPLIFY = FALSE,
                     orig1 = ifelse(breaks - pad_rows <= 0, 1,
                                    pad_rows + 1)[seq_len(n_tiles)],
                     orig2 = ifelse(breaks - pad_rows <= 0,
                                    breaks[-1:0] - breaks[seq_len(n_tiles)],
                                    breaks[-1:0] - breaks[seq_len(n_tiles)]
                                    + pad_rows)[-1:0])

    if (is.null(cl)) {
        cl <- raster::getCluster()
        on.exit(raster::returnCluster(), add = TRUE)
        stopifnot(!is.null(cl))
    }

    # export

    if (!is.null(export)) {
        parallel::clusterExport(cl, export)
    }

    # start process cluster
    pb <- txtProgressBar(max = length(breaks) + 1, style = 3)

    .arg_fun <- function(i) {
        setTxtProgressBar(pb, i)
        c(list(b = breaks[[i]]), x = x, fun = fun, args)
    }

    .io_fun <- function(b, x, fun, ...) {

        # crop adding pads
        x <- raster::crop(x, raster::extent(x, r1 = b$r1, r2 = b$r2,
                                            c1 = 1, c2 = ncol(x)))

        # process it
        res <- fun(x, ...)
        stopifnot(inherits(res, c("RasterLayer", "RasterStack", "RasterBrick")))

        # crop removing pads
        res <- raster::crop(res, raster::extent(res,
                                                r1 = b$orig1,
                                                r2 = b$orig2 + 1,
                                                c1 = 1,
                                                c2 = ncol(res)))

        # export to temp file
        filename <- tempfile(fileext = ".tif")
        raster::writeRaster(res, filename = filename, overwrite = TRUE)

        filename
    }

    tmp_tiles <- snow::dynamicClusterApply(cl = cl,
                                           fun = .io_fun,
                                           n = length(breaks),
                                           argfun = .arg_fun)

    setTxtProgressBar(pb, length(breaks) + 1)
    close(pb)
    on.exit(unlink(tmp_tiles))
    # end process cluster

    # merge to save final result with '...' parameters
    message("Merging files...", appendLF = TRUE)
    do.call(raster::merge, c(lapply(tmp_tiles, raster::brick), list(...)))
}



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

stopifnot(file.exists(paste0(split_dir, "/grid.rds")))

vrt_dirs <- list.dirs(split_dir,
                      full.names = TRUE,
                      recursive = FALSE)

my_cluster <- snow::makeSOCKcluster(20)
snow::clusterEvalQ(my_cluster,
                   .libPaths("/home/alber.ipia/R/x86_64-pc-linux-gnu-library/4.0"))
time_start_end <- snow::clusterApplyLB(cl = my_cluster,
                                       x = vrt_dirs,
                                       fun = classify_subtile,
                                       out_dir = split_out_dir,
                                       model_file = model_file,
                                       cube_name = cube_name,
                                       satellite = satellite,
                                       sensor = sensor,
                                       parse_info = parse_info)
snow::stopCluster(my_cluster)

saveRDS(time_start_end, file = paste0(split_out_dir, "/processing_time.rds"))

my_grid <- readRDS(paste0(split_dir, "/grid.rds"))

merged_tb <- merge_subtiles(cube_name = cube_name,
                            split_out_dir = split_out_dir,
                            grid = my_grid)
