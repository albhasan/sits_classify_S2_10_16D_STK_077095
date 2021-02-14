TODO: Adapt from Sinop to current classification
TODO: Convert from logit back to probabilities


#' @title parallel raster brick
#' @name .sits_split_cluster
#' @keywords internal
#'
#' @description Process chunks of raster brick in parallel.
#'
#' @param file             a path to raster file.
#' @param n_tiles          number of horizontal chunks to break the raster.
#' @param pad_rows         number of overlaping rows of each chunk.
#' @param fun              a function that receives RasterBrick and
#'                         returns any Raster*.
#' @param args             additional arguments to pass to \code{fun} function.
#' @param cl               snow cluster object
#' @param ...              optional arguments to merge final raster
#'                         (see \link[raster]{writeRaster} function)
#'
#' @return  RasterBrick object
#'
sits_split_cluster <- function(file, n_tiles, pad_rows, fun, args = NULL,
                                cl = NULL, ...) {

    stopifnot(file.exists(file))
    stopifnot(n_tiles > 0)
    stopifnot(pad_rows >= 0)

    # open brick
    b <- raster::brick(file)

    # define blocks
    blocks <- ceiling(seq(1, nrow(b) + 1, length.out = n_tiles + 1))
    blocks <- mapply(list,
                     r1 = ifelse(blocks - pad_rows <= 0, 1,
                                 blocks - pad_rows)[seq_len(n_tiles)],
                     r2 = ifelse(blocks + pad_rows - 1 > nrow(b), nrow(b),
                                 blocks + pad_rows - 1)[-1:0], SIMPLIFY = FALSE,
                     orig1 = ifelse(blocks - pad_rows <= 0, 1,
                                    pad_rows + 1)[seq_len(n_tiles)],
                     orig2 = ifelse(blocks - pad_rows <= 0,
                                    blocks[-1:0] - blocks[seq_len(n_tiles)],
                                    blocks[-1:0] - blocks[seq_len(n_tiles)]
                                    + pad_rows + 1)[-1:0])

    # start progress bar
    pb <- txtProgressBar(max = length(blocks) + 1, style = 3)

    .arg_fun <- function(i) {

        setTxtProgressBar(pb, i)
        c(list(block = blocks[[i]]), x = file, fun = fun, args)
    }

    .sits_cluster_block_fun <- function(block, x, fun, ...) {

        # open brick
        b <- raster::brick(x)

        # crop adding overlaps
        x <- raster::crop(b, raster::extent(b,
                                            r1 = block$r1,
                                            r2 = block$r2,
                                            c1 = 1,
                                            c2 = ncol(b)))

        # process it
        res <- fun(x, ...)
        stopifnot(inherits(res, c("RasterLayer", "RasterStack", "RasterBrick")))

        # crop removing overlaps
        res <- raster::crop(res, raster::extent(res,
                                                r1 = block$orig1,
                                                r2 = block$orig2,
                                                c1 = 1,
                                                c2 = ncol(res)))

        # export to temp file
        filename <- tempfile(fileext = ".tif")
        raster::writeRaster(res, filename = filename, overwrite = TRUE)

        filename
    }

    .apply_cluster <- function() {

        if (purrr::is_null(cl)) {
            return(lapply(seq_along(blocks), function(i) {
                do.call(.sits_cluster_block_fun, args = .arg_fun(i))
            }))
        }

        snow::dynamicClusterApply(cl = cl,
                                  fun = .sits_cluster_block_fun,
                                  n = length(blocks),
                                  argfun = .arg_fun)
    }

    # apply function to blocks
    tmp_tiles <- .apply_cluster()

    # stop progress bar
    setTxtProgressBar(pb, length(blocks) + 1)
    close(pb)

    # on exit, remove temp files
    on.exit(unlink(tmp_tiles))

    # merge to save final result with '...' parameters
    message("Merging files...", appendLF = TRUE)
    do.call(raster::merge, c(lapply(tmp_tiles, raster::brick), list(...)))
}






















library(inSitu)
# Retrieve the set of samples for the Mato Grosso region
# Select the data for classification
mato_grosso_samples <- inSitu::br_mt_1_8K_9classes_6bands
mato_grosso_2bands  <-sits_select(mato_grosso_samples, bands =c("NDVI", "EVI"))
# build a machine learning model for this area
svm_model <-sits_train(mato_grosso_2bands,sits_svm())

evi_file <-system.file("extdata/Sinop", "Sinop_evi_2014.tif", package = "inSitu")
ndvi_file <-system.file("extdata/Sinop", "Sinop_ndvi_2014.tif", package = "inSitu")

files <-c(ndvi_file, evi_file)

time_file <-system.file("extdata/Sinop", "timeline_2014.txt", package = "inSitu")
timeline_2013_2014 <-scan(time_file,character())

sinop <-sits_cube(type = "BRICK",
                  satellite = "TERRA",
                  sensor  = "MODIS",
                  name = "Sinop",
                  timeline = timeline_2013_2014,
                  bands =c("NDVI", "EVI"),
                  files = files)

probs <-sits_classify(sinop,
                      ml_model = svm_model,memsize = 16,
                      multicores = 3)

# TODO: run bayesian
bayes <- sits::sits_smooth(
    probs,
    multicores = 3,
    type = "bayes"
)

sinop_label <-sits_label_classification(sinop_bayes, output_dir =tempdir())
