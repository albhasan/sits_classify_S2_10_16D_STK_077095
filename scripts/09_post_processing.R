# Compute bayes and label classification probabilities using multiple cores.


split_clusterR <- function(x, n_tiles, pad_rows, fun,
                           args = NULL, export = NULL, cl = NULL, ...) {

    stopifnot(n_tiles > 0)
    stopifnot(pad_rows >= 0)

    breaks <- ceiling(seq(1, nrow(x) + 1, length.out = n_tiles + 1))
    breaks <- mapply(list,
                     r1 = ifelse(breaks - pad_rows < 0, 1,
                                 breaks - pad_rows)[seq_len(n_tiles)],
                     r2 = ifelse(breaks + pad_rows - 1 > nrow(x), nrow(x),
                                 breaks + pad_rows - 1)[-1:0], SIMPLIFY = FALSE,
                     orig1 = ifelse(breaks - pad_rows < 0, 1,
                                    pad_rows + 1)[seq_len(n_tiles)],
                     orig2 = ifelse(breaks - pad_rows < 0,
                                    breaks[-1:0] - breaks[seq_len(n_tiles)],
                                    breaks[-1:0] - breaks[seq_len(n_tiles)]
                                    + pad_rows))

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
        res <- raster::crop(res, raster::extent(res, r1 = b$orig1, r2 = b$orig2,
                                                c1 = 1, c2 = ncol(res)))

        # export to temp file
        filename <- tempfile(fileext = ".tif")
        raster::writeRaster(res, filename = filename, overwrite = TRUE)

        filename
    }

    tmp_tiles <- snow::dynamicClusterApply(cl = cl, fun = .io_fun,
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