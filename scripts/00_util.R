#' Add coordinates as columns to an SF object.
#'
#' @param point_sf A sf object.
#' @return         A sf object.
add_coords <- function(point_sf){
    xy <- point_sf %>%
        sf::st_coordinates() %>%
        magrittr::set_colnames(c("longitude", "latitude")) %>%
        tidyr::as_tibble()
    point_sf %>%
        dplyr::bind_cols(xy) %>%
        return()
}


#' Build a list of start and end pixels
#'
#' @param r_obj    A raster object.
#' @param n_row    An integer. The number of rows in the grid.
#' @param n_col    An integer. The number of columns in the grid.
#' @param v_size   An integer. The vertical size of a cell in the grid.
#' @param h_size   An integer. The horizontal size of a cell in the grid.
#' @return         A dataframe of 2 column-lists.
build_grid <- function(n_row, n_col, v_size, h_size) {

    breaks_v <- unique(c(seq(1, n_row, by = v_size), n_row + 1))
    v_start <- breaks_v[seq_len(length(breaks_v) - 1)]
    v_end <- breaks_v[-1] - 1

    breaks_h <- unique(c(seq(1, n_col, by = h_size), n_col + 1))
    h_start <- breaks_h[seq_len(length(breaks_h) - 1)]
    h_end <- breaks_h[-1] - 1

    v_interval <- mapply(list, r1 = v_start, r2 = v_end, SIMPLIFY = FALSE)
    h_interval <- mapply(list, c1 = h_start, c2 = h_end, SIMPLIFY = FALSE)

    return(expand.grid(v = v_interval, h = h_interval))
}


#' Build a tesselation of subtiles of a raster using the given grid.
#'
#' @param grid       A grid created using the function build_grid.
#' @param image_path Path to a raster.
#' @param out_dir    A path.
#' @param out_file   A path.
#' @return           Paths to VRT files.
build_vrt <- function(grid, image_path, out_dir, out_file){
    r_obj <- raster::raster(image_path)
    stopifnot(dir.exists(out_dir))
    res <- lapply(seq_len(nrow(grid)), function(i){
        my_extent <- raster::extent(r_obj,
                                    c1 = grid$h[[i]]$c1,
                                    c2 = grid$h[[i]]$c2,
                                    r1 = grid$v[[i]]$r1,
                                    r2 = grid$v[[i]]$r2)
        out_dir <- paste0(out_dir, "/",
                          paste(grid$h[[i]]$c1,
                                grid$v[[i]]$r1,
                                sep = "_"))
        if (!dir.exists(out_dir))
            dir.create(out_dir)
        stopifnot(dir.exists(out_dir))

        file_name <- paste0(out_dir, "/", out_file)

        gdalUtils::gdalbuildvrt(
            gdalfile = raster::filename(r_obj),
            output.vrt = file_name,
            te = c(attr(my_extent, "xmin"),
                   attr(my_extent, "ymin"),
                   attr(my_extent, "xmax"),
                   attr(my_extent, "ymax")),
            tr = c(raster::xres(r_obj), raster::xres(r_obj)))
        return(file_name)
    })
    return(unlist(res))
}

#' Remove invalid samples of time series.
#'
#' @param  sits_tb A sits_tibble.
#' @report report  When TRUE, not cleaning is done, just marking the offending samples.
#' @return A sits_tibble.
clean_ts <- function(sits_tb, report = FALSE){
    sits_tb %>%
        tidyr::drop_na() %>%
        dplyr::mutate(has_na    = purrr::map_int(time_series, function(x){return(sum(is.na(x)))}),
                      has_null  = purrr::map_int(time_series, function(x){return(sum(is.null(x), na.rm = TRUE))}),
                      has_overflow  = purrr::map_int(time_series, function(x){return(sum(sum(as.matrix(x[,2:ncol(x)]) < -1, na.rm = TRUE), sum(as.matrix(x[,2:ncol(x)]) > 1, na.rm = TRUE)))}),
                      time_mean = purrr::map_dbl(time_series, function(x){return(mean(x[[1]]))}),
                      n_cols    = purrr::map_int(time_series, ncol),
                      n_rows    = purrr::map_int(time_series, nrow)) %>%
        (function(.data){
            if (report){
                return(.data)
            }else{
                .data <- .data %>%
                    tidyr::drop_na() %>%
                    dplyr::filter(!has_null,
                                  n_cols > 1,
                                  n_rows > 0) %>%
                    dplyr::mutate(time_series = purrr::map(time_series, function(x){
                        my_approx <- function(v) {
                            apply(v, 2,
                                  function(x) {
                                      i <- tryCatch({
                                          approx(x, n = length(x))
                                      }, error = function(e) list(y = rep(0, length(x))))
                                      return(i$y)
                                  })
                        }
                        data_mt <- as.matrix(x[,2:ncol(x)])
                        data_mt[data_mt <= -1] <- NA
                        data_mt[data_mt >= 1]  <- NA
                        interp_mt <- my_approx(data_mt)
                        x %>%
                            dplyr::select(Index) %>%
                            dplyr::bind_cols(tibble::as_tibble(interp_mt)) %>%
                            return()
                    })) %>%
                    dplyr::select(-has_na, -has_null, -time_mean,
                                  -overflow, -n_cols, -n_rows)
                n_removed <- nrow(sits_tb) - nrow(.data)
                if (n_removed > 0)
                    warning(sprintf("Removed %s invalid samples out of  %s",
                                    n_removed, nrow(sits_tb)))
                return(.data)
            }
        }) %>%
        return()
}


#' Compute the information entropy in nats.
#'
#' @param img_path A length-one character. Path to a sits probability file.
#' @param out_file A length-one character. Path to the file to store the results.
#' @return         out_file.
compute_entropy <- function(img_path, out_file){
    # 2021-01-06
    n_bands <- img_path %>%
        ensurer::ensure_that(file.exists(.)) %>%
        gdalUtils::gdalinfo() %>%
        stringr::str_extract(pattern = "Band [0-9]+") %>%
        .[!is.na(.)] %>%
        dplyr::last() %>%
        stringr::str_extract(pattern = "[0-9]+") %>%
        as.numeric() %>%
        ensurer::ensure_that(is.numeric(.), . > 1,
                             err_desc = "Invalid number of bands!")
    exp_bands <- paste(sprintf("-%s %s --%s_band=%s", LETTERS[1:n_bands],
                               img_path, LETTERS[1:n_bands], 1:n_bands),
                       collapse = " ")
    exp_gdal <- paste0( "'(",
                        paste(
                            sprintf("%s.astype(numpy.float64)/10000 * numpy.log(%s.astype(numpy.float64)/10000)",
                                    LETTERS[1:n_bands],
                                    LETTERS[1:n_bands]),
                            collapse = " + "
                        ),
                        ") * -1'")
    cmd <- sprintf("gdal_calc.py %s --outfile=%s --calc=%s --NoDataValue=-9999 --type='Float64' --creation-option='COMPRESS=LZW' --creation-option='BIGTIFF=YES'",
                   exp_bands, out_file, exp_gdal)
    res <- system(cmd)
    invisible(out_file)
}


# Return a cube
get_cube <- function(cube){
    cube_path <- tibble::tribble(
        ~name, ~dir_path,
        "cube",   "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/077095",
        "mini_1", "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/mini_077095_1",
        "mini_2", "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/mini_077095_2",
        "mini_bayes", "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/cube/mini_077095_bayes"
    ) %>%
        ensurer::ensure_that(all(dir.exists(.$dir_path)),
                             err_desc = "Invalid cube directories!")

    cube_dir <- cube_path %>%
        dplyr::filter(name == cube) %>%
        ensurer::ensure_that(nrow(.) == 1,
                             err_desc = sprintf("Cube not found. Available options are %s",
                                                paste(cube_path$name, collapse = ", "))) %>%
        dplyr::pull(dir_path)

    sits::sits_cube(type = "STACK",
                    name = "S2_10_16D_STK_077095",
                    satellite = "SENTINEL-2",
                    sensor = "MSI",
                    data_dir = cube_dir,
                    delim = "_",
                    parse_info = c("mission", "sp_resolution",
                                   "time_resolution", "type",
                                   "version", "tile", "date",
                                   "end_date", "band")) %>%
        return()
}


#' Test if the data in a sits_tibble is valid.
#'
#' @param x A sits tibble.
#' @return  The given sits_tibble or error.
is_sits_valid <- function(x){
    .has_names <- function(y){
        sapply(y, function(z){
            !is.null(names(z))
        })
    }
    res <- x %>%
        dplyr::mutate(n_rows = purrr::map_int(time_series, nrow),
                      n_cols = purrr::map_int(time_series, ncol),
                      n_na   = purrr::map_int(time_series, function(x){
                          return(sum(is.na(x)))
                      })) %>%
        ensurer::ensure_that(all(.$n_rows > 1),
                             err_desc = "Wrong number of steps in time series!") %>%
        ensurer::ensure_that(length(unique(.$n_rows)) == 1,
                             err_desc = "Number of steps in time series don't match!") %>%
        ensurer::ensure_that(all(.$n_cols > 1),
                             err_desc = "Wrong number of variables in time series!") %>%
        ensurer::ensure_that(length(unique(.$n_cols)) == 1,
                             err_desc = "Number of variables don't match!") %>%
        ensurer::ensure_that(all(.$n_na == 0),
                             err_desc = "NAs found in time series!") %>%
        ensurer::ensure_that(sum(.has_names(.)) == 0,
                             err_desc = "The columns must not have list names internally!") %>%
        ensurer::ensure_that(!("grouped_df" %in% class(.)),
                             err_desc = "Grouped tibbles are not supported in sits!") %>%
        ensurer::ensure_that("sits" %in% class(.),
                             err_desc = "The tibble is not a sits tibble")
    invisible(x)
}




