source("~/Documents/bdc_access_key.R")
# Get time series for the sample points.
library(dplyr)
library(ensurer)
library(purrr)
library(readr)
library(sits)
library(sf)
library(tibble)


#---- Set up ----

my_cube <- "paper-deforestation"
my_tiles <- "077095"
samples_file <- "./data/samples/alber3_bdc077095.csv"

stopifnot(file.exists(samples_file))



#---- Script -----

source("./scripts/00_util.R")

samples_tb <- samples_file %>%
    readr::read_csv(
        col_types = readr::cols(id         = readr::col_integer(),
                                longitude  = readr::col_double(),
                                latitude   = readr::col_double(),
                                label      = readr::col_character(),
                                start_date = readr::col_date(format = ""),
                                end_date   = readr::col_date(format = ""))
    ) %>%
    dplyr::mutate(time_series = NA) %>%
    magrittr::set_class(class(cerrado_2classes))

samples_time <- samples_tb %>%
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

samples_tmp <- tempfile(pattern = "samples_",
                        fileext = ".shp")

samples_sf <- samples_tb %>%
    sf::st_as_sf(coords = c("longitude", "latitude")) %>%
    sf::st_set_crs(value = 4326) %>%
    (function(x){
        x %>%
            sf::write_sf(samples_tmp)
        return(x)
    })

coords_label <- samples_sf %>%
    add_coords() %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::mutate(id_coords = stringr::str_c(round(longitude, digits = 10),
                                             round(latitude,  digits = 10),
                                             sep = "_")) %>%
    dplyr::select(label, id_coords)

samples_ts <- sits::sits_get_data(cube = data_cube,
                                  file = samples_tmp)

samples_tb  <- samples_ts %>%
    dplyr::mutate(id_coords = stringr::str_c(round(longitude, digits = 10),
                                             round(latitude,  digits = 10),
                                             sep = "_")) %>%
    dplyr::select(-label) %>%
    dplyr::left_join(coords_label, by = "id_coords") %>%
    dplyr::distinct(id_coords, .keep_all = TRUE)

samples_tb %>%
    dplyr::count(label)
# label             n
# 1 Deforestation   157
# 2 Forest          414
# 3 NatNonForest    171
# 4 Pasture         460

# Check the number of lines and columns on each time series.
# NOTE: The column SCL is fill with NAs.
samples_tb %>%
    clean_ts(report = TRUE) %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(n_obs = dplyr::n(),
                     ncol_mean = mean(n_cols),
                     nrow_mean = mean(n_rows),
                     time_mean = mean(time_mean),
                     sum_na = sum(has_na),
                     sum_null = sum(has_null),
                     sum_overflow = sum(has_overflow),
                     test_na_col = sum_na / n_obs) %>%
    print(n = Inf)
# label         n_obs ncol_mean nrow_mean time_mean sum_na sum_null sum_overflow test_na_col
# 1 Deforestation   157        15        25    17914.   3925        0            0          25
# 2 Forest          414        15        25    17914.  10350        0            0          25
# 3 NatNonForest    171        15        25    17914.   4275        0            0          25
# 4 Pasture         460        15        25    17914.  11500        0            0          25


samples_tb2 <- samples_tb %>%
    dplyr::select(longitude, latitude, start_date, end_date,
                  label, cube, time_series) %>%
    dplyr::mutate(time_series = purrr::map(time_series, function(x){
        return(dplyr::select(x, -SCL))
    }))

# Remove names from longitude & latitude columns
for (i in 1:ncol(samples_tb2)) {
    x <- samples_tb2[[i]]
    names(x) <- NULL
    samples_tb2[[i]] <- x
}

samples_tb2 %>%
    is_sits_valid() %>%
    saveRDS("./data/samples/samples_tb.rds")
