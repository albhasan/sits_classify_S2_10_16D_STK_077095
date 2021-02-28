# Get time series for the sample points.
library(dplyr)
library(ensurer)
library(readr)
library(sits)
library(sf)
library(tibble)


#---- Set up ----

samples_file <- "./data/samples/alber3_bdc077095.csv"
#samples_abandoned_file <- "./data/samples/abandoned3.shp"

stopifnot(file.exists(samples_file))
#stopifnot(file.exists(samples_abandoned_file))


#---- Script -----

source("./scripts/00_util.R")

data_cube <- get_cube("cube")

samples_tb <- samples_file %>%
    readr::read_csv(
        col_types = readr::cols(id         = readr::col_integer(),
                                longitude  = readr::col_double(),
                                latitude   = readr::col_double(),
                                label      = readr::col_character(),
                                start_date = readr::col_date(format = ""),
                                end_date   = readr::col_date(format = ""))
    )

my_start_date <- samples_tb$start_date[[1]]
my_end_date   <- samples_tb$end_date[[1]]

samples_tb <- samples_tb %>%
    # samples_abandoned_file %>%
    # sf::read_sf() %>%
    # add_coords() %>%
    # sf::st_set_geometry(NULL) %>%
    # tibble::as_tibble() %>%
    # dplyr::mutate(start_date = my_start_date,
    #               end_date   = my_end_date,
    #               label = "Abandoned",
    #               id = FID + 100000) %>%
    # dplyr::select(id, longitude, latitude, label, start_date, end_date) %>%
    # dplyr::bind_rows(samples_tb) %>%
    dplyr::arrange(id) %>%
    dplyr::mutate(id = 1:nrow(.)) %>%
    dplyr::mutate(id_coords = stringr::str_c(round(longitude, digits = 10),
                                             round(latitude,  digits = 10),
                                             sep = "_")) %>%
    # NOTE: Remove duplicates
    dplyr::distinct(id_coords, .keep_all = TRUE) %>%
    (function(x){
        my_count <- dplyr::count(x, label)
        print(my_count, n = Inf)
        print(sum(dplyr::pull(my_count, n)))
        invisible(x)
    })
# label             n
# * <chr>         <int>
# 1 Deforestation   157
# 2 Forest          415
# 3 NatNonForest    171
# 4 Pasture         460
# [1] 1203
# label             n
# 1 Abandoned       208
# 2 Deforestation   157
# 3 Forest          415
# 4 NatNonForest    171
# 5 Pasture         460
# SUM:
# [1] 1411

label_tb <- samples_tb %>%
    dplyr::select(id_coords, label)

samples_tmp <- tempfile(pattern = "samples_", fileext = ".shp")

samples_sf <- samples_tb %>%
    sf::st_as_sf(coords = c("longitude", "latitude")) %>%
    sf::st_set_crs(value = 4326) %>%
    dplyr::mutate(start_date = dplyr::first(sits::sits_timeline(data_cube)) - 1,
                  end_date   = dplyr::last(sits::sits_timeline(data_cube)) + 1) %>%
    sf::write_sf(samples_tmp)

samples_ts <- sits::sits_get_data(cube = data_cube,
                                  file = samples_tmp)
samples_ts <- samples_ts %>%
    dplyr::mutate(id_coords = stringr::str_c(round(longitude, digits = 10),
                                             round(latitude,  digits = 10),
                                             sep = "_")) %>%
    dplyr::select(-label) %>%
    dplyr::left_join(label_tb, by = "id_coords") %>%
    dplyr::select("longitude", "latitude", "start_date", "end_date", "label",
                  "cube", "time_series")
samples_ts %>%
    dplyr::count(label)
# A tibble: 4 x 2
# label             n
# * <chr>         <int>
# 1 Deforestation   157
# 2 Forest          414
# 3 NatNonForest    171
# 4 Pasture         460
#   label             n
# 1 Abandoned       208
# 2 Deforestation   157
# 3 Forest          414
# 4 NatNonForest    171
# 5 Pasture         460

# Check the number of lines and columns on each time series.
samples_tb2 <- samples_ts %>%
    (function(x){
        x %>%
            clean_ts(report = TRUE) %>%
            dplyr::group_by(label) %>%
            dplyr::summarise(ncol_mean = mean(n_cols),
                             nrol_mean = mean(n_rows),
                             sum_na = sum(has_na),
                             sum_null = sum(has_null),
                             sum_overflow = sum(has_overflow)) %>%
            print()
        invisible(x)
    })

# Remove names from longitude & latitude columns
for (i in 1:ncol(samples_tb2)) {
    x <- samples_tb2[[i]]
    names(x) <- NULL
    samples_tb2[[i]] <- x
}

samples_tb2 %>%
    is_sits_valid() %>%
    saveRDS("./data/samples/samples.rds")
