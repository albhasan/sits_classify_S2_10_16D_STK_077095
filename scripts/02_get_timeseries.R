# Get time series for the sample points.
library(dplyr)
library(readr)
library(sits)
library(sf)

#---- Set up ----

samples_file <- "./data/samples/alber3_bdc077095.csv"
#samples_file <- "./data/samples/samples_paper.csv"

source("./scripts/00_util.R")

#---- Script -----

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

samples_tb %>%
    dplyr::count(label)

label_tb <- samples_tb %>%
    dplyr::mutate(id_coords = stringr::str_c(
        round(longitude, digits = 6),
        round(latitude, digits = 6),
        sep = "_")) %>%
    dplyr::select(id_coords, label)

samples_tmp <- tempfile(pattern = "samples_", fileext = ".shp")
samples_sf <- samples_file %>%
    sf::st_read(options = c("X_POSSIBLE_NAMES=longitude","Y_POSSIBLE_NAMES=latitude"),
                quiet = TRUE) %>%
    sf::st_set_crs(value = 4326) %>%
    dplyr::mutate(start_date = dplyr::first(sits::sits_timeline(data_cube)) - 1,
                  end_date   = dplyr::last(sits::sits_timeline(data_cube)) + 1) %>%
    sf::write_sf(samples_tmp)

samples_ts <- sits::sits_get_data(cube = data_cube,
                                  file = samples_tmp)
samples_ts <- samples_ts %>%
    dplyr::mutate(id_coords = stringr::str_c(
        round(longitude, digits = 6),
        round(latitude, digits = 6),
        sep = "_")) %>%
    dplyr::select(-label) %>%
    dplyr::left_join(label_tb, by = "id_coords") %>%
    dplyr::select("longitude", "latitude", "start_date", "end_date", "label",
                  "cube", "time_series")

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
    #saveRDS("./data/samples/samples_paper.rds")