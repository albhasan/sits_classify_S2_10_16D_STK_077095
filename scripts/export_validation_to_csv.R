library(dplyr)
library(sf)
library(readr)

class_labels <- c(`1` = "Deforestation",
                  `2` = "Forest",
                  `3` = "NatNonForest",
                  `4` = "Pasture")

source("./scripts/00_util.R")

validation_shp <- "./data/validation/validation.shp"
stopifnot(file.exists(validation_shp))

# Read the validation points.
validation_sf <- validation_shp %>%
    sf::read_sf() %>%
    ensurer::ensure_that(all(class_labels %in% .$class),
                         err_des = "Unknown labels found!") %>%
    dplyr::filter(class %in% class_labels) %>%
    ensurer::ensure_that(length(unique(.$class)) >= length(class_labels),
                         err_desc = "Unsampled labels!")
# Print the number of samples.
validation_sf %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    (function(x) {
        x %>%
            dplyr::count(class) %>%
            print()
        print(nrow(x))
        invisible(x)
    })

validation_sf %>%
    add_coords() %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(longitude, latitude, label = class) %>%
    readr::write_csv("./data/validation/validation.csv")
