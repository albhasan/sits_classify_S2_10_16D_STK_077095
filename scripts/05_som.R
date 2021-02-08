# Use SOM on the samples.
library(dplyr)
library(magrittr)
library(sits)



#---- Util ----

run_som <- function(my_samples){
    return(
        sits::sits_som_map(my_samples,
                           grid_xdim  = 10,
                           grid_ydim  = 10,
                           alpha      = 1.0,
                           rlen       = 100,
                           distance   = "euclidean",
                           som_radius = 1)
    )
}



#---- Script -----

samples_file <- "./data/samples/samples.rds"
stopifnot(file.exists(samples_file))


samples_tb <- samples_file %>%
    readRDS() %>%
    tidyr::unnest(time_series) %>%
    dplyr::mutate(EVI  = 2.5 * (B08 - B04)/(B08 + 6 * B04 - 7.5 * B02 + 1),
                  NDVI = (B08 - B04)/(B08 + B04),
                  NDMI = (B08 - B11)/(B08 + B11)) %>%
    tidyr::nest(time_series = tidyselect::matches("(Index|B[0-9]+|I$)")) %>%
    magrittr::set_class(class(cerrado_2classes)) %>%
    sits::sits_select(c("EVI", "NDVI", "NDMI"))

set.seed(123)
som_cluster <- run_som(samples_tb)

# Save to files
for (my_type in c("codes", "mapping")) {
    for (b in sits::sits_bands(samples_tb)) {
        png(file = paste0("./data/samples/plot_som_samples_",
                          my_type, "_", b, ".png"))
        plot(som_cluster,
             type = my_type,
             whatmap = match(b, sits::sits_bands(samples_tb)))
        dev.off()
    }
}

cluster_overall <- sits::sits_som_evaluate_cluster(som_cluster)
# NOTE: There is no confusion matrix here!
cluster_overall$confusion_matrix

som_samples_tb <- som_cluster %>%
    sits::sits_som_clean_samples() %>%
    (function(x){
        x %>%
            count(eval) %>%
            print(n = Inf)
        invisible(x)
    }) %>%
    dplyr::filter(eval == "clean")
# eval        n
# 1 analyze   204
# 2 clean     818
# 3 remove    388

time_series_tb <- samples_file %>%
    readRDS() %>%
    dplyr::mutate(id_coords = stringr::str_c(
        round(longitude, digits = 6),
        round(latitude, digits = 6),
        sep = "_")) %>%
    dplyr::select(id_coords, time_series)

som_samples_tb %>%
    dplyr::mutate(id_coords = stringr::str_c(
        round(longitude, digits = 6),
        round(latitude, digits = 6),
        sep = "_")) %>%
    dplyr::select(-time_series) %>%
    dplyr::left_join(time_series_tb, by = "id_coords") %>%
    dplyr::select(-id_coords) %>%
    saveRDS("./data/samples/samples_som.rds")