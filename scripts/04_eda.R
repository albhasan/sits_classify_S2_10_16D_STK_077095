library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)

samples_tb <- "./data/samples/samples.rds" %>%
#samples_tb <- "./data/samples/samples_som.rds" %>%
#samples_tb <- "./data/samples/samples_paper.rds" %>%
    readRDS()

samples_tb %>%
    dplyr::select("longitude", "latitude", "start_date", "end_date", "label",
                  "cube", "time_series") %>%
    (function(x){
        print(dplyr::count(x, label))
        invisible(x)
    }) %>%
    tidyr::unnest(time_series) %>%
    dplyr::mutate(EVI  = 2.5 * (B08 - B04)/(B08 + 6 * B04 - 7.5 * B02 + 1),
                  NDVI = (B08 - B04)/(B08 + B04),
                  NDMI = (B08 - B11)/(B08 + B11)) %>%
    dplyr::select(-longitude, -latitude, -cube,
                  Label = label)



#---- Plot samples' time series ----

f_plot <- function(x){
    x %>%
        ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes(x = Index,
                                           y = Value,
                                           group = interaction(Index, Band)),
                              outlier.size = 0.5) +
        ggplot2::geom_smooth(ggplot2::aes(x = Index,
                                          y = Value,
                                          group =  Band,
                                          color = Label)) +
        ggplot2::theme(axis.text.x = element_text(angle = 90)) +
        ggplot2::facet_grid(rows = vars(Label),
                            cols = vars(Band)) %>%
        return()
}


plot_tb <- samples_tb %>%
    tidyr::pivot_longer(cols = !tidyselect::all_of(c("start_date",
                                                     "end_date",
                                                     "Label", "Index")),
                        names_to = "Band",
                        values_to = "Value")

plot_tb %>%
    dplyr::filter(Band %in% c("B02", "B03", "B04")) %>%
    f_plot() +
    ggplot2::ggtitle("S2 samples - Flat bands") +
    ggplot2::ggsave("./data/samples/plot_samples_bands_flat.png")
    #ggplot2::ggsave("./data/samples/plot_samples_som_bands_flat.png")
    #ggplot2::ggsave("./data/samples/plot_samples_paper_bands_flat.png")
plot_tb %>%
    dplyr::filter(Band %in% c("B08", "B8A", "B11", "B12")) %>%
    f_plot() +
    ggplot2::ggtitle("S2 samples - Bands") +
    ggplot2::ggsave("./data/samples/plot_samples_bands.png")
    #ggplot2::ggsave("./data/samples/plot_samples_som_bands.png")
    #ggplot2::ggsave("./data/samples/plot_samples_paper_bands.png")
plot_tb %>%
    dplyr::filter(Band %in% c("EVI", "NDVI", "NDMI")) %>%
    f_plot() +
    ggplot2::ggtitle("S2 samples - Vegetation Indexes") +
    ggplot2::ggsave("./data/samples/plot_samples_indices.png")
    #ggplot2::ggsave("./data/samples/plot_samples_som_indices.png")
    #ggplot2::ggsave("./data/samples/plot_samples_paper_indices.png")



#---- PCA ----

# Return the standard deviation explained by each principal component.
pca_sd <- function(x){
    x %>%
        prcomp(center = TRUE, scale = TRUE) %>%
        magrittr::extract2("sdev") %>%
        magrittr::set_names(stringr::str_c("PC",
                                           str_pad(1:length(.), pad = "0",
                                                   width = 2))) %>%
        dplyr::bind_rows() %>%
        return()
}

plot_pca <- samples_tb %>%
    dplyr::select(-start_date, -end_date, -Index) %>%
    dplyr::group_by(Label) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(data_bands = purrr::map(data, dplyr::select, B01:B8A),
                  data_indices = purrr::map(data, dplyr::select, EVI:NDMI)) %>%
    dplyr::mutate(pca_bands = purrr::map(data_bands, pca_sd),
                  pca_indices = purrr::map(data_indices, pca_sd))

plot_pca %>%
    tidyr::unnest(pca_bands) %>%
    dplyr::select(-data, -data_bands, -data_indices, -pca_indices) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("PC"),
                        names_to = "PC", values_to = "SD") %>%
    dplyr::group_by(Label) %>%
    dplyr::mutate(cum_SD = cumsum(SD),
                  total = sum(SD),
                  cum_norm_SD = cum_SD/total) %>%
    dplyr::select(-total) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot() +
    ggplot2::geom_step(ggplot2::aes(x = PC, y = cum_norm_SD,
                                    group = Label, color = Label)) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = "dashed") +
    ggplot2::ggtitle("PCA using bands") +
    ggplot2::ylab("Explained SD") +
    ggplot2::ylim(0, 1) +
    ggplot2::ggsave("./data/samples/plot_samples_pca_bands.png")
    #ggplot2::ggsave("./data/samples/plot_samples_som_pca_bands.png")
    #ggplot2::ggsave("./data/samples/plot_samples_paper_pca_bands.png")

plot_pca %>%
    tidyr::unnest(pca_indices) %>%
    dplyr::select(-data, -data_bands, -data_indices, -pca_bands) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("PC"),
                        names_to = "PC", values_to = "SD") %>%
    dplyr::group_by(Label) %>%
    dplyr::mutate(cum_SD = cumsum(SD),
                  total = sum(SD),
                  cum_norm_SD = cum_SD/total) %>%
    dplyr::select(-total) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot() +
    ggplot2::geom_step(ggplot2::aes(x = PC, y = cum_norm_SD,
                                    group = Label, color = Label)) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = "dashed") +
    ggplot2::ggtitle("PCA using indices") +
    ggplot2::ylab("Explained SD") +
    ggplot2::ylim(0, 1) +
    ggplot2::ggsave("./data/samples/plot_samples_pca_indices.png")
    #ggplot2::ggsave("./data/samples/plot_samples_som_pca_indices.png")
    #ggplot2::ggsave("./data/samples/plot_samples_paper_pca_indices.png")
