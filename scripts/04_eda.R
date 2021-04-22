library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)

samples_tb <- "./data/samples/samples_tb.rds" %>%
    readRDS()

samples_tb %>%
    dplyr::count(label)
# label             n
# 1 Deforestation   157
# 2 Forest          414
# 3 NatNonForest    171
# 4 Pasture         460



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
    dplyr::select(-longitude, -latitude, -cube, Label = label) %>%
    tidyr::unnest(time_series) %>%
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
plot_tb %>%
    dplyr::filter(Band %in% c("B08", "B8A", "B11", "B12")) %>%
    f_plot() +
    ggplot2::ggtitle("S2 samples - Bands") +
    ggplot2::ggsave("./data/samples/plot_samples_bands.png")
plot_tb %>%
    dplyr::filter(Band %in% c("EVI", "NDVI", "NDMI")) %>%
    f_plot() +
    ggplot2::ggtitle("S2 samples - Vegetation Indexes") +
    ggplot2::ggsave("./data/samples/plot_samples_indices.png")