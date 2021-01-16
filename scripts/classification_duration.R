# Duration of the classification
library(dplyr)
library(ggplot2)
library(magrittr)

time_file <- "./results/paper_defor/077095_split/processing_time.rds"
stopifnot(file.exists(time_file))

time_df <- time_file %>%
    readRDS() %>%
    as.data.frame() %>%
    ensurer::ensure_that(nrow(.) > 0, ncol(.) > 0,
                         err_desc = "No data found!") %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_rownames(value = NULL) %>%
    magrittr::set_colnames(value = c("end_time", "start_time")) %>%
    dplyr::mutate(class_time = difftime(end_time, start_time, units = "min"))
time_df

# Total classification time
(total_time <- difftime(max(time_df$end_time),
                        min(time_df$start_time),
                        units = "hours"))

# Mean classification time of a subtile
(mean_time <- mean(as.vector(time_df$class_time)))

# Standard deviation
sd_time <- sd(as.vector(time_df$class_time))

time_df %>%
    ggplot() +
    geom_histogram(aes(class_time)) +
    stat_function(fun = dnorm,
                  args = list(mean = mean_time,
                              sd = sd_time))
