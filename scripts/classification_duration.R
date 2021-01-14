# Duration of the classification

time_vec <- readRDS("./data/cube/077095_split_results/processing_time.rds")
time_df <- as.data.frame(t(as.data.frame(time_vec)))
rownames(time_df) <- NULL
colnames(time_df) <- c("end_time", "start_time")
time_df["class_time"] <- difftime(time_df$end_time, time_df$start_time,
                                  units = "min")
head(time_df)

# Total classification time
(total_time <- difftime(max(time_df$end_time), min(time_df$start_time),
                        units = "hours"))

# Mean classification time of a subtile
(mean_time <- mean(as.vector(time_df$class_time)))
sd_time <- sd(as.vector(time_df$class_time))


#hist(as.vector(time_df$class_time), breaks = 19)
ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(time_comp)) +
    ggplot2::stat_function(fun = dnorm,
                           args = list(mean = mean_time,
                                       sd = sd_time))
