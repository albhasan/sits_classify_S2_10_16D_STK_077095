library(dplyr)
library(sits)

samples_file <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/samples/samples.rds"
model_file    <- paste0("/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor", ml_model.rds)

samples_tb <- samples_file %>%
    readRDS() %>%
    sits_select(my_bands)

ml_model <- sits::sits_train(samples_tb,
                             ml_method = ml_method)
saveRDS(ml_model, model_file)