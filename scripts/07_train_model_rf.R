library(dplyr)
library(sits)

samples_file <- "./data/samples/samples_tb.rds"

model_file   <- "./results/paper_defor_rf/ml_model.rds"

my_bands     <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")
ml_method    <- sits::sits_rfor(num_trees = 2000)

stopifnot(file.exists(samples_file))
stopifnot(dir.exists(dirname(model_file)))
if (file.exists(model_file))
    stop("Model file already exits!")

samples_tb <- samples_file %>%
    readRDS() %>%
    sits_select(my_bands)

ml_model <- sits::sits_train(samples_tb,
                             ml_method = ml_method)
saveRDS(ml_model, model_file)