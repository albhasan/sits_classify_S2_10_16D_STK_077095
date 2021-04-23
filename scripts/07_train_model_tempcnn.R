# NOTE: Use this page to install tensorflow with miniconda:
#       https://stackoverflow.com/questions/63220597/python-in-r-error-could-not-find-a-python-environment-for-usr-bin-python

library(dplyr)
library(sits)

samples_file <- "./data/samples/samples_tb.rds"

model_file   <- "./results/paper_defor_tempcnn/ml_model.rds"
ml_method    <- sits::sits_rfor(num_trees = 2000)

my_bands     <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")
stopifnot(file.exists(samples_file))
stopifnot(dir.exists(dirname(model_file)))
if (file.exists(model_file))
    stop("Model file already exits!")

samples_tb <- samples_file %>%
    readRDS() %>%
    sits_select(my_bands)

ml_method    <- sits::sits_TempCNN(
    samples = NULL,
    cnn_layers = c(64, 64, 64),
    cnn_kernels = c(5, 5, 5),
    cnn_activation = "relu",
    cnn_L2_rate = 1e-06,
    cnn_dropout_rates = c(0.5, 0.5, 0.5),
    mlp_layers = c(256),
    mlp_activation = "relu",
    mlp_dropout_rates = c(0.5),
    optimizer = keras::optimizer_adam(lr = 0.001),
    epochs = 150,
    batch_size = 128,
    validation_split = 0.2,
    verbose = 0
)

ml_model <- sits::sits_train(samples_tb,
                             ml_method = ml_method)

saveRDS(ml_model, model_file)
