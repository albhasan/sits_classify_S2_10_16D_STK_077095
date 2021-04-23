library(dplyr)
library(sits)

samples_file <- "./data/samples/samples_tb.rds"

model_file   <- "./results/paper_defor_resnet/ml_model.rds"

my_bands     <- c("B02", "B03", "B04", "B08", "B8A",  "B11", "B12")

stopifnot(file.exists(samples_file))
stopifnot(dir.exists(dirname(model_file)))
if (file.exists(model_file))
    stop("Model file already exits!")

samples_tb <- samples_file %>%
    readRDS() %>%
    sits_select(my_bands)

ml_method <- sits::sits_ResNet(
    samples = NULL,
    blocks = c(64, 128, 128),
    kernels = c(8, 5, 3),
    activation = "relu",
    optimizer = keras::optimizer_adam(lr = 0.001),
    epochs = 300,
    batch_size = 64,
    validation_split = 0.2,
    verbose = 1)
ml_model <- sits::sits_train(samples_tb,
                             ml_method = ml_method)
saveRDS(ml_model, model_file)
