TODO:
    - Run the bayesian smoother here, if possible. Check the newest sits.

library(dplyr)
library(sits)

probs_file <- "/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/results/paper_defor2/S2_10_16D_STK_077095_probs_2018_7.tif"
stopifnot(file.exists(probs_file))


# Build a cube from the probabilities
?sits::sits_cube(type = "PROBS",
                 name = "probs_cube")
