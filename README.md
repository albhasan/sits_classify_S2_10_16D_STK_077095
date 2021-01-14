# sits_classify_S2_10_16D_STK_077095

Classification of deforestation using sits and Brazil Data Cubes.

* Cube Sentinel2 077095 STACK.
* Resolution of 10 meters and 16 days.
* Classifier in the R package SITS.

## TODO:

* Apply PRODES mask and compute accuracy

## DONE:
* Get time series of the samples (use file in ./data/samples/alber3_bdc077095.csv
* Download the images of the BDC (use file in ./scripts/S2_10_16D_STK-1.txt)
* Analize sample time series looking for outliers. SOM?
* Review classifications of mini-cubes with Michelle and Rolf.
* Run classifications of mini cubes using the samples of the paper.
* Compute entropy in for the results
* Download UMaryland data