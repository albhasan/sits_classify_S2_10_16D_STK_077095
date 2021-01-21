#!/bin/bash
# Crop Global Forest Watch dataA

in_file="/home/alber.ipia/Documents/data/global_forest_watch/Hansen_GFC-2019-v1.7_lossyear_10S_070W.tif"
out_file="/home/alber.ipia/Documents/data/global_forest_watch/Hansen_GFC-2019-v1.7_lossyear_10S_070W_077095.tif"

# BBOX BDC tile small 077095
coords="-65.2752794737273661 -10.9811925085195607 -63.6803226757691903 -9.9471581104979627"


# Set the PRODES' EPSG
srs="-s_srs EPSG:4326 -t_srs EPSG:4326"

# Set file options
opts="-dstnodata -9999 -ot Byte -co COMPRESS=LZW -co BIGTIFF=YES -overwrite -q"

gdalwarp ${srs} ${opts} -te ${coords} ${in_file} ${out_file}

exit 0