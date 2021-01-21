#!/bin/bash
# Split PRODES

in_file="/home/alber.ipia/Documents/data/prodes/2019/PDigital2000_2019_AMZ_gtif.tif"
out_file="/home/alber.ipia/Documents/data/prodes/2019/PDigital2000_2019_AMZ_gtif_077095.tif"

# BBOX BDC tile small 077095
coords="-65.2752794737273661 -10.9811925085195607 -63.6803226757691903 -9.9471581104979627"

# Set the PRODES' EPSG
srs="-s_srs EPSG:4674 -t_srs EPSG:4674"

# Set file options
opts="-dstnodata -9999 -ot Byte -co COMPRESS=LZW -co BIGTIFF=YES -overwrite -q"

gdalwarp ${srs} ${opts} -te ${coords} ${in_file} ${out_file}

exit 0