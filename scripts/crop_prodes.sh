#!/bin/bash
# Split PRODES

in_file="/home/alber.ipia/Documents/data/prodes/2019/PDigital2000_2019_AMZ_gtif.tif"
out_file="/home/alber.ipia/Documents/data/prodes/2019/PDigital2000_2019_AMZ_gtif_077095.tif"

# BBOX BDC tile small 077095
coords="-65.2752799481130950 -10.9812035370481702 -63.6803303411144910 -9.9471581104979609"

# Set the PRODES' EPSG
srs="-s_srs EPSG:4674 -t_srs EPSG:4674"

# Set file options
opts="-dstnodata -9999 -ot Byte -co COMPRESS=LZW -co BIGTIFF=YES -overwrite -q"

gdalwarp ${srs} ${opts} -te ${coords} ${in_file} ${out_file}

exit 0