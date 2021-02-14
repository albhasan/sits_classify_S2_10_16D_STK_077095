#!/bin/bash
# Split PRODES

in_file="/home/alber.ipia/Documents/data/prodes/2019/PDigital2000_2019_AMZ_gtif.tif"
out_file="/home/alber.ipia/Documents/data/prodes/2019/PDigital2000_2019_AMZ_gtif_079082.tif"

# BBOX BDC tile small 077095
coords="-61.7861717874733571 1.7799781237788999 -60.2741731961653429 2.8272289261749077"

# Set the PRODES' EPSG
srs="-s_srs EPSG:4674 -t_srs EPSG:4674"

# Set file options
opts="-dstnodata -9999 -ot Byte -co COMPRESS=LZW -co BIGTIFF=YES -overwrite -q"

gdalwarp ${srs} ${opts} -te ${coords} ${in_file} ${out_file}

exit 0