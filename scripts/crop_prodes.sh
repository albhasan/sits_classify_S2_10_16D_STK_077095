#!/bin/bash
# Split PRODES

in_file2019="/home/alber.ipia/Documents/data/prodes/2019/PDigital2000_2019_AMZ_gtif.tif"
in_file2020="/home/alber.ipia/Documents/data/prodes/2020/PDigital2000_2020_AMZ.tif"

out_file_077095_2019="/home/alber.ipia/Documents/data/prodes/2019/PDigital2000_2019_AMZ_gtif_077095.tif"
out_file_077095_2020="/home/alber.ipia/Documents/data/prodes/2020/PDigital2000_2020_AMZ_077095.tif"

out_file_079082_2019="/home/alber.ipia/Documents/data/prodes/2019/PDigital2000_2019_AMZ_gtif_079082.tif"
out_file_079082_2020="/home/alber.ipia/Documents/data/prodes/2020/PDigital2000_2020_AMZ_079082.tif"

# BBOX BDC tile small
coords_077095="-65.2752794737273661 -10.9811925085195607 -63.6803226757691903 -9.9471581104979627"
coords_079082="-61.9812848968900596 0.6341446655011242   -60.4584675262516669 1.6872839955765291"

# Set the PRODES' EPSG
srs="-s_srs EPSG:4674 -t_srs EPSG:4674"

# Set file options
opts="-dstnodata -9999 -ot Byte -co COMPRESS=LZW -co BIGTIFF=YES -overwrite -q"

gdalwarp ${srs} ${opts} -te ${coords_077095} ${in_file2019} ${out_file_077095_2019}
gdalwarp ${srs} ${opts} -te ${coords_077095} ${in_file2020} ${out_file_077095_2020}
#gdalwarp ${srs} ${opts} -te ${coords_079082} ${in_file2019} ${out_file_079082_2019}
#gdalwarp ${srs} ${opts} -te ${coords_079082} ${in_file2020} ${out_file_079082_2020}

exit 0