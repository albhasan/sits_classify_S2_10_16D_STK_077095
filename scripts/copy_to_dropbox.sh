#!/bin/bash
# Copy files to dropbox directoty on sdb-desktop.

base_dir="/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/"
remote_dir="/home/alber/Documents/Dropbox_esensing/alber/sits_classify_S2_10_16D_STK_077095"

rsync -avz "${base_dir}/results"      alber@150.163.3.150:"${remote_dir}"
rsync -avz "${base_dir}/data/samples/*" alber@151.163.3.150:"${remote_dir}/data/samples/"
