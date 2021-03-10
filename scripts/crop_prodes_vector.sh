#!/bin/bash

base_dir="/home/alber.ipia/Documents/sits_classify_S2_10_16D_STK_077095/data/prodes/yearly_deforestation_biome"

cloud_zip="cloud_biome.zip"
forest_zip="forest_biome.zip"
hydrography_zip="hydrography_biome.zip"
no_forest_zip="no_forest_biome.zip"
yearly_deforestation_zip="yearly_deforestation_biome.zip"

cd ${base_dir}
mkdir ${base_dir}/tmp
mkdir ${base_dir}/074095
mkdir ${base_dir}/077095
mkdir ${base_dir}/079082
mkdir ${base_dir}/083096
mkdir ${base_dir}/082090
mkdir ${base_dir}/085088
mkdir ${base_dir}/087087

unzip ${cloud_zip} -d ${base_dir}/tmp
unzip ${forest_zip} -d ${base_dir}/tmp
unzip ${hydrography_zip} -d ${base_dir}/tmp
unzip ${no_forest_zip} -d ${base_dir}/tmp
unzip ${yearly_deforestation_zip} -d ${base_dir}/tmp

# Extent of BDC_SM tiles in EPSG:4674 (Prodes' SRS)
# 074095 -69.9496362634197624,-10.7877784644479604 : -68.3401355384619364, -9.7293973305111479
# 077095 -65.2752799481130950,-10.9812035370481702 : -63.6803303411144910, -9.9471581104979609
# 079082 -61.7861721004006270,  1.7799465865711850 : -60.2741608986789288,  2.8272289261749068
# 083096 -55.9172549617417829,-12.1209572033416197 : -54.3484373041839675,-11.1367011781553504
# 082090 -57.4097455357031166, -6.2347694375812193 : -55.8693259016744435, -5.2360923813641449
# 085088 -52.8210166677286423, -4.2674628548003541 : -51.2939548328616226, -3.2681996917657181
# 087087 -49.8011290275847429, -3.2494949085574278 : -48.2691817720764220, -2.2312908516513139
#
# Polygon coordinates
# 074095 -69.89176357071052337  -9.72939733051114786, -68.34013553846193645  -9.81028919041443181, -68.39237870230023475 -10.7877784644479604,  -69.94963626341976237 -10.70664297402402987, -69.89176357071052337  -9.72939733051114786
# 077095 -65.23432383171812887  -9.94715811049796095, -63.68033034111449098 -10.00312526882050079, -63.71563101099975057 -10.9812035370481702,  -65.27527994811309497 -10.92506424111168073, -65.23432383171812887  -9.94715811049796095
# 079082 -61.7591511435201781    2.82722892617490684, -60.27416089867892879   2.78844448811835699, -60.29601433183734827   1.77994658657118499, -61.78617210040062702   1.81870430536794192, -61.7591511435201781    2.82722892617490684
# 083096 -55.91025802839327241 -11.13670117815535043, -54.34843730418396746 -11.14274290153585056, -54.34971360624486891 -12.12095720334161975, -55.91725496174178289 -12.11489476177287017, -55.91025802839327241 -11.13670117815535043
# 082090 -57.39756929337381308  -5.2360923813641449,  -55.86932590167444346  -5.25028309885830069, -55.87602567210036142  -6.23476943758121926, -57.40974553570311656  -6.22055485899459359, -57.39756929337381308  -5.2360923813641449
# 085088 -52.82101666772864235  -3.27868962766306193, 51.30354996961398228   -3.26819969176571812, -51.29395483286162261  -4.25696130829863062, -52.81682112324738654  -4.26746285480035414, -52.82101666772864235  -3.27868962766306193
# 087087 -49.80112902758474291  -2.25818834983067518, -48.28942601531942103  -2.23129085165131391, -48.26918177207642202  -3.22257572139355819, -49.78624193013653354  -3.24949490855742784, -49.80112902758474291  -2.25818834983067518

extent_074095="-69.9496362634197624 -10.7877784644479604 -68.3401355384619364  -9.7293973305111479"
extent_077095="-65.2752799481130950 -10.9812035370481702 -63.6803303411144910  -9.9471581104979609"
extent_079082="-61.7861721004006270   1.7799465865711850 -60.2741608986789288   2.8272289261749068"
extent_083096="-55.9172549617417829 -12.1209572033416197 -54.3484373041839675 -11.1367011781553504"
extent_082090="-57.4097455357031166  -6.2347694375812193 -55.8693259016744435  -5.2360923813641449"
extent_085088="-52.8210166677286423  -4.2674628548003541 -51.2939548328616226  -3.2681996917657181"
extent_087087="-49.8011290275847429  -3.2494949085574278 -48.2691817720764220  -2.2312908516513139"

ogr2ogr -clipsrc ${extent_077095} ${base_dir}/074095/cloud_biome.shp                ${base_dir}/tmp/cloud_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/074095/forest_biome.shp               ${base_dir}/tmp/forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/074095/hydrography_biome.shp          ${base_dir}/tmp/hydrography_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/074095/no_forest_biome.shp            ${base_dir}/tmp/no_forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/074095/yearly_deforestation_biome.shp                                     ${base_dir}/tmp/yearly_deforestation_biome.shp

ogr2ogr -clipsrc ${extent_077095} ${base_dir}/077095/cloud_biome.shp                ${base_dir}/tmp/cloud_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/077095/forest_biome.shp               ${base_dir}/tmp/forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/077095/hydrography_biome.shp          ${base_dir}/tmp/hydrography_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/077095/no_forest_biome.shp            ${base_dir}/tmp/no_forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/077095/yearly_deforestation_biome.shp ${base_dir}/tmp/yearly_deforestation_biome.shp

ogr2ogr -clipsrc ${extent_077095} ${base_dir}/079082/cloud_biome.shp                ${base_dir}/tmp/cloud_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/079082/forest_biome.shp               ${base_dir}/tmp/forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/079082/hydrography_biome.shp          ${base_dir}/tmp/hydrography_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/079082/no_forest_biome.shp            ${base_dir}/tmp/no_forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/079082/yearly_deforestation_biome.shp ${base_dir}/tmp/yearly_deforestation_biome.shp

ogr2ogr -clipsrc ${extent_077095} ${base_dir}/083096/cloud_biome.shp                ${base_dir}/tmp/cloud_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/083096/forest_biome.shp               ${base_dir}/tmp/forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/083096/hydrography_biome.shp          ${base_dir}/tmp/hydrography_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/083096/no_forest_biome.shp            ${base_dir}/tmp/no_forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/083096/yearly_deforestation_biome.shp ${base_dir}/tmp/yearly_deforestation_biome.shp

ogr2ogr -clipsrc ${extent_077095} ${base_dir}/082090/cloud_biome.shp                ${base_dir}/tmp/cloud_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/082090/forest_biome.shp               ${base_dir}/tmp/forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/082090/hydrography_biome.shp          ${base_dir}/tmp/hydrography_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/082090/no_forest_biome.shp            ${base_dir}/tmp/no_forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/082090/yearly_deforestation_biome.shp ${base_dir}/tmp/yearly_deforestation_biome.shp

ogr2ogr -clipsrc ${extent_077095} ${base_dir}/085088/cloud_biome.shp                ${base_dir}/tmp/cloud_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/085088/forest_biome.shp               ${base_dir}/tmp/forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/085088/hydrography_biome.shp          ${base_dir}/tmp/hydrography_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/085088/no_forest_biome.shp            ${base_dir}/tmp/no_forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/085088/yearly_deforestation_biome.shp ${base_dir}/tmp/yearly_deforestation_biome.shp

ogr2ogr -clipsrc ${extent_077095} ${base_dir}/087087/cloud_biome.shp                ${base_dir}/tmp/cloud_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/087087/forest_biome.shp               ${base_dir}/tmp/forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/087087/hydrography_biome.shp          ${base_dir}/tmp/hydrography_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/087087/no_forest_biome.shp            ${base_dir}/tmp/no_forest_biome.shp
ogr2ogr -clipsrc ${extent_077095} ${base_dir}/087087/yearly_deforestation_biome.shp ${base_dir}/tmp/yearly_deforestation_biome.shp

rm -rf ${base_dir}/tmp

exit 0