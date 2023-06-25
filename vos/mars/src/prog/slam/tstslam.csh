#!/bin/tcsh

# This is a test script for slam.py.
# stdout/err is captured in tstslam.log inside each test directory
#
# NOTE: a config file is necessary to run this script. If slam_config.txt
# doesn't exist in the top-level directory:
#
# 1) pip install -r requirements.txt
# 2) ./slam_config_writer.py
# 3) Run this script

# Available datasets:
set dataset = test/Goniometry
#set dataset = test/Ingenuity_flight3 
#set dataset = test/Ingenuity_flight4 
echo "Dataset: ${dataset}"

# Important note: due to an issue with processing fisheye lenses, Heli
# flights need to be run differently. The below environment variable needs to
# be set, and extra parameters need to be added to marsnav2. To account for 
# this, do the following:
#
# 1) Set the following environment variable:
setenv MARS_CONFIG_PATH \
/home/ayoubfra/vicar/mvor/bundle/marsnav/heliDev/calibration:$MARS_CONFIG_PATH
#
# 2) Open ../slam_config_writer.py, and set 'dataset_type': 'heli'
# This will set set the following variable in slam.py:
# 		extra_heli_params = "fixed_site=3 -site coord_index=3" 
#
# 3) ./slam_config_writer
# 4) ./tstslam.csh 


# Main script:

python ./slam.py slam_config.txt $dataset/image_list.txt \
	--outdir $dataset/output/ --runtype small_run -v >& $dataset/tstslam.log


echo "*****PROCESSING COMPLETE*****" 

echo "\nThe following files should have been generated (N = number of batches):"
echo "     connectivity.txt"
echo "     final_image_list.txt"
echo "     image_list_cropped.txt"
echo "     images_batch{0-N}.txt"
echo "     initial_poses.ply"
echo "     initial_poses.txt"
echo "     navigation_all_batches.csv"
echo "     navigation_all_batches.xml"
echo "     navigation_merge.log"
echo "     navigation_temp.xml"
echo "     navigation_batch{0-N}.csv"
echo "     navigation_batch{0-N}.log"
echo "     navigation_batch{0-N}.nav"
echo "     navigation_final.log"
echo "     navigation_final.xml"
echo "     navigation_all_batches.csv"
echo "     navigation_all_batches.xml"
echo "     posegraph_poses.g2o"
echo "     poses_original.txt"
echo "     poses_optimized.txt"
echo "     tiepoints_all.tpt"
echo "     tiepoints_all_final.csv"
echo "     tiepoints_all_final.tpt"
echo "     tiepoints_batch{0-N}_final.csv"
echo "     tiepoints_batch{0-N}_final.tpt"
echo "     tiepoints_batch{0-N}.log"
echo "     tiepoints_batch{0-N}.tpt"
echo "     tiepoints_for_final_BA.log"
echo "NOTE: The final output of SLAM is navigation_final.xml"
