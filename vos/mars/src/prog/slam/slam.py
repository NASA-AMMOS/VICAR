#!/usr/bin/env python3
#
# Simultaneous Localization and Mapping (SLAM) script 
#
# To run:
# 	python slam.py slam_config filelist --outdir OUTDIR --runtype RUNTYPE   
# <OR>
# 	./slam.py slam_config filelist --outdir OUTDIR --runtype RUNTYPE
#
# A tstslam.csh script is provided, where datasets and parameters can be 
# specified. Simply call:
# 	./tstslam.csh 
#
# Parameters:
#
# argv[1] = path to the SLAM config file
# argv[2] = path to the list of input files
# OPTIONAL argv[3] = output directory for generated files
# OPTIONAL argv[4] = run type; default and other types provided in 
# slam_config_writer.py
#
# Note: Changes to parameters must be made in slam_config_writer.py, followed by 
# ./slam_config_writer to update slam_config.txt

# ==============================================================================

import sys
import os
import configparser
import argparse
import math
import subprocess


# ==============================================================================
# Read input script parameters, using Python's 'argparser':

parser = argparse.ArgumentParser()
parser.add_argument("configfile", help="Input SLAM config file location")
parser.add_argument("inputimages", help="Input image file locations")
parser.add_argument("--outdir", help="Output directory")
parser.add_argument("--runtype", help="Type of run to perform")
parser.add_argument("-v", "--verbose", action="store_true", \
					help="Print verbose script (non-VICAR) output")
args = parser.parse_args()
verbose = args.verbose

configfile = args.configfile
print('Input SLAM config file location: ', configfile)

input_images_file = args.inputimages
print('Input image file locations: ', input_images_file)

if args.outdir:
	output_directory = args.outdir
else:
	print('No output directory provided; will create output/ in \
		  current directory.')
	output_directory = "output"

check_dir = os.path.isdir(output_directory)
if not check_dir:  # create if it doesn't exist
	os.mkdir(output_directory)  # use makedirs if intermediate dirs needed!
	print('Created output directory: ', output_directory)
else:
	print('Output directory already exists: ', output_directory)

if args.runtype:
	run_type = args.runtype
	print('Run type: ', run_type)
else:
	run_type = 'DEFAULT'
	print('No run type provided; default run parameters will be used.')

# Done parsing, print out intermediate values:

inputimages = open(str(args.inputimages), "rt")  
initial_image_list = inputimages.readlines()  
inputimages.close()
numimages_initial = len(initial_image_list)
print('Number of initial images: ', numimages_initial)


# ==============================================================================

# Read input parameters from the SLAM config file, and set up parameters for 
# VICAR sub-processes

# -----General parameters-----

config = configparser.ConfigParser()
config.read(configfile)
#print('Available test types: ', config.sections())

batch_merge_strategy = config[run_type]['batch_merge_strategy']
batch_overlap = int(config[run_type]['batch_overlap'])
create_initial_pointings_ply = config.getboolean(run_type, \
	'create_initial_pointings_ply')
create_pandas_dataframes = config.getboolean(run_type, \
	'create_pandas_dataframes')
dataframe_data_type = config[run_type]['dataframe_data_type']
dataset_type = config[run_type]['dataset_type']
images_per_batch = int(config[run_type]['images_per_batch']) 
marslib_location = config[run_type]['marslib_location']
merge_type_navigation = config[run_type]['merge_type_navigation']
merge_type_tiepoints = config[run_type]['merge_type_tiepoints']
path_to_scripts = os.getcwd()
recompute_xyz_after_posegraph = config.getboolean(run_type, 'recompute_xyz_after_posegraph')
remove_intermediate_files = config.getboolean(run_type, \
	'remove_intermediate_files')
run_final_ba = config.getboolean(run_type, 'run_final_ba') 
run_keyframe_selection = config.getboolean(run_type, 'run_keyframe_selection') 
run_pose_graph_optimization = config.getboolean(run_type, 'run_pose_graph_optimization')
run_tiepointing_only = config.getboolean(run_type, 'run_tiepointing_only')
user = config[run_type]['user']
print_verbose_vicar_output = config.getboolean(run_type, \
	'print_verbose_vicar_output') 

if verbose:
	print('Batch merge strategy: ', batch_merge_strategy)
	print('Number of images which overlap between batches: ', batch_overlap)
	print('Create initial pointings PLY?', create_initial_pointings_ply)
	print('Dataframe data type: ', dataframe_data_type)
	print('Dataset type: ', dataset_type)
	print('Number of images per batch: ', images_per_batch)
	print('Run final BA? ', run_final_ba)
	print('Apply keyframe selection? ', run_keyframe_selection)
	print('Location of $MARSLIB: ', marslib_location)
	print('Apply pose graph optimization? ', run_pose_graph_optimization)
	print('Run tiepointing only? ', run_tiepointing_only)
	print('User: ', user)
	print('Verbose VICAR output? ', print_verbose_vicar_output)


# -----Parameters specific to 'small_run'-----

if run_type == 'small_run':
	use_first_images = int(config[run_type]['use_first_images'])
	if use_first_images > 0:  # Set to '-1' to process all images
		print(f'\nUsing the first {use_first_images} images.')
		temp = initial_image_list[:use_first_images]
		initial_image_list = temp
		use_first_images_file = os.path.join(output_directory, 
										 "image_list_cropped.txt")
		f = open(use_first_images_file, "w")
		for j in range(len(initial_image_list)):
			f.write(initial_image_list[j])
			# print(initial_image_list[j])
		f.close()
		input_images_file = use_first_images_file


# -----Parameters specific to 'full_run': nothing at the moment-----
#if run_type == 'full_run'
#	.....


# -----'keyframe_selection' parameters-----
keyframe_selection = config[run_type]['keyframe_selection_location']
#keyframe_selection = marslib_location + 'keyframe_selection'
ks_disp_connect = config[run_type]['ks_disp_connect']
ks_disp_cost = config[run_type]['ks_disp_cost']
ks_edge_dist = config[run_type]['ks_edge_dist']
ks_sequential = config[run_type]['ks_sequential']
ks_sel_percent = config[run_type]['ks_sel_percent']
 

# -----marsautotie2' parameters-----
marsautotie2 = marslib_location + 'marsautotie2'
tie_cross = config[run_type]['tie_cross']
tie_gtm = config[run_type]['tie_gtm']
tie_nbest = config[run_type]['tie_nbest']
tie_numtilts = config[run_type]['tie_numtilts']
tie_pair_match = config[run_type]['tie_pair_match']


# -----marsnav2' parameters-----
marsnav2 = config[run_type]['marsnav2_location']
#marsnav2 = marslib_location + 'marsnav2'
ba_do_bboxcheck = config[run_type]['ba_do_bboxcheck']
ba_do_mvt = config[run_type]['ba_do_mvt'] 
ba_do_triopt = config[run_type]['ba_do_triopt']
ba_inertia = config[run_type]['ba_inertia']
ba_loss_function = config[run_type]['ba_loss_function']
ba_max_remove = config[run_type]['ba_max_remove']
ba_remove = config[run_type]['ba_remove'] 


# -----posegraph' parameters-----
posegraph = config[run_type]['posegraph_location'] 
#posegraph = marslib_location + 'posegraph'
pg_edge_dist = config[run_type]['pg_edge_dist'] 
pg_use_essential = config[run_type]['pg_use_essential'] 


if print_verbose_vicar_output:
	print('VICAR-specific parameters:')
	print('keyframe_selection: ', ks_disp_connect)
	print('keyframe_selection: ', ks_disp_cost)
	print('keyframe_selection: ', ks_edge_dist)
	print('keyframe_selection: ', ks_sequential)
	print('keyframe_selection: ', ks_sel_percent)
	print('marsautotie2: ', tie_cross)
	print('marsautotie2: ', tie_gtm)
	print('marsautotie2: ', tie_nbest)
	print('marsautotie2: ', tie_numtilts)
	print('marsautotie2: ', tie_pair_match)
	print('marsnav2: ', ba_do_bboxcheck)
	print('marsnav2: ', ba_do_mvt)
	print('marsnav2: ', ba_do_triopt)
	print('marsnav2: ', ba_inertia)
	print('marsnav2: ', ba_loss_function)
	print('marsnav2: ', ba_max_remove)
	print('marsnav2: ', ba_remove)


# Paths to utility scripts:

slam_nav_dataframe = path_to_scripts + '/util/slam_nav_dataframe.py' 
slam_tie_dataframe = path_to_scripts + '/util/slam_tie_dataframe.py'
slam_dataframe2params = path_to_scripts + '/util/slam_dataframe2params.py'
slam_merge_nav_df = path_to_scripts + '/util/slam_merge_nav_df.py'
slam_merge_nav = path_to_scripts + '/util/slam_merge_nav.py'
slam_headers2camparams = path_to_scripts + '/util/slam_headers2camparams.py'
# Completely replaced by the latest version of posegraph:
# slam_run_posegraph = path_to_scripts + '/util/slam_run_posegraph.py'

# ==============================================================================

# Check for special conditions; exit if values not allowed, or apply changes:

# NOTE: The overlap MUST be at least one number smaller than the batch size,
# otherwise there is no sliding window!

if batch_overlap > images_per_batch - 1:
	print('Overlap must be at least one smaller than batch size! Exiting.')
	quit()

# Important note: due to an issue with processing fisheye lenses, Heli
# flights need to be run as follows:

if dataset_type == 'heli':

	# Set the following in the provided tstslam.csh script:
	# setenv MARS_CONFIG_PATH /home/ayoubfra/vicar/mvor/bundle/marsnav/heliDev/calibration:$MARS_CONFIG_PATH
	extra_heli_params = "fixed_site=3 -site coord_index=3"
else:
	extra_heli_params = ""

# ==============================================================================
# *****************************Main script:****************************
# ==============================================================================

# Clean intermediate files in the output directory, if needed. Otherwise, 
# slam skips creating files already there and continues with only the
# steps not run yet. This should always be set to true when running on a 
# new dataset in the same output directory!

if remove_intermediate_files:
	print('Removing intermediate files in the output directory prior '
		 'to running SLAM.') 
	subprocess.run(['rm', output_directory+'/*'])

if verbose:
	print('\nInitial image list, prior to keyframe selection: \n')
	for line in initial_image_list:
		print(line)

# Create a PLY file from the initial camera centers (acquired from the image 
# labels), which can be helpful in debugging:

if create_initial_pointings_ply:
	if verbose:
		print('Creating initial pointings PLY from camera centers.')
	subprocess.run([slam_headers2camparams, input_images_file, \
					os.path.join(output_directory, 'initial_poses.txt'), \
					'--gen_ply', 'True'])


# ==============================================================================
# 1) OPTIONAL Apply keyframe selection
# ==============================================================================
#
# NOTE: Avoid doing this if possible because it's too slow! It's more 
# efficient to run keyframe selection outside of this script and use the
# generated image list as input
#
# If a keyframes file is provided, both it and the tiepoints file it was 
# produced from must be supplied, and MUST be named respectively 
# tiepoints_all.tpt and keyframes.txt so they're recognized below and not 
# re-computed

if run_keyframe_selection:

	print('Applying keyframe selection')

	# Obtain tiepoints for ALL images (can be very slow):

	logfile = os.path.join(output_directory, 				  								"tiepoints_for_keyframe_selection.log")
	out_tiepoints = os.path.join(output_directory, "tiepoints_all.tpt")
	if os.path.exists(out_tiepoints):
		print(f'{out_tiepoints} exists; will not recompute tiepoints.')
	else:
		log = open(logfile, "w")
		subprocess.run([marsautotie2, 'INP='+input_images_file, \
				   'OUT='+out_tiepoints, tie_pair_match, tie_cross, tie_gtm,\
				    tie_nbest, tie_numtilts], stdout=log)
		log.close()

	# Now, run keyframe_selection:

	out_keyframes = os.path.join(output_directory, "keyframes.txt")
	if os.path.exists(out_keyframes):
		print(f'{out_keyframes} exists; will not recompute keyframes.')
	else:
		logfile = os.path.join(output_directory, "keyframe_selection.log")
		log = open(logfile, "w")
		subprocess.run([keyframe_selection, 'INP='+input_images_file, \
				   'OUT='+out_keyframes, 'IN_TPT='+out_tiepoints, \
				   ks_edge_dist, ks_sequential, ks_sel_percent], \
				   stdout=log)
		log.close()

	# Check to see if a keyframes file was actually generated. If not,
	# something went wrong, so keep the original image list:

	if os.path.exists(out_keyframes):
		keyframe_images = open(str(out_keyframes), "rt")  
		final_image_list = keyframe_images.readlines()  
		keyframe_images.close()
		numimages_final = len(final_image_list)

		if verbose:
			print('\nFinal number of images: ', numimages_final)
			print('\nFinal image list, after keyframe selection: ')
			for line in final_image_list:
				print(line)

	else:
		print('Not enough keyframes found!')
		print('Keeping the original list of frames.')
		final_image_list = initial_image_list
		numimages_final = len(final_image_list)

else:  # no keyframe_selection

	final_image_list = initial_image_list
	numimages_final = len(final_image_list)

	# if verbose:	
	print('No keyframe selection applied; final image list unchanged '
		  'from initial.')
	print('Final number of images: ', numimages_final)

final_images_file = os.path.join(output_directory, 
							 "final_image_list.txt")
f = open(final_images_file, "w")
for j in range(len(final_image_list)):
	f.write(final_image_list[j])
	# print(final_image_list[j])
f.close()


# ==============================================================================
# 2) Compute the number of batches needed, and create tiepoint and navigation
# files for each
# ==============================================================================

sliding_factor = images_per_batch - batch_overlap
numbatches = math.ceil((numimages_final-batch_overlap)/(images_per_batch-batch_overlap))

print('Number of batches: ', numbatches)
print('Sliding factor (images per batch - overlap): ', sliding_factor)

counter = 0

for i in range(numbatches):

	print('\nCurrent batch: ', i)

	current_batch_images_path = os.path.join(output_directory, \
										 "images_batch" + \
										 str(i) + ".txt")

	f = open(current_batch_images_path, "w")
	print('Current batch images: ')
	for j in range(images_per_batch):
		if counter+j < numimages_final:
			f.write(final_image_list[counter+j])
			print(final_image_list[counter+j])
	f.close()

	# Now, given the current batch, extract tiepoints.
	# NOTE: If these exist already, don't re-process!

	out_tiepoints = os.path.join(output_directory, "tiepoints_batch" + \
								 str(i) + ".tpt")
	print('Current tiepoints saved to: ', out_tiepoints)

	if os.path.exists(out_tiepoints):
		print(f'{out_tiepoints} exists; will not recompute tiepoints.')
	else:

		logfile = os.path.join(output_directory, "tiepoints_batch" + \
							   str(i) + ".log")
		log = open(logfile, "w") 
		subprocess.run([marsautotie2, 'INP='+current_batch_images_path, \
					   'OUT='+out_tiepoints, tie_pair_match, tie_cross, \
					   tie_gtm, tie_nbest, tie_numtilts], stdout=log)
		log.close()
	
	# Stop here and exit if 'run_tiepointing_only' is set:

	if run_tiepointing_only:
		print('Running marsautotie2 only; exiting.')
		quit()
	
	# Now, apply 'marsnav2' (eventually interchangeably with posegraph):
	# NOTE 1: If nav files exist already, don't re-process!
	# NOTE 2: marsnav2 can be directly replaced by posegraph in this step, or 
	# one can be run after the other, but given smaller batch sizes, it's 
	# preferable to run marsnav2 here and leave posegraph for the very end

	out_nav = os.path.join(output_directory, "navigation_batch" + \
						   str(i) + ".nav")
	if os.path.exists(out_nav):
		print(f'{out_nav} exists; will not re-run marsnav2.')
	else:

		logfile = os.path.join(output_directory, "navigation_batch" + \
							   str(i) + ".log")
		log = open(logfile, "w") 
		subprocess.run([marsnav2, 'INP='+current_batch_images_path, \
					   'OUT='+out_nav, 'IN_TPT='+out_tiepoints, \
					   'TP_TYPE=EXT_TP', 'CM_ORI=CM_ORI', \
					   'OUT_SOL=TESTER', ba_remove, ba_max_remove, \
					   ba_inertia, ba_loss_function, ba_do_mvt, \
					   ba_do_triopt, ba_do_bboxcheck, \
					   extra_heli_params], stdout=log)
		log.close()

		# The final tiepoints files get saved to 'tptlist.tpt' in the 
		# RUN DIRECTORY (path_to_scripts), so they must be moved and renamed
		# to be batch-specific:

		augmented_tpt_file = os.path.join(path_to_scripts, "tptlist.tpt")
		if os.path.exists(augmented_tpt_file):
			subprocess.run(['mv', augmented_tpt_file, \
					os.path.join(output_directory,
					"tiepoints_batch" + str(i) + "_final.tpt")])
		
	# Next, sort the content of the current AUGMENTED tiepoint file into a
	# Pandas dataframe. Note that logging is not currently 
	# needed here, but can add "stdout=log" to the subprocess
	# call as in cases above if needed.

	if create_pandas_dataframes:
		augmented_tpt_file_current = os.path.join(output_directory,
					"tiepoints_batch" + str(i) + "_final.tpt")
		subprocess.run([slam_tie_dataframe, \
						 augmented_tpt_file_current, str(counter)]) # '-v'
	
	# Do the same with the current navigation file:

	if create_pandas_dataframes:
		subprocess.run([slam_nav_dataframe, \
						out_nav, str(counter), dataframe_data_type])  # '-v'
	
	# Merge batches incrementally:

	if batch_merge_strategy == 'incremental':

		print(f'\nMerging batches incrementally; appending batch {i}')

		# Create XML file from the current navigation files:

		print('Creating a single navigation file from all currently '
	    	  'available batches.')
		logfile = os.path.join(output_directory, "navigation_current_merge.log")
		log = open(logfile, "w")
		subprocess.run([slam_merge_nav, output_directory], \
						stdout=log)  # '-v'
		log.close()
		
		# Create a Pandas dataframe from the resulting 
		# 'navigation_all_batches.xml" file:

		subprocess.run([slam_nav_dataframe, \
							os.path.join(output_directory, 
							"navigation_all_batches.xml"), str(0), \
							dataframe_data_type])  #'-v'

		if create_pandas_dataframes:	
	
			# Extract out the cam parameters or pointing to file, for further 
			# analysis. Note that navigation_all_batches.csv was generated 
			# by the above call

			subprocess.run([slam_dataframe2params, \
							os.path.join(output_directory, 
							"navigation_all_batches.csv"), \
							dataframe_data_type])  # '-v'
			print(f'Merging all currently available navigation '
				   'dataframes up to batch {i} into a single file.')
			logfile = os.path.join(output_directory, 
					  "navigation_table_up_to_batch_" + str(i) + ".log")
			log = open(logfile, "w")
			subprocess.run([slam_merge_nav_df, \
				output_directory, merge_type_navigation, \
				dataframe_data_type], stdout=log)  # '-v'
			log.close()

			# Rename the obtained 'navigation_table.csv' file to
			# 'navigation_table_up_to_batch_$i.csv':

			file1 = os.path.join(output_directory, "navigation_table.csv")
			file2 = os.path.join(output_directory, 					"navigation_table_up_to_batch_" + str(i) + ".csv")
			os.rename(file1, file2)

	counter = counter + sliding_factor


# ==============================================================================
# 3) If specified, "join" ALL of the batch navigation files into one.
# Merging tiepoint files is currently unsupported
# ==============================================================================
#
# NOTE 1: merge types allowed: mean, median, mode, quantile
# NOTE 2: can add "-v" for verbose output

if batch_merge_strategy == 'full':

	# Create XML file from the final navigation files:

	print('\nCreating a single navigation file from all available batches.')
	logfile = os.path.join(output_directory, "navigation_merge.log")
	log = open(logfile, "w")
	subprocess.run([slam_merge_nav, output_directory, \
					 '-v'], stdout=log)
	log.close()
	
	# Create a Pandas .csv dataframe from the resulting 
	# 'navigation_all_batches.xml" file, with the same base name:

	subprocess.run([slam_nav_dataframe, \
		os.path.join(output_directory, "navigation_all_batches.xml"), \
		str(0), dataframe_data_type])  # '-v'

	"""
	if create_pandas_dataframes:	

		# Extract out the cam parameters or pointing to file, for further 
		# analysis. Note that navigation_all_batches.csv was generated by 
		# the above call

		subprocess.run([slam_dataframe2params, \
						os.path.join(output_directory, 
						"navigation_all_batches.csv"), \
						dataframe_data_type])  # '-v'
		print('Merging all available navigation dataframes into a '
			  'single file, navigation_table.csv.')
		logfile = os.path.join(output_directory, \
						 "navigation_table_all_batches.log")
		log = open(logfile, "w")
		subprocess.run([slam_merge_nav_df, \
					output_directory, merge_type_navigation, \
					dataframe_data_type],  # '-v'
					stdout=log)
		log.close()

		# Rename the obtained 'navigation_table.csv' file to
		# 'navigation_table_final.csv':

		os.rename(os.path.join(output_directory, "navigation_table.csv"), \
			os.path.join(output_directory, "navigation_table_all_batches.csv"))
	"""


# ==============================================================================
# 4) OPTIONAL Run a final bundle adjustment to further adjust the cameras 
# and to obtain the final set of XYZ points
# ==============================================================================

if run_final_ba:

	print('\nFinal bundle adjustment run.')
	
	# If keyframe selection was run (or a previous run was performed), we have 
	# the tiepoints file and don't need to re-compute it

	if os.path.exists(os.path.join(output_directory, "tiepoints_all.tpt")):
		out_tiepoints = os.path.join(output_directory, "tiepoints_all.tpt")
	else:
		# Obtain tiepoints for ALL images (can be very slow):
		print('Computing tiepoints across ALL ' 
			  'images first (can be very slow).')
		logfile = os.path.join(output_directory,  							  							"tiepoints_for_final_BA.log")
		log = open(logfile, "w")
		out_tiepoints = \
			os.path.join(output_directory, "tiepoints_all.tpt")
		subprocess.run([marsautotie2, 'INP='+final_images_file, \
				   'OUT='+out_tiepoints, tie_pair_match, tie_cross, tie_gtm,\
				   tie_nbest, tie_numtilts], stdout=log)
		log.close()

	# Run marsnav2, using the navigation created from all batches as the input
	# navtable:

	out_nav = os.path.join(output_directory, "navigation_final.xml")
	navtable = os.path.join(output_directory, "navigation_all_batches.xml") 
	logfile = os.path.join(output_directory, "navigation_final.log")
	log = open(logfile, "w") 
	subprocess.run([marsnav2, 'INP='+final_images_file, \
					'OUT='+out_nav, 'IN_TPT='+out_tiepoints, \
					'TP_TYPE=EXT_TP', 'CM_ORI=CM_ORI', 'OUT_SOL=TESTER', \
					 ba_remove, ba_max_remove, ba_inertia, ba_loss_function,\
					'NAVTABLE='+navtable, ba_do_mvt, ba_do_triopt, \
					ba_do_bboxcheck, extra_heli_params], stdout=log)
	log.close()

	# The final tiepoints files get saved to 'tptlist.tpt' in the 
	# run directory (path_to_scripts), so they must be moved:
	augmented_tpt_file = os.path.join(path_to_scripts, "tptlist.tpt")
	if os.path.exists(augmented_tpt_file):
		subprocess.run(['mv', augmented_tpt_file, \
				os.path.join(output_directory,
				"tiepoints_all_final.tpt")])

	# Finally, create Pandas dataframes from the resulting 
	#"navigation_final.xml" and "tiepoints_all_final.tpt" files:

	if create_pandas_dataframes:
		subprocess.run([slam_nav_dataframe, \
				os.path.join(output_directory, "navigation_final.xml"), \
				str(0), dataframe_data_type])  # '-v'
		subprocess.run([slam_tie_dataframe, \
				os.path.join(output_directory, "tiepoints_all_final.tpt"), \
				str(0)]) # '-v'
	

# ==============================================================================
# 5) OPTIONAL Apply pose graph optimization on the final camera pointings
# ==============================================================================
# Finally, pose graph optimization is performed over all poses to correct the
# remaining drift of camera poses. After optimization, we must update the XYZ
# points using the optimized poses

if run_pose_graph_optimization:

	print('Applying pose graph optimization')

	# If keyframe selection was run (or a previous run was performed), we have 
	# the tiepoints file and don't need to re-compute it

	if os.path.exists(os.path.join(output_directory, "tiepoints_all.tpt")):
		out_tiepoints = os.path.join(output_directory, "tiepoints_all.tpt")
	else:
		# Obtain tiepoints for ALL images (can be very slow):
		print('Computing tiepoints across ALL ' 
			  'images first (can be very slow).')
		logfile = os.path.join(output_directory,  							  							"tiepoints_for_posegraph.log")
		log = open(logfile, "w")
		out_tiepoints = \
			os.path.join(output_directory, "tiepoints_all.tpt")
		subprocess.run([marsautotie2, 'INP='+final_images_file, \
				   'OUT='+out_tiepoints, tie_pair_match, tie_cross, tie_gtm,\
				   tie_nbest, tie_numtilts], stdout=log)
		log.close()

	# Run posegraph:

	out_nav_pg = os.path.join(output_directory, "navigation_posegraph.xml")
	out_g2o = os.path.join(output_directory, "posegraph_poses.g2o")
	logfile = os.path.join(output_directory, "navigation_posegraph.log")
	log = open(logfile, "w") 
	subprocess.run([posegraph, 'INP='+final_images_file, \
					'OUT='+out_nav_pg, 'OUT_G2O='+out_g2o, \
					'IN_TPT='+out_tiepoints, 'CM_ORI=CM_ORI', \
					'OUT_SOL=TESTER', pg_edge_dist, pg_use_essential], \
					stdout=log)
	log.close()

	# Create a Pandas dataframes from the resulting 
	#"navigation_posegraph.xml":

	if create_pandas_dataframes:
		subprocess.run([slam_nav_dataframe, \
				os.path.join(output_directory, "navigation_posegraph.xml"), \
				str(0), dataframe_data_type])  # '-v'

	# A few files get saved to the run directory (path_to_scripts), so they 
	# must be moved:
	connectivity_file = os.path.join(path_to_scripts, "connectivity.txt")
	poses_original_file = os.path.join(path_to_scripts, "poses_original.txt")
	poses_optimized_file = os.path.join(path_to_scripts, "poses_optimized.txt")
	subprocess.run(['mv', connectivity_file, \
				os.path.join(output_directory, "connectivity.txt")])
	subprocess.run(['mv', poses_original_file, \
				os.path.join(output_directory, "poses_original.txt")])
	subprocess.run(['mv', poses_optimized_file, \
				os.path.join(output_directory, "poses_optimized.txt")])

	if recompute_xyz_after_posegraph:

		# Run marsnav2, using the navigation created posegraph as the input
		# navtable, just to re-compute the XYZ points:

		print('XYZ points being recomputed with fixed (final) camera \
			   pointings after pose graph optimization.')

		# Use ALL images as reference, so that none of them are adjusted
		# (numbering starts at '1'):
		
		refimage = '1,-'+str(numimages_final)
		print('Reference images (all): ', refimages)

		out_nav = os.path.join(output_directory, \
				 "navigation_after_posegraph.xml")
		navtable = os.path.join(output_directory, "navigation_posegraph.xml") 
		logfile = os.path.join(output_directory, \
				 "navigation_after_posegraph.log")
		log = open(logfile, "w") 
		subprocess.run([marsnav2, 'INP='+final_image_list, \
				'OUT='+out_nav, 'IN_TPT='+out_tiepoints, \
			    'TP_TYPE=EXT_TP', 'CM_ORI=CM_ORI', 'OUT_SOL=TESTER', \
				 ba_remove, ba_max_remove, ba_inertia, ba_loss_function,\
			    'REFIMAGE='+refimage, 'NAVTABLE='+navtable, \
				extra_heli_params], stdout=log)
		log.close()

		# The final tiepoints files get saved to 'tptlist.tpt' in the 
		# run directory (path_to_scripts), so they must be moved:
		augmented_tpt_file = os.path.join(path_to_scripts, "tptlist.tpt")
		if os.path.exists(augmented_tpt_file):
			subprocess.run(['mv', augmented_tpt_file, \
					os.path.join(output_directory,
					"tiepoints_xyz_after_posegraph.tpt")])

		# Finally, create Pandas dataframes from the resulting 
		#"navigation_final.xml" and "tiepoints_all_final.tpt" files:

		if create_pandas_dataframes:
			subprocess.run([slam_nav_dataframe, \
					os.path.join(output_directory, "navigation_after_posegraph.xml"), \
					str(0), dataframe_data_type])  # '-v'
			subprocess.run([slam_tie_dataframe, \
					os.path.join(output_directory, "tiepoints_xyz_after_posegraph.tpt"), \
					str(0)]) # '-v'

else:
	print('No pose graph optimization applied; final camera pointings '
		  'after SLAM not further optimized.')

# ==============================================================================

