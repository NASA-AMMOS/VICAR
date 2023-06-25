#!/usr/bin/env python3
#
# Simultaneaous Localization and Mapping (SLAM) config writer script 
# Documentation for 'configparser' can be found here:
# https://docs.python.org/3/library/configparser.html
#
# The file consists of sections, each of which contains keys with values

# =====================================================================

# *****Parameter description - general parameters:
#
# batch_merge_strategy:         choose between 'full' (merges all at the end) 									and 'incremental', which appends each new one
# batch_overlap:                number of images which overlap between batches
# create_initial_pointings_ply: create a PLY file with initial cam pointings
# create_pandas_dataframes:     create Pandas dataframes from tiepoints and 								navigation files
# dataframe_data_type:          choose between 'camparams' (cahvore) and
#                               'pointing'
# dataset_type:                 choose between 'heli' and 'other'
# images_per_batch:             number of images per batch
# merge_type_navigation:        merge batches via mean, median, mode, quantile
# merge_type_tiepoints:         merge batches via mean, median, mode, quantile
# print_verbose_vicar_output:   decide whether to print intermediate results
# recompute_xyz_after_posegraph:recompute XYZ points with final cameras
# remove_intermediate_files:    delete files in the output dir before a run
# run_final_ba:                 run a final marsnav2 BA with the entire set
#								of cameras
# run_keyframe_selection:       decide whether to run keyframe selection
# run_pose_graph_optimization:  decide wheter to run pose graph optimization
# run_tiepointing_only:         run tiepointing only and exit the script
# use_first_images:             number of images to use at the start of the 								sequence

# =====================================================================
# *****Parameters for 'keyframe_selection':
# disp_connect:                 display tiepoint connectivity matrix for the 									input images going into keyframe selection. 								VALID=(DISP_CONNECT, NODISP_CONNECT),      									DEFAULT="DISP_CONNECT"
# disp_cost:					display keyframe selection cost function 									values per image pair. VALID=(DISP_COST, 									NODISP_COST), DEFAULT="DISP_COST"
# edge_dist: 					distance from the edge of the image to ignore 									for tiepoint purposes, since the fisheye 									effect can affect tiepointing quality. 									DEFAULT=.05
# sequential:                   Choose between 'sequential' and 'nosequential' 									(global) selection modes. DEFAULT="SEQUENTIAL"
# sel_percent:                  Percentage of top-scoring frames to keep during 								global keyframe selection. DEFAULT=80.0
#
# Sample run:
# keyframe_selection INP=filelist OUT=keyframes.txt in_tpt=tiepoints.tpt

# =====================================================================
# *****Parameters for 'marsautotie2', with defaults to be used below:
# cross: 						DEFAULT=-CROSS 
# gtm: 							DEFAULT=3 
# nbest: 						DEFAULT=5 
# numtilts: 					DEFAULT=3
# pair_match: 					DEFAULT=3 
#
# Sample run:
# $MARSLIB/marsautotie2 INP=filelist OUT=tiepoints.tpt PAIR_MATCH=3 \
#     -CROSS GTM=3 NBEST=5 NUMTILTS=3
#
# NOTE: Since marsautotie2 has many parameters, handling of these becomes easier 
# by specifying any that may be tested later on and simply setting their values 
# to the PDF defaults

# =====================================================================
# *****Parameters for 'marsnav2', with defaults to be used below:
# do_bboxcheck                  DEFAULT=NO_BBOXCHECK
# do_mvt                        DEFAULT=DO_MVT
# do_triopt                     DEFAULT=DO_TRIOPT
# inertia:						DEFAULT=0.001
#		Typical values:
# 		Goniometry: inertia=0.001
# 		Ingenuity_flight5 (sol76): inertia=0.001
#		Ingenuity_sol64: inertia=0.001
#		Pahrump_NCAM and any Perseverance NCAM: inertia=0.06
#		Pahrump_MRDI: inertia=0.001 
# loss_function:				DEFAULT=HUBERLOSS
# max_remove					DEFAULT=10 
# remove:						DEFAULT=REMOVE 
#
# Sample run:
# $MARSLIB/marsnav2 inp=image_list.txt out=navigation.xml in_tpt=tiepoints.tpt \
# tp_type=EXT_TP cm_ori=CM_ORI out_sol=tester -do_mvt -do_triopt
#
# NOTE: Since marsnav2 has many parameters, handling of these becomes easier by 
# specifying any that may be tested later on and simply setting their values to 
# the PDF defaults

# =====================================================================
# *****Parameters for 'posegraph', with defaults to be used below:
# edge_dist: 					distance from the edge of the image to ignore 									for tiepoint purposes, since the fisheye 									effect can affect tiepointing quality. 									DEFAULT=.05
# use_essential:                use or not the essential matrix decomposition vs
#                               relative telemetry values to compute relative
#								poses.
#								DEFAULT=NOUSE_ESSENTIAL
#
# Sample run:
# posegraph inp=image_list.txt out=updated_pointing.nav out_g20=relposes.g2o \
# in_tpt=images.tpt

# =====================================================================

import configparser

config = configparser.ConfigParser()

config['DEFAULT'] = {
	# general parameters:
			'batch_merge_strategy': 'incremental',  # 'full', 'incremental'
			'batch_overlap': '2',
			'create_initial_pointings_ply': 'True',
			'create_pandas_dataframes': 'True',
			'dataframe_data_type': 'pointing',
			'dataset_type': 'heli',
			'images_per_batch': '3',
			'marslib_location': '/usr/local/vicar/m20-d/mars/lib/x86-64-linx/',
			'merge_type_navigation': 'mean', 
			'merge_type_tiepoints': 'mean', 
			'print_verbose_vicar_output': 'True',
			'recompute_xyz_after_posegraph': 'False',
			'remove_intermediate_files': 'False',
			'run_final_ba': 'True',
            'run_keyframe_selection': 'True',
			'run_pose_graph_optimization': 'True',
			'run_tiepointing_only': 'False',
			'user': 'mhessflores',
			 #
	# keyframe_selection:
			 # Use a local build or the $MARSLIB location:
			'keyframe_selection_location':  
				'/usr/local/vicar/m20-d/mars/lib/x86-64-linx/keyframe_selection',
				#'/home/mhessflores/code/keyframe_selection/keyframe_selection',
			'ks_disp_connect': 'DISP_CONNECT=DISP_CONNECT',
			'ks_disp_cost': 'DISP_COST=DISP_COST',
			'ks_edge_dist': 'EDGE_DIST=.05',
			'ks_sequential': 'SEQUENTIAL=SEQUENTIAL',
			'ks_sel_percent': 'SEL_PERCENT=80.0',
			#  
	# marsautotie2:
			# NOTE: Use -NOUPSAMPLE for large image sizes
			'tie_cross': '-CROSS', 
			'tie_gtm': 'GTM=3', 
			'tie_nbest': 'NBEST=5',
			'tie_numtilts': 'NUMTILTS=3',
			'tie_pair_match': 'PAIR_MATCH=3', 
			 #
	# marsnav2:
			 'ba_do_bboxcheck': 'DO_BBOXCHECK=NO_BBOXCHECK', # NO_BBOXCHECK def
			 'ba_do_mvt': 'DO_MVT=DO_MVT', # NO_MVT default in PDF
			 'ba_do_triopt': 'DO_TRIOPT=DO_TRIOPT', # NO_TRIOPT default in PDF
			 'ba_inertia': 'INERTIA=0.001', # 0 default in PDF
			 'ba_loss_function': 'LOSS_FUNCTION=HUBERLOSS', 
			 'ba_max_remove': 'MAX_REMOVE=10', # Used if REMOVE set
			 'ba_remove': 'REMOVE=REMOVE', # NOREMOVE default in PDF
			 							   # TRIVIALLOSS default in PDF
			 # Use a local build or the $MARSLIB location:
			 'marsnav2_location': \
				'/usr/local/vicar/m20-d/mars/lib/x86-64-linx/marsnav2',
			    #'/home/mhessflores/code/marsnav2_mvt/marsnav2',
			 #
	# posegraph:
			 # Use a local build or the $MARSLIB location:
			 'posegraph_location': \
				'/usr/local/vicar/m20-d/mars/lib/x86-64-linx/posegraph',
			 #'posegraph_location': '/home/mhessflores/code/posegraph/posegraph',
			 'pg_edge_dist': 'EDGE_DIST=.05',
			 'pg_use_essential': 'USE_ESSENTIAL=NOUSE_ESSENTIAL'
			 }

config['small_run'] = {}
config['small_run']['batch_merge_strategy'] = 'full'
config['small_run']['batch_overlap'] = '1'  # '1'
config['small_run']['create_initial_pointings_ply'] = 'True'
config['small_run']['create_pandas_dataframes'] = 'True'
config['small_run']['images_per_batch'] = '2'  # '2'
config['small_run']['print_verbose_vicar_output'] = 'False'
config['small_run']['remove_intermediate_files'] = 'False'
config['small_run']['run_final_ba'] = 'True'
config['small_run']['run_keyframe_selection'] = 'False'
config['small_run']['run_pose_graph_optimization'] = 'True'
# Specific to 'small_run'; set to '-1' to process all images
config['small_run']['use_first_images'] = '6'  

config['full_run'] = {}
config['full_run']['batch_overlap'] = '3'
config['full_run']['images_per_batch'] = '5'

with open('slam_config.txt', 'w') as configfile:
    config.write(configfile)
