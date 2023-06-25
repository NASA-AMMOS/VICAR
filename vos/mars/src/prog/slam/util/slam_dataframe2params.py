#!/usr/bin/env python3

# This script reads in navigation CSV Pandas dataframes files and returns 
# a single merged table containing information for all scene cameras.

# ==============================================================================

import os
import sys
import argparse
import numpy as np
import pandas as pd
import glob
import csv

# ==============================================================================

# Main script:

# Use Python's 'argparser':
parser = argparse.ArgumentParser()
parser.add_argument("input_navcsv", help="Input dataframe CSV file")
parser.add_argument("mode", help="Choose between 'camparams' (CAHVORE) \
	and 'pointing'")
parser.add_argument("-v", "--verbose", action="store_true", help="Print verbose script output")
args = parser.parse_args()
verbose = args.verbose

if verbose:
	print('\n-----SLAM_DATAFRAME2PARAMS.PY-----\n')
	print('Input navigation dataframe CSV file: ', args.input_navcsv)
	print('Dataframe data type: ', args.mode)

mode = args.mode

currentfiledata = pd.read_csv(args.input_navcsv)
if verbose:
	print('Contents of the current file: ', currentfiledata)

# Save just the final parameter values to file, for use by 'parse_cahvore' 
# or the posegraph framework:

if mode == 'camparams':
	outfile_camparams = os.path.dirname(args.input_navcsv) + \
			'/navigation_cam_params.txt'
if mode == 'pointing':
	outfile_camparams = os.path.dirname(args.input_navcsv) + \
			'/navigation_pointings.txt'

numcols = len(currentfiledata.columns)

# Initialize empty dataframe:
final_values_cam_params = pd.DataFrame({'A' : []})

# NOTE: for some reason the number of columns here needs to be two greater
# than in the same call within merge_navigation_dataframes.py, probably 
# because the CSV counts the index and image numbers as columns, not just
# the data: 

if mode == 'camparams':
	if numcols == 26:  # CAHV
		final_values_cam_params = \
			currentfiledata[currentfiledata.columns[14:26]]
	if numcols == 38:  # CAHVOR
		final_values_cam_params = \
			currentfiledata[currentfiledata.columns[20:38]]
	if numcols == 44:  # CAHVORE
		final_values_cam_params = \
			currentfiledata[currentfiledata.columns[23:44]]

elif mode == 'pointing':
	if numcols == 16:
		final_values_cam_params = currentfiledata[currentfiledata.columns[9:16]]

else:
	sys.exit('Invalid mode! Exiting')

final_values_cam_params.to_csv(outfile_camparams, sep=" ", index=False, header=False, escapechar=" ", quoting=csv.QUOTE_NONE)  # float_format='%10.3f'

