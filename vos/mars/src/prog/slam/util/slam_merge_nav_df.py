#!/usr/bin/env python3

# This script reads in navigation CSV Pandas dataframes files and returns 
# a single merged table containing pointing or camera parameter information
# for all scene cameras.

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
parser.add_argument("input_navcsv", help="Input directory to search for \
		navigation CSV files")
parser.add_argument("merge_type", help="Type of merge to perform; choose \
		between 'mean', 'median', 'mode', and 'quantile'")
parser.add_argument("mode", help="Choose between 'camparams' (CAHVORE) \
	and 'pointing'")
parser.add_argument("-v", "--verbose", action="store_true", help="Print \
		verbose script output")
args = parser.parse_args()
verbose = args.verbose

if verbose:
	print('\n-----SLAM_MERGE_NAV_DF.PY-----\n')
	print('Input directory to search for navigation CSV files: ', \
		  args.input_navcsv)
	print('Type of merge to perform: ', args.merge_type)
	print('Dataframe data type: ', args.mode)

csv_dir = str(args.input_navcsv)
csvs = csv_dir + "navigation_batch*.csv"
csv_files = glob.glob(csvs)
if verbose:
	print('Found CSV files: ', csv_files)

merged_table = pd.DataFrame()
merge_type = args.merge_type
mode = args.mode

for count, line in enumerate(csv_files):
	currentfiledata = pd.read_csv(line)
	if verbose:
		print('Contents of the current file: ', currentfiledata)

	# Perform an outer merge with the merged table accumulated so far:

	if merged_table.empty:
		merged_table = currentfiledata	
		if verbose:
			print('Contents added to empty table: ', merged_table)
	else:
		merged_table = pd.merge(merged_table, currentfiledata, how='outer')
		if verbose:
			print('Result after merging: ', merged_table)

# Remove 'Unnamed' columns:

merged_table.drop(merged_table.filter(regex="Unname"),axis=1, inplace=True)

if verbose:
	print('Final merged table: ', merged_table)

# For all the multiple "versions" of the same camera model (for example,
#  final CAHVOR values computed multiple times for the same camera at each 
# batch), average or perform other statistical operations to get the "best" 
# set of parameters:

if merge_type == 'mean':
	final_values = merged_table.groupby(['image_id']).mean()
	if verbose:
		print('Mean values per image ID: ', final_values)

# Currently doesn't work; Pandas not detecting 'mode'	
if merge_type == 'mode':  
	final_values = merged_table.groupby(['image_id']).mode()
	if verbose:
		print('Mode values per image ID: ', final_values)

if merge_type == 'median':
	final_values = merged_table.groupby(['image_id']).median()
	if verbose:
		print('Mode values per image ID: ', final_values)

if merge_type == 'quantile':
	final_values = merged_table.groupby(['image_id']).quantile(0.5) #default
	if verbose:
		print('Mode values per image ID: ', final_values)

# Print the final CAHV, CAHVOR, or CAHVORE values:

if verbose:
	print('Final grouped data: ', final_values)

# Save the complete set of before/after cam params to CSV file:

outfile = os.path.splitext(str(sys.argv[1]))[0]+'navigation_table.csv'  
final_values.to_csv(outfile)

# Additionally, save just the final CAHV, CAHVOR, CAHVORE, or pointing values 
# to file, for use by the posegraph framework:

if mode == 'camparams':
	outfile_camparams = os.path.dirname(args.input_navcsv) + \
			'/navigation_cam_params.txt'
if mode == 'pointing':
	outfile_camparams = os.path.dirname(args.input_navcsv) + \
			'/navigation_pointings.txt'

numcols = len(final_values.columns)

# Initialize empty dataframe:
final_values_cam_params = pd.DataFrame({'A' : []})

if mode == 'camparams':
	if numcols == 24:  # CAHV
		final_values_cam_params = final_values[final_values.columns[12:24]]
	if numcols == 36:  # CAHVOR
		final_values_cam_params = final_values[final_values.columns[18:36]]
	if numcols == 42:  # CAHVORE
		final_values_cam_params = final_values[final_values.columns[21:42]]

if mode == 'pointing':
	if numcols == 14:  # S, V1, V2, V3, X, Y, Z
		final_values_cam_params = final_values[final_values.columns[7:14]]

final_values_cam_params.to_csv(outfile_camparams, sep=" ", index=False, \
	header=False, escapechar=" ", \
	quoting=csv.QUOTE_NONE)  # float_format='%10.3f'


# Perform a statistical analysis on the obtained values. For now, simply 
# output this to stdout (or pipe to a log file) for analysis, but eventually
# this can be used to stop a run if the values (such as standard deviation) 
# are higher than some preset threshold. 

# The main values we care about are the camera position 'C' and the pointing
# direction 'A':

if mode == 'camparams':

	Cbeforestats = merged_table.groupby(['image_id'])[['C1before', 'C2before', 'C3before']].describe()  # can use individual stats such as 'std'

	Cafterstats = merged_table.groupby(['image_id'])[['C1after', 'C2after', 'C3after']].describe()  

	Abeforestats = merged_table.groupby(['image_id'])[['A1before', 'A2before', 'A3before']].describe()  

	Aafterstats = merged_table.groupby(['image_id'])[['A1after', 'A2after', 'A3after']].describe()  

	if verbose:
		print('C statistics before BA: ', Cbeforestats)
		print('C statistics after BA: ', Cafterstats)
		print('A statistics before BA: ', Abeforestats)
		print('A statistics after BA: ', Aafterstats)

# Apply "describe" on all pointing parameters:

if mode == 'pointing':

	Sbeforestats = merged_table.groupby(['image_id'])['Sbefore'].describe() 
	Safterstats = merged_table.groupby(['image_id'])['Safter'].describe()
	V1beforestats = merged_table.groupby(['image_id'])['V1before'].describe() 
	V1afterstats = merged_table.groupby(['image_id'])['V1after'].describe()
	V2beforestats = merged_table.groupby(['image_id'])['V2before'].describe()
	V2afterstats = merged_table.groupby(['image_id'])['V2after'].describe()
	V3beforestats = merged_table.groupby(['image_id'])['V3before'].describe() 
	V3afterstats = merged_table.groupby(['image_id'])['V3after'].describe()
	Xbeforestats = merged_table.groupby(['image_id'])['Xbefore'].describe() 
	Xafterstats = merged_table.groupby(['image_id'])['Xafter'].describe()
	Ybeforestats = merged_table.groupby(['image_id'])['Ybefore'].describe() 
	Yafterstats = merged_table.groupby(['image_id'])['Yafter'].describe()
	Zbeforestats = merged_table.groupby(['image_id'])['Zbefore'].describe() 
	Zafterstats = merged_table.groupby(['image_id'])['Zafter'].describe()

	if verbose:
		print('S statistics before BA: ', Sbeforestats)
		print('S statistics after BA: ', Safterstats)
		print('V1 statistics before BA: ', V1beforestats)
		print('V1 statistics after BA: ', V1afterstats)
		print('V2 statistics before BA: ', V2beforestats)
		print('V2 statistics after BA: ', V2afterstats)
		print('V3 statistics before BA: ', V3beforestats)
		print('V3 statistics after BA: ', V3afterstats)
		print('X statistics before BA: ', Xbeforestats)
		print('X statistics after BA: ', Xafterstats)
		print('Y statistics before BA: ', Ybeforestats)
		print('Y statistics after BA: ', Yafterstats)
		print('Z statistics before BA: ', Zbeforestats)
		print('Z statistics after BA: ', Zafterstats)

