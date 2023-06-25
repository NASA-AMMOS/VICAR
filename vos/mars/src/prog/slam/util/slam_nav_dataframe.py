#!/usr/bin/env python3

# This script reads in a final navigation file and returns all camera
# information as a Pandas dataframe. Supports CAHV, CAHVOR, and CAHVORE.
#
# IMPORTANT NOTE: If this script is used to create a dataframe from CAHVORE 
# parameters as opposed to pointing, the script will only work if ALL cameras 
# in the navigation file use the same model, either CAHV, CAHVOR, or CAHVORE. 
# Also, the regular expressions used to search for these parameters are designed
# for floating-points values. If there integer values, the script could fail.
# The capability for reading integers and other types such as NaN and Inf will 
# be added to the script.
#
# Sample data:
#
"""
  <solution index1="40" index2="200" index3="24" solution_id="TESTER">
    <image filter="0" frame_id="MONO" image_id="1000384000" instrument="MAHLI" unique_id="MSLMM_460261040">
      <original_parameters type="MSLmahliCamera">
        <parameter id="S" value="-0.70059502"/>
        <parameter id="V1" value="0.09582320"/>
        <parameter id="V2" value="0.35122100"/>
        <parameter id="V3" value="-0.61370099"/>
        <parameter id="X" value="2.14974999"/>
        <parameter id="Y" value="-0.80801398"/>
        <parameter id="Z" value="-0.45830500"/>
      </original_parameters>
      <original_camera_model type="CAHVOR">
        <parameter id="C" type="float_3" value1="2.1492473" value2="-0.8463313" value3="-0.47701354"/>
        <parameter id="A" type="float_3" value1="0.00925501" value2="0.92720251" value3="0.37444850"/>
        <parameter id="H" type="float_3" value1="-1700.42324769" value2="-59.40464796" value3="2415.79266337"/>
        <parameter id="V" type="float_3" value1="2266.96699506" value2="-106.05119844" value3="1800.17096294"/>
        <parameter id="O" type="float_3" value1="0.044392" value2="0.931335" value3="0.36145"/>
        <parameter id="R" type="float_3" value1="0.001413" value2="0.039832" value3="-0.102819"/>
        <reference_frame name="ROVER_NAV_FRAME" index1="40" index2="200" 
index3="24"/>
      </original_camera_model>
    </image>
    <pointing_parameters type="MSLmahliCamera">
      <parameter id="S" value="-0.70059502"/>
      <parameter id="V1" value="0.09582320"/>
      <parameter id="V2" value="0.35122100"/>
      <parameter id="V3" value="-0.61370099"/>
      <parameter id="X" value="2.14974999"/>
      <parameter id="Y" value="-0.80801398"/>
      <parameter id="Z" value="-0.45830500"/>
    </pointing_parameters>
    <camera_model type="CAHVOR">
      <parameter id="C" type="float_3" value1="2.14924736" value2="-0.84633134" value3="-0.47701354"/>
      <parameter id="A" type="float_3" value1="0.00925501" value2="0.92720251" value3="0.37444850"/>
      <parameter id="H" type="float_3" value1="-1700.423248" value2="-59.404648" value3="2415.79266337"/>
      <parameter id="V" type="float_3" value1="2266.966995" value2="-106.051198" value3="1800.17096294"/>
      <parameter id="O" type="float_3" value1="0.044392" value2="0.931335" value3="0.36145"/>
      <parameter id="R" type="float_3" value1="0.001413" value2="0.039832" value3="-0.102819"/>
      <reference_frame name="ROVER_NAV_FRAME" index1="40" index2="200" 
index3="24"/>
    </camera_model>
  </solution>
"""
# ==============================================================================

import os
import sys
import argparse
import numpy as np
import pandas as pd
import re  # regular expressions

# ==============================================================================

# Extracts the CAHV, CAHVOR, or CAHVORE values from a navigation file, given
# the # start line in the file for said camera:

def extract_cahvore(contents, camera_start_line):

	current_line = []
	model_type = []
	j = camera_start_line

	if 'type="CAHV"' or 'type="CAHVOR"' or 'type="CAHVORE"' \
		in contents[camera_start_line]:

		# Extract 'C':
		nextline = contents[j+1]
		C = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		#print(C)
		# Extract 'A':
		nextline = contents[j+2]
		A = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		#print(A)
		# Extract 'H':
		nextline = contents[j+3]
		H = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		#print(H)
		# Extract 'V':
		nextline = contents[j+4]
		V = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		#print(V)
		model_type = "CAHV"
	if 'type="CAHVOR"' or 'type="CAHVORE"' in contents[j]:
		# Extract 'O':
		nextline = contents[j+5]
		O = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		#print(O)
		# Extract 'R':
		nextline = contents[j+6]
		R = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		#print("R ' ", R)
		#if not R or len(R) < 3:
			# Try to find integers:
		model_type = "CAHVOR"
	if 'type="CAHVORE"' in contents[j]:
		# Extract 'E':
		nextline = contents[j+7]
		E = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		#print("E = ", E)
		model_type = "CAHVORE"

	# Append all data to a numpy array. Use a list comprehension to 	
	# concatenate all values in each variable, and then append to the
	# total list. The inline loop is equivalent to a nested for loop:

	if 'type="CAHV"' in contents[j]:
		current_line = [j for i in [C, A, H, V] for j in i] 
	if 'type="CAHVOR"' in contents[j]:
		current_line = [j for i in [C, A, H, V, O, R] for j in i] 
	if 'type="CAHVORE"' in contents[j]:
		current_line = [j for i in [C, A, H, V, O, R, E] for j in i] 
	return [current_line, model_type]


# ==============================================================================

# Extracts the camera pointing values S, V1, V2, V3, X, Y, and from a
# navigation file, given the start line in the file for said camera:

def extract_pointing(contents, camera_start_line):

	current_line = []
	j = camera_start_line

	if '_parameters type=' in contents[camera_start_line]:

		# Extract 'S':
		nextline = contents[j+1]
		S = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		#if failed, try to find integers:
		if not S:
			S = np.array(re.findall("[0-9]+", nextline)).astype(float)
		#print(S)
		# Extract 'V1':
		nextline = contents[j+2]
		V1 = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		if not V1:
			V1 = np.array(re.findall("[0-9]+", nextline)).astype(float)
		#print(V1)
		# Extract 'V2':
		nextline = contents[j+3]
		V2 = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		if not V2:
			V2 = np.array(re.findall("[0-9]+", nextline)).astype(float)
		#print(V2)
		# Extract 'V3':
		nextline = contents[j+4]
		V3 = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		if not V3:
			V3 = np.array(re.findall("[0-9]+", nextline)).astype(float)
		#print(V3)
		# Extract 'X':
		nextline = contents[j+5]
		X = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		if not X:
			X = np.array(re.findall("[0-9]+", nextline)).astype(float)
		#print(X)
		# Extract 'Y':
		nextline = contents[j+6]
		Y = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		if not Y:
			Y = np.array(re.findall("[0-9]+", nextline)).astype(float)
		#print(Y)
		# Extract 'Z':
		nextline = contents[j+7]
		Z = np.array(re.findall("[+-]?\d+\.\d+", nextline)).astype(float)
		if not Z:
			Z = np.array(re.findall("[0-9]+", nextline)).astype(float)
		#print(Z)

		# Append all data to a numpy array. Use a list comprehension to 	
		# concatenate all values in each variable, and then append to the
		# total list. The inline loop is equivalent to a nested for loop:

		current_line = [j for i in [S, V1, V2, V3, X, Y, Z] for j in i] 

	return current_line

# ==============================================================================

# Main script:

# Use Python's 'argparser':
parser = argparse.ArgumentParser()
parser.add_argument("input_navfile", help="Input navigation file location")
parser.add_argument("first_image_index", help="Index of the first image \
	in the batch (first image in the scene has index = 0)")
parser.add_argument("mode", help="Choose between 'camparams' (CAHVORE) \
	and 'pointing'")
parser.add_argument("-v", "--verbose", action="store_true", help="Print \
		verbose script output")
args = parser.parse_args()
verbose = args.verbose

if verbose:
	print('\n-----SLAM_NAV_DATAFRAME.PY-----\n')
	print('Input navigation file location: ', args.input_navfile)
	print('Index of the first image in the current batch: ', \
		  args.first_image_index)

inputfile = open(str(args.input_navfile), "rt") 
contents = inputfile.readlines()  # read the entire file to string
inputfile.close()

# To keep indexing consistent across batches, we need to track the index 
# (0-based) of the first absolute image of this batch. It should be a
# multiple of the sliding window's size minus the overlap:

first_image_index = int(args.first_image_index)
mode = args.mode

# Parse all lines related to a given camera:

camera_start_lines = []
camera_end_lines = []

# 1) Find the number of cameras, the start line of the data for each, and 
# the end line:

for count, line in enumerate(contents):
	if "<solution index" in line:
		camera_start_lines.append(count)
	if "</solution>" in line:
		camera_end_lines.append(count)

if verbose:
	print('Camera start lines: ', camera_start_lines)
	print('Camera end lines: ', camera_end_lines)	
	
# Make sure the number of lines match! Otherwise, the file may be corrupted:

if len(camera_start_lines) is not len(camera_end_lines):
	print('Error: number of camera start and end lines must match! Exiting.')
	sys.exit()

# 2) Now, append all data for a camera per rows, one for the original model
# followed by one for the final model for each:

alldata = []
model_type = []

for i in range(len(camera_start_lines)):
	current_line = []
	for j in range(camera_start_lines[i], camera_end_lines[i]):

		# *****NOTE: this currently fails if there are integers instead of
		# floats!! Use the pointing mode only.*****

		if mode == "camparams":

			# a) Initial camera models:
			if "<original_camera_model type=" in contents[j]:
				[original_model, model_type] = \
					extract_cahvore(contents, j)
				current_line = original_model
				if verbose:
					print('Current line after original model: ', \
						   current_line)

			# b) Final camera models:
			if "<camera_model type=" in contents[j]:
				[final_model, model_type] = extract_cahvore(contents, j)
				current_line = [y for x in [current_line, final_model] \
								for y in x]
				if verbose:		
					print('Current line after final model: ', \
						  current_line)
				alldata.append(current_line)

			if verbose:
				print('Model type for the current camera: ', model_type)

		elif mode == "pointing":

			# a) Initial pointing:
			if "<original_parameters type=" in contents[j]:
				original_pointing = extract_pointing(contents, j)
				current_line = original_pointing
				if verbose:
					print('Current line after original pointing: ', \
						  current_line)

			# b) Final pointing:
			if "<pointing_parameters type=" in contents[j]:
				final_pointing = extract_pointing(contents, j)
				current_line = [y for x in [current_line, final_pointing] \
								for y in x]
				if verbose:		
					print('Current line after final pointing: ', current_line)
				alldata.append(current_line)

		else:
			sys.exit("Invalid mode! Exiting.")


# Create a column vector containing the consecutive image IDs, taking into
# account the initial image number provided by argv[2]:

image_ids = np.arange(first_image_index, \
	first_image_index+len(camera_start_lines), dtype=int)[np.newaxis]
if verbose:
	print('Image IDs: ', image_ids)

finalmatrix = np.hstack((image_ids.T, alldata))

# Create a Pandas dataframe from the complete table, and save to CSV:

alldata = np.array(alldata)

if mode == "camparams":

	if model_type == "CAHV":
		df = pd.DataFrame(finalmatrix, columns=['image_id', 'C1before', \
			 'C2before', 'C3before', 'A1before', 'A2before', 'A3before', \
			 'H1before', 'H2before', 'H3before', 'V1before', 'V2before', \
			 'V3before', 'C1after', 'C2after', 'C3after', 'A1after', \
			 'A2after', 'A3after', 'H1after', 'H2after', 'H3after', \
			 'V1after', 'V2after', 'V3after'])

	if model_type == "CAHVOR":
		df = pd.DataFrame(finalmatrix, columns=['image_id', 'C1before', \
			 'C2before', 'C3before', 'A1before', 'A2before', 'A3before', \
			 'H1before', 'H2before', 'H3before', 'V1before', 'V2before', \
			 'V3before', 'O1before', 'O2before', 'O3before','R1before', \
			 'R2before', 'R3before', 'C1after', 'C2after', 'C3after', \
			 'A1after', 'A2after', 'A3after', 'H1after', 'H2after', \
			 'H3after', 'V1after', 'V2after', 'V3after', 'O1after', \
			 'O2after', 'O3after','R1after', 'R2after', 'R3after'])

	if model_type == "CAHVORE":
		df = pd.DataFrame(finalmatrix, columns=['image_id', 'C1before', \
			 'C2before', 'C3before', 'A1before', 'A2before', 'A3before', \
			 'H1before', 'H2before', 'H3before', 'V1before', 'V2before', \
			 'V3before', 'O1before', 'O2before', 'O3before','R1before', \
			 'R2before', 'R3before', 'E1before', 'E2before', 'E3before', \
			 'C1after', 'C2after', 'C3after', 'A1after', 'A2after', \
			 'A3after', 'H1after', 'H2after', 'H3after', 'V1after', \
			 'V2after', 'V3after', 'O1after', 'O2after', 'O3after', \
			 'R1after', 'R2after', 'R3after', 'E1after', 'E2after', \
			 'E3after'])

if mode == "pointing":

	df = pd.DataFrame(finalmatrix, columns=['image_id', 'Sbefore', \
		 'V1before', 'V2before', 'V3before', 'Xbefore', 'Ybefore', \
		 'Zbefore', 'Safter', 'V1after', 'V2after', 'V3after', \
		 'Xafter', 'Yafter', 'Zafter'])

if verbose:
	print('Final dataframe: ', df)

# Save to the same location and filename, but change the extension:

outfile = os.path.splitext(str(sys.argv[1]))[0]+'.csv'  
df.to_csv(outfile)

