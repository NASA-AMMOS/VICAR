#!/usr/bin/env python3

# This script reads in a list of files and returns a text file with the  
# CAHV, CAHVOR, or CAHVORE parameters per camera (row). 

# ==============================================================================

import os
import sys
import argparse
import numpy as np
import pandas as pd
import subprocess

# ==============================================================================

# This script reads in a file with the camera model parameters per row 
# and returns a PLY file with the camera centers ('C' values):

def cahvore_to_pose_ply(camparamfile, outplyfile):

	myfile = open(camparamfile, "rt") 
	contents = myfile.readlines()         
	numlines = len(contents)

	if os.path.exists(outplyfile):
		os.remove(outplyfile)

	# Open output file:
	f = open(outplyfile, "a")

	# Write PLY header:
	f.write("ply\n")
	f.write("format ascii 1.0\n")
	f.write("element vertex %d\n" % numlines)
	f.write("property float x\n")
	f.write("property float y\n")
	f.write("property float z\n")
	f.write("property uchar diffuse_red\n")
	f.write("property uchar diffuse_green\n")
	f.write("property uchar diffuse_blue\n")
	f.write("end_header\n")

	for line in contents:
		newstr = ''.join((ch if ch in '0123456789.-e' else ' ') \
							for ch in line)
		listOfNumbers = [float(i) for i in newstr.split()]
		f.write("%f %f %f 255 255 255\n" % (listOfNumbers[0], \
				 listOfNumbers[1], listOfNumbers[2]))

	myfile.close()
	f.close()

# ==============================================================================

# This script reads in a VICAR header and returns the 18 CAHVOR camera 
# parameters:

def parse_cahvore(file_location, verbose):

	myfile = open(file_location, "rt") 
	contents = myfile.readlines()  # read the entire file to string

	# First, figure out if the model type is CAHV, CAHVOR, or CAHVORE:
	# There is a line in the label which says, for example,
	# "MODEL_TYPE='CAHVOR'". Find this line and parse the type of model 
	# in order to read the proper set of params:

	model_type = []
	for line in contents:
		if 'MODEL_TYPE=' in line:
			if 'CAHV' in line:
				model_type = 'CAHV'
			if 'CAHVOR' in line:
				model_type = 'CAHVOR'
			if 'CAHVORE' in line:
				model_type = 'CAHVORE'
	
	if verbose:
		print('Model type for the current camera: ', model_type)	 
	
	if model_type == 'CAHV':
		params = ['MODEL_COMPONENT_1', 'MODEL_COMPONENT_2', 				 					  'MODEL_COMPONENT_3', 'MODEL_COMPONENT_4']

	if model_type == 'CAHVOR':
		params = ['MODEL_COMPONENT_1', 'MODEL_COMPONENT_2', 				  					  'MODEL_COMPONENT_3', 'MODEL_COMPONENT_4',
				  'MODEL_COMPONENT_5', 'MODEL_COMPONENT_6']

	if model_type == 'CAHVORE':
		params = ['MODEL_COMPONENT_1', 'MODEL_COMPONENT_2',
	              'MODEL_COMPONENT_3', 'MODEL_COMPONENT_4',
				  'MODEL_COMPONENT_5', 'MODEL_COMPONENT_6',
				  'MODEL_COMPONENT_7']
		
	values = []

	for line in contents:
		for p in params:
		    if p in line:
		        line = line.replace(p, '')
		        newstr = ''.join((ch if ch in '0123456789.-e' else ' ') \
						 	for ch in line)
		        listOfNumbers = [float(i) for i in newstr.split()]
		        values.append(listOfNumbers[0])
		        values.append(listOfNumbers[1])
		        values.append(listOfNumbers[2])
	
	myfile.close()

	return values

# ==============================================================================

# Main script:

# Use Python's 'argparser':
parser = argparse.ArgumentParser()
parser.add_argument("inputimages", help="Input image file locations")
parser.add_argument("outfile", help="Output file")
parser.add_argument("--gen_ply", help="Generate a PLY file from the camera centers. It will be saved with the same base name as the output file, but with a PLY extension.")
parser.add_argument("-v", "--verbose", action="store_true", help="Print verbose script (non-VICAR) output")
args = parser.parse_args()
verbose = args.verbose

if verbose:
	print('\n-----SLAM_HEADERS2CAMPARAMS.PY-----\n')
	print('Input image file locations: ', args.inputimages)
	print('Output file to be saved here: ', args.outfile)
	if args.gen_ply:
		print('Output PLY of the camera centers to be generated.')
	else:
		print('No output PLY of the camera centers to be generated.')

# Done parsing, print out intermediate values:

inputimages = open(str(args.inputimages), "rt")  
image_list = inputimages.readlines()  # read entire file to string
inputimages.close()
numimages_initial = len(image_list)
if verbose:
	print('Number of images: ', numimages_initial)

cam_values = []

# Extract the image headers for all images in the input file list. These 
# will then be parsed by 'parse_cahvore' to extract the CAHV, CAHVOR, or 
# CAHVORE values for each:

subprocess.run(['bash', 'slam_read_headers.sh', args.inputimages])

for i in range(1, numimages_initial+1):
	cam_values.append(parse_cahvore('pointing' + str(i) + '.txt', verbose))
	subprocess.run(['rm', 'pointing' + str(i) + '.txt'])

# Open output file for writing:
cam_values = np.array(cam_values)

# Change the formatting from scientific notation to decimal using 'fmt':
np.savetxt(args.outfile, cam_values, fmt='%f')

if args.gen_ply:
	base = os.path.splitext(args.outfile)[0]
	out_ply = base + '.ply'
	cahvore_to_pose_ply(args.outfile, out_ply)

