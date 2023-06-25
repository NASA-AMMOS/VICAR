#!/usr/bin/env python3

# This script reads in a set of XML navigation files and "combines" them
# into a final XML file corresponding to all available cameras. 
#
# NOTE: for all batch versions of a camera's parameter, the most RECENT
# is taken as the final value for the output XML file. This may not be 
# the best solution, and other statistical solutions are being considered
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
        <parameter id="C" type="float_3" value1="2.1492473" value2="-0.84633134" value3="-0.47701354"/>
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

import os, os.path
import sys
import argparse
import numpy as np
import pandas as pd
import re  # regular expressions
import glob
import csv
import xml.etree.ElementTree as ET
from xml.etree.ElementTree import tostring
from collections import Counter

# ==============================================================================

# Count tags:
# https://stackoverflow.com/questions/14897426/counting-number-of-xml-tags-in-python-using-xml-dom-minidom

def count_tags(filename):
    my_tags = []
    for event, element in ET.iterparse(filename):
        my_tags.append(element.tag)
    my_keys = Counter(my_tags).keys()
    my_values = Counter(my_tags).values()
    my_dict = dict(zip(my_keys, my_values))
    return my_dict


# ==============================================================================

# Main script:

# Use Python's 'argparser':
parser = argparse.ArgumentParser()
parser.add_argument("input_nav", help="Input directory to search for \
		navigation .nav files")
parser.add_argument("-v", "--verbose", action="store_true", help="Print verbose script output")
args = parser.parse_args()
verbose = args.verbose

if verbose:
	print('\n-----SLAM_MERGE_NAV.PY-----\n')
	print('Input directory to search for navigation .nav '
		  'files: ', args.input_nav)

nav_dir = str(args.input_nav)
navs = nav_dir + "navigation_batch*.nav"
nav_files = glob.glob(navs)

# Sort so they're in the correct numerical order:

nav_files.sort(key=lambda f: int(re.sub('\D', '', f)))
if verbose:
	print('Found .nav files: ', nav_files)

if verbose:
	print('Displaying the fields in the first file.')
	elemList = []
	first_tree = ET.parse(nav_files[0])
	for elem in first_tree.iter():
	    elemList.append(elem.tag)

	# Remove duplicities by convertion to set and back to list:
	elemList = list(set(elemList))

	# Print out the result (sanity check):
	print(elemList)
	first_root = first_tree.getroot()
	first_cams = first_root.findall('solution')
	if verbose:
		print('Number of images in first file: ', len(first_cams))


# Parse the first file:

cam_attrib_list = []
initial_tree = ET.parse(nav_files[0])
initial_root = initial_tree.getroot()
for cam in initial_root.findall('solution/image'):
	cam_attrib_list.append(cam.attrib)
	print('First image attribute: ', cam.attrib)

# Iteratively parse the rest of the files:

for nav in nav_files[1:]:

	current_tree = ET.parse(nav)
	current_root = current_tree.getroot()
	current_solution = current_root.findall('solution')
	current_cams = current_root.findall('solution/image')

	if verbose:
		print('Number of current images: ', len(current_cams))

	repetition_indices = []

	for cam in current_cams:
		if verbose:
			print('Current image attribute: ', cam.attrib)
		if cam.attrib not in cam_attrib_list:
			if verbose:
				print('Appended current image attribute to list')
			cam_attrib_list.append(cam.attrib)
		else:
			repetition_index = cam_attrib_list.index(cam.attrib)
			repetition_indices.append(repetition_index)
			initial_root = initial_tree.getroot()
	
	if verbose:
		print('Repetition indices to remove: ', repetition_indices)

	for i in range(len(repetition_indices)):

		# Can't remove the first one followed by the second for example,
		# since upon removal of the first that one would have taken its
		# place. Instead, look for the index of the first element to be
		# removed, and sequentially remove that one multiple times:

		to_remove = \
			initial_root.findall('solution')[repetition_indices[0]]
		initial_root.remove(to_remove)

	initial_root.extend(current_solution)

if verbose:
	print('Final image attributes: ')
	for i in range(len(cam_attrib_list)):
		print(cam_attrib_list[i])

# Open the output file for writing, which MUST be named 
# 'navigation_temp.xml' (as expected by slam.py):

out_nav = os.path.join(nav_dir, "navigation_temp.xml")
outfile = open(out_nav, "wb") 
initial_tree.write(outfile)
outfile.close()


# Post-processing:

# 1) Move <surface_model/> to the very bottom. First, find the beginning 
# and end of the <surface_model> element:

surface_model_lines= []
with open (out_nav,"rt") as infile:
	lines = infile.readlines()
	for count,line in enumerate(lines):
		if "surface_model" in line:
			surface_model_lines.append(count)

# 2) The above file has some of the 'solution' values not indented
# properly for some reason. Similarly, the final <pointing_correction/>
# closing tag is indented too much. This should not matter since the file
# will still be parsed correctly, but the point of whitespace is mainly
# for human readability

surface_model_string = []
out_nav_final = os.path.join(nav_dir, "navigation_all_batches.xml")
with open(out_nav_final,"w") as outfile:
	with open (out_nav,"rt") as infile:
		lines = infile.readlines()
		for count,line in enumerate(lines):
			if line[0:9] == "<pointing":
				outfile.write('<?xml version="1.0" encoding="UTF-8"?>\n'+line)
			elif (count >= surface_model_lines[0]) and \
				 (count <= surface_model_lines[1]):
				surface_model_string.append(line)
			elif line[0:9] == "<solution":
				outfile.write("  "+line)
			elif "</pointing_correction>" in line:
				for info in surface_model_string:
					outfile.write(info)
				outfile.write(line.lstrip())
			else:
				outfile.write(line)


