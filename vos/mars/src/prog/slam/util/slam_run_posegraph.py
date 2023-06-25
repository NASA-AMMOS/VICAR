#!/usr/bin/env python3

# This script runs pose graph optimization on a set of relative camera poses
# given the cameras' absolute CAHV, CAHVOR, or CAHVORE values
#
# NOTE: the script is currently designed to work only with CAHVORE-type 
# parameters, but will be updated to work with pointings as well

# ==============================================================================

import sys
import os
import numpy as np
import scipy
from scipy.spatial.transform import Rotation 
import matplotlib.pyplot as plot
from mpl_toolkits.mplot3d import Axes3D
from optparse import OptionParser

# ==============================================================================

# This function reads in a file with 18 CAHVOR camera parameters per row 
# and returns a PLY file with the camera centers. The function also takes as 
# input the connectivity matrix as provided by the output information file from 
# keyframe_selection (sys.argv[2]), so we need to read the following (for 
# example):
#
# ==========
# Keyframe scores matrix:
# Line 0: 0.000000 0.015808 0.000000 0.000000 0.000000 0.000000 0.000000 
# 0.000000 0.000000 0.000000 
# Line 1: ..... 
#
# Connectivity matrix (matches per pair):
# Line 0: 0 615 6 5 5 4 7 3 3 0 
# Line 1: .....
# ==========
#
# IMPORTANT NOTE: this was designed to be a test script and not for production
# purposes, so all tests were performed using CAHVOR data. The below function
# will be updated to also handle CAHV, CAHVORE, and pointing.

def cahvor_to_relative_poses(cahvore_values_file, keyframes_file, 								 output_file, approach):

	# A) First, process the keyframes file, which also contains the tiepoints
	# connectivity matrix:

	print('1) Extracting tiepoints connectivity matrix from keyframes file\n')
	print('Input file: ', keyframes_file)
	keyframesfile = open(keyframes_file, "rt")  
	keyframescontents = keyframesfile.readlines() 
	numlines = len(keyframescontents)

	flag = 0
	connectivity_matrix = np.array([])

	for line in keyframescontents:

		# Find lines which follow the text "Connectivity matrix (matches 
		# per pair):" AND begin with "Line ":

		if "Connectivity matrix (matches per pair):" in line:
			flag = 1
		if "Line " in line and flag==1:
			linesplit = line.split(':')
			currentline = np.fromstring(linesplit[1], dtype=int, sep=' ')
			connectivity_matrix = np.vstack((connectivity_matrix, \
				 np.array(currentline))) if connectivity_matrix.size \
				 else np.array(currentline)

	print('Connectivity matrix: \n', connectivity_matrix)
	connrows, conncols = connectivity_matrix.shape

	# B) Now, process the file which contains CAHVORE parameters per camera:

	print('\n2) Process the file with CAHVOR parameters per camera\n')
	print('Input file: ', cahvore_values_file)

	cahvorfile = open(cahvore_values_file, "rt")  
	cahvorcontents = cahvorfile.readlines() 
	numlines = len(cahvorcontents)

	if os.path.exists(output_file):
		os.remove(output_file)

	# Open output file:
	f = open(output_file, "a")

	# Store all CAHVOR values

	C_all = np.array([]).reshape(0, 3) # position
	A_all = np.array([]).reshape(0, 3) # principal axis
	H_all = np.array([]).reshape(0, 3)
	V_all = np.array([]).reshape(0, 3)
	O_all = np.array([]).reshape(0, 3)
	R_all = np.array([]).reshape(0, 3)

	# Store all rotation matrices:
	M_all = np.array([]).reshape(0, 9)

	# B) Extract CAHV per camera to get position and orientation values:

	print('3) Extract CAHV per camera and at the same time add the position '
		  'and quaternion orientation as vertices to the output g2o file:\n')

	counter = 0
	for line in cahvorcontents:
		newstr = ''.join((ch if ch in '0123456789.-e' else ' ') \
					for ch in line)
		listOfNumbers = [float(i) for i in newstr.split()]

		# Extract CAHV (don't need O, R, or E):
		C = np.array([[listOfNumbers[0],listOfNumbers[1],listOfNumbers[2]]])
		A = np.array([[listOfNumbers[3],listOfNumbers[4],listOfNumbers[5]]])
		H = np.array([[listOfNumbers[6],listOfNumbers[7],listOfNumbers[8]]])
		V = np.array([[listOfNumbers[9],listOfNumbers[10],listOfNumbers[11]]])
		# 
		C_all = np.vstack([C_all, C])
		A_all = np.vstack([A_all, A])
		H_all = np.vstack([H_all, H])
		V_all = np.vstack([V_all, V])
		
		# Compute the following:
		# hc = A dot H (pixels)
		hc = np.dot(A, np.transpose(H))  # dot, matmul
		# vc = A dot V (pixels)
		vc = np.dot(A, np.transpose(V))  # dot, matmul
		# hs = ||A cross H|| (pixels)
		hs = np.linalg.norm(np.cross(A, H))
		# vs = ||A cross V|| (pixels)
		vs = np.linalg.norm(np.cross(A, V))
		# Hprime = (H - hc*A)/hs
		Hprime = (H - hc*A)/hs
		# Vprime = (V - vc*A)/vs
		Vprime = (V - vc*A)/vs
		# Rotation matrix: M = [H'; -V'; -A]
		M = np.vstack((Hprime, -Vprime, -A))
		print('Rotation matrix = \n', M)

		M_all = np.vstack([M_all, np.reshape(M, (1, 9))])
		r = Rotation.from_matrix(M)
		quat = r.as_quat()
		print('quat = ', quat)

		# Add vertex, of the form: VERTEX_SE3:QUAT ID x y z qw qx qy qz
		# Note: Ceres quats are (w,x,y,z), while as_quat uses (x,y,z,w)

		sp = ' '
		fileline = "VERTEX_SE3:QUAT " + str(counter) + sp + str(C[0,0]) + \
				sp + str(C[0,1]) + sp + str(C[0,2]) + sp + str(quat[3]) + \
				sp + str(quat[0]) + sp + str(quat[1]) + sp + str(quat[2]) \
				+ '\n'
		counter = counter + 1
		f.write(fileline)

	# ====================
	# The script has two modes:
	#
	# 1) A "naive" mode which only performs pairwise-consecutive relative
	# pose computations, and
	# 2) A more complete approach which loads the connectivity matrix between
	# pairs, either from 'marsautotie2', 'marsnav2', or 'keyframe_selection',
	# in order to create the pose graph from that information
	#
	# We compute RELATIVE positions and orientations between (i, j) pairs as
	# follows:
	#
	# 		C_rel = C_j - C_i
	#
	# R_rel = Create rotation matrix for cam 1, cam 2, then do R1*inv(R2) for
	# relative orientation, extract angles
	#
	# Correlation info: NEED TO FIX THIS! Compute it dynamically or at least
	# better understand how it works..
	# info = '20 0 0 0 0 0 20 0 0 0 0 20 0 0 0 20 0 0 20 0 20'
	# info = '1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 1 0 0 1 0 1'
	info = '400 0 0 0 0 0 400 0 0 0 0 400 0 0 0 400 0 0 400 0 400'

	# ==========
	# Approach (1):

	if approach == 1:

		print('4) Process edges sequentially')

		for i in range(connrows-1):

			j = i + 1
			print("Current pair: ", i, j)
			C_rel = C_all[j, :] - C_all[i, :]
			print("C_rel = Cj-Ci: ", C_rel)
			Mi = np.reshape(M_all[i, :], (3, 3))
			print("Mi = \n", Mi)
			Mj = np.reshape(M_all[j, :], (3, 3))
			print("Mj = \n", Mj)
			R_rel = Mi * np.linalg.inv(Mj)  # Mj*inv(Mi)
			print("R_rel = Mi*inv(Mj): \n", R_rel)  # Mj*inv(Mi)
			# Extract quaternion:
			r = Rotation.from_matrix(R_rel)
			quat = r.as_quat()
			print(quat)

			# Add edge, of the form (example):
			# EDGE_SE3:QUAT 0 1   0.939375 -0.022347 -0.028831   0.1175925
			# -0.3414281 -0.7976288 0.4831016   100.000000 0.000000 0.000000
			# 0.000000 0.000000 0.000000   100.000000 0.000000 0.000000
			# 0.000000 0.000000   100.000000 0.000000 0.000000 0.000000  
			# 400.000000 0.000000 0.000000   400.000000 0.000000   400.000000
			# Note: Ceres quats are (w,x,y,z), while as_quat uses (x,y,z,w)

			fileline = "EDGE_SE3:QUAT " + str(i) + sp + str(j) + sp + \
						 str(C_rel[0]) + sp + str(C_rel[1]) + sp + \
						 str(C_rel[2]) + sp + str(quat[3]) + sp + \
						 str(quat[0]) + sp + str(quat[1]) + sp + \
						 str(quat[2]) + sp + info + '\n'
			f.write(fileline)

	# ==========
	# Approach (2):

	if approach == 2:

		print('4) Process edges based on the tiepoint connectivity matrix')

		for i in range(connrows):
			for j in range(conncols):
				if connectivity_matrix[i,j] != 0:
					print(i, j)
					C_rel = C_all[j, :] - C_all[i, :]
					print("C_rel = Cj-Ci: ", C_rel)
					Mi = np.reshape(M_all[i, :], (3, 3))
					print("Mi = \n", Mi)
					Mj = np.reshape(M_all[j, :], (3, 3))
					print("Mj = \n", Mj)
					R_rel = Mi * np.linalg.inv(Mj)  # Mj*inv(Mi)
					print("R_rel = Mi*inv(Mj): \n", R_rel)  # Mj*inv(Mi)
					# Extract quaternion:
					r = Rotation.from_matrix(R_rel)
					quat = r.as_quat()
					print(quat)

					# Add edge:
					# *****NOTE: NEED THE +1 HERE BECAUSE THE FILE IS
					# ACTUALLY WRITTEN WRONG, should only have 9 columns,
					# check!!
					# Note: Ceres quats are (w,x,y,z), while as_quat uses
					# (x,y,z,w)

					fileline = "EDGE_SE3:QUAT " + str(i) + sp + str(j+1) + \
								sp + str(C_rel[0]) + sp + str(C_rel[1]) + \
								sp + str(C_rel[2]) + sp + str(quat[3]) + \
								sp + str(quat[0]) + sp + str(quat[1]) + \
								sp + str(quat[2]) + sp + info + '\n'
					f.write(fileline)

	cahvorfile.close()
	keyframesfile.close()
	f.close()

# ==============================================================================

# Sets axes equal in the 'plot_results' function below:

def set_axes_equal(axes):

    print('Set the axes of the 3D plot to have equal scale.')

    x_limits = axes.get_xlim3d()
    y_limits = axes.get_ylim3d()
    z_limits = axes.get_zlim3d()

    x_range = abs(x_limits[1] - x_limits[0])
    x_middle = numpy.mean(x_limits)
    y_range = abs(y_limits[1] - y_limits[0])
    y_middle = numpy.mean(y_limits)
    z_range = abs(z_limits[1] - z_limits[0])
    z_middle = numpy.mean(z_limits)

    length = 0.5 * max([x_range, y_range, z_range])

    axes.set_xlim3d([x_middle - length, x_middle + length])
    axes.set_ylim3d([y_middle - length, y_middle + length])
    axes.set_zlim3d([z_middle - length, z_middle + length])

# ==============================================================================

# This function plots the results from the 3D pose graph optimization, by
# comparing g2o files before and after optimization as displayed in different
# colors. Lines are drawn between consecutive vertices. The 'make_axes_equal'
# flag is boolean.  
#
# The files have the following format:
#	   ID x y z q_x q_y q_z q_w

def plot_results (initial_poses, optimized_poses, make_axes_equal):

	# Read the original and optimized poses files:

	poses_original = None
	if initial_poses != '':
		poses_original = np.genfromtxt(initial_poses, usecols = (1, 2, 3))

	poses_optimized = None
	if optimized_poses != '':
		poses_optimized = np.genfromtxt(optimized_poses, usecols = (1, 2, 3))

	# Plot the results:
	figure = plot.figure()

	if poses_original is not None:
		axes = plot.subplot(1, 2, 1, projection='3d')
		plot.plot(poses_original[:, 0], poses_original[:, 1], \
				  poses_original[:, 2], '-', alpha=0.5, color="green")
		plot.title('Original')
		if options.axes_equal:
			axes.set_aspect('equal')
			set_axes_equal(axes)

	if poses_optimized is not None:
		axes = plot.subplot(1, 2, 2, projection='3d')
		plot.plot(poses_optimized[:, 0], poses_optimized[:, 1], \
				  poses_optimized[:, 2], '-', alpha=0.5, color="blue")
		plot.title('Optimized')
		if options.axes_equal:
			axes.set_aspect('equal')
			set_axes_equal(plot.gca())

	# Display the plot and wait for the user to close:
	plot.show()

# ==============================================================================

# Main script:

# Use Python's 'argparser':
parser = argparse.ArgumentParser()
parser.add_argument("posegraph_location", help="Location of the posegraph executable")
parser.add_argument("cahvore_values_file", \
	help="Input CAHVORE values config file location")
parser.add_argument("keyframes_file", help="Input keyframes file location")
parser.add_argument("relative_poses_file", \
	help="Where to store the relative poses file")
parser.add_argument("optimized_poses_file", help="Where to store the optimized poses file")
parser.add_argument("-v", "--verbose", action="store_true", help="Print verbose script (non-VICAR) output")
args = parser.parse_args()
verbose = args.verbose

if verbose:
	print('\n-----SLAM_RUN_POSEGRAPH.PY-----\n')
	print('Location of the posegraph executable: ', args.posegraph_location)
	print('Input CAHVORE values config file location: ', \
		args.cahvore_values_file)
	print('Input keyframes file location: ', args.keyframes_file)
	print('Where to store the relative poses file: ', \
			 args.relative_poses_file)
	print('Where to store the optimized poses file: ', \
			 args.optimized_poses_file)


#1) OPTIONAL: run posegraph on a list of images as-is, PRIOR to bundle
# adjustment. In that case, we just read the image labels and extract the
# CAHV(OR(E)) parameters directly. In the general case, BA is applied first
# and we generate a file with the parameters from there. In any case, the
# input here is a list of CAHV, CAHVOR, or CAHVORE values per line (for each 
# image).

#2) Read a list of CAHV(OR(E)) parameters and keyframes to compute relative
# poses:

# 'approach' hard-coded for now; '1' is sequential, '2' based on the tiepoint
# connectivity matrix. This will eventually be removed in favor of whichever 
# approach works best and should not be a parameter

approach = 1
cahvor_to_relative_poses(args.cahvore_values_file, args.keyframes_file, \
						 args.relative_poses_file, approach) 

#3) Apply posegraph:

subprocess.run([args.posegraph_location, args.relative_poses_file, \
			    args.optimized_poses_file])

#4) Plot results:

plot_results(args.relative_poses_file, args.optimized_poses_file, 'True')

