#!/usr/bin/bash

# This script reads in a list of image files and returns their headers in 
# separate files.

# Input file list: 

file=$1

# printf '-----SLAM_READ_HEADERS.SH\n-----'
# printf 'Extracting headers in list in %s; saving to pointing$n.txt\n' "$file" 

declare -i n=1

while IFS= read -r line
do
	# printf '%s\n' "$line" 
	# Option 1: This one provides just the calibration:
    # $MARSLIB/marsrelabel "$line" TEST.IMG OUT_CM=pointing$n.txt
	# Option 2: This one prints all header information:
	$R2LIB/label -list "$line" > pointing$n.txt
	let "n=n+1"
done <"$file"

