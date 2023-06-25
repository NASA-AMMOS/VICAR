#!/usr/bin/env csh

# This is a test script for obj2plane.
# The first parameter is an optional tested program's location, (TESTDIR below) e.g. ..
# stdout/err is captured in tstobj2plane.log

set log="tstobj2plane.log"
source ../../../test_setup.csh $log $0 $1

# Test the two algorithm types and a couple of different decimation values
# Results saved in the respective plane_{reg,svd}.txt file

# CASE 1: with decimation, and solving using regression
echo "CASE 1: with decimation, and solving using regression" >& tmp.log
$TESTDIR/obj2plane INP=test_mesh_octahedron.OBJ OUT=plane_octahedron_dec2_reg.txt DECIMATION=2 ALGO=regression >>& tmp.log
echo "CASE 1 (regression, decimation=2) produces this best-fit plane:" >>& tmp.log
head plane_octahedron_dec2_reg.txt >>& tmp.log
echo "\n\n" >>& tmp.log

# CASE 2: no decimation, and solving using SVD
echo "CASE 2: no decimation, and solving using SVD" >>& tmp.log
$TESTDIR/obj2plane INP=test_mesh_teapot.OBJ OUT=plane_teapot_dec1_svd.txt DECIMATION=1 ALGO=svd >>& tmp.log
echo "CASE 2 (svd, no decimation) produces this best-fit plane:" >>& tmp.log
head plane_teapot_dec1_svd.txt >>& tmp.log

cp tmp.log tstobj2plane.log

rm tmp.log plane_octahedron_dec2_reg.txt plane_teapot_dec1_svd.txt

