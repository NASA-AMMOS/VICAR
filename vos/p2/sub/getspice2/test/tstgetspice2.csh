#!/usr/bin/env csh

# This is a test script for getspice2.
# The first parameter is an optional tested program's location, (TESTDIR below) e.g. ..
# The second parameter is an optional alternate MARS_CONFIG_PATH.
# stdout/err is captured in tstgetspice2.log

set log="tstgetspice2.log"
source ../../../test_setup.csh $log $0 $1 $2

setenv SPICE_CONFIG_FILE spice/ms_config.x86-64-linx

taetm -s tstgetspice2.pdf

cat session.log >>& $log

