#!/usr/bin/env csh

# This is a test script for carto_subs.
# The first parameter is an optional tested program's location, (TESTDIR below) e.g. ..
# stdout/err is captured in tstcarto_subs.log

set log="tstcarto_subs.log"
source ../../../test_setup.csh $log $0 $1

./tstcarto_subs >>& $log
