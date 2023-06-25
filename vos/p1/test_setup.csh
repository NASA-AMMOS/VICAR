# This file is sourced from p[12]/*/*/test/*.csh
# $1 is the log name
# $2 is the csh test script name
# $3 (optional) is the tested program's location (alternate for $MARSLIB)

# If a test script is run by a developer testing a new version of a
# p[12] program, we want the option to run the program being tested
# from a location other than R2LIB, i.e. where a new version has
# just been compiled. All other programs should be found in R2LIB as
# usual. Otherwise, we want all p[12] programs to be found in R2LIB.
# So a developer might run a test script as:
# tstfoo.csh ..
# because the new binary is in the parent of the test directory.
# Or using the standard $R2LIB/foo:
# tstfoo.csh

set log=$1

echo "Starting $2 ..." >& $log

if ($3 == "") then
    setenv TESTDIR $R2LIB
else
    setenv TESTDIR $3
endif

echo "TESTDIR: $TESTDIR" >>& $log
