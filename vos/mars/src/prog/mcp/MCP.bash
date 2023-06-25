#!/home/jnm/bin/bash

#Use this instead of /bin/bash if you are trying to be flexible.
#!/usr/bin/env bash

######################################################################
#
# AUTHOR: Kurt Schwehr, schwehr@ri.cmu.edu
# START DATE: Sep-1999
#
# $Id: MCP.bash,v 1.20 1999/11/25 02:30:53 schwehr Exp $
#
# PROJECT: mvacs SSI
# 
# DESCRIPTION: Should do everything required for processing stereo
# pairs to produce output for the WITS user interface.
#
# INPUTS: see help
#
# ENV VARIABLES USED:
#
# OUTPUT: A whole bunch of things.
#
# Notes: Designed to be called by either a luser or a MCP (master 
#		control program).
#
# See help for more.
#
######################################################################

echo ------------------------------------------------------------
echo --- Welcome to MCP
echo ------------------------------------------------------------

declare -ri EXIT_FAILURE=1
declare -ri EXIT_SUCCESS=0


declare -ri TERSE=1
declare -ri TRACE=4
declare -ri VERBOSE=8
declare -ri BOMBASTIC=16

if [ -z "$SSI_VERBOSITY" ]; then
    declare -i debugLevel=4
else
    declare -i debugLevel=$SSI_VERBOSITY
fi

# Twisted way to get down to the fundamental script name.
tmp=${0##/*/}
tmp=${tmp%%.bash}
tmp=${tmp##*.}
tmp=${tmp##*/}
declare -r SCRIPT_NAME=$tmp

# $1 is the level to compare against debugLevel
# $2 is the string to echo to stdout.
DebugEcho()
{
    declare -ir val=$1
    if [ "$debugLevel" -ge "$val" ]; then
	#echo $2
	echo "${SCRIPT_NAME}(`printf "%02d" $1`) $2"
    fi
}

#DebugEcho $TERSE     "Terse is on"
#DebugEcho $TRACE     "Trace is on"
DebugEcho $VERBOSE   "Verbose is on"
DebugEcho $BOMBASTIC "Bombastic is on"

DebugEcho $TERSE "debugLevel           = $debugLevel"


######################################################################
# Global variables and constants
######################################################################

#if [ -z "$SSI_IMAGE_EXT" ]; then
#    declare -r ext=img
#    #declare -r ext=cal
#    DebugEcho $TRACE "setting ext = $ext"
#else
#    declare -r ext=$SSI_IMAGE_EXT
#fi

if [ -z "$SSI_TOP_DIR" ]; then
    declare -r SSI_TOP_DIR=/project/mars98_work/m01/james/current
    export SSI_TOP_DIR
    DebugEcho $TRACE "setting top dir = $SSI_TOP_DIR"
fi

if [ -z "$SCRIPT_DIR" ]; then
    declare -r SCRIPT_DIR=${SSI_TOP_DIR}/bin
    export SCRIPT_DIR
    DebugEcho $VERBOSE "setting script dir = $SSI_SCRIPT_DIR"
fi

if [ -z "$BIN_DIR" ]; then
    declare -r BIN_DIR=${SSI_TOP_DIR}/bin/`uname`
    export BIN_DIR
    DebugEcho $VERBOSE "setting bin dir = $SSI_BIN_DIR"
fi

if [ -z "$SSI_DATA_TOP_DIR" ]; then
    declare -r SSI_DATA_TOP_DIR=${SSI_TOP_DIR}/data
    export SSI_DATA_TOP_DIR
    DebugEcho $VERBOSE "setting data top dir = $SSI_DATA_TOP_DIR"
fi

if [ -z "$SSI_STATUS_DIR" ]; then
    declare -r SSI_STATUS_DIR=${SCRIPT_DIR}/status
    DebugEcho $VERBOSE "setting status dir = $SSI_STATUS_DIR"
fi

SetStatus()
{
    echo -n "MCP         -- $1" > $SSI_STATUS_DIR/MCP.status
}

SetStatus "starting up"

#if [ -z "$SSI_RUNTIME_CONTROL_DIR" ]; then
#    echo "FIX: Runtime control should be same as queue dir."
#    declare -r SSI_RUNTIME_CONTROL_DIR=${SCRIPT_DIR}/runtime_control
#    DebugEcho $VERBOSE "setting runtime control dir= $SSI_RUNTIME_CONTROL_DIR"
#fi

if [ -z "$SSI_STEREO_QUEUE_DIR" ]; then
    declare -r SSI_STEREO_QUEUE_DIR=${SSI_TOP_DIR}/queues/toJames
    DebugEcho $TRACE "ssi stereo queue dir = $SSI_STEREO_QUEUE_DIR"
fi

if [ ! -d $SSI_STEREO_QUEUE_DIR ]; then
    echo "ERROR: Queue directory does not exist: "
    echo     $SSI_STEREO_QUEUE_DIR
    exit $EXIT_FAILURE
fi

DebugEcho $VERBOSE ""
DebugEcho $VERBOSE  "Image Extension      = $ext"
DebugEcho $VERBOSE  "SSI_TOP_DIR          = $SSI_TOP_DIR"
DebugEcho $VERBOSE  "SSI_DATA_TOP_DIR     = $SSI_DATA_TOP_DIR"
DebugEcho $VERBOSE  "SSI_STEREO_QUEUE_DIR = $SSI_STEREO_QUEUE_DIR"
DebugEcho $VERBOSE  ""
DebugEcho $VERBOSE  "SCRIPT_DIR           = $SCRIPT_DIR"
DebugEcho $VERBOSE  "BIN_DIR              = $BIN_DIR"

declare -i sleepTime=30	# Default how long to sleep between checks, in seconds.

#declare -r imageFileNamePattern="s*_*[lr].$ext"	# Would be nice to say img,cal.
#declare -r leftImageFileNamePattern="s*_*l.$ext"
declare -r imageFileNamePattern="s*_*[lr].[ic][ma][gl]"	# Would be nice to say img,cal.
declare -r leftImageFileNamePattern="s*_*l.[ic][ma][gl]"
declare -r solDirectoryPattern="sol???"

declare -r stereopair2wits=${SCRIPT_DIR}/stereopair2wits.bash
declare -r rm=/bin/rm	# GNU rm seems to segfault a lot on IRIX 6.5???
declare -r gnuDate=${BIN_DIR}/gnu_date # Use the GNU date program.

###############################################
# Go to the queue dir so it is out working dir.

# This will keep the path length down with the for loop.
cd $SSI_STEREO_QUEUE_DIR

# force all control files to be cleared.
$rm -f *.rtc

DebugEcho $VERBOSE  "FIX: write the PID to $SSI_STATUS_DIR  write MCP.pid"

######################################################################
# Email logging
######################################################################
#
# This section will send an email out to an address.
# The goal is to use procmail to build a web page that shows the runs
# that occur.
#
# FIX: do we want to spawn this off to the background in case the
# mail gets slow to send?

#declare -r RECIPIENT_1=schwehr@mvacs.ucla.edu
#declare -r RECIPIENT_2=zbinden@mvacs.ucla.edu

#declare -ar recipients=( schwehr@mvacs.ucla.edu zbinden@mvacs.ucla.edu )
#declare -ar recipients=( schwehr@mvacs.ucla.edu )


HOSTNICK=`uname -n`

# TURNED off
if [ 0 == 1 ]; then

for person in "${recipients[@]}"
do
   DebugEcho $VERBOSE  ""
   DebugEcho $VERBOSE  Sending mail notification to $person

# NOTE: these must be hard tabs!!
cat <<- EOF # | mail $person
	PROGRAM     MCP -- Master Control Program
	ARGS        $0 $1 $2 $3 $4 $5 $6
	USER        $USER
	NODE        `uname -n`
	DOMAINNAME  `domainname`
	OS          `uname`
	OSREL       `uname -r`
	PROCESSOR   `uname -p`
	Hst`nslookup $HOSTNICK | grep -i name`
	PWD         `pwd`
EOF

done # for

fi

######################################################################
# GENERIC HELPER FUNCTIONS
######################################################################

# This script may use stuff not implimented in bash 1.x
checkBashVersion()
{
    declare -ir minBashVersion=2
    declare -ir bashMajorVersion=${BASH_VERSION%%.*}

    if [ $minBashVersion -gt $bashMajorVersion ]; then
	echo "*** ERROR:"
	echo "  Found bash version $BASH_VERSION."
	echo "  Need version ${minBashVersion}.x or higher."
	exit $EXIT_FAILURE
    fi
    DebugEcho $VERBOSE  "  Bash version okay."
}

# Just for safety, let's make sure we are in a known environment.
# If not, then someone will probably have to do some tweaking.
checkOS()
{
    declare -r okOSName=SunOS
    declare -r OSName=`uname`
    if [ $okOSName != $OSName ]; then
	echo "*** WARNING:"
	echo "  Found OS: $OSName"
	echo "  Expected: $okOSName"
	echo "  CONTINUE AT YOUR OWN RISK"
    fi

    declare -r okOSrev="5.7"
    declare -r OSrev=`uname -r`
    if [ $okOSrev != $OSrev ]; then
	echo "*** WARNING:"
	echo "  Found OS rev: $OSrev"
	echo "  Expected: $okOSrev"
	echo "  CONTINUE AT YOUR OWN RISK"
    fi
    DebugEcho $VERBOSE  "  OS checks out okay."
}

######################################################################
# Work Functions
######################################################################

declare -r PAUSE_FILE="pause.rtc"
declare -r RESUME_FILE="resume.rtc"
declare -r STOP_FILE="stop.rtc"

checkRuntimeControl()
{
    if [ -e "$STOP_FILE" ]; then
	$rm -f "$STOP_FILE"
	echo "Found stop file... EXITING NOW!"
	SetStatus "Stopped/done.  Found stop.rtc"
	exit $EXIT_SUCCESS
    fi

    if [ -e "$PAUSE_FILE" ]; then
	$rm -f "$PAUSE_FILE"
	DebugEcho $TRACE "Found pause file.  Waiting for $RESUME_FILE"

	while [ ! -e "$RESUME_FILE" ]; do
	    DebugEcho $BOMBASTIC "Sleeping 10 seconds"
	    SetStatus "Paused.  Waiting for resume.rtc"
	    sleep 10
	done
	$rm -f $RESUME_FILE
	DebugEcho $TRACE "Resuming"
	SetStatus "Resuming"
    fi
}

# Watch the queue for new images, and start the processing of them.
update ()
{
    SetStatus "Scanning directory"
    DebugEcho $VERBOSE  "Scanning queue dirctory: "
    DebugEcho $VERBOSE  -n "  Number of new image pairs:"

    for leftImage in $leftImageFileNamePattern
    do
        checkRuntimeControl

	#rightImage=${leftImage%%l.$ext}r.$ext
	rightImage=${leftImage%%l.[ic][ma][gl]}r.[ic][ma][gl]
	if [ ! -f $leftImage ]; then
	    DebugEcho $TRACE "Queue directory empty"
	    DebugEcho $VERBOSE "cannot open $leftImage - check your links - this may be fine."
	    true # nop - empty directory.
	elif [ -f $rightImage ]; then
	    DebugEcho $TRACE "Files remaining: `ls -1 $leftImageFileNamePattern  2>/dev/null | wc -l`"
	    DebugEcho $VERBOSE  ""
	    DebugEcho $VERBOSE  " -------------------------------------------------------"
	    DebugEcho $VERBOSE  "MCP: Calling stereopair2wits $leftImage $rightImage"
	    # FIX: let planet day number work... remove sol000
	    DebugEcho $VERBOSE  $stereopair2wits $leftImage $rightImage # sol000
	    SetStatus "processing $leftImage $rightImage"
	    $stereopair2wits $leftImage $rightImage #sol000
	    $rm $leftImage $rightImage
	else
	    DebugEcho $VERBOSE  "  Postponing $leftImage... waiting for $rightImage."
	    DebugEcho $VERBOSE  "  Patience is a virtue."
	fi
    done
    SetStatus "done with update"
}



######################################################################
# Main code - starts here
######################################################################

DebugEcho $VERBOSE  ""
DebugEcho $VERBOSE  "Paranoid Mode:"
checkBashVersion
checkOS

#
# Command line options
#
case "$1" in

u | 'update')
    DebugEcho $VERBOSE  "update:"
    DebugEcho $VERBOSE 
    update
    ;;

uc | 'update-continuous')
    DebugEcho $VERBOSE  "update-continuous:"
    DebugEcho $VERBOSE 

    if [ $# -eq 2 ]; then
	sleepTime=$2
    fi
    DebugEcho $VERBOSE  "Time between update starts = $sleepTime (seconds)"
    if [ 1 -gt $sleepTime ]; then
	DebugEcho $VERBOSE  "ERROR: must specify a sleep time of 1 or more seconds."
	exit 1
    fi

    declare -i startTime
    declare -i curTime
    declare -i deltaTime

    DebugEcho $VERBOSE  "Setting up startTime"
    startTime=`$gnuDate \+\%s`
    startTime=$startTime-$sleepTime

    while [ 1 ]; do

        # Look for responce from the lusers who control me.
        checkRuntimeControl

	DebugEcho $VERBOSE  ""
	startTime=`$gnuDate \+\%s`
	update
	curTime=`$gnuDate \+\%s`
	deltaTime=$curTime-$startTime

	if [ $deltaTime -lt $sleepTime ]; then
	    deltaTime=$sleepTime-$deltaTime
	    DebugEcho $VERBOSE  ""
	    DebugEcho $VERBOSE  "sleeping for $deltaTime seconds..."
	    SetStatus "Idle but running"
	    sleep $deltaTime
	fi

    done
    ;;

*)
    echo ""
    echo "usage: $0 {update|update-continuous}"
    echo ""
    echo "  update      -- check for new files; update symbolic links & queue"
    echo "  update-continuous [optionalNumSeconds]"
    echo "              -- run update every x seconds"
    echo "                 Defaults to $sleepTime seconds"
    echo ""
    echo "  "
    exit 1
    ;;

esac



######################################################################
# END OF FILE

#
# $Log: MCP.bash,v $
# Revision 1.20  1999/11/25 02:30:53  schwehr
# removed mkdir-subdirs
#
# Revision 1.19  1999/11/25 02:29:54  schwehr
# Removed the "clean-data" function.  Not a good thing to have around.
#
# Revision 1.18  1999/11/20 01:22:29  schwehr
# Minor debug echo cleanup.
#
# Revision 1.17  1999/11/20 00:56:10  schwehr
# Now parses down to the core script name and puts it in SCRIPT_NAME.
# Minor formatting improvement of output text.
#
# Revision 1.16  1999/11/18 22:10:07  schwehr
# Added SetStatus which writes to the status directory.
# Minor cleanup up unused clean functions.
#
# Revision 1.15  1999/11/16 19:54:07  schwehr
# Reduced the amount of output.
# Commented out the image extension.  Now looks for both cal and img
# 	always.
#
# Revision 1.14  1999/11/16 18:44:28  schwehr
# fixed DebugEcho to strip off the path of the binary.
#
# Revision 1.13  1999/11/12 06:43:21  schwehr
# Added checkRuntimeControl between each image pair being
# processed... stop sooner!
#
# Revision 1.12  1999/11/12 05:51:34  schwehr
# Scary change to use either cal or img extension... may not work.
#
# Revision 1.11  1999/11/08 21:43:22  schwehr
# Added runtime control.
#
# Revision 1.10  1999/11/04 17:03:16  schwehr
# Added DebugEcho and SSI_VERBOSITY.
# Now prints out a lot less info by default.
#
# Revision 1.9  1999/10/27 06:15:51  zbinden
# change the image extension from .img to .cal
#
# Revision 1.8  1999/10/25 06:08:32  schwehr
# Added welcome message.
# More verbose about setting SSI_STEREO_QUEUE_DIR.
#
# Revision 1.7  1999/10/22 20:48:13  schwehr
# Changed the top dir to preMORT5... but now Stubb's fsMaster.bash
# 	script should set these variables...
# Queue dir has been moved.
# Need to add a pid record to a file in the bin/status dir.
# Added SSI_RUNTIME_CONFIG_DIR and SSI_STATUS_DIR variables... need
# 	implimenting.
#
# Revision 1.6  1999/10/14 03:37:17  schwehr
# Should hopefully run on any mvacs sun now... please work!
#
# Revision 1.5  1999/10/13 17:04:28  schwehr
# Now fails if not running on fury.  Hopefully will be fixed soon!!!!
#
# Revision 1.4  1999/10/11 21:21:23  schwehr
# No longer force output dir to sol000.
# Added stub for mkdir-subdirs... or some such.
# Changed SSI_TOP_DIR.
#
# Revision 1.3  1999/10/06 18:28:18  schwehr
# switched the top dir to be retreat1.
#
# Revision 1.2  1999/10/06 18:27:17  schwehr
# Prototype of email notification from mort4 development script.
#
# Revision 1.1.1.1  1999/09/15 17:14:52  schwehr
# The version used for mort4.
#
# Revision 1.6  1999/09/14 20:26:30  schwehr
# Fixed the count of new files in the queue with help from Drav.
#
# Revision 1.5  1999/09/14 17:55:40  schwehr
# trivial fix.
#
# Revision 1.4  1999/09/14 17:37:52  schwehr
# Cleanup of variables and paths.
# Output info is much butter.
# Hid unused functions.
#
# Revision 1.3  1999/09/14 16:47:20  schwehr
# Added ext variable for the image file name extension.
# Move SSI variables up towards top.
#
#
