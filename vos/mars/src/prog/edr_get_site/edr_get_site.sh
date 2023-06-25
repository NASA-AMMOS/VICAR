#! /bin/sh

## Returns the site of the EDR parameter from the
## KEYWORD parameter.

## Author: Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
## Date: August 4, 2003
## Version: 1.2
## Dependencies:  edr_lbl_get_value

MY_NAME=`basename $0`
USAGE="Usage: edr_get_site edr_filename"
KEYWORD='ROVER_MOTION_COUNTER'       ## gets site from rover motion ctr
#KEYWORD='COORDINATE_SYSTEM_INDEX '  ## gets site from Rover Coord Sys

## =======================================================
## =======================================================

## handle echo for Linux...sigh...
if [ `uname -s | tr '[a-z]' '[A-Z]'` = LINUX ]
    then 
        WRITE="echo -e "      
    else
        WRITE="echo "
fi

## =======================================================
## =======================================================

## Arg count check
if [ $# -ne 1 ]
    then ${WRITE} "${MY_NAME}: Incorrect number of arguments"
         ${WRITE} ${USAGE} 1>&2
         exit 1
fi

## =======================================================
## =======================================================

EDR=$1

if [ ! -f ${EDR} ]
     then ${WRITE} "Error: File \"${EDR}\" does not exist" >&2
     exit 1
elif [ ! -r ${EDR} ]
     then ${WRITE} "File \"${EDR}\" is not readable" >&2
     exit 1
fi

## =======================================================
## =======================================================

VALUE=`${MARSLIB}/edr_lbl_get_value ${EDR} ${KEYWORD}`


if [ $? -ne 0 -o "${VALUE}Z" = "Z" ]
    then ${WRITE} "Error: Could not retrieve value of ${KEYWORD} from EDR" >&2
         exit 1       
fi

## =======================================================
## =======================================================

## grab the first element of the list
SITE=`echo ${VALUE} | sed 's/^ *( *\([^, ]*\) *,.*$/\1/'`

## return if successful
if [ "${SITE}Z" = "Z" ] 
    then ${WRITE} "edr_get_site: Could not retrieve SITE from ${VALUE}" >&2
         exit 1
    else
         ${WRITE} ${SITE}
         exit 0
fi

exit 
