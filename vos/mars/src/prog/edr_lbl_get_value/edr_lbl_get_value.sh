#! /bin/sh

## Returns value associated with the KEYWORD of an EDR label

## Author: Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
## Date: August 4, 2003
## Version: 1.2.1
## Dependencies: None

MY_NAME=`basename $0`

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
if [ $# -ne 2 ]
    then ${WRITE} "\n${MY_NAME}: Incorrect number of arguments"
         ${WRITE} "Usage: ${MY_NAME} edr_file keyword"
         ${WRITE} ""
         ${WRITE} " NOTE: Called by the MER XML scripts only. "
         ${WRITE} " Utilizes a simple parse routine with the " 
         ${WRITE} " constraint that keyword-value pair be on "
         ${WRITE} " the same line.  If multiple keywords are "
         ${WRITE} " found, value of the first match is returned.\n "
         
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

KEYWORD=$2

## grab the first instance of KEYWORD
LINE=`head -n 450 ${EDR} | grep ${KEYWORD} | head -n 1`

if [ "${LINE}Z" = "Z" ] 
    then ${WRITE} "${MY_NAME}: Could not find an entry for ${KEYWORD}" 1>&2
         exit 1
fi


## grab the value of the keyword => value string
VALUE=`${WRITE} ${LINE} | sed 's/^[^=]*= *\([^ ]*\).*$/\1/'` 

if [ "${VALUE}Z" = "Z" ] 
    then ${WRITE} "${MY_NAME}: Could not retrieve value of ${KEYWORD}" 1>&2
         exit 1
fi

## =======================================================
## =======================================================

## remove antisocial characters
VALUE=`${WRITE} ${VALUE} | tr -d '\010' | tr -d '\011' | tr -d '\013' \
            | tr -d '\012' | tr -d '\015'`

${WRITE} ${VALUE}

exit
