#! /bin/sh 
#-xv

## Formats an integer to include leading zeros

## Author: Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
## Date: August 4, 2003
## Version: 1.2
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
    then ${WRITE} "${MY_NAME}: Incorrect number of arguments"
         ${WRITE} "Usage: ${MY_NAME} number length"
         ${WRITE}
         exit 1
fi

## =======================================================
## =======================================================


## Grab the value to be formatted
VALUE="$1"

VALUE=`echo ${1} | tr -d ' ' | sed 's/^\([0-9][0-9]*\)$/\1/'`

if [ "${VALUE}Z" = "Z" ]
    then ${WRITE} "${MY_NAME}: Format of number argument incorrect: ${VALUE}" 1>&2
         exit 1
fi


## Grab the number of digits in format
FORMAT_LENGTH=`echo ${2} | sed 's/^\([0-9][0-9]*\)$/\1/'`
if [ "${FORMAT_LENGTH}Z" = "Z" ]
    then ${WRITE} "${MY_NAME}: Format of length argument incorrect: ${FORMAT_LENGTH}" 1>&2
         exit 1
fi

## Get length of value
CUR_VALUE=`expr ${VALUE} + 0`
CUR_LENGTH=`echo ${VALUE} | wc -c | xargs expr -1 +`


DONE="0"
while [ ${DONE} != "1" ]
do
    if [ ${CUR_LENGTH} -ge ${FORMAT_LENGTH} ]
        then DONE="1"
    else
        CUR_VALUE="0${CUR_VALUE}"
        CUR_LENGTH=`expr ${CUR_LENGTH} + 1`
fi
done

${WRITE} ${CUR_VALUE}
