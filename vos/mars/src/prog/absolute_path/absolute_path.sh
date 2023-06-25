#! /bin/sh
#-xv

## Converts given filepath to absolute filepath, the path
## of the parent directory MUST exist.

## Author: Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
## Date: May 19, 2004
## Version: 1.0
## Dependencies: None

MY_NAME=`basename $0`
PURPOSE="\nPurpose: Script returns absolute path of filepath argument."
USAGE="\nUsage: ${MY_NAME} [ filepath | --help ]\n \
\n \
   filepath\t- File path to be translated\n \
   --help\t\t- Prints this message and exits \n \
\n \
 **NOTE:  filepath parent directory MUST exist, the base file need not\n"

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
    then ${WRITE} "${MY_NAME}: Incorrect number of arguments" 1>&2
         ${WRITE}  ${USAGE} 1>&2
#         ${WRITE}
         exit 1
fi

## =======================================================
## =======================================================

if [ "${1}Z" = "--helpZ" ] ; then
    ${WRITE} $PURPOSE 
    ${WRITE} $USAGE 
    exit 0
fi


## =======================================================
## =======================================================

## Grab the filename
ORIGFILE="$1"

## split into directory and basename
RELATIVE_DIR=`dirname  ${ORIGFILE}`
FILE_BASE=`basename ${ORIGFILE}`

if [ ! -d ${RELATIVE_DIR} ]; then
   ${WRITE} "${MY_NAME}: Directory does not exist: ${RELATIVE_DIR}" 1>&2
   exit 1
fi


## translate to Absolute directory
ABSOLUTE_DIR=`(cd ${RELATIVE_DIR}; pwd ; )`

## combine to form absolute path
ABSOLUTE_PATH="${ABSOLUTE_DIR}/${FILE_BASE}"

if [ "${ABSOLUTE_DIR}Z" = "/Z" ]; then
    ABSOLUTE_PATH="/${FILE_BASE}"
else
    ABSOLUTE_PATH="${ABSOLUTE_DIR}/${FILE_BASE}"
fi

## return abolsute filepath
${WRITE} ${ABSOLUTE_PATH}

## Exit successfully
exit 0


