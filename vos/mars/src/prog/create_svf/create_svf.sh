#! /bin/sh
 
## Check that SVF master exists, if not, create one.

## Author: Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
## Date: September 19, 2003
## Version: 1.2.1
## Dependencies: None

MY_NAME=`basename $0`
PURPOSE="\nPurpose: Utility script that checks if the resulting version\n\
\t 0 SVF exists, if not, one is created with link "
USAGE="\nUsage: ${MY_NAME} -d dir -m mission -v variant [ -h ]\n \
\n \
\t-d dir\t\t- Directory to which SVF will be written (req'd)\n \
\t-m mission\t- Mission name of SVF (req'd)\n \
\t-v variant\t- Variant of the SVF: master / daily / generic\n \
\t-h\t\t- Prints this message and exits\n "

CREATE_DIR=NONE
MISSION_NAME=NONE
VARIANT_TYPE=NONE
VARIANT_ENTRY=""
FILE_TYPE="Generic"

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
if [ $# -lt 1 ]
    then ${WRITE} "${MY_NAME}: Incorrect number of arguments"
         ${WRITE} ${USAGE} 1>&2
         exit 1
fi

## =======================================================
## =======================================================

while :
do
    case "$1" in
    -d) shift; 
        if [ $# -gt 0 ]
            then CREATE_DIR="$1"
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing directory name.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -m) shift; 
        if [ $# -gt 0 ]
            then MISSION_NAME=`echo $1 | tr '[a-z]' '[A-Z]'`
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing mission value.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -v) shift; 
        if [ $# -gt 0 ]
            then VARIANT_TYPE=`echo $1 | tr '[a-z]' '[A-Z]'`
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing variant type.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -h) ${WRITE} $PURPOSE; ${WRITE} $USAGE; exit 0 ;;
    "") break ;;
    *) ${WRITE} "${MY_NAME}: *** Error - Unknown parameter: ${1}.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
            ;;
    esac
    shift
done


if [ ${CREATE_DIR} = NONE ]
    then
        ${WRITE} "${MY_NAME}: *** Error - directory not specified." 1>&2
        exit 1
fi

if [ ${MISSION_NAME} = NONE ]
    then
        ${WRITE} "${MY_NAME}: *** Error - mission name not specified." 1>&2
        exit 1
fi

if [ ${VARIANT_TYPE} != NONE ]
    then
        if [ ${VARIANT_TYPE} = MASTER ]
            then
                VARIANT_ENTRY=' variant="Master_SVF" '
                FILE_TYPE="Master"
        elif [ ${VARIANT_TYPE} = DAILY ]
           then
                VARIANT_ENTRY=' variant="Daily_SVF" '   
                FILE_TYPE="Daily"
        elif [ ${VARIANT_TYPE} = GENERIC ]
           then
                VARIANT_ENTRY=''
                FILE_TYPE="Generic"
        else
                ${WRITE} "${MY_NAME}: *** Warning - Unrecognized variant type." 1>&2
                ${WRITE} "${MY_NAME}: Automatically setting type: GENERIC" 1>&2
                VARIANT_ENTRY=''
                FILE_TYPE="Generic"
        fi
    else
        ${WRITE} "${MY_NAME}: *** Warning - Unspecified variant type." 1>&2
        ${WRITE} "${MY_NAME}: Automatically setting type: GENERIC" 1>&2
        VARIANT_ENTRY=''
        FILE_TYPE="Generic"
fi


if [ ! -d ${CREATE_DIR} ]
    then
        ${WRITE} "${MY_NAME}: *** Error - ${CREATE_DIR} does not exist" 1>&2
        exit 1
fi

EMPTY_RMC_CONTENT="<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?> \n\
<rmc_file mission=\"${MISSION_NAME}\" ${VARIANT_ENTRY} > \n\
\t<priority> \n\
\t</priority> \n\
\n\
</rmc_file> \n"
 

NEW_SVF_FILE="${CREATE_DIR}/${MISSION_NAME}_${FILE_TYPE}_00000.svf"
NEW_LINK_FILE="${CREATE_DIR}/${MISSION_NAME}_${FILE_TYPE}.svf" 

## check if it already exists...
if [ -f ${NEW_SVF_FILE} ]
    then 
        exit 0
fi

ls ${CREATE_DIR}/${MISSION_NAME}_${FILE_TYPE}_[0-9]*.svf > /dev/null 2>&1
if [ $? -ne "0" ]
    then
        ${WRITE} ${EMPTY_RMC_CONTENT} > ${NEW_SVF_FILE}
        if [ $? -ne 0 ]
            then
                ${WRITE} "${MY_NAME}: Could not write to new SVF ${NEW_SVF_FILE}" 1>&2
                exit 1
        fi
else
    ${WRITE} "${MY_NAME}: *** Error - Later versions of SVF ${FILE_TYPE} found without original" 1>&2
    ${WRITE} "${MY_NAME}: *** Error - Original: ${NEW_SVF_FILE}" 1>&2  
    ${WRITE} "Exiting..." 1>&2
    exit 1
fi



if [ -f ${NEW_LINK_FILE} ]
    then
        rm -f ${NEW_LINK_FILE}
fi

ln -s `basename ${NEW_SVF_FILE}` ${NEW_LINK_FILE}

if [ $? -ne 0 ]
    then
        ${WRITE} "${MY_NAME}:\tCould not link from ${NEW_LINK_FILE} " 1>&2
        ${WRITE} "\t\tto new SVF ${NEW_SVF_FILE}" 1>&2
        exit 1
fi

exit 0
