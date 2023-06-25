#! /bin/sh

## Creates temporary RMC file for use with mer_addsite

## Author: Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
## Date: September 19, 2003
## Version: 1.2.1
## Dependencies: None

MY_NAME=`basename $0`
PURPOSE="\nPurpose: Utility script that creates a temporary RMC file for\n\
\t use with the mer_addsite script.  Returns full filename\n\
\t of the tempoary RMC file if successful, otherwise  \n\
\t returns with error code."
USAGE="\nUsage: ${MY_NAME} -s site -m mission -sid id [ -csn csn | -ref refCsn \n \
\t\t\t | -off offset | -ori orient | -d dir | -f file | -h ]\n \
\n \
\t-m mission\t- Mission name of RMC (req'd) \n \
\t-s site\t\t- Site of the solution (req'd) \n \
\t-sid id\t\t- Solution id of the new solution (req'd) \n \
\t-csn csn\t- Coord sys name of the solution \n \
\t-ref refCsn\t- Coord sys name of reference frame \n \
\t-off offset\t- Solution offset '(x,y,z)' \n \
\t-ori orient\t- Solution orienation '(s,v1,v2,v3)' \n \
\t-d dir\t\t- Directory to which RMC file will be written \n \
\t-f file\t\t- Name of RMC file to use \n \
\t-h\t\t- Prints this message and exits\n"

CREATE_DIR=NONE
FILE_NAME=NONE
MISSION_NAME=NONE
SOLUTION_ID=NONE
SITE=NONE
PREV_SITE=NONE
CSN=NONE
REF_CSN=NONE
OFFSET=NONE
ORIENT=NONE

OFF_X=NONE
OFF_Y=NONE
OFF_Z=NONE
ORI_S=NONE
ORI_V1=NONE
ORI_V2=NONE
ORI_V3=NONE

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
        if [ $# -gt 0 ]; then
            CREATE_DIR="$1"
            CREATE_DIR=`${MARSLIB}/absolute_path ${CREATE_DIR}`
            if [ $? -ne 0 ]; then
               ${WRITE} "${MY_NAME}: Error occurred during program invocation." \
                        " Exiting..." 1>&2
               exit 1
            fi            
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing directory.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -f) shift; 
        if [ $# -gt 0 ]
            then FILE_NAME="$1"
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing filename.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -m) shift; 
        if [ $# -gt 0 ]
            then MISSION_NAME=`echo $1 | tr '[a-z]' '[A-Z]'`
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing mission.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -sid) shift; 
        if [ $# -gt 0 ]
            then SOLUTION_ID="$1"
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing solution id.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -s) shift; 
        if [ $# -gt 0 ]
            then SITE="$1"
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing site value.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -csn) shift; 
        if [ $# -gt 0 ]
            then CSN=`echo $1 | tr '[a-z]' '[A-Z]'`
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing coordinate sys name.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -ref) shift; 
        if [ $# -gt 0 ]
            then REF_CSN=`echo $1 | tr '[a-z]' '[A-Z]'`
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing reference coordinate sys name.  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -off) shift; 
        if [ $# -gt 0 ]
            then OFFSET="$1"
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing offset (x,y,z).  Exiting..." 1>&2
            ${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -ori) shift; 
        if [ $# -gt 0 ]
            then ORIENT="$1"
        else
            ${WRITE} "${MY_NAME}: *** Error - Missing quaternion (s,v1,v2,v3).  Exiting..." 1>&2
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

if [ ${MISSION_NAME} = NONE ]
    then
        ${WRITE} "${MY_NAME}: *** Error - mission name not specified." 1>&2
        exit 1
fi
if [ ${SOLUTION_ID} = NONE ]
    then
        ${WRITE} "${MY_NAME}: *** Error - solution id not specified." 1>&2
        exit 1
fi
if [ ${SITE} = NONE ]
    then
        ${WRITE} "${MY_NAME}: *** Error - site not specified." 1>&2
        exit 1
    else
        if [ ${SITE} -lt 1 ]
            then
                 ${WRITE} "${MY_NAME}: *** Error - site must be a positive integer" 1>&2
                 exit 1
        fi
        PREV_SITE=`expr ${SITE} - 1`
fi

if [ ${CSN} = NONE ]
    then
        CSN="SITE_FRAME"
fi

if [ ${REF_CSN} = NONE ]
    then
        REF_CSN="SITE_FRAME"
fi

if [ ${OFFSET} = NONE ]
    then
        OFFSET='(0.0,0.0,0.0)'
fi

if [ ${ORIENT} = NONE ]
    then
        ORIENT='(1.0,0.0,0.0,0.0)'
fi


if [ ${FILE_NAME} = NONE ]
    then
        FILE_NAME="tmp_${$}.rmc"
fi


if [ ${CREATE_DIR} = NONE ]
    then
        CREATE_DIR="/tmp"
    else
        if [ ! -d ${CREATE_DIR} ]
            then
                ${WRITE} "${MY_NAME}: *** Error - Directory ${CREATE_DIR} does not exist" 1>&2
                exit 1
        fi
fi

FILEPATH="${CREATE_DIR}/${FILE_NAME}"


##Check format of OFFSET
RE_ERROR=0
MATCH=`expr $OFFSET : \([-0-9.]*[0-9.],[-0-9.]*[0-9.],[-0-9.]*[0-9.]\)`
if [ $MATCH -eq 0 ] ## 0 means no match, positive means match
    then
        ${WRITE} "${MY_NAME}: *** Error - Offset format incorrect." 1>&2
        RE_ERROR=1
fi

##Check format of ORIENT
MATCH=`expr $ORIENT : \([-0-9.]*[0-9.],[-0-9.]*[0-9.],[-0-9.]*[0-9.],[-0-9.]*[0-9.]\)`
if [ $MATCH -eq 0 ] ## 0 means no match, positive means match
    then
        ${WRITE} "${MY_NAME}: *** Error - Orientation format incorrect." 1>&2
        RE_ERROR=1
fi

if [ $RE_ERROR -eq 1 ]
    then
        ${WRITE} "${MY_NAME}: Exiting..." 1>&2
        exit 1
fi

## GRAB OFFSET
OFF_X=`echo ${OFFSET} | sed 's/^[^0-9.]\([^, ]*\) *,.*$/\1/'`
OFF_Y=`echo ${OFFSET} | sed 's/^[^,]*, *\([^, ]*\) *,.*$/\1/'`
OFF_Z=`echo ${OFFSET} | sed 's/^[^,]*,[^,]*, *\([^) ]*\) *)$/\1/'`

## GRAB ORIENT
ORI_S=`echo  $ORIENT | sed 's/^[^0-9.]\([^, ]*\) *,.*$/\1/'`
ORI_V1=`echo $ORIENT | sed 's/^[^,]*, *\([^, ]*\) *,.*$/\1/'`
ORI_V2=`echo $ORIENT | sed 's/^[^,]*, *[^,]*, *\([^, ]*\) *,.*$/\1/'`
ORI_V3=`echo $ORIENT | sed 's/^.*, *\([-0-9.]*\) *).*$/\1/'`

## MAKE ADDATE
NOW_DATE=`date -u +%Y-%m-%dT%H:%M:%SZ`


RMC_CONTENT="<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?> \n\
<rmc_file mission=\"${MISSION_NAME}\" > \n\
\t<priority> \n\
\t<entry solution_id=\"${SOLUTION_ID}\" /> \n\
\t</priority> \n\
\n\
\t<solution solution_id=\"${SOLUTION_ID}\" name=\"${CSN}\" index1=\"${SITE}\"> \n\
\t\t<reference_frame name=\"${REF_CSN}\" index1=\"${PREV_SITE}\" /> \n\
\t\t<offset x=\"${OFF_X}\" y=\"${OFF_Y}\" z=\"${OFF_Z}\" /> \n\
\t\t<orientation s=\"${ORI_S}\" v1=\"${ORI_V1}\" v2=\"${ORI_V2}\" v3=\"${ORI_V3}\" /> \n\
\t</solution> \n\
\n\
</rmc_file> \n"
 

$WRITE ${RMC_CONTENT} > ${FILEPATH}

if [ $? -ne 0 ]
    then
        rm ${FILEPATH} > /dev/null 2>&1
        ${WRITE} "${MY_NAME} Error occured while writing contents out to $FILEPATH" 1>&2
        exit 1
fi

${WRITE} ${FILEPATH}

exit 0

