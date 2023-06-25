#! /bin/sh
# -xv
 
## Attempts to create lock, holds it for a time, deletes lock.
## If cannot create lock, it sleeps for a bit.

## Author: Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
## Date: May 19, 2004
## Version: 1.2
## Dependencies: absolute_path

SLEEP_PERIOD="5"
FILE_NAME=NONE
LOCK_OWN="0"
ATTEMPT_LIMIT="5"

MY_NAME=`basename $0`
USAGE="\nUsage: ${MY_NAME} -f file [ -w wait_time | -l att_count ]  \n \
\n \
\t-f file\t\t- File to be locked (required)\n \
\t-l att_count\t- Limit of attempts before quitting (default: ${ATTEMPT_LIMIT})\n
\t-w wait_time\t- Number of seconds to wait before reattempting \n \
\t\t\t. to acquire lock (default: ${SLEEP_PERIOD}).\n  "


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
    -w) shift; 
        if [ $# -gt 0 ]
            then SLEEP_PERIOD="$1"
            if [ $SLEEP_PERIOD -lt 0 ]
                then ${WRITE} "${MY_NAME}: Error - wait_time must be non-negative." 1>&2
                     exit 1
            fi
        else
            ${WRITE} "${MY_NAME}: Error - Missing sleep period value.  Exiting..." 1>&2
            #${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -l) shift; 
        if [ $# -gt 0 ]
            then ATTEMPT_LIMIT="$1"
            if [ $ATTEMPT_LIMIT -lt 0 ]
                then ${WRITE} "${MY_NAME}: Error - att_count must be positve." 1>&2
                     exit 1
            fi
        else
            ${WRITE} "${MY_NAME}: Error - Missing attempt limit value.  Exiting..." 1>&2
            #${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    -f) shift; 
        if [ $# -gt 0 ]; then
            FILE_NAME="$1"
            FILE_NAME=`${MARSLIB}/absolute_path ${FILE_NAME}`
            if [ $? -ne 0 ]; then
               ${WRITE} "${MY_NAME}: Error occurred during program invocation." \
                        " Exiting..." 1>&2
               exit 1
            fi               
        else
            ${WRITE} "${MY_NAME}: Error - Missing filename name value.  Exiting..." 1>&2
            #${WRITE} $USAGE 1>&2
            exit 1
        fi ;;
    ?*) ${WRITE} "${MY_NAME}: Unrecognized parameter: ${1}.  Exiting..." 1>&2; exit 1 ;;
    *) break ;;
    esac
    shift
done

if [ ${FILE_NAME} = "NONE" ]
    then ${WRITE} "${MY_NAME}: Error - no filename was specified. " 1>&2
         exit 1
fi

if [ ! -f ${FILE_NAME} ]
    then ${WRITE} "Error - file ${FILE_NAME} does not exist.  Exiting..." 1>&2
         exit 1
elif [ ! -r $FILE_NAME  ]
    then ${WRITE} "Error - file ${FILE_NAME} is not readable. Exiting..." 1>&2
         exit 1
fi

#LOCK_NAME=`${WRITE} ${FILE} | sed 's/^\(..*\)\.[^\.][^\.]*$/\1.lock/'`
LOCK_NAME="${FILE_NAME}.lock"

trap 'if [ ${LOCK_OWN} -eq "1" ] ; then ${WRITE} "  Releasing lock..."1>&2 ; \
         rm -f ${LOCK_NAME} ; exit 1 ; else ${WRITE} "  No lock to release" 1>&2 ; \
         exit 1; fi' 1 2 3 15 

while [ ${LOCK_OWN} = "0" ]
do
    if [ ${ATTEMPT_LIMIT} -lt 1 ]
        then
            ${WRITE} "${MY_NAME}: Unable to acquire file lock. Exiting..." 1>&2
            exit 1
    fi

    ln -s ${FILE_NAME} ${LOCK_NAME} > /dev/null 2>&1

    if [ $? -eq 0 ]
        then LOCK_OWN="1"
    else
        ATTEMPT_LIMIT=`expr ${ATTEMPT_LIMIT} - 1`
        ${WRITE} "${MY_NAME}: Unable to acquire lock.  Will try ${ATTEMPT_LIMIT} more times..." 1>&2
        if [ ${ATTEMPT_LIMIT} -lt 1 ]
            then
                ${WRITE} "${MY_NAME}: Unable to acquire file lock `basename ${LOCK_NAME}`" 1>&2
                exit 1
        fi
        sleep ${SLEEP_PERIOD}
    fi
done

if [ ! -f ${LOCK_NAME} ]
    then ${WRITE} "${MY_NAME}: Unable to create lock file ${LOCK_NAME}.  Exiting..." 1>&2
         exit 1
fi

${WRITE} ${LOCK_NAME}
exit 0

