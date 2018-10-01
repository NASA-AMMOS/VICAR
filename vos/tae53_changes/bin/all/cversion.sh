NOOP=noop
#
# CHANGE LOG
# 29-jul-94	Intergraph port:  use acc (not cc); add -Atarg=native...dag

VERSION_FILE=./.version.c


# first get the version number
#
if (test "$1")
then
    VERSION=$1
else
    echo -n 'enter version number: '	# prompt user
    read VERSION         				# get version number
    test $VERSION || VERSION=`date`		# give default if nullstring
fi
echo VERSION=\"${VERSION}\"


# now get the version variable name
#
if (test "$2")
then
	VERSION_VAR=$2
else
	echo -n 'enter version variable name: ' 	# prompt user
	read VERSION_VAR                        	# get version var name
	test $VERSION_VAR || VERSION_VAR=version	# default for null string
fi
echo VERSION_VAR=${VERSION_VAR}


# create the version file
#
cat << END_OF_FILE > ${VERSION_FILE}
/*
 *	This module provides the version number for a c program.
 */


      char VERSION_VAR[] = {VERSION};	/* get user supplied value */

END_OF_FILE
sleep 1
set -x	

if [ $TAEPLAT = ingr ] ; then
CCMD="acc -Atarg=native"
else
if [ $TAEPLAT = x86_macosx ]; then
CCMD="gcc -m32"
else
if [ $TAEPLAT = x86_linux]; then
CCMD="gcc -m32"
else
CCMD=cc
fi
fi
fi
$CCMD -c \
	-DVERSION="\"${VERSION}\"" \
	-DVERSION_VAR="${VERSION_VAR}" \
	$VERSION_FILE

