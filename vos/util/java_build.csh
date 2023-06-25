#!/bin/tcsh
# Note: tcsh is used instead of csh due to restrictions on the length of an
# env var in Solaris 2.7.  ${CLASSPATH} needs to be more than 1024 characters.
# The above line may need to be changed to the correct path for tcsh on your
# system.  If csh is ever fixed (as it is on Linux), change this back to csh.
#
# This script builds the contents of the java directory.
echo "*********************************************"
echo "****          JAVA BUILD STARTS          ****"
echo "*********************************************"

# For compilation only, we want CLASSPATH to point at the source
if ($?CLASSPATH != 0) then
   setenv CLASSPATH ${V2JAVA}:"${CLASSPATH}"
else
   setenv CLASSPATH ${V2JAVA}
endif

# The /home/cmbld/.history file gives the Java Mac build heartburn
# periodically so delete it in the beginning of a full java build.
rm -rf /home/cmbld/.history
 
# Create output product directories if necessary
if ( ! -e ${V2HTML} ) then
   mkdir ${V2HTML}
endif
if ( ! -e ${V2HTML}/jars ) then
   mkdir ${V2HTML}/jars
endif
if ( ! -e ${V2HTML}/javadoc ) then
   mkdir ${V2HTML}/javadoc
endif

cd ${V2TOP}

# Create a softlink between ${V2HTML}/jars and ${V2TOP}/jars to
# move the jars directory out of html.  Needed to do this to
# avoid getting "Word too long".
#ln -s html/jars jars

# Fetch all the externals
${V2UTIL}/java_fetch_externals.csh
cp /usr/local/vicar/maven-artifacts/jars/* ${V2HTML}/jars  # deploy maven jars reqd for compile

#
# Now add all of those jars into ${CLASSPATH}.  This is basically what vicset1
# does, but at the time vicset1 is run, these jars are not present... yet
# they will be needed for the build process.  So we must do it again here.
# We could simply source vicset1 again, but that would introduce a dependency
# on ${V2TOP} which we would like to avoid here, so developers can use this
# script in private directories by setting ${V2HTML} and ${V2JAVA}.
#
# First, expunge old references
if ($?CLASSPATH != 0) then
   setenv CLASSPATH `${V2TOP}/util/remove_element.csh "${CLASSPATH}" ${V2TOP}/\*`
endif

# Now, set up ${CLASSPATH} again, just like vicset1 does.
if ("${CLASSPATH}" == "") then
#    setenv CLASSPATH ${V2TOP}/j/\*:${V2TOP}/j
    setenv CLASSPATH ${V2HTML}/jars/\*:${V2HTML}/jars
else
#    setenv CLASSPATH "${CLASSPATH}":${V2TOP}/j/\*:${V2TOP}/j
    setenv CLASSPATH "${CLASSPATH}":${V2HTML}/jars/\*:${V2HTML}/jars
endif

# The packagelist.tmp file is accumulated during the build and used for
# running javadoc on all packages at once at the end.  Non-packaged files
# have javadoc run on them during their individual build.
if ( -e ${V2HTML}/packagelist.tmp ) then
   /bin/rm -f ${V2HTML}/packagelist.tmp
endif

# Recursively go through each directory and build it.  Accumulates
# packagelist.tmp.  Ignores any directory named "test".

# Note that we are explicitly listing the top-level directories (TLD)
# in the java tree.  If you add another TLD, add to this list!  In two places.

# Phase 1 generates all source code (e.g. from IDL or JAXB)
echo "*********************************************"
echo "**** CODE GENERATION PHASE OF JAVA BUILD ****"
echo "*********************************************"
cd ${V2JAVA}
foreach tld (jpl com)
  find $tld -name test -prune -o -type d -exec $V2UTIL/java_build_dir.csh {} generate \;
end

# Phase 2 compiles the code and does everything else
echo "*********************************************"
echo "****     COMPILE PHASE OF JAVA BUILD     ****"
echo "*********************************************"
cd ${V2JAVA}
foreach tld (jpl com)
  find $tld -name test -prune -o -type d -exec $V2UTIL/java_build_dir.csh {} compile \;
end

# Phase 3 generates javadoc on all the compiled packages.
echo "*********************************************"
echo "****     JAVADOC PHASE OF JAVA BUILD     ****"
echo "*********************************************"
$V2UTIL/java_build_javadoc.csh

echo "*********************************************"
echo "****  EXTENDED MAVEN COPY TO JAVA BUILD  ****"
echo "*********************************************"
set MVNART="${V2TOP}/../maven-artifacts"
echo "**** Using directory: '${MVNART}'"
mkdir -pv ${MVNART}/{temp,src-jars,assemblies,jars,wars}
mkdir -pv ${MVNART}/temp/{inc,sources,templates}

# Unpack JNI generated files into 'temp/inc' subdir
echo "**** Unpacking JNI files. ..."
ls -1 ${MVNART}/assemblies/*-jni.tar.gz
cd ${MVNART}/temp
# unpack multiple
ls -1 ${MVNART}/assemblies/*-jni.tar.gz | xargs -I {} tar -xzvf {}
# correct perms inconsistencies
chmod -R 664 inc/*
# copy all JNI files
mkdir -pv ${V2HTML}/inc
cp -pr inc/* ${V2HTML}/inc/.

# Unpack templates into 'temp/templates' subdir
echo "**** Unpacking template files. ..."
ls -1 ${MVNART}/assemblies/*-templates.tar.gz
cd ${MVNART}/temp
# unpack multiple
ls -1 ${MVNART}/assemblies/*-templates.tar.gz | xargs -I {} tar -xzvf {}
# correct perms inconsistencies
chmod -R 664 templates/*
# copy all template files
mkdir -pv ${V2TEMPLATES}
cp -pr templates/* ${V2TEMPLATES}/.

# Unpack source jars into 'temp/sources' subdir
echo "**** Unpacking source files. ..."
ls -1 ${MVNART}/src-jars/
cd ${MVNART}/temp
# unpack multiple, exclude metadata
unzip -o ${MVNART}/src-jars/\*-sources.jar -x "*META-INF*" -d sources/
# correct perms inconsistencies in assembly plugin
find sources -type d -exec chmod 775 {} +
find sources -type f -exec chmod 664 {} +
# copy specific package source only
mkdir -pv ${V2JAVA}/jpl/mipl
# cp -pR sources/jpl/mipl/* ${V2JAVA}/jpl/mipl/.  # all source trees
# ADD NEW UNPACKED SOURCE TREES HERE
cp -pR sources/jpl/mipl/io ${V2JAVA}/jpl/mipl/.

# The build is done...
echo "*********************************************"
echo "****        THE BUILD IS FINISHED        ****"
echo "*********************************************"
