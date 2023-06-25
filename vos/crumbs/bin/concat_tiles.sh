#!/bin/csh -f

if($#argv < 2) then
 goto usage
endif

if(! -e $1) then
 echo Whoops - Specified input file does not exist: $1
 goto usage
endif

# Set lines to be all unique texture specs
set lines = `grep Texture2 *.iv | sed -e 's/^.* \"//' -e 's/\.rgb.*$//' | sort -u`

# Create a temporary file (textures.incl) that defines the textures to be used
echo 'Separator {' >textures.incl
foreach line ($lines)
 echo DEF Texture_$line >>textures.incl
 grep Texture2 *.iv | grep $line | sed -e 's/^.*://' | sort -u >>textures.incl
end
echo } >>textures.incl

# Incorporate the texture predefinitions at the top of the file,
# replace all texture usages with references to the predefinitions,
# change the LOD nodes to LevelOfDetail nodes,
# and prepend _ to all separator names that begin with a digit
sed -e 's/File { name "/include(/' -e 's/" }.*$/)/' $1 | m4 \
| sed -e 's/Texture2 {filename \"/USE Texture_/' -e 's/\.rgb.*$//' \
| sed -e 's/Group { # 1 /include(textures.incl)Group { # 1 /' | m4 \
| sed -e "s/DEF[ ]*\([0-9]\)/DEF _\1/" >$2

exit

usage:
echo Usage: $0 input_filename output_filename
echo Expands all Inventor files referenced in the input file in place
echo and then removes duplicate texture image references.
