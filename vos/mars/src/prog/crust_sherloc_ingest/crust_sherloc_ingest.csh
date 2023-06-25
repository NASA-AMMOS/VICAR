#!/bin/csh
#
# Ingest a set of SHERLOC files and make a JSON file for CRUST import.
#
# Usage:
#   crust_sherloc_ingest RLS.CSV out.json
#
# First arg is the RLS file to read.  This is, conveniently, the file
# containing all the information we need (courtesy K. Uckert).  It has lines
# of the form:
# number, spec_name, image_name, samp, line
#
# To that we merely need to extract site,drive from the image filename,
# and build the JSON.
setenv V2PARAM_FILE "/tmp/vicar_param_${USER}$$.temp"

if ($# != 2) then
   head -14 $0
   exit
endif

alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'

set rls = $1
set out = $2

echo rls = $rls
echo out = $out

# Loop through the file and write out the JSON.
# Skip the first line, it's the header.
# Also remove pesky CR's

rm -f $out

echo '[' >>$out

set lnum = 1
set first = 0
foreach text ("`tail -n +2 $rls | tr -d \\r`")
   if ("$text" == "") continue

   if ($lnum % 10 == 0) then
      echo "Line $lnum"
   endif
   MATH lnum = $lnum+1

   # There's a second header that defines the fields, skip it too
   set num = `echo $text | cut -d, -f1`
   if ("$num" == "number") continue
   set spec_name = `echo $text | cut -d, -f2`
   set image_name = `echo $text | cut -d, -f3`
   set samp = `echo $text | cut -d, -f4`
   set line = `echo $text | cut -d, -f5`

   # Convert to one-based.  The source data defines 0,0 as the
   # upper left corner of the upper left pixel, so adding 0.5 makes
   # that the proper 1,1 at the center of the UL pixel.

   MATH line = $line+0.5
   MATH samp = $samp+0.5

   # Extract the site, drive from the image filename

   set s = `echo $image_name | sed -e 's/^............................\(...\).*$/\1/'`
   set d = `echo $image_name | sed -e 's/^...............................\(....\).*$/\1/'`

   # Remove leading 0's

   set s = `echo $s | sed -e 's/^00\([0-9]*\)/\1/' -e 's/^0\([0-9]*\)/\1/'`
   set d = `echo $d | sed -e 's/^000\(.\)/\1/' -e 's/^00\(..\)/\1/' -e 's/^0\(...\)/\1/'`

   # Write out the JSON

   if ($first != 0) echo '  },' >>$out
   set first = 1

   echo '  {' >>$out
   echo '    "context_filename": "'$image_name'",' >>$out
   echo '    "line": '$line',' >>$out
   echo '    "sample": '$samp',' >>$out
   echo '    "site_id": '$s',' >>$out
   echo '    "drive_id": '$d',' >>$out
   echo '    "spectral_filename": "'$spec_name'",' >>$out
   echo '    "spectrum_number": '$num >>$out

end

if ($first != 0) echo '  }' >>$out
echo ']' >>$out

echo "COMPLETE"

