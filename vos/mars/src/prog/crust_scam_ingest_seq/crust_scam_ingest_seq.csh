#!/bin/csh
#
# Ingest a whole set of RMI sequences.  The list file should contain FDR images
# and associated spectral .LBL files for a single sequence on a single
# site/drive on a single sol.  The script will sort them appropriately, split
# at RMI boundaries, and build up the JSON file with results from each.
#
# Usage:
#   crust_scam_ingest_seq lis out.json
#
setenv V2PARAM_FILE "/tmp/vicar_param_${USER}$$.temp"

if ($# != 2) then
   head -14 $0
   exit
endif

set lis = $1
set out = $2

# Set up output file

rm -f $out
echo '[' >>$out
set first = 0

# Sort list by sclk.  This is complicated because we don't know what kind of
# pathnames we might have.

# First, remove all pathnames from the file, and then sort by sclk

set tmplis = /tmp/${lis:t:r}_$$.tmp

cat $lis | sed -e 's#^.*/\([^/]*\)$#\1#' | sort -k 1.10,1.23 >$tmplis

# Now go through the file and look for non-RMI's plus the RMI's on either side

set shotlis = /tmp/crust_shotlist_$$.tmp
set shotjson = /tmp/crust_shotjson_$$.tmp

set rmi_before = ""
set rmi_after = ""

rm -f $shotlis
set shot_count = 0

foreach key ("`cat $tmplis`")

   # Get the actual pathname

   set keypath = `grep "$key" $lis | head -1`

   # Get the type code

   set code = `echo $key | sed -e 's/^.......................\(...\).*$/\1/'`

   # If we found an RMI, close off the section and process it

   if ("$code" == "FDR") then
      set rmi_before = $rmi_after
      set rmi_after = $keypath
      if ($shot_count > 0) then

         # process!

         echo "############"
         echo CRUST INGEST: $shotlis $shotjson $rmi_before $rmi_after
         echo "############"
         $MARSLIB/crust_scam_ingest $shotlis $shotjson $rmi_before $rmi_after

         # append json file to master

         if (`wc -l $shotjson | cut -d ' ' -f 1` > 2) then
            if ($first != 0) then
               echo "," >>$out
            endif
            set first = 1
            cat $shotjson | tr -d '[]' >>$out
         endif
         rm $shotjson

         # reset for next set

         rm $shotlis
         set shot_count = 0
      endif
   else

      # We found a shot, save it for future processing

      echo "$keypath" >>$shotlis
      @ shot_count++
   endif
end

# If there's any remaining list, process it.  Same as above, sigh... except
# there is no trailing RMI.  But since after is moved to before when we hit
# the RMI, at this point the leading RMI is actually in rmi_after.

if ($shot_count > 0) then
   echo "############"
   echo CRUST INGEST: $shotlis $shotjson $rmi_after
   echo "############"
   $MARSLIB/crust_scam_ingest $shotlis $shotjson $rmi_after

   if (`wc -l $shotjson | cut -d ' ' -f 1` > 2) then
      if ($first != 0) then
         echo "," >>$out
      endif
      set first = 1
      cat $shotjson | tr -d '[]' >>$out
   endif
endif

echo ']' >>$out
rm -f $shotjson $shotlis $tmplis

echo "*** FINISHED ***"

