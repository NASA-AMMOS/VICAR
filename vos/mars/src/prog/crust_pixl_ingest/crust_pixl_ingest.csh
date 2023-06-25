#!/bin/csh
#
# Ingest a set of PIXL files and make a JSON file for CRUST import.
#
# Usage:
#   crust_pixl_ingest.csh RXL.CSV out.json PC*.VIC [PC*.VIC ...]
#
# First arg is the RXL file to read.  This has coordinates of each shot in
# each image.
# Second arg is the output JSON file, for CRUST upload
# Remaining arguments are one or more MCC images matching what's in the RXL.
#
setenv V2PARAM_FILE "/tmp/vicar_param_${USER}$$.temp"

if ($# < 3) then
   head -14 $0
   exit
endif

alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'

set rxl = $1
set out = $2
set mccinp = "$argv[3*]"

echo rxl = $rxl
echo out = $out
echo mcc = $mccinp

# Get filename of spectral file.  Assumes the version numbers match!

set rfs = `echo $rxl | sed -e s/RXL/RFS/`

# Get the header line.  It looks something like this:
# PMC,x,y,z,geom_corr,PMC_0005_MCC_i,PMC_0005_MCC_j,PMC_0006_MCC_i,PMC_0006_MCC_j,PMC_2815_MCC_i,PMC_2815_MCC_j,PMC_2816_MCC_i,PMC_2816_MCC_j


set hdr = "`head -1 $rxl`"


# Loop through the header by groups

set pmc_count = 0
set index = 6
set mcc_list =
set pmc_list =
set site =
set drive =

while (1)

   # Extract the name for this column

   set name = "`echo $hdr | cut -d, -f${index}`"
   if ("$name" == "") break

   # Extract the PMC from that name

   set pmc =  `echo $name | sed -e 's/PMC_//' -e 's/_MCC.*$//'`

   # Loop through the given images looking for a PMC match

   set mcc_name = "no"
   set s = 0
   set d = 0
   foreach ff ($mccinp)
      set f = ${ff:t}
      set fpmc = `echo $f | sed -e 's/^............................................\(....\).*$/\1/'`
      if ("$fpmc" == "$pmc") then
         set mcc_name = $f

         # Extract the site, drive from the filename

         set s = `echo $f | sed -e 's/^............................\(...\).*$/\1/'`
         set d = `echo $f | sed -e 's/^...............................\(....\).*$/\1/'`

         # Remove leading 0's

         set s = `echo $s | sed -e 's/^00\([0-9]\)/\1/' -e 's/^0\([1-9][0-9]*\)/\1/'`
         set d = `echo $d | sed -e 's/^000\(.\)/\1/' -e 's/^00\(..\)/\1/' -e 's/^0\(...\)/\1/'`

         ## old method of getting it from label
         ## $R2LIB/getlab $f -prop itm_task=identification lab_item=ROVER_MOTION_COUNTER element=1 -int
         ## set s = `v2param itm_name`
         ## $R2LIB/getlab $f -prop itm_task=identification lab_item=ROVER_MOTION_COUNTER element=2 -int
         ## set d = `v2param itm_name`

         break
      endif
   end

   # Add it to the list (or "no" for not found)

   set mcc_list = ($mcc_list $mcc_name)
   set pmc_list = ($pmc_list $pmc)
   set site = ($site $s)
   set drive = ($drive $d)

   MATH index = $index+2
   MATH pmc_count = $pmc_count+1
end

# We now have a list of PMC values and the MCC image that goes with each.
# Now we can loop through the entire file and write out the JSON.  Skip the
# first line, it's the header.

rm -f $out

echo '[' >>$out

set lnum = 1
set first = 0
foreach text ("`tail -n +2 $rxl`")
   if ("$text" == "") continue

   if ($lnum % 10 == 0) then
      echo "Line $lnum"
   endif
   MATH lnum = $lnum+1

   set my_pmc = `echo $text | cut -d, -f1`

   # Loop through the i,j fields

   set i = 1
   set field = 6

   while ($i <= $pmc_count)

      # Skip if no entry

      if ("$mcc_list[$i]" == "no") then
         MATH i = $i+1
         MATH field = $field+2
         continue
      endif

      # Extract line and sample

      set samp = "`echo $text | cut -d, -f${field}`"
      MATH f2 = $field+1
      set line = "`echo $text | cut -d, -f${f2}`"

      # Convert to one-based.  The source data defines 0,0 as the
      # upper left corner of the upper left pixel, so adding 0.5 makes
      # that the proper 1,1 at the center of the UL pixel.

      MATH line = $line+0.5
      MATH samp = $samp+0.5

      # Write out the JSON

      if ($first != 0) echo '  },' >>$out
      set first = 1

      echo '  {' >>$out
      echo '    "context_filename": "'${mcc_list[$i]}'",' >>$out
      echo '    "line": '$line',' >>$out
      echo '    "sample": '$samp',' >>$out
      echo '    "site_id": '${site[$i]}',' >>$out
      echo '    "drive_id": '${drive[$i]}',' >>$out
      echo '    "spectral_filename": "'$rfs'",' >>$out
      echo '    "spectrum_number": '$my_pmc >>$out

      MATH i = $i+1
      MATH field = $field+2

      #### !!!!
      #### IF you want more than one image entry, remove the statement below
      #### CRUST seems to support multiple images...
      #### !!!!
####      MATH i = $pmc_count+1

   end
end

if ($first != 0) echo '  }' >>$out
echo ']' >>$out

echo "COMPLETE"

