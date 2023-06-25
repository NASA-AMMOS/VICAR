#!/bin/csh
#
# Extract pointing info from a SuperCam spectral label file, run marsshot
# on it to get the location in the RMI, and create a file for CRUST upload.
#
# Usage:
#    crust_scam_ingest spec.lis out.json rmiimg1 [rmiimg2] [nav]
#
# where spec.lis is the label file for the spectral observation, out.json is
# the output JSON file for CRUST upload, rmiimg1 is the name of the first
# RMI image, and rmiimg2 is the name of the (optional) second RMI image.
# nav is an optional nav file, used only if 2 RMI's are given.  If not provided,
# nav will be determined using marsautonav if possible unless the nav
# parameter is -nonav .
#
setenv V2PARAM_FILE "/tmp/vicar_param_${USER}$$.temp"

if ($# < 3 || $# > 5) then
   head -14 $0
   exit
endif

alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'

set lis = $1
set out = $2
set rmi1 = $3
set rmi2 = ""
if ($# > 3) set rmi2 = $4
set nav = ""
if ($# > 4) set nav = $5

# If we have two RMIs but no nav file, make one

set tmpnav = 0
set tmp = ""
if ("$rmi2" != "" && "$nav" == "") then
   set tmp = /tmp/scam_crust_$$
   # Large FOV angle because of the weird RMI A offpoint
   $MARSLIB/marsautotie \( $rmi1 $rmi2 \) ${tmp}.tie density=50 busy=20 -inf band=2 fov=50 point=cm=label
   $MARSLIB/marsnav \( $rmi1 $rmi2 \) ${tmp}.nav ${tmp}.tie out_sol=crust ref=1 inertia=\(.1 .1 .1\) -inf
   if ($? == 1) then
      set nav = ${tmp}.nav
   else
      echo "MARSNAV failed, continuing without nav..."
      set nav = ""
   endif
   rm ${tmp}.tie
   set tmpnav = 1
endif
if ("$nav" == "-nonav") set nav = ""

# Extract site and drive from RMI

$R2LIB/getlab $rmi1 -prop itm_task=identification lab_item=ROVER_MOTION_COUNTER element=1 -int
set site = `v2param itm_name`
$R2LIB/getlab $rmi1 -prop itm_task=identification lab_item=ROVER_MOTION_COUNTER element=2 -int
set drive = `v2param itm_name`

echo site = $site drive = $drive

# Set up the output file

rm -f $out
echo '[' >> $out
set first = 0

# Loop through the inputs and do the work

foreach file ("`cat $lis`")

   if ("$file" == "") continue

   # Extract the pointing from the label

   set line = `strings $file | grep -A10 '^GROUP.*=.*RSM_ARTICULATION_STATE' | grep 'ARTICULATION_DEVICE_ANGLE[^_]' | sed -e 's/^.*(//' -e 's/<rad>//'g`

   set az_rad = `echo $line | cut -d, -f 1`
   set el_rad = `echo $line | cut -d, -f 2`

   echo az = $az_rad
   echo el = $el_rad

   MATH az_deg = ${az_rad}*180/3.1415926535
   MATH el_deg = ${el_rad}*180/3.1415926535

   echo az_deg = $az_deg
   echo el_deg = $el_deg

   # Run the program... since the hotspot is the same for all the SCAM
   # instruments we don't bother distinguishing instruments.

   if ($rmi2 == "") then
      $MARSLIB/marsshot $rmi1 -scam-libs pointing=\($az_deg $el_deg\)
   else
      set navx = ""
      if ("$nav" != "") set navx = "nav=${nav}"
      $MARSLIB/marsshot \( $rmi1 $rmi2 \) -scam-libs pointing=\($az_deg $el_deg\) -interp $navx -tight
   endif

   set line = `v2param -i 0 image_pos`
   set samp = `v2param -i 1 image_pos`

   if ($first != 0) echo '  },' >>$out
   set first = 1

   # Note that for RMI the (non-image) data file is always .fits

   echo '  {' >>$out
   echo '    "context_filename": "'${rmi1:t}'",' >>$out
   echo '    "line": '$line',' >>$out
   echo '    "sample": '$samp',' >>$out
   echo '    "site_id": '${site}',' >>$out
   echo '    "drive_id": '${drive}',' >>$out
   echo '    "spectral_filename": "'${file:t:r}.fits'",' >>$out
   echo '    "spectrum_number": 1' >>$out

end

if ($first != 0) echo '  }' >>$out
echo ']' >>$out

if ($tmpnav == 1) rm ${tmp}.nav

echo "COMPLETE"

