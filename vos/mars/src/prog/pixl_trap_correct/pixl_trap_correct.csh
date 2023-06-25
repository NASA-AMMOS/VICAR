#!/bin/csh
#
# Run trapezoidal correction for a PIXL image.  The PIXL MCC is mounted
# at an 18 degree angle to the XRay boresight (although in actuality
# 21.75 degrees works better).  Trapezoid correction uses a surface model
# derived from the PIXL SLI (Structured Light) system to reproject the image
# to the point of view of the X-ray.
#
# Usage:
#   pixl_trap_correct fdr rxt [ obj ]
#
# Where fdr is the FDR (or RAS/RAD/RAF) to correct, rxt is the name of the
# RXT.LBL file containing the plane definition, and obj is the optional mesh
# name.  If obj is given, mesh mode is used... if not, surface normal (plane)
# mode.
#
# For matching, given an MCC look for RXT that matches:
# PE.poooo..........._...RXT_Nsssddddrrrrrrrrr.......J..\.LBL
# where p is the special product flag, oooo is the sol sss is the site, dddd
# is the drive, and rrrrrrrrr is the RTT value
#
# Or given an RXT loof for matching images:
# PC.poooo..........._...FDR_Nsssddddrrrrrrrrr.......J..\.VIC
#
# The obj file is the one created by the RXT process; it has the same name
# as the RXT LBL file itself.  Note that the RXT is ignored if OBJ is given.
#
if ($# != 2 && $# != 3) then
   head -27 $0
   exit
endif

set fdr = $1
set rxt = $2

set mode = plane
set obj = ""
if ($# == 3) then
   set mode = mesh
   set obj = $3
endif

alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'

set out = `echo ${fdr:t:r} | sed -e s/FDR./FDRT/ -e s/RAS./RAST/ -e s/RAD./RADT/ -e s/RAF./RAFT/`.VIC
# ICM name strips out last 6 chars for PIXL
set strip = `echo ${fdr:t:r} | sed -e 's/.....$//'`
set strip2 = `echo ${out:t:r} | sed -e 's/.....$//'`
set icm_fwd = "ICM-${strip}-${strip2}-J00.VIC"
set icm_rev = "ICM-${strip2}-${strip}-J00.VIC"


# Figure out the surface model stuff

if ("$mode" == "mesh") then
   set surf = "-mesh surf_mesh=$obj surf_coord=pixl_base"
else
   # Default surface, with 25.5 mm standoff
   # Default is a point 25.5mm in front of the faceplate, aligned with the xray
   set ground = "(-0.136 0.131 0.2482)"
   set normal = "(0 0 -1)"
   # Extract from the ODL label
   set line = `grep SURFACE_GROUND_LOCATION $rxt`
   if ("$line" != "") then
      set ground = `echo "$line" | sed -e 's/^.*=//'`
   endif
   set line = `grep SURFACE_NORMAL_VECTOR $rxt`
   if ("$line" != "") then
      set normal = `echo "$line" | sed -e 's/^.*=//'`
   endif

   set surf = "ground=$ground normal=$normal surf_coord=pixl_base"
endif

# Run the trapezoid correction.  The ICM output is the reverse ICM which
# maps from trap back to input.
# The output pos magic number is the coordinate of the virtual output camera
# measured in the sensor frame.  This is the center of the Xray at the
# faceplate.

echo surf params = $surf
echo $MARSLIB/marsmos inp=$fdr out=$out "$surf" -pixl_sensor -use_alt alt_axis=\(1 0 0\) alt_angle=21.75 -norad icm=$icm_rev point=cm=label maxnl=9000 maxns=9000
$MARSLIB/marsmos inp=$fdr out=$out "$surf" -pixl_sensor -use_alt alt_axis=\(1 0 0\) alt_angle=21.75 -norad icm=$icm_rev point=cm=label maxnl=9000 maxns=9000

# Fix the labels

$R2LIB/label -add $out -prop property=identification item='"GEOMETRY_PROJECTION_TYPE=TRAPEZOID"'

# Fix the labels in the reverse ICM.  Because every pixel "correlated",
# just get the count of pixels and divide by the total number of pixels.

$R2LIB/hist $icm_rev -nohist -exclude nb=1 sb=1
set count = `v2param COUNT`
$R2LIB/getlab $icm_rev -system -int lab_item=NL
set nl = `v2param itm_name`
$R2LIB/getlab $icm_rev -system -int lab_item=NS
set ns = `v2param itm_name`

MATH pct = $count/($nl*$ns)*100

echo pct=$pct count=$count nl=$nl ns=$ns

$R2LIB/label -add $icm_rev -prop property=DERIVED_IMAGE_PARMS item='"CORRELATION_PIXEL_COUNT='$count' CORRELATION_AVERAGE_SCALE=1.0 CORRELATION_OVERLAP_PERCENTAGE='$pct' STEREO_PRODUCT_ID='${fdr:t:r}'"'

# Now use dispinvert to make the forward ICM.

echo $MARSLIB/marsdispinvert inp=\( $fdr $out \) out=$icm_fwd.tmp in_disp=$icm_rev point=cm=label
$MARSLIB/marsdispinvert inp=\( $fdr $out \) out=$icm_fwd.tmp in_disp=$icm_rev point=cm=label

# And self-correlate to get subpixel accuracy

echo $MARSLIB/marscor3 inp=\( $fdr $out \) out=$icm_fwd in_disp=$icm_fwd.tmp band=2 templ=9 search=15 q=0.8 -gores gore_q=0.8 gore_p=3 -gore_rev -amoeba8 ftol=.001 -multi -gauss
$MARSLIB/marscor3 inp=\( $fdr $out \) out=$icm_fwd in_disp=$icm_fwd.tmp band=2 templ=9 search=15 q=0.8 -gores gore_q=0.8 gore_p=3 -gore_rev -amoeba8 ftol=.001 -multi -gauss
rm $icm_fwd.tmp

# Finally, compress the ICMs

mv ${icm_fwd} ${icm_fwd}.temp
$R2LIB/comprs ${icm_fwd}.temp ${icm_fwd} -basic2
rm ${icm_fwd}.temp

mv ${icm_rev} ${icm_rev}.temp
$R2LIB/comprs ${icm_rev}.temp ${icm_rev} -basic2
rm ${icm_rev}.temp

