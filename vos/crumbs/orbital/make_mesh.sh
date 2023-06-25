#! /bin/csh  -f
#Inputs:
set dem = $1
set skin = $2
set dem_scale = $3 
set dem_offset = $4
set out_dir = $5
set dem_size = $8
set skin_size = $9
set dem_enhance_scale = $10

#setenv CRUMBS_HOME /home/mslopgs/users/ozp/orbital_test/crumbs/
#setenv CRUMBS_BIN /home/mslopgs/tools/bin/
echo CRUMBS_HOME=$CRUMBS_HOME
echo CRUMBS_BIN=$CRUMBS_BIN

# Rover Position + Origin_Offset_Vector.X
set origin_line = $6
# Rover Position - Origin_Offset_Vector.Y
#!!!!ozp
set origin_sample = $7
echo $origin_sample
#set mesh_name = "ls $2 | sed 's/\(.*\)\..*/\1/'r'"
#set mesh_name = `echo $2 | basename | sed 's/\.VIC$//g'`
#set mesh_name = `basename $1 .VIC`
switch ($11)
  case dem:
    set mesh_name = `basename $1 .VIC`
    echo $mesh_name
    breaksw
  case skin:
    set mesh_name = `basename $2 .VIC`
    breaksw
  default:
    set mesh_name = `basename $2 .VIC`
endsw
set skin_name = `basename $2 .VIC`

echo $mesh_name
echo $skin_name

echo $1
echo $2
echo $3
echo $4
echo $5
echo $6
echo $7
echo $8

echo $mesh_name

# Set MATH alias - takes an arithmetic assignment statement 
# as argument, e.g., newvar = var1 + var2 
# Separate all items and operators in the expression with blanks 
alias MATH 'set \!:1 = `echo "\!:3-$" | bc -l`'
MATH cam_pos_x = (($dem_scale*($origin_line-1) + $dem_scale * ($origin_line - $dem_size)) / 2.0) 
MATH skin_size = $skin_size+1
#!!!!ozp
MATH cam_pos_y = (($dem_scale*(1 - ($origin_sample)) + $dem_scale * ($dem_size -($origin_sample))) / 2.0)
echo "cam_pos_x:"
echo $cam_pos_x
echo "cam_pos_y:"
echo $cam_pos_y
echo "origin_line:"
echo $origin_line
echo "origin_sample:"
echo $origin_sample
set cam_pos_z = '-10000'
#MATH xfov = (360.0 / 3.14159 * a(($dem_scale * ($dem_size / 2)) / $cam_pos_z))
# for whatever reason we are getting the negative of the value
MATH xfov = (-360.0 / 3.141592653589793 * a(($dem_scale * (($dem_size-0.7499997065) / 2)) / $cam_pos_z))
echo "xfov"
echo $xfov

# apply scale and offset
#set dem_name = "ls $dem | sed 's/\(.*\)\..*/\1/'"
#java -Xmx1024M jpl.mipl.io.jConvertIIO inp=$dem out=$out_dir/../dem_tmp.vic format=vicar oform=float embed_vicar_label=true ri=true
#$P2LIB/f2 $out_dir/../dem_tmp.vic $out_dir/../dem.vic func='"'$dem_scale'*IN1+'$dem_offset'"'
#$P2LIB/f2 $dem $out_dir/../dem.vic func='"-1*IN1 +('$dem_offset')"'
# !!!!ozp exagerrated one
$P2LIB/f2 $dem $out_dir/../dem.vic func='"(-1*IN1 +('$dem_offset'))*'$dem_enhance_scale'"'
#!!!! zero it out for testing with 2D mesh
#$P2LIB/f2 $dem $out_dir/../dem.vic func='"(0*IN1)"'
echo "make_mesh.dem_offset=$dem_offset"

#create xyz
$P2LIB/f2 out=$out_dir/../x.vic ns=$dem_size nl=$dem_size func='"'$dem_scale'*('$origin_line'-LINE)"' -real
$P2LIB/f2 out=$out_dir/../y.vic ns=$dem_size nl=$dem_size func='"'$dem_scale'*(SAMPLE-'$origin_sample')"' -real
#$P2LIB/viccub \($out_dir/../x.vic, $out_dir/../y.vic, $out_dir/../dem.vic\) $out_dir/../xyz.vic
$P2LIB/viccub \($out_dir/../x.vic, $out_dir/../y.vic, $out_dir/../dem.vic\) $out_dir/../$mesh_name.xyz.VIC
#transcode $out_dir/../$mesh_name.xyz.VIC
#transcode $skin
#transcode $dem

#create camera model
#For the camera model, X=east, Y=north, Z=down.
#put the camera looking along +Z
#with north = up:
$V2EXT/cmodgen/x86-linux/bin/cmodgen cahvor $cam_pos_x,$cam_pos_y,$cam_pos_z 0,0,1 1,0,0 $skin_size,$skin_size $xfov,$xfov $out_dir/../$mesh_name.cahvor

#run mesh on mipl Linux machine

mkdir -p $out_dir
#$PGS_BIN_DIR/do_wedge -res_max 40000000 -res_min 40000 -lod_scale 40000 -tileres 200000 -rng_max 1500000 -htmap_xres $dem_size -key "" -cmod $out_dir/../$mesh_name.cahvor -oss $out_dir $out_dir/../xyz.IMG $out_dir/../$skin_name.IMG $mesh_name 1
#!!!!ozp $PGS_BIN_DIR/do_wedge -xyz_gaps 0 -res_max 100000 -res_min 100000 -lod_scale 100000 -tileres 2000000000 -rng_max 1500000 -htmap_xres $dem_size -key "" -cmod $out_dir/../$mesh_name.cahvor -oss $out_dir $out_dir/../$mesh_name.xyz.IMG $out_dir/../$skin_name.IMG $mesh_name 1
$CRUMBS_BIN/do_wedge -xyz_gaps 0 -res_max 100000 -res_min 100000 -lod_scale 100000 -tileres 2000000000 -rng_max 1500000 -htmap_xres $dem_size -key "" -cmod $out_dir/../$mesh_name.cahvor -oss $out_dir $out_dir/../$mesh_name.xyz.VIC $out_dir/../$skin_name.VIC $mesh_name 1
#$PGS_BIN_DIR/do_wedge -res_max 7000 -res_min 7000 -lod_scale 7000 -rng_max 15000000 -htmap_xres $dem_size -key "" -cmod $out_dir/../$mesh_name.cahvor -oss $out_dir $out_dir/../xyz.IMG $out_dir/../$skin_name.IMG $mesh_name 1
echo $skin_name.IMG
echo $out_dir/../$skin_name.IMG
