# %M% %I% %E% %U%
# Build rules for SUMMITT programs

DEPENDFILE = ./depends

# system-specific macros: LIB
VISION = ../../../litwin

LIB =  -lgrape -lpath -limage -lvsl -lds \
        -L$(PDS_LIBRARY)/src/lib -loal_others_c \
        -L$(VISION)/lib/$(ARCH) -lcahvor \
        -L$(V2OLB) -lp2sub -lrtl \
        -L/usr/X11R6/lib -lX11 -lm


DEV_LIB_DIR = ../../../$(LIB_DIR)/lib/$(ARCH)
INCL = -I../../../$(LIB_DIR)/include \
       -I$(VISION)/include -I../../.. \
       -I$(P2INC) \
       -I$(V2INC) \
       -I$(COIN_ROOT)/include \
       -I$(PDS_LIBRARY)/src/lablib3 -I$(PDS_LIBRARY)/src/oal

VINC = -I/ware/proj/etc/vtk2.0/common \
	-I/ware/proj/etc/vtk2.0/graphics \
	-I/ware/proj/etc/vtk2.0/imaging \
	-I/ware/proj/etc/vtk2.0/patented

ifdef V2_COVERAGE
CC_COV_FLAGS = -fprofile-arcs -ftest-coverage
LD_COV_FLAGS = -fprofile-generate
else
CC_COV_FLAGS =
LD_COV_FLAGS =
endif

LDFLAGS = -L$(DEV_LIB_DIR) $(LD_COV_FLAGS)

CCFLAGS = $(OPT32) $(INCL) $(FLAGS) $(CC_COV_FLAGS)

VLIB = -L/ware/proj/etc/vtk2.0/lib/$(ARCH) \
	-lVTKImaging -lVTKGraphics -lVTKPatented -lVTKGraphics -lVTKCommon \
	-lGL -lXm -lXt -lXmu -lXi -lXext

# MIPL pipeline and support
PROGS = tmerge tmslave fstfilter edit_forest fst2img fstquad \
	octreeextents tilemesh vstcover tile_cat

# test programs
TPROGS = accum_tree asd2vst asdmesh clip_octree cm2pnt \
	fixmesh img2oct make_tris mdl2iv \
	multitex obj2oct octreestats \
	pnt2oct ranclip randump ranfilter range2oct range2xyz \
	sfc2vst summitt_match summitt_merge xcmod geopt

# support functions
SLIB = summitt_func.o patch.o forest.o 
SLIB_X = ./$(VICCPU)/summitt_func.o ./$(VICCPU)/patch.o ./$(VICCPU)/forest.o 
OBJ = $(SLIB) config.o marchtri.o mmerge.o ranfltr.o rng2oct.o s2vst.o \
	tm_msg.o vista.o vststrip.o

# test/old programs
XPROGS = find_test findmaxz img2vtk objgrp scale_octree \
	scale_match vtkcubeoctree

# all source files, for make depend
SRC = $(PROGS:=.C) $(TPROGS:=.C) $(XPROGS:=.C) $(OBJ:.o=.C) 

depend:
	$(CC) -M $(CCFLAGS) $(SRC) > $(DEPENDFILE)
ifeq ($(DEPENDFILE),$(wildcard $(DEPENDFILE)))
include $(DEPENDFILE)
endif

# default target = MIPL pipeline programs
progs:	$(PROGS) tmsa

all:	$(PROGS) tmsa $(TPROGS)

.C.o:
	$(CC) -c $(CCFLAGS) $*.C -o ./$(VICCPU)/$*.o

# temp
t1: t1.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)
t2: t2.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)
t3: t3.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

accum_tree: accum_tree.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)
	
asdmesh: asdmesh.o cmpoint.o $(SLIB) 
	$(CC) $(LDFLAGS) -o $@ asdmesh.o cmpoint.o $(SLIB_X) $(LIB)
	
asd2vst: asd2vst.o vista.o vststrip.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/asd2vst.o ./$(VICCPU)/vista.o ./$(VICCPU)/vststrip.o $(LIB)
vista.o: vista.C
	$(CC) -c $(CCFLAGS) -o ./$(VICCPU)/$@ $*.C

clip_octree: clip_octree.o $(SLIB)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/clip_octree.o $(SLIB_X) $(LIB)

cm2pnt: cm2pnt.o cmpoint.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/cm2pnt.o ./$(VICCPU)/cmpoint.o $(LIB)

cmptest: cmptest.o cmpoint.o $(SLIB)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/cmptest.o ./$(VICCPU)/cmpoint.o $(SLIB_X) $(LIB)

edgetest: edgetest.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

edit_forest: edit_forest.o cmpoint.o $(SLIB)
	$(CC) $(LDFLAGS) -o $@ edit_forest.o cmpoint.o $(SLIB_X) $(LIB)

find_test: find_test.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

findmaxz:  findmaxz.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

fixmesh: fixmesh.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

fst2img: fst2img.o interp_map.o $(SLIB)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/fst2img.o ./$(VICCPU)/interp_map.o $(SLIB_X) $(LIB)

fstfilter: fstfilter.o $(SLIB)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/fstfilter.o $(TCLLIB) $(SLIB_X) $(LIB)
fstfilter.o: fstfilter.C
	$(CC) -c $(CCFLAGS) $(TCLINC) $*.C

tile_cat: tile_cat.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/tile_cat.o $(LIB) -L$(COIN_ROOT)/lib -lCoin

fstquad: fstquad.o $(SLIB)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/fstquad.o $(SLIB_X) $(LIB)

geopt: geopt.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

ht2iv: ht2iv.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

img2oct: img2oct.o $(SLIB)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/img2oct.o $(SLIB_X) $(LIB)

img2vtk: img2vtk.o
	$(CC) $(LDFLAGS) -o $@ $(CCFLAGS) $? $(VLIB) $(LIB)
img2vtk.o: img2vtk.C
	$(CC) -c $(CCFLAGS) $(VINC) $*.C

make_tris: make_tris.o marchtri.o $(SLIB)
	$(CC) $(LDFLAGS) -o $@ make_tris.o marchtri.o $(SLIB_X) $(LIB)

mdl2iv: mdl2iv.o $(SLIB)
	$(CC) $(LDFLAGS) -o $@ mdl2iv.o $(SLIB_X) $(LIB)

mdlsize: mdlsize.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

meshstat: meshstat.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

mquad: mquad.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

multitex: multitex.o $(SLIB)
	$(CC) $(LDFLAGS) -o $@ multitex.o $(SLIB_X) $(LIB)

obj2oct: obj2oct.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

objgrp: objgrp.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

octcube: octcube.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

octreeextents: octreeextents.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

octreestats: octreestats.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

pnt2oct: pnt2oct.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

ranfilter: ranfilter.o ranfltr.o
	$(CC) $(LDFLAGS) -o $@ ranfilter.o ranfltr.o $(LIB)

ran2asd: ran2asd.o cmpoint.o
	$(CC) $(LDFLAGS) -o $@ ran2asd.o cmpoint.o $(LIB) 

range2oct: range2oct.o rng2oct.o
	$(CC) $(LDFLAGS) -o $@ range2oct.o rng2oct.o $(LIB)

range2xyz: range2xyz.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

ranclip: ranclip.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

randump: randump.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

ranscale: ranscale.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

rxft: rxft.o
	$(CC) $(LDFLAGS) -o $@ $? $(SLIB_X) $(LIB)

scale_octree: scale_octree.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

scale_match: scale_match.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

sfc2vst: sfc2vst.o s2vst.o vista.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/sfc2vst.o ./$(VICCPU)/s2vst.o ./$(VICCPU)/vista.o $(LIB)

summitt_match: summitt_match.o $(SLIB) mmerge.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/summitt_match.o $(SLIB_X) ./$(VICCPU)/mmerge.o $(LIB)

summitt_merge: summitt_merge.o $(SLIB)
	$(CC) $(LDFLAGS) -o $@ summitt_merge.o $(SLIB_X) $(LIB)

TMSOBJ = tilemesh.o tileout.o tileout_obj.o marchtri.o ranfltr.o rng2oct.o $(SLIB)
TMSOBJ_X = ./$(VICCPU)/tilemesh.o ./$(VICCPU)/tileout.o ./$(VICCPU)/tileout_obj.o ./$(VICCPU)/marchtri.o ./$(VICCPU)/ranfltr.o ./$(VICCPU)/rng2oct.o $(SLIB_X)
tilemesh: $(TMSOBJ)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ $(TMSOBJ_X) $(LIB) 

TMOBJ = tmerge.o tmproc.o tm_msg.o tmshare.o config.o cmpoint.o $(SLIB)
tmerge: $(TMOBJ)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ $(TMOBJ) $(LIB) 

tmpslave: tmpslave.o $(OBJ)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/tmpslave.o $(OBJ) $(LIB) 

TSOBJ = tmmatch.o tmshare.o cmpoint.o mmerge.o ranfltr.o rng2oct.o \
	s2vst.o terrquad.o vista.o vststrip.o $(SLIB)
TSOBJ_X = ./$(VICCPU)/tmmatch.o ./$(VICCPU)/tmshare.o ./$(VICCPU)/cmpoint.o ./$(VICCPU)/mmerge.o ./$(VICCPU)/ranfltr.o ./$(VICCPU)/rng2oct.o \
	./$(VICCPU)/s2vst.o ./$(VICCPU)/terrquad.o ./$(VICCPU)/vista.o ./$(VICCPU)/vststrip.o $(SLIB_X)
tmslave: tmslave.o $(TSOBJ) ./$(VICCPU)/tm_msg.o 
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/tmslave.o $(TSOBJ) ./$(VICCPU)/tm_msg.o $(LIB)
tmsa: tmsa.o $(TSOBJ)
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/tmsa.o $(TSOBJ_X) $(LIB)
tmsa.o: tmslave.C
	$(CC) -c $(CCFLAGS) -DSTANDALONE -o ./$(VICCPU)/tmsa.o tmslave.C

vstdump: vstdump.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)
vstcover: vstcover.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIB)

vtkcubeoctree: vtkcubeoctree.o $(SLIB)
	$(CC) $(LDFLAGS) -o $@ $(CCFLAGS) vtkcubeoctree.o $(SLIB) $(VLIB) $(LIB)
vtkcubeoctree.o: vtkcubeoctree.C
	$(CC) -c $(CCFLAGS) $(VINC) $*.C

xcmod: xcmod.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ ./$(VICCPU)/xcmod.o $(LIB)

xyz2range: xyz2range.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ $? $(LIB)
