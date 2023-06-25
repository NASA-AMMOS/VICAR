CC = g++
INCL = -I../../$(LIB_DIR)/include

#
# Note: these new optional #define-able conditionally-compilable settings
#       are for Dave Kagels' Image Class libraries (for more info, go
#       to the image lib's directory).
#
#        _NO_DATASERVER_ will not use Bill Lincoln's libds (DataServer)
#        _NO_IMAGEDISP_ will not use Dave Kagels' ImageDisp Objects
#
# Note2: if you are on a machine that can get to the Iris Performer libraries
#        and you are going to use the PerfRenderer (Iris Performer
#        encapsulation for GRAPE), you will need to include the following
#        define:
#
#        -D_USE_PERFORMER_
#
# Note3: libgrape_hs.a includes support for hyperspectral octree voxels.
#	 The default library doesn't, but does have fixed-length nodes.
#
# Note4: To include support for PVM_Dataports, make sure the vsl/ and
#	net/ libraries have PVM support, then include
#
#	-I$(PVM_ROOT)/include -DPVM_DATAPORT

#CCFLAGS = -DFUNCPROTO $(INCL) -I$(PVM_ROOT)/include -D$(ARCH) $(FLAGS) \
#		-D_NO_DATASERVER_ -D_NO_IMAGEDISP_ -D_USE_PERFORMER_

# enable openGL?
GLFLAG = 
#GLFLAG = -DDO_GL

# X11 is for mac
CCFLAGS = $(OPT32) -DFUNCPROTO $(INCL) $(FLAGS) -I/opt/X11/include

LIB = ../../../$(LIB_DIR)/lib/$(ARCH)/libgrape.a
HSLIB = ../../../$(LIB_DIR)/lib/$(ARCH)/libgrape_hs.a

OBJS =  poly_object_1.o object.o scene.o camera.o parameter.o \
	img_info.o data.o clock.o d_envelope.o glrend.o matrix.o \
	distribute.o wave_object_1.o animate.o playback.o renderer.o metarender.o \
	hazerend.o hazeinfo.o oglrend.o light.o color.o texture.o surface.o \
	perf_object.o perf_renderer.o vector_ops.o \
	aejError.o aejMatrixAlgo.o aejTransformAlgo.o aejMatrix.o \
	aejTransform.o

# RGB-only
RGBOBJ = octree.o object_creator.o

# hyperspectral versions (compile-time selection)
HSOBJ = octree_hs.o object_creator_hs.o

SRC = $(OBJS:.o=.C) $(RGBOBJ:.o=.C)

all: $(LIB)

$(LIB): $(OBJS) $(RGBOBJ)
	(cd ./$(VICCPU);ar rc $(LIB) $(OBJS) $(RGBOBJ))

$(HSLIB): $(OBJS) $(HSOBJ)
	(cd ./$(VICCPU);ar rc $(HSLIB) $(OBJS) $(HSOBJ))

.C.o:
	$(CC) -c $(CCFLAGS) $*.C -o ./$(VICCPU)/$*.o

img_info.o: img_info.C 
	$(CC) -c $(CCFLAGS) $(GL_FLAG) img_info.C -o ./$(VICCPU)/img_info.o

glrend.o: glrend.C
	$(CC) -c $(CCFLAGS) $(GL_FLAG) glrend.C -o ./$(VICCPU)/glrend.o

octree_hs.o: octree.C ../../development/include/grape/octree.h
	$(CC)  -c $(CCFLAGS) -DFLEXCOLOR -o ./$(VICCPU)/octree_hs.o octree.C 

object_creator_hs.o: object_creator.C ../../development/include/grape/octree.h
	$(CC)  -c $(CCFLAGS) -DFLEXCOLOR -o ./$(VICCPU)/object_creator_hs.o object_creator.C

clean:
	\rm -f $(LIB) $(OBJS)
	make

depend:
	makedepend  $(CCFLAGS) $(GCCINC) $(SRC)
