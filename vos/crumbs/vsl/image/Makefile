#CC = g++

INCL = -I../../$(LIB_DIR)/include -I../.. \
	-I/usr/include/CC

CCFLAGS = $(OPT32) -DFUNCPROTO $(INCL) $(FLAGS)

LIB = ../../../$(LIB_DIR)/lib/$(ARCH)/libimage.a

SRCS =  image.C imagedata.C newdata.C imagedisp.C datatypes.C filetypes.C \
	disptypes.C imagefile.C asvcfile.C datamod.C datammap.C datads.C \
	pgmfile.C ppmfile.C modfile.C vicarfile.C rlafile.C dsfile.C \
	pdsfile.C kapfile.C geodata.C kap_geodata.C pds_geodata.C \
	map_geodata.C ranfile.C rgbfile.C camera_geodata.C

OBJS =  $(SRCS:.C=.o)


$(LIB): $(OBJS) 
	(cd ./$(VICCPU); ar rc $(LIB) $(OBJS))

.C.o:
	$(CC) -c $(CCFLAGS) $*.C -o ./$(VICCPU)/$*.o

pdsfile.o: pdsfile.C 
	$(CC) -c $(CCARCH_FLAGS) $(CCFLAGS) pdsfile.C -o ./$(VICCPU)/pdsfile.o
vicarfile.o: vicarfile.C 
	$(CC) -c $(CCARCH_FLAGS) $(CCFLAGS) vicarfile.C -o ./$(VICCPU)/vicarfile.o

man: image.z geodata.z

image.z: image.man
	rm -f image.z
	highlight <image.man >image
	pack image
	cp image.z /ware/proj/man/man1/.
	
geodata.z: geodata.man
	rm -f geodata.z
	highlight <geodata.man >geodata
	pack geodata
	cp geodata.z /ware/proj/man/man1/.

clean:
	\rm -f $(LIB) $(OBJS)
	make

depend:
	makedepend $(CCFLAGS) $(GCCINC) $(SRCS)
