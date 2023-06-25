# arch-specific: $MOTIF_OBJS

INCL = -I../../$(LIB_DIR)/include -I/usr/include/X11 -I/usr/include/CC 

#CCFLAGS = -g -DFUNCPROTO $(INCL) -I$(PVM_ROOT)/include -I/usr/include/CC -D$(ARCH) $(FLAGS)
CCFLAGS = $(OPT32) -g -DFUNCPROTO $(INCL) -I/usr/include/CC $(FLAGS)

#LDFLAGS = -L. -L../../$(LIB_DIR)/lib/$(ARCH) -L/usr/X11R5/lib -L$(PVM_ROOT)/lib/$(PVM_ARCH)
LDFLAGS = -L. -L../../$(LIB_DIR)/lib/$(ARCH) -L/usr/X11R5/lib

LIB = ../../../$(LIB_DIR)/lib/$(ARCH)/libpath.a

# removed ParEdit.o and pathedit.o, needs Motif/X
OBJS = path.o polypath.o spline.o wvspline.o survspline.o linearspline.o \
	$(MOTIF_OBJS)
SRCS = $(OBJS:.o=.C)

all: $(LIB)

$(LIB): $(OBJS)
	(cd ./$(VICCPU);ar rc $(LIB) $(OBJS))

.C.o:
	$(CC) -c $(CCFLAGS) $*.C -o ./$(VICCPU)/$*.o

path_tests: path_tests.o $(LIB)
	#$(CC) -o path_tests path_tests.o $(LDFLAGS) -lpath -lvsl -l_network -lvsl -lpvm3 -lm
	$(CC) -o path_tests path_tests.o $(LDFLAGS) -lpath -lvsl -l_network -lvsl -lm

path_tests.o: path_tests.C
	$(CC) -c $(CCFLAGS) path_tests.C

clean:
	\rm -f $(LIB) $(OBJS) test test.o
	make

depend: 
	makedepend $(CCFLAGS) $(GCCINC) $(SRCS)
