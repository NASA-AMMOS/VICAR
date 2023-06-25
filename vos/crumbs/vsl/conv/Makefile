VISION = ../../litwin

LIBS =  -limage -lvsl -L$(PDS_LIBRARY)/src/lib -loal_others_c \
        -L$(VISION)/lib/$(ARCH) -lds -lcahvor \
        -L$(V2OLB) -lp2sub \
        -L/usr/X11R6/lib -L/opt/X11/lib -lX11 -lm

INCL = ../../$(LIB_DIR)/include \
       -I$(P2INC) \
       -I$(PDS_LIBRARY)/src/lablib3 -I$(PDS_LIBRARY)/src/oal \
	-I../..

ifdef V2_COVERAGE
CC_COV_FLAGS = -fprofile-arcs -ftest-coverage
LD_COV_FLAGS = -fprofile-generate
else
CC_COV_FLAGS =
LD_COV_FLAGS =
endif

CCFLAGS = $(OPT32) -DFUNCPROTO -I$(INCL) $(FLAGS) $(CC_COV_FLAGS)

LDFLAGS = -L../../$(LIB_DIR)/lib/$(ARCH) $(CCFLAGS) $(LD_COV_FLAGS)

SRCS = convert.C imap.C tile.C imgstat.C diffstat.C imgval.C
OBJS = $(SRCS:.C=.o)

#PROG = convert imap tile imgstat img16to8
PROG = convert imgstat img16to8

all: $(PROG)

.C.o:
	$(CC) -c $(CCFLAGS) $*.C -o ./$(VICCPU)/$*.o

convert: convert.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/convert ./$(VICCPU)/convert.o $(LIBS)

imap: imap.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/imap ./$(VICCPU)/imap.o $(LIBS)

tile: tile.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/tile ./$(VICCPU)/tile.o $(LIBS)

imgstat: imgstat.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/imgstat ./$(VICCPU)/imgstat.o $(LIBS)

img16to8: img16to8.o
	$(CC) $(LDFLAGS) -o ./$(VICCPU)/$@ $(CCFLAGS) ./$(VICCPU)/$? $(LIBS)

diffstat: diffstat.o
	$(CC) $(LDFLAGS) -o diffstat ./$(VICCPU)/diffstat.o $(LIBS)

imgval: imgval.o
	$(CC) $(LDFLAGS) -o imgval ./$(VICCPU)/imgval.o $(LIBS)

install:
	cp $(PROG) ../../../$(LIB_DIR)/bin/$(ARCH)

clean:
	\rm -f $(PROG) $(OBJS)
	make $(OPT32)

depend:
	makedepend $(CCFLAGS) $(GCCINC) $(SRCS)
