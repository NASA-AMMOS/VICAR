CC = g++
LIB_DIR = dev
INCL = -I../../$(LIB_DIR)/include -I/usr/include/X11 -I/usr/include/CC
#INCL = -I../../$(LIB_DIR)/include -I/usr/include/X11 -I/usr/include/CC \
#                -I$(PVM_ROOT)/include

#CCFLAGS = -DFUNCPROTO $(INCL) -D$(ARCH) $(FLAGS) -DPVM_DATAPORT
CCFLAGS = $(OPT32) -DFUNCPROTO $(INCL) $(FLAGS)

LIB = ../../../$(LIB_DIR)/lib/$(ARCH)/libvsl.a

OBJS =  file.o time_value.o preferences.o Transform.o dataport.o
SRCS = $(OBJS:.o=.C)

$(LIB): $(OBJS)
	(cd ./$(VICCPU); ar rc $(LIB) $(OBJS))

.C.o:
	$(CC) -c $(CCFLAGS) $*.C -o ./$(VICCPU)/$*.o

clean:
	\rm -f $(LIB) $(OBJS)
	make

depend:
	makedepend $(CCFLAGS) $(GCCINC) $(SRCS)
