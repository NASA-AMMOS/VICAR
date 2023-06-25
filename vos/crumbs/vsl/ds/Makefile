# arch-specific: $LIBS

CC = gcc
CXX = g++

INCL = -I../../$(LIB_DIR)/include

CCFLAGS = $(OPT32) -DFUNCPROTO $(INCL) $(FLAGS)

LDFLAGS = -L../../../$(LIB_DIR)/lib/$(ARCH) -L/usr/X11R6/lib -L/opt/X11/lib $(FLAGS)

LIB = ../../../$(LIB_DIR)/lib/$(ARCH)/libds.a

OBJS = main.o daemon.o sigchild.o tcpserv.o error.o util.o
OBJC = tcpcli.o error.o util.o dataport.o

# Data server daemon

#all: client daemon
all: client

daemon: $(OBJS)
	$(CXX) $(LDFLAGS) -o daemon $(OBJS) $(LIBS)

.c.o:
	$(CC) -c $(CCFLAGS) $*.c -o ./$(VICCPU)/$*.o

.C.o:
	$(CXX) -c $(CCFLAGS) $*.C -o ./$(VICCPU)/$*.o

# Client library

client: $(OBJC)
	(cd ./$(VICCPU);ar rc $(LIB) $(OBJC))

install:
	mv daemon ../../$(LIB_DIR)/bin/$(ARCH)

clean:
	\rm -f $(LIB) $(OBJS) $(OBJC)
	make

depend:
	makedepend $(CCFLAGS) $(GCCINC) \
	tcpcli.c util.c error.c tcpserv.c sigchild.c daemon.c main.c
