.KEEP_STATE:

CCC=CC
CCFLAGS = $(OPT32) -g -DFUNCPROTO -I../../$(LIB_DIR)/include -I$(PVM_ROOT)/include $(FLAGS)

OBJS = net_lib.o dataport.o

LDFLAGS = -L. -L$(PVM_ROOT)/lib/$(PVM_ARCH) $(FLAGS)

LIB = lib_network.a

LIBS = -l_network -lpvm3 -lmalloc

basic: lib_network.a
	cp $(LIB) ../../$(LIB_DIR)/lib/$(ARCH)/$(LIB)

all: list_test net_test mesg_test pmake
	cp $(LIB) ../../$(LIB_DIR)/lib/$(ARCH)/$(LIB)
	cp mesg_test $(PVM_ROOT)/bin/$(PVM_ARCH)/mesg_test
	cp any_cmd $(PVM_ROOT)/bin/$(PVM_ARCH)/any_cmd

parallel: 
	make $(LIB)
	pmake net_test mesg_test
	cp $(LIB) ../../$(LIB_DIR)/lib/$(ARCH)/$(LIB)
	cp mesg_test $(PVM_ROOT)/bin/$(PVM_ARCH)/mesg_test

$(LIB): $(OBJS)
	ar rc $(LIB) $(OBJS)

list_test: list_test.o $(LIB)
	$(CCC) -o list_test list_test.o $(LDFLAGS) $(LIBS)

net_test: net_test.o $(LIB)
	$(CCC) -o net_test net_test.o $(LDFLAGS) $(LIBS)

mesg_test: mesg_test.o $(LIB)
	$(CCC) -o mesg_test mesg_test.o $(LDFLAGS) $(LIBS)

pmake: pmake.o
	$(CCC) -o pmake pmake.o $(LDFLAGS) $(LIBS) 

.C.o:
	$(CCC) -c $(CCFLAGS) $*.C

clean:
	\rm -f $(LIB) $(OBJS)
	make

depend:
	makedepend $(CCFLAGS) $(GCCINC) \
		net_lib.C net_test.C mesg_test.C pmake.C
