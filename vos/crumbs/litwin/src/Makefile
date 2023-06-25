INCLUDE_PATH = -I. -I../include -I$(P2INC)
OBJS = Camera_Model.o
SRCS = Camera_Model.C
CPLUSPLUS = $(CC)
CFLAGS = $(OPT32)
LIB = ../../lib/$(ARCH)/libcahvor.a

all: $(LIB)

$(LIB): $(OBJS)
	(cd ./$(VICCPU); ar -r $(LIB) $(OBJS))

.c.o:
	$(CC) $(CFLAGS) $(INCLUDE_PATH) -c $*.c -o ./$(VICCPU)/$*.o

Camera_Model.o: Camera_Model.C
	$(CPLUSPLUS) $(CFLAGS) $(INCLUDE_PATH) -c Camera_Model.C -o ./$(VICCPU)/Camera_Model.o

depend:
	makedepend $(INCLUDE_PATH) $(GCCINC) $(SRCS)
