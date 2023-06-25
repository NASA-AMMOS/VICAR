MKALL	= make
MKCLN	= make clean
MKDEP	= make depend

all:
	@echo "Making vsl"; mkdir -p ./vsl/$(VICCPU); cd vsl; $(MKALL)
	@echo "Making image"; mkdir -p ./image/$(VICCPU); cd image; $(MKALL)
	@echo "Making grape"; mkdir -p ./grape/$(VICCPU); cd grape; $(MKALL)
	@echo "Making ds"; mkdir -p ./ds/$(VICCPU); cd ds; $(MKALL)
	@echo "Making path"; mkdir -p ./path/$(VICCPU); cd path; $(MKALL)
	@echo "Making conv"; mkdir -p ./conv/$(VICCPU); cd conv; $(MKALL)

clean:
	@echo "Making vsl"; cd vsl; $(MKCLN)
	@echo "Making image"; cd image; $(MKCLN)
	#@echo "Making net"; cd net; $(MKCLN)
	@echo "Making grape"; cd grape; $(MKCLN)
	@echo "Making ds"; cd ds; $(MKCLN)
	@echo "Making path"; cd path; $(MKCLN)
	@echo "Making conv"; cd conv; $(MKCLN)

depend:
	@echo "Making vsl"; cd vsl; $(MKDEP)
	@echo "Making image"; cd image; $(MKDEP)
	#@echo "Making net"; cd net; $(MKDEP)
	@echo "Making grape"; cd grape; $(MKDEP)
	@echo "Making ds"; cd ds; $(MKDEP)
	@echo "Making path"; cd path; $(MKDEP)
	@echo "Making conv"; cd conv; $(MKDEP)
