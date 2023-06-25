# Build SUMMITT/CRUMBS
# builds everything except ivconv (see franks_loader dir).

# set this to current (CRUMBS home) directory
# make sure it is commented out before delivering
#CRUMBS_HOME=/home/ozp/msl/SUMMITTDIR/CM_CRUMBS

export ARCH=$(VICCPU)
export LIB_DIR=dev
export FLAGS=-O2
# the -I is for macs
export CC=g++ $(OPT32) -I/opt/X11/include
sb=$(CRUMBS_HOME)/bin/$(VICCPU)/

all: clean litwin vsl_dir xmln summitt clean_build

# Todd Litwin's vision/camera model library
litwin: litwin/lib/$(VICCPU)/libcahvor.a
litwin/lib/$(VICCPU)/libcahvor.a:
	mkdir -p litwin/lib/$(VICCPU)
	mkdir -p litwin/src/$(VICCPU)
	cd $(CRUMBS_HOME)/litwin/src && make OPT32=$(OPT32)

# VSL library and tools
VLIBS = dev/lib/$(VICCPU)/libds.a \
	dev/lib/$(VICCPU)/libgrape.a \
	dev/lib/$(VICCPU)/libimage.a \
	dev/lib/$(VICCPU)/libpath.a \
	dev/lib/$(VICCPU)/libvsl.a
VTOOLS = bin/convert bin/img16to8 bin/imgstat
vsl_dir:
	mkdir -p $(CRUMBS_HOME)/dev/lib/$(VICCPU)
	mkdir -p $(sb)
	mkdir -p $(CRUMBS_HOME)/tasks/ramps/src/$(VICCPU)
	cd $(CRUMBS_HOME)/vsl && make OPT32=$(OPT32)
	mv $(CRUMBS_HOME)/vsl/conv/$(VICCPU)/convert $(sb)
	mv $(CRUMBS_HOME)/vsl/conv/$(VICCPU)/img16to8 $(sb)
	mv $(CRUMBS_HOME)/vsl/conv/$(VICCPU)/imgstat $(sb)

clean:
	\rm -rf $(VLIBS) litwin/lib/$(VICCPU)/libcahvor.a \
         litwin/src/$(VICCPU)/*.o $(sb)/* \
         vsl/conv/$(VICCPU)/*.o vsl/ds/$(VICCPU)/*.o vsl/path/$(VICCPU)/*.o vsl/image/$(VICCPU)/*.o \
         vsl/grape/$(VICCPU)/*.o vsl/vsl/$(VICCPU)/*.o \
         xmln/$(VICCPU)/*.o xmln/xmln \
         $(rs)/$(VICCPU)/*.o $(rs)/fst2img \
         $(rs)/fstquad $(rs)/tilemesh $(rs)/tmsa $(rs)/xcmod
	rm -rf litwin/lib/$(VICCPU)
	rm -rf litwin/src/$(VICCPU)
	rm -rf $(CRUMBS_HOME)/dev/lib/$(VICCPU)
	rm -rf vsl/grape/$(VICCPU)
	rm -rf $(rs)/$(VICCPU)
	rm -rf $(sb)
clean_build:
	\rm -rf litwin/src/$(VICCPU)/*.o  \
         vsl/conv/$(VICCPU)/*.o vsl/ds/$(VICCPU)/*.o vsl/path/$(VICCPU)/*.o vsl/image/$(VICCPU)/*.o \
         vsl/grape/$(VICCPU)/*.o vsl/vsl/$(VICCPU)/*.o \
         xmln/*.o $(rs)/$(VICCPU)/*.o \
         $(rs)/$(VICCPU) \
         vsl/grape/$(VICCPU) \
         vsl/image/$(VICCPU) \
         vsl/ds/$(VICCPU) \
         vsl/vsl/$(VICCPU) \
         vsl/conv/$(VICCPU) \
         vsl/path/$(VICCPU) \
         litwin/src/$(VICCPU) \
# XML parser
#xmln: bin/xmln
#bin/xmln:
#	cd xmln && make OPT32=$(OPT32) && mv xmln $(sb)

# CRUMBS apps
summitt: bin/tile_cat bin/fst2img bin/fstquad bin/tilemesh bin/tmsa bin/xcmod
#rs=$(CRUMBS_HOME)/tasks/ramps/src
rs=tasks/ramps/src
bin/tile_cat: 
	cd $(rs) && make tile_cat OPT32=$(OPT32) && mv ./$(VICCPU)/tile_cat $(sb)
bin/fst2img: 
	cd $(rs) && make fst2img OPT32=$(OPT32) && mv ./$(VICCPU)/fst2img $(sb)
bin/fstquad: 
	cd $(rs) && make fstquad OPT32=$(OPT32) && mv ./$(VICCPU)/fstquad $(sb)
bin/tilemesh: 
	cd $(rs) && make tilemesh OPT32=$(OPT32) && mv ./$(VICCPU)/tilemesh $(sb)
bin/tmsa: 
	cd $(rs) && make tmsa OPT32=$(OPT32) && mv ./$(VICCPU)/tmsa $(sb)
bin/xcmod: 
	cd $(rs) && make xcmod OPT32=$(OPT32) && mv ./$(VICCPU)/xcmod $(sb)
