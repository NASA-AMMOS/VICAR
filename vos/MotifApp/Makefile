all:
	@if [ -z "$$VICCPU" ]; then echo "Error: VICCPU is not set. Source the VICAR setup script first."; exit 1; fi
	-mkdir "$$VICCPU"
	-chmod +x makelinks
	-if [ -d "$$VICCPU" ]; then ( cd "$$VICCPU" && rm -f * ); fi;
	./makelinks
	(cd "$$VICCPU" && make)

