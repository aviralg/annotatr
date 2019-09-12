R := R
RMDIR := rm -rf
MKDIR := mkdir -p

all: install

build: clean
	$(R) CMD build .

install: clean
	$(R) CMD INSTALL --with-keep.source .

clean:
	rm -rf annotatr*.tar.gz
	rm -rf *.Rcheck
	rm -rf src/*.so
	rm -rf src/*.o

document:
	$(R) -e "devtools::document()"

check: build
	$(R) CMD check annotatr_*.tar.gz

test:
	$(R) -e "devtools::test()"

gctorture:
	$(R) -e "library(devtools); gctorture(TRUE); devtools::test();"

.PHONY: all build install clean document check test gctorture
