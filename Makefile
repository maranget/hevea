#### Standard configuration parameters
# Compile using ocamlopt, to use ocamlc set TARGET=byte
TARGET=opt
# Install prefix
PREFIX?=/usr/local
# Library directory of hevea
LIBDIR=$(PREFIX)/lib/hevea
# Where to install programms
BINDIR=$(PREFIX)/bin
#Where to install hevea.sty
LATEXLIBDIR=$(PREFIX)/lib/hevea
##### Advanced configuration parameters
SUF=
DIR=
OCAMLC=$(DIR)ocamlc$(SUF)
#OCAMLFLAGS=-w +a-4-9 -warn-error +a
OCAMLFLAGS=-w +a-3-4-9-41-45-67
OCBFLAGS=-j 4 -classic-display

#### End of configuration parameters
#### The Makefile uses ocamlbuild if present.
PGM=hevea.byte hacha.byte esponja.byte bibhva.byte
PGMNATIVE=$(PGM:.byte=.native)

all: $(TARGET)

install: config.sh
	./install.sh $(TARGET)

byte: ocb-byte
opt: ocb-opt
both: ocb-both

include libs.def

config.sh: Makefile libs.def handle402.sh
	@( cat handle402.sh &&\
	echo PGM=\"$(PGM)\" &&\
	echo PGMNATIVE=\"$(PGMNATIVE)\" &&\
	echo BINDIR=$(BINDIR) &&\
	echo LIBDIR=$(LIBDIR) &&\
	echo LATEXLIBDIR=$(LATEXLIBDIR) &&\
	echo OCAMLFLAGS=\"$(OCAMLFLAGS)\" &&\
	echo OCBFLAGS=\"$(OCBFLAGS)\" &&\
	echo ALLLIB=\"$(ALLLIB)\" && \
	echo HTMLLIB=\"$(HTMLLIB)\" && \
	echo TEXTLIB=\"$(TEXTLIB)\" && \
	echo INFOLIB=\"$(INFOLIB)\") > $@

clean:: config.sh
	sh ocb.sh clean && rm -f config.sh

ocb-byte: config.sh
	sh ocb.sh byte

ocb-opt: config.sh
	sh ocb.sh opt

ocb-both: config.sh
	sh ocb.sh both

ocb: ocb-$(TARGET)


clean::
	rm -f *~ #*# html/*~ html/#*# text/*~ text/#*# info/*~ info/#*# 


