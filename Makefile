CFLAGS  ?= -O2 -march=native

GNATMAKE    = gprbuild -dm -p
GNATCLEAN   = gprclean -q
GNATINSTALL = gprinstall
GNATPP      = gnatpp -q -cl2 -c3 -i3 -M99 -nM -neM -ntM -nnM -N --use-on-new-line -t -dd --eol=lf -rf

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

.PHONY: build debug profile tools format clean install

build:
	$(GNATMAKE) -P tools/dcf_ada.gpr -cargs $(CFLAGS)

debug:
	$(GNATMAKE) -P tools/dcf_ada.gpr -XMode=debug -cargs $(CFLAGS)

profile:
	$(GNATMAKE) -P tools/dcf_ada.gpr -XMode=profiling -cargs $(CFLAGS)

tools:
	$(GNATMAKE) -P tools/tools.gpr -cargs $(CFLAGS)

format:
	$(GNATPP) -P tools/dcf_ada.gpr -XMode=debug -cargs $(CFLAGS)
	$(GNATPP) -P tools/tools.gpr -XMode=debug -cargs $(CFLAGS)
	rm **/*.npp

clean:
	$(GNATCLEAN) -P tools/dcf_ada.gpr
	$(GNATCLEAN) -P tools/tools.gpr
	rm -rf bin build

install:
	$(GNATINSTALL) -p -q -f --install-name='dcf-ada' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P tools/dcf_ada.gpr
	install bin/* $(PREFIX)/bin/
