CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

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
	$(GNATMAKE) -P dcf_ada.gpr -cargs $(CFLAGS) -largs $(LDFLAGS)

debug:
	$(GNATMAKE) -P dcf_ada.gpr -XMode=debug -cargs $(CFLAGS) -largs $(LDFLAGS)

profile:
	$(GNATMAKE) -P dcf_ada.gpr -XMode=profiling -cargs $(CFLAGS) -largs $(LDFLAGS)

tools:
	$(GNATMAKE) -P tools.gpr -cargs $(CFLAGS) -largs $(LDFLAGS)

format:
	$(GNATPP) -P dcf_ada.gpr -XMode=debug -cargs $(CFLAGS)
	$(GNATPP) -P tools.gpr -XMode=debug -cargs $(CFLAGS)
	rm **/*.npp

clean:
	$(GNATCLEAN) -P dcf_ada.gpr
	$(GNATCLEAN) -P tools.gpr
	rm -rf bin lib obj

install:
	$(GNATINSTALL) --relocate-build-tree -p -q -f --install-name='dcf-ada' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P dcf_ada.gpr
	install bin/* $(PREFIX)/bin/
