CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

GNATMAKE  = gprbuild -dm -p
GNATCLEAN = gprclean -q

.PHONY: build clean

build:
	$(GNATMAKE) -P zipada.gpr -cargs $(CFLAGS) -largs $(LDFLAGS)

clean:
	$(GNATCLEAN) -P zipada.gpr
	rm -rf bin obj
