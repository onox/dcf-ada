PREFIX ?= /usr

.PHONY: build clean install uninstall

build:
	cd dcf && alr build --validation
	cd zipdcf && alr build --validation

clean:
	cd dcf && alr clean
	cd zipdcf && alr clean
	rm -rf dcf/build zipdcf/build

install:
	install zipdcf/build/bin/* $(PREFIX)/bin/

uninstall:
	rm $(PREFIX)/bin/zipdcf
	rm $(PREFIX)/bin/unzipdcf
