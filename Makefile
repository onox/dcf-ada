PREFIX ?= /usr

.PHONY: build clean install uninstall

build:
	alr build

clean:
	alr clean
	rm -rf build

install:
	install build/bin/* $(PREFIX)/bin/

uninstall:
	rm $(PREFIX)/bin/zipdcf
	rm $(PREFIX)/bin/unzipdcf
