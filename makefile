prefix  =$(dir $(shell which gnatls))..
INSTALL =/usr/bin/install -c

all: bin gnoga demo tests

gnoga:
	cd src && gprbuild -Pgnoga.gpr

bin:
	-mkdir bin
	-mkdir obj
	-mkdir lib

install: gnoga
	$(INSTALL) -d $(prefix)/lib/gnoga
	$(INSTALL) -d $(prefix)/include/gnoga

	$(INSTALL) -m 444 src/gnoga-installed.gpr $(prefix)/lib/gnat/gnoga.gpr
	$(INSTALL) -m 444 lib/* $(prefix)/lib/gnoga
	$(INSTALL) -m 444 src/* $(prefix)/include/gnoga

uninstall:
	rm -rf $(prefix)/lib/gnoga
	rm -rf $(prefix)/include/gnoga
	rm -f $(prefix)/lib/gnat/gnoga.gpr

demo: snake adablog

adablog:
	cd demo/adablog && gprbuild

snake:
	cd demo/snake && gprbuild

tests:
	cd test && gprbuild

clean:
	cd src && gprclean
	cd test && gprclean
	cd demo/adablog && gprclean
	cd demo/snake && gprclean
	-cd bin && rm *.db
