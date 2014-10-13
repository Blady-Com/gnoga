prefix  =$(dir $(shell which gnatls))..
INSTALL =/usr/bin/install -c

all: bin gnoga tutorials snake

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

ace_editor:
	cd js && git clone https://github.com/ajaxorg/ace-builds.git

demo: snake adaedit adablog

adablog:
	cd demo/adablog && gprbuild

snake:
	cd demo/snake && gprbuild

adaedit: ace_editor
	cd demo/adaedit && gprbuild

tests:
	cd test && gprbuild

tutorials:
	cd tutorial/tutorial-01 && gprbuild
	cd tutorial/tutorial-02 && gprbuild
	cd tutorial/tutorial-03 && gprbuild
	cd tutorial/tutorial-04 && gprbuild
	cd tutorial/tutorial-05 && gprbuild
	cd tutorial/tutorial-06 && gprbuild
	cd tutorial/tutorial-07 && gprbuild
	cd tutorial/tutorial-08 && gprbuild

clean:
	cd src && gprclean -Pgnoga.gpr
	cd test && gprclean
	cd demo/adablog && gprclean
	cd demo/snake && gprclean
	cd demo/adaedit && gprclean
	cd tutorial/tutorial-01 && gprclean
	cd tutorial/tutorial-02 && gprclean
	cd tutorial/tutorial-03 && gprclean
	cd tutorial/tutorial-04 && gprclean
	cd tutorial/tutorial-05 && gprclean
	cd tutorial/tutorial-06 && gprclean
	cd tutorial/tutorial-07 && gprclean
	cd tutorial/tutorial-08 && gprclean
	- cd js && rm -rf ace-builds
