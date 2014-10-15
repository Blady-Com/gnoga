prefix  =$(dir $(shell which gnatls))..
INSTALL =/usr/bin/install -c

all: gnoga tutorials snake

gnoga:
	cd src && gprbuild -p -Pgnoga.gpr

release:
	cd src && gprbuild -p -Pgnoga.gpr -XPRJ_BUILD=Release

install: release
	cd src && gprinstall -f -p gnoga.gpr -XPRJ_BUILD=Release

uninstall:
	cd src && gprinstall --uninstall gnoga.gpr

old_install: release
	$(INSTALL) -d $(prefix)/lib/gnoga
	$(INSTALL) -d $(prefix)/include/gnoga

	$(INSTALL) -m 444 src/gnoga-installed.gpr $(prefix)/lib/gnat/gnoga.gpr
	$(INSTALL) -m 444 lib/* $(prefix)/lib/gnoga
	$(INSTALL) -m 444 src/* $(prefix)/include/gnoga

old_uninstall:
	rm -rf $(prefix)/lib/gnoga
	rm -rf $(prefix)/include/gnoga
	rm -f $(prefix)/lib/gnat/gnoga.gpr

ace_editor:
	cd js && git clone https://github.com/ajaxorg/ace-builds.git

demo: snake adaedit adablog

adablog:
	cd demo/adablog && gprbuild -p

snake:
	cd demo/snake && gprbuild -p

adaedit: ace_editor
	cd demo/adaedit && gprbuild -p

tests:
	cd test && gprbuild -p

tutorials:
	cd tutorial/tutorial-01 && gprbuild -p
	cd tutorial/tutorial-02 && gprbuild -p
	cd tutorial/tutorial-03 && gprbuild -p
	cd tutorial/tutorial-04 && gprbuild -p
	cd tutorial/tutorial-05 && gprbuild -p
	cd tutorial/tutorial-06 && gprbuild -p
	cd tutorial/tutorial-07 && gprbuild -p
	cd tutorial/tutorial-08 && gprbuild -p
	cd tutorial/tutorial-09 && gprbuild -p

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
	cd tutorial/tutorial-09 && gprclean
	- cd js && rm -rf ace-builds
