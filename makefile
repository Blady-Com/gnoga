PREFIX=$(dir $(shell which gnatls))..
TARGET=$(shell gcc -dumpmachine)

ifeq ($(strip $(findstring darwin, $(TARGET))),darwin)
	PRJ_TARGET=OSX
endif

all: gnoga gnoga_tools tutorials snake

gnoga:
	cd src && gprbuild -p -Pgnoga.gpr

gnoga_tools:
	cd tools && gprbuild -p -Ptools.gpr

release:
	cd src && gprbuild -p -Pgnoga.gpr -XPRJ_BUILD=Release

install: release gnoga_tools
	cd src && gprinstall -f --prefix=$(PREFIX) -p gnoga.gpr -XPRJ_BUILD=Release
	cd tools && gprinstall -f --prefix=$(PREFIX) -p --mode=usage tools.gpr

uninstall:
	- cd src && gprinstall -f --uninstall gnoga.gpr
	- cd tools && gprinstall -f --uninstall tools.gpr

ace_editor:
	- cd js && git clone https://github.com/ajaxorg/ace-builds.git

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
	- cd tutorial/tutorial-10 && gprbuild -p
	- cd tutorial/tutorial-11 && gprbuild -p

clean:
	cd src && gprclean -Pgnoga.gpr
	cd tools && gprclean -Ptools.gpr
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
	cd tutorial/tutorial-10 && gprclean
	cd tutorial/tutorial-11 && gprclean
	- cd bin && rm *.db
	- cd bin && rm temp.txt
	- cd js && rm -rf ace-builds
