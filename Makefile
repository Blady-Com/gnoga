PREFIX=$(dir $(shell which gnatls))..

# If you do no have grptools comment out these two lines and uncomment one of
# the lines below.

BUILDER=gprbuild
CLEANER=gprclean

# If you do not have gprtools switch to gnatmake and gnatclean
# this is the case on MSYS with MinGW, using Cygwin's own gcc/ada
# or on some linux distros
#
#BUILDER=gnatmake
#CLEANER=gnatclean
#
# If using MinGW on Cygwin32 or 64 you can use the following:
#
# For 32bit
#BUILDER=i686-w64-ming32-gnatmake.exe
#CLEANER=i686-w64-ming32-gnatclean.exe
#
# For 64bit
#BUILDER=x86_64-w64-mingw32-gnatmake.exe
#CLEANER=x86_64-w64-mingw32-gnatclean.exe

TARGET=$(shell gcc -dumpmachine)

ifeq ($(strip $(findstring darwin, $(TARGET))),darwin)
	PRJ_TARGET=OSX
else
ifeq ($(strip $(findstring mingw32, $(TARGET))),mingw32)
	PRJ_TARGET=Windows
else
ifeq ($(strip $(findstring cygwin, $(TARGET))),cygwin)
	PRJ_TARGET=Windows
else
ifeq ($(strip $(findstring freebsd, $(TARGET))),freebsd)
	PRJ_TARGET=Freebsd
else
ifeq ($(strip $(findstring linux, $(TARGET))),linux)
	PRJ_TARGET=Linux
else
	PRJ_TARGET=Unix
endif
endif
endif
endif
endif

all: gnoga gnoga_tools demo tutorials

setup:
ifeq (${PRJ_TARGET}, Windows)
	@echo "Setting up for Windows build"
	- copy src\gnoga-application.windows src\gnoga-application.adb
	- cp src/gnoga-application.windows src/gnoga-application.adb
else
ifeq (${PRJ_TARGET}, OSX)
	@echo "Setting up for Mac OS X build"
	cp src/gnoga-application.osx src/gnoga-application.adb
else
	@echo "Linux/FreeBSD or Unix build"
endif
endif

gnoga: setup
	cd src && $(BUILDER) -p -Pgnoga.gpr -XPRJ_TARGET=${PRJ_TARGET}

gnoga_secure: gnoga
	cd ssl && $(BUILDER) -p -Pgnoga_secure.gpr -XPRJ_TARGET=${PRJ_TARGET}

gnoga_tools:
	cd tools && $(BUILDER) -p -Ptools.gpr -XPRJ_TARGET=${PRJ_TARGET}

release: setup
	cd src && $(BUILDER) -p -Pgnoga.gpr -XPRJ_BUILD=Release -XPRJ_TARGET=${PRJ_TARGET}

install: release gnoga_tools
	touch deps/simple_components/strings_edit-text_edit.o
	cd src && gprinstall -f --prefix=$(PREFIX) -p gnoga.gpr -XPRJ_BUILD=Release
	cd tools && gprinstall -f --prefix=$(PREFIX) -p --mode=usage --install-name=tools tools.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p components.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p components-connections_server.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p components-connections_server-http_server.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p strings_edit.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p tables.gpr

install_debug: gnoga gnoga_tools
	touch deps/simple_components/strings_edit-text_edit.o
	cd src && gprinstall -a -f --prefix=$(PREFIX) -p gnoga.gpr
	cd tools && gprinstall -f --prefix=$(PREFIX) -p --mode=usage --install-name=tools tools.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p components.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p components-connections_server.gpr --install-name=components-connections_server
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p components-connections_server-http_server.gpr --install-name=components-connections_server-http_server
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p strings_edit.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p tables.gpr

uninstall:
	- gprinstall -f --prefix=$(PREFIX) --uninstall gnoga.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall components.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall components-connections_server.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall components-connections_server-http_server.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall strings_edit.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall tables.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall tools.gpr

native_gtk: src/gnoga_gtk_window.c
	cd obj && gcc -c ../src/gnoga_gtk_window.c `pkg-config --cflags gtk+-3.0,webkit2gtk-3.0`

native_osx:
	- cd deps && git clone https://github.com/MacGapProject/MacGap2.git
	@echo
	@echo "Native XCode project is now in deps/MacGap2"
	@echo "See docs/native_mac_apps.md for instructions"

ace_editor:
	- cd js && git clone https://github.com/ajaxorg/ace-builds.git

demo: snake mine_detector chattanooga adaedit adablog connect_four

snake:
	cd demo/snake && $(BUILDER) -Psnake.gpr -XPRJ_TARGET=${PRJ_TARGET}

mine_detector:
	cd demo/mine_detector && $(BUILDER) -Pmine_detector.gpr -XPRJ_TARGET=${PRJ_TARGET}

chattanooga:
	cd demo/chattanooga && $(BUILDER) -Pchattanooga.gpr -XPRJ_TARGET=${PRJ_TARGET}

adaedit: ace_editor
	cd demo/adaedit && $(BUILDER) -Padaedit.gpr -XPRJ_TARGET=${PRJ_TARGET}

adablog:
	- cd demo/adablog && $(BUILDER) -Padablog.gpr -XPRJ_TARGET=${PRJ_TARGET}

connect_four:
	cd demo/connect_four && $(BUILDER) -Pconnect_four.gpr -XPRJ_TARGET=${PRJ_TARGET}

tests:
	cd test && $(BUILDER) -Ptest.gpr -XPRJ_TARGET=${PRJ_TARGET}

tests_ssl:
	cd test_ssl && $(BUILDER) -Ptest_ssl.gpr -XPRJ_TARGET=${PRJ_TARGET}

tutorials:
	cd tutorial/tutorial-01 && $(BUILDER) -Ptutorial_01.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tutorial/tutorial-02 && $(BUILDER) -Ptutorial_02.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tutorial/tutorial-03 && $(BUILDER) -Ptutorial_03.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tutorial/tutorial-04 && $(BUILDER) -Ptutorial_04.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tutorial/tutorial-05 && $(BUILDER) -Ptutorial_05.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tutorial/tutorial-06 && $(BUILDER) -Ptutorial_06.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tutorial/tutorial-07 && $(BUILDER) -Ptutorial_07.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tutorial/tutorial-08 && $(BUILDER) -Ptutorial_08.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tutorial/tutorial-09 && $(BUILDER) -Ptutorial_09.gpr -XPRJ_TARGET=${PRJ_TARGET}
	- cd tutorial/tutorial-10 && $(BUILDER) -Ptutorial_10.gpr -XPRJ_TARGET=${PRJ_TARGET}
	- cd tutorial/tutorial-11 && $(BUILDER) -Ptutorial_11.gpr -XPRJ_TARGET=${PRJ_TARGET}

clean:
	cd src && $(CLEANER) -r -Pgnoga.gpr
	cd ssl && $(CLEANER) -r -Pgnoga_secure.gpr
	cd tools && $(CLEANER) -Ptools.gpr
	cd test && $(CLEANER) -Ptest.gpr
	cd test_ssl && $(CLEANER) -Ptest_ssl.gpr
	cd test/tickets/001 && $(CLEANER) -Ptest.gpr
	cd test/tickets/002 && $(CLEANER) -Ptest.gpr
	cd demo/snake && $(CLEANER) -Psnake.gpr
	cd demo/mine_detector && $(CLEANER) -Pmine_detector.gpr
	cd demo/chattanooga && $(CLEANER) -Pchattanooga.gpr
	cd demo/adablog && $(CLEANER) -Padablog.gpr
	cd demo/connect_four && $(CLEANER) -Pconnect_four.gpr
	cd demo/adaedit && $(CLEANER) -Padaedit.gpr
	cd tutorial/tutorial-01 && $(CLEANER) -Ptutorial_01.gpr
	cd tutorial/tutorial-02 && $(CLEANER) -Ptutorial_02.gpr
	cd tutorial/tutorial-03 && $(CLEANER) -Ptutorial_03.gpr
	cd tutorial/tutorial-04 && $(CLEANER) -Ptutorial_04.gpr
	cd tutorial/tutorial-05 && $(CLEANER) -Ptutorial_05.gpr
	cd tutorial/tutorial-06 && $(CLEANER) -Ptutorial_06.gpr
	cd tutorial/tutorial-07 && $(CLEANER) -Ptutorial_07.gpr
	cd tutorial/tutorial-08 && $(CLEANER) -Ptutorial_08.gpr
	cd tutorial/tutorial-09 && $(CLEANER) -Ptutorial_09.gpr
	cd tutorial/tutorial-10 && $(CLEANER) -Ptutorial_10.gpr
	cd tutorial/tutorial-11 && $(CLEANER) -Ptutorial_11.gpr
	- cd obj && rm gnoga_gtk_window.o
	- cd deps && rm -rf MultiMarkdown-4
	- cd docs && rm html/*.html
	- cd docs && rm -rf html/gnoga_rm
	- rm bin/multimarkdown
	- cd bin && rm *.db
	- cd bin && rm temp.txt
	- cd bin && rm gnoga-test
	- cd js && rm -rf ace-builds

bin/multimarkdown:
	- cd deps && git clone git://github.com/fletcher/MultiMarkdown-4.git
	- cd deps/MultiMarkdown-4 && git submodule init
	- cd deps/MultiMarkdown-4 && git submodule update
	- cd deps/MultiMarkdown-4 && make
	- mv deps/MultiMarkdown-4/multimarkdown bin/
rm-docs:
	gnatdoc -Psrc/gnoga.gpr --no-subprojects -XDevelopment=Debug -XLegacy=Ada2005 -XAtomic_Access=GCC-long-offsets -XTasking=Multiple -XTraced_objects=Off -XPRJ_TARGET=Unix -XPRJ_BUILD=Debug

html-docs: bin/multimarkdown
	cd docs && ../bin/multimarkdown user_guide.md > html/user_guide.html
	cd docs && ../bin/multimarkdown api_summary.md > html/api_summary.html
	cd docs && ../bin/multimarkdown native_mac_apps.md > html/native_mac_apps.html
	cd docs && ../bin/multimarkdown native_gtk_apps.md > html/native_gtk_apps.html
