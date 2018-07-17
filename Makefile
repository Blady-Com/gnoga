PREFIX=$(shell echo $(dir $(shell which gnatls)) | sed "s:/cygdrive/\\(.\\):\\1\\::" )..
GPRCHECK=$(shell gprbuild --version)
TARGET=$(shell gcc -dumpmachine)

ATOMIC_ACCESS=GCC-long-offsets
#if using GNAT GPL prior to 2014 or earlier on a 32bit host (Windows or Linux)
#you need to change this to:
#
#ATOMIC_ACCESS=GCC-built-ins

ifeq ($(strip $(findstring GPRBUILD, $(GPRCHECK))),GPRBUILD)
	BUILDER=gprbuild --target=${TARGET}
	CLEANER=gprclean --target=${TARGET}
else
	BUILDER=gnatmake
	CLEANER=gnatclean
endif

# If using MinGW on Cygwin32 or 64 you can use the following:
#
# For 32bit
#BUILDER=i686-w64-ming32-gnatmake.exe
#CLEANER=i686-w64-ming32-gnatclean.exe
#
# For 64bit
#BUILDER=x86_64-w64-mingw32-gnatmake.exe
#CLEANER=x86_64-w64-mingw32-gnatclean.exe

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

ifdef ComSpec
	COPY=copy
	MOVE=move
	RM=del
	RMS=del /S /Q
	PATHSEP2=\\
else
	COPY=cp
	MOVE=mv
	RM=rm
	RMS=rm -rf
	PATHSEP2=/
endif

PATHSEP=$(strip $(PATHSEP2))

ifeq (${PRJ_TARGET}, Windows)
	SET_READONLY=attrib +R
	UNSET_READONLY=attrib -R
	BUILD_SQLITE3=sqlite3
else
	SET_READONLY=chmod -w *
	UNSET_READONLY=chmod +w *
	BUILD_SQLITE3=
endif

all: gnoga gnoga_tools $(BUILD_SQLITE3) demo tutorials

setup:
	$(MAKE) -C src

basic_components:
	$(MAKE) -C components

xpm_parser:
	cd deps/simple_components/xpm && $(BUILDER) -p -Pxpm_parser.gpr

# Gnoga with DEBUG on by default
gnoga: setup basic_components xpm_parser
	- cd lib && $(UNSET_READONLY)
	cd src && $(BUILDER) -p -Pgnoga.gpr -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS}
	cd deps/simple_components && ar rc ../../lib/libgnoga.a *.o
	cd deps/simple_components/xpm && ar rc ../../../lib/libgnoga.a *.o
	- $(RM) include$(PATHSEP)*.ad?
	$(COPY) src$(PATHSEP)*.ad[sb] include
	$(COPY) src$(PATHSEP)gnoga-server-model-table.adb include
	$(COPY) deps$(PATHSEP)simple_components$(PATHSEP)*.ads include
	- $(COPY) deps$(PATHSEP)simple_components$(PATHSEP)*.ali lib
	$(COPY) deps$(PATHSEP)simple_components$(PATHSEP)xpm$(PATHSEP)*.ads include
	- $(COPY) deps$(PATHSEP)simple_components$(PATHSEP)xpm$(PATHSEP)*.ali lib
	cd lib && $(SET_READONLY)

gnoga_secure: gnoga
	- cd lib && $(UNSET_READONLY)
	cd ssl && $(BUILDER) -p -Pgnoga_secure.gpr -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS}
	cd deps/simple_components && ar rc ../../lib/libgnoga.a *.o
	cd lib && $(SET_READONLY)

gnoga_tools: gnoga
	cd tools && $(BUILDER) -p -Ptools.gpr -XPRJ_TARGET=${PRJ_TARGET}

# Gnoga build with DEBUG off
release: setup basic_components xpm_parser
	- cd lib && $(UNSET_READONLY)
	cd src && $(BUILDER) -p -Pgnoga.gpr -XPRJ_BUILD=Release -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS}
	cd deps/simple_components && ar rc ../../lib/libgnoga.a *.o
	cd deps/simple_components/xpm && ar rc ../../../lib/libgnoga.a *.o
	- $(RM) include$(PATHSEP)*.ad?
	$(COPY) src$(PATHSEP)*.ad[sb] include
	$(COPY) src$(PATHSEP)gnoga-server-model-table.adb include
	$(COPY) deps$(PATHSEP)simple_components$(PATHSEP)*.ads include
	- $(COPY) deps$(PATHSEP)simple_components$(PATHSEP)*.ali lib
	$(COPY) deps$(PATHSEP)simple_components$(PATHSEP)xpm$(PATHSEP)*.ads include
	- $(COPY) deps$(PATHSEP)simple_components$(PATHSEP)xpm$(PATHSEP)*.ali lib
	cd lib && $(SET_READONLY)
	cd deps/zanyblue && make -C src setup library zbmcompile.app

# Needed for Zanyblue install
include deps/zanyblue/src/mkfile/version.mk
ZBGNATXDEFS=-XV_MAJOR=$(V_MAJOR)
ZBGNATXDEFS+=-XV_MINOR=$(V_MINOR)
ZBGNATXDEFS+=-XV_PATCH=$(V_PATCH)

# Install Gnoga with DEBUG off
install: release gnoga_tools xpm_parser
	touch deps/simple_components/strings_edit-text_edit.o
	- cd lib && $(UNSET_READONLY)
	cd src && gprinstall -f --prefix=$(PREFIX) -p gnoga.gpr -XPRJ_BUILD=Release
	cd tools && gprinstall -f --prefix=$(PREFIX) -p --mode=usage --install-name=tools tools.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p components.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p components-connections_server.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p components-connections_server-http_server.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p strings_edit.gpr
	cd deps/simple_components && gprinstall -f --prefix=$(PREFIX) -p tables.gpr
	cd deps/simple_components/xpm && gprinstall -f --prefix=$(PREFIX) -p xpm_parser.gpr
	cd deps/zanyblue && gprinstall -f --prefix=$(PREFIX) -p --install-name=zblib src/zblib.gpr $(ZBGNATXDEFS)
	cd deps/zanyblue && gprinstall -f --prefix=$(PREFIX) -p --install-name=zbmcompile -aP src -aP lib/gnat src/zbmcompile/zbmcompile.gpr $(ZBGNATXDEFS)
	cd lib && $(SET_READONLY)

# Install Gnoga with DEBUG on
install_debug: gnoga gnoga_tools xpm_parser
	touch deps/simple_components/strings_edit-text_edit.o
	- cd lib && $(UNSET_READONLY)
	cd src && gprinstall -a -f --prefix=$(PREFIX) -p gnoga.gpr --install-name=gnoga
	cd tools && gprinstall -f --prefix=$(PREFIX) -p --mode=usage --install-name=tools tools.gpr
	cd deps/simple_components && gprinstall -a -f --prefix=$(PREFIX) -p components.gpr
	cd deps/simple_components && gprinstall -a -f --prefix=$(PREFIX) -p components-connections_server.gpr --install-name=components-connections_server
	cd deps/simple_components && gprinstall -a -f --prefix=$(PREFIX) -p components-connections_server-http_server.gpr --install-name=components-connections_server-http_server
	cd deps/simple_components && gprinstall -a -f --prefix=$(PREFIX) -p --install-name=strings_edit strings_edit.gpr
	cd deps/simple_components && gprinstall -a -f --prefix=$(PREFIX) -p --install-name=tables tables.gpr
	cd deps/simple_components/xpm && gprinstall -a -f --prefix=$(PREFIX) -p xpm_parser.gpr
	cd deps/zanyblue && gprinstall -a -f --prefix=$(PREFIX) -p --install-name=zblib src/zblib.gpr $(ZBGNATXDEFS)
	cd deps/zanyblue && gprinstall -f --prefix=$(PREFIX) -p --install-name=zbmcompile -aP src -aP lib/gnat src/zbmcompile/zbmcompile.gpr $(ZBGNATXDEFS)
	cd lib && $(SET_READONLY)

uninstall:
	- gprinstall -f --prefix=$(PREFIX) --uninstall gnoga.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall components.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall components-connections_server.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall components-connections_server-http_server.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall strings_edit.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall tables.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall xpm_parser.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall tools.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall zblib.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall zbmcompile.gpr

native_gtk: src/gnoga_gtk_window.c
	cd obj && gcc -c ../src/gnoga_gtk_window.c `pkg-config --cflags gtk+-3.0,webkit2gtk-3.0`

native_osx:
	- cd deps && git clone https://github.com/MacGapProject/MacGap2.git
	@echo
	@echo "Native XCode project is now in deps/MacGap2"
	@echo "See docs/native_mac_apps.md for instructions"

electron:
	@echo "Please insure that Node.js - npm is installed and on path."
	@echo
	- cd deps && git clone https://github.com/atom/electron-quick-start
	- cd deps/electron-quick-start && npm install
	$(COPY) html$(PATHSEP)electron.html deps$(PATHSEP)electron-quick-start$(PATHSEP)index.html
	$(COPY) js$(PATHSEP)electron_boot.js deps$(PATHSEP)electron-quick-start
	$(COPY) js$(PATHSEP)jquery.min.js deps$(PATHSEP)electron-quick-start
	@echo
	@echo "Native desktop using electron is now in deps/electron-quick-start"
	@echo "The example is set to connect to any running gnoga app on localhost:8080"
	@echo "Start your gnoga app and then run 'nmp start' in deps/electron-quick-start"
	@echo "See docs/native_desktop_apps.md for instructions on full desktop development"

# Zanyblue with DEBUG on
zanyblue:
	cd deps/zanyblue && make -C src BUILD=Debug setup library zbmcompile.app

deps/PragmARC:
	- cd deps && git clone https://github.com/jrcarter/PragmARC.git

pragmarc: deps/PragmARC
	- cd deps/PragmARC && git pull

demo: snake mine_detector chattanooga adaedit adablog password_gen linxtris random_int adaothello tic_tac_toe db_maker

snake:
	cd demo/snake && $(BUILDER) -Psnake.gpr -XPRJ_TARGET=${PRJ_TARGET}

mine_detector:
	cd demo/mine_detector && $(BUILDER) -Pmine_detector.gpr -XPRJ_TARGET=${PRJ_TARGET}

chattanooga:
	cd demo/chattanooga && $(BUILDER) -Pchattanooga.gpr -XPRJ_TARGET=${PRJ_TARGET}

adaedit:
	- cd demo/adaedit && $(BUILDER) -Padaedit.gpr -XPRJ_TARGET=${PRJ_TARGET}

adablog:
	- cd demo/adablog && $(BUILDER) -Padablog.gpr -XPRJ_TARGET=${PRJ_TARGET}

connect_four: zanyblue
	cd demo/connect_four && ../../deps/zanyblue/bin/zbmcompile -i -v -G strings connectfour_messages connectfour
	cd demo/connect_four && $(BUILDER) -Pconnect_four.gpr -XPRJ_TARGET=${PRJ_TARGET}

linxtris:
	cd demo/linxtris && $(BUILDER) -Plinxtris.gpr -XPRJ_TARGET=${PRJ_TARGET}
	@echo "usage: bin/linxtris -data_dir demo/linxtris/"

password_gen: pragmarc
	- cd demo/password_gen && $(BUILDER) -Ppassword_gen.gpr -XPRJ_TARGET=${PRJ_TARGET}

random_int:
	cd demo/random_int && $(BUILDER) -Prandom_int.gpr -XPRJ_TARGET=${PRJ_TARGET}

adaothello:
	cd demo/adaothello && $(BUILDER) -Padaothello.gpr -XPRJ_TARGET=${PRJ_TARGET}

tic_tac_toe: pragmarc
	cd demo/tic_tac_toe && $(BUILDER) -Ptic_tac_toe.gpr -XPRJ_TARGET=${PRJ_TARGET}

db_maker: pragmarc
	cd demo/db_maker && $(BUILDER) -Pdb_maker.gpr -XPRJ_TARGET=${PRJ_TARGET}

tests: gnoga
	cd test && $(BUILDER) -Ptest.gpr -XPRJ_TARGET=${PRJ_TARGET}

tests_ssl: gnoga_secure
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

lib/libsqlite3.a:
	cd deps/simple_components/sqlite-sources && gcc -s -c -O2 -o sqlite3.o sqlite3.c
	cd deps/simple_components/sqlite-sources && ar rcs libsqlite3.a sqlite3.o
	cd deps/simple_components/sqlite-sources && $(MOVE) libsqlite3.a ..$(PATHSEP)..$(PATHSEP)..$(PATHSEP)lib
	cd deps/simple_components/sqlite-sources && $(RM) sqlite3.o

sqlite3: lib/libsqlite3.a

cleanall: clean
	$(MAKE) -C components uninstall
	- cd src && $(RM) gnoga-application.adb
	- cd deps && $(RMS) MultiMarkdown-4
	- cd deps && $(RM) dpm-debug.log
	- cd deps && $(RMS) electron-quick-start
	- cd deps && $(RMS) PragmARC
	- $(RM) bin/multimarkdown
	- cd docs && $(RM) html/*.html
	- cd docs && $(RMS) html/gnoga_rm

clean:
	cd lib && $(UNSET_READONLY)
	- cd lib && $(RM) *.a*
	- cd include && $(RM) *.ad?
	- cd deps/zanyblue && make -C src clean
	- cd deps/PragmARC && $(RM) *.ali *.o
	cd src && $(CLEANER) -r -Pgnoga.gpr
	cd ssl && $(CLEANER) -r -Pgnoga_secure.gpr
	cd deps/simple_components/xpm && $(CLEANER) -r -Pxpm_parser.gpr
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
	- cd demo/connect_four && $(RM) connectfour_messages*
	cd demo/linxtris && $(CLEANER) -Plinxtris.gpr
	cd demo/adaothello && $(CLEANER) -Padaothello.gpr
	cd demo/adaedit && $(CLEANER) -Padaedit.gpr
	-cd demo/password_gen && $(CLEANER) -Ppassword_gen.gpr
	-cd demo/random_int && $(CLEANER) -Prandom_int.gpr
	-cd demo/tic_tac_toe && $(CLEANER) -Ptic_tac_toe.gpr
	- cd deps/zanyblue && make -C src clean
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
	- cd obj && $(RM) gnoga_gtk_window.o
	- cd bin && $(RM) *.db
	- cd bin && $(RM) temp.txt
	- cd bin && $(RM) gnoga-test

bin/multimarkdown:
	- cd deps && git clone git://github.com/fletcher/MultiMarkdown-4.git
	- cd deps/MultiMarkdown-4 && git submodule init
	- cd deps/MultiMarkdown-4 && git submodule update
	- cd deps/MultiMarkdown-4 && make
	- $(MOVE) deps/MultiMarkdown-4/multimarkdown bin/

rm-docs: gnoga
	gnatdoc -Psrc/gnoga.gpr --no-subprojects -XDevelopment=Debug -XLegacy=Ada2005 -XAtomic_Access=GCC-long-offsets -XTasking=Multiple -XTraced_objects=Off -XPRJ_TARGET=Unix -XPRJ_BUILD=Debug

html-docs: bin/multimarkdown
	cd docs && ../bin/multimarkdown user_guide.md > html/user_guide.html
	cd docs && ../bin/multimarkdown api_summary.md > html/api_summary.html
	cd docs && ../bin/multimarkdown native_mac_apps.md > html/native_mac_apps.html
	cd docs && ../bin/multimarkdown native_gtk_apps.md > html/native_gtk_apps.html

gps:
	gps -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS} &
