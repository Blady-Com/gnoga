GPRCHECK=$(shell gprbuild --version)
TARGET=$(shell gcc -dumpmachine)
CWD=$(CURDIR)
PREFIX=$(CWD)/inst_folder

ATOMIC_ACCESS=GCC-long-offsets
# If using GNAT GPL prior to 2014 or earlier on a 32bit host (Windows or Linux)
# You need to change this to:
# ATOMIC_ACCESS=GCC-built-ins
# If using GNAT which supports pragma Atomic for 64-bit scalar variables for GNAT hosted on a 64-bit OS.
# You can change this to:
# ATOMIC_ACCESS=Pragma-atomic

ifeq ($(strip $(findstring GPRBUILD, $(GPRCHECK))),GPRBUILD)
	BUILDER=gprbuild -p --target=${TARGET}
	INSTALLER=gprinstall -p -f --target=${TARGET}
	CLEANER=gprclean --target=${TARGET}
else
	BUILDER=gnatmake -p
	INSTALLER=gprinstall -p -f
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

ifeq ($(shell echo "check_quotes"),"check_quotes")
   BUILD_OS := Windows
else
   BUILD_OS := UnixLike
endif

ifeq ($(BUILD_OS),Windows)
	COPY=copy
	MOVE=move
	MKDIR=mkdir
	RM=del
	RMS=del /S /Q
	PATHSEP2=\\
else
	COPY=cp -p
	MOVE=mv
	MKDIR=mkdir
	RM=rm -f
	RMS=rm -rf
	PATHSEP2=/
endif

PATHSEP=$(strip $(PATHSEP2))

ifeq (${BUILD_OS}, Windows)
	SET_READONLY=attrib +R
	UNSET_READONLY=attrib -R
	BUILD_SQLITE3=sqlite3
else
	SET_READONLY=chmod -w *
	UNSET_READONLY=chmod +w *
	BUILD_SQLITE3=
endif

ifeq ($(BUILD_OS),Windows)
	GPR_PROJECT_PATH_SEP=;
else
	GPR_PROJECT_PATH_SEP=:
endif
export GPR_PROJECT_PATH=$(CWD)/build/share/gpr$(GPR_PROJECT_PATH_SEP)$(CWD)/build/lib/gnat
# If Simple Components, Zanyblue or PragmArc libs are already existing in your environnement
# Put their location in GPR_PROJECT_PATH and comment the previous line

help :
	@echo "-----------------------------------------------------------------------------"
	@echo "--                                                                         --"
	@echo "-- make <entry>                                                            --"
	@echo "--                                                                         --"
	@echo "-- <entry> ::= help        -- print this message                           --"
	@echo "--         | all           -- build gnoga and all dependencies (debug mode)--"
	@echo "--         | release       -- build gnoga in release mode                  --"
	@echo "--         | install       -- install gnoga release mode                   --"
	@echo "--         | install_debug -- install gnoga debug mode                     --"
	@echo "--         | uninstall     -- uninstall gnoga                              --"
	@echo "--         | demo          -- build all demos                              --"
	@echo "--         | tutorials     -- build all tutorials                          --"
	@echo "--         | gnoga_tools   -- build all tools                              --"
	@echo "--         | tests         -- build all tests                              --"
	@echo "--         | clean         -- clean build files                            --"
	@echo "--         | clean_all     -- clean build files and deps                   --"
	@echo "--         | rm-docs       -- build reference manual                       --"
	@echo "--         | html-docs     -- build html docs                              --"
	@echo "--         | gps           -- launch GPS with gnoga environnement          --"
	@echo "--         | check_rules   -- check gnoga with AdaControl                  --"
	@echo "--                                                                         --"
	@echo "--         PREFIX           = $(PREFIX)                                    --"
	@echo "--         GPRCHECK         = $(GPRCHECK)                                  --"
	@echo "--         TARGET           = $(TARGET)                                    --"
	@echo "--         GPR_PROJECT_PATH = $(GPR_PROJECT_PATH)                          --"
	@echo "--         ATOMIC_ACCESS    = $(ATOMIC_ACCESS)                             --"
	@echo "--         BUILDER          = $(BUILDER)                                   --"
	@echo "--         PRJ_TARGET       = $(PRJ_TARGET)                                --"
	@echo "--         BUILD_OS         = $(BUILD_OS)                                  --"
	@echo "--                                                                         --"
	@echo "-----------------------------------------------------------------------------"

all: deps $(BUILD_SQLITE3) setup basic_components gnoga gnoga_tools demo tutorials

setup:
	$(MAKE) -C src

basic_components:
	$(MAKE) -C components

# Mandatory dependances
deps : simple_components

simple_components:
	$(BUILDER) -P deps/simple_components/lib_components.gpr -XAtomic_Access=${ATOMIC_ACCESS} -XLegacy=Ada2012
	$(INSTALLER) --prefix="$(CWD)/build" --install-name=components deps/simple_components/lib_components.gpr -XAtomic_Access=${ATOMIC_ACCESS} -XLegacy=Ada2012

lib/libsqlite3.a:
	- $(MKDIR) lib
	cd deps/simple_components/sqlite-sources && gcc -s -c -O2 -o sqlite3.o sqlite3.c
	cd deps/simple_components/sqlite-sources && ar rcs libsqlite3.a sqlite3.o
	cd deps/simple_components/sqlite-sources && $(MOVE) libsqlite3.a ..$(PATHSEP)..$(PATHSEP)..$(PATHSEP)lib
	cd deps/simple_components/sqlite-sources && $(RM) sqlite3.o

sqlite3: lib/libsqlite3.a

# Zanyblue with DEBUG on
zanyblue:
	- cd deps/zanyblue/src && "$(MAKE)" BUILD=Debug APPDIRS="zbmcompile zbinfo"
	- cd deps/zanyblue/src && "$(MAKE)" INSTALL_DIR="$(CWD)/build" APPDIRS="zbmcompile zbinfo" install

pragmarc:
	$(BUILDER) -P deps/PragmARC/lib_pragmarc.gpr
	$(INSTALLER) --prefix="$(CWD)/build" --install-name=pragmarc deps/PragmARC/lib_pragmarc.gpr

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

bin/multimarkdown:
	- cd deps && git clone git://github.com/fletcher/MultiMarkdown-4.git
	- cd deps/MultiMarkdown-4 && git submodule init
	- cd deps/MultiMarkdown-4 && git submodule update
	- cd deps/MultiMarkdown-4 && "$(MAKE)"
	- $(MKDIR) bin
	- $(MOVE) deps/MultiMarkdown-4/multimarkdown bin/

# Gnoga with DEBUG on by default
gnoga:
	$(BUILDER) -P src/gnoga.gpr -XPRJ_TARGET=${PRJ_TARGET}

gnoga_secure:
	$(BUILDER) -P ssl/gnoga_secure.gpr -XPRJ_TARGET=${PRJ_TARGET}

gnoga_tools:
	$(BUILDER) -P tools/tools.gpr -XPRJ_TARGET=${PRJ_TARGET}

# Gnoga build with DEBUG off
release: deps $(BUILD_SQLITE3) setup basic_components
	$(BUILDER) -P src/gnoga.gpr -XPRJ_BUILD=Release -XPRJ_TARGET=${PRJ_TARGET}

# Install Gnoga and deps with DEBUG off
install: release gnoga_tools
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=components deps/simple_components/lib_components.gpr -XAtomic_Access=${ATOMIC_ACCESS} -XLegacy=Ada2012
	- $(COPY) lib$(PATHSEP)libsqlite3.a "$(PREFIX)$(PATHSEP)lib"
	- $(MAKE) -C deps/zanyblue/src INSTALL_DIR="$(PREFIX)" APPDIRS="zbmcompile zbinfo" install
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=pragmarc deps/PragmARC/lib_pragmarc.gpr
	cd src && $(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga gnoga.gpr -XPRJ_BUILD=Release -XPRJ_TARGET=${PRJ_TARGET}
	cd tools && $(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga --mode=usage tools.gpr -XPRJ_BUILD=Release -XPRJ_TARGET=${PRJ_TARGET}

# Install Gnoga and deps with DEBUG on
install_debug:
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=components deps/simple_components/lib_components.gpr -XAtomic_Access=${ATOMIC_ACCESS} -XLegacy=Ada2012
	- $(COPY) lib$(PATHSEP)libsqlite3.a "$(PREFIX)$(PATHSEP)lib"
	- $(MAKE) -C deps/zanyblue/src INSTALL_DIR="$(PREFIX)" BUILD=Debug APPDIRS="zbmcompile zbinfo" install
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=pragmarc deps/PragmARC/lib_pragmarc.gpr
	cd src && $(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga gnoga.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tools && $(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga --mode=usage tools.gpr -XPRJ_TARGET=${PRJ_TARGET}

# Install Gnoga alone with DEBUG on
install_gnoga_debug:
	cd src && $(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga gnoga.gpr -XPRJ_TARGET=${PRJ_TARGET}
	cd tools && $(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga --mode=usage tools.gpr -XPRJ_TARGET=${PRJ_TARGET}

uninstall:
	- $(INSTALLER) --prefix="$(PREFIX)" --install-name=components --uninstall lib_components.gpr
	- $(INSTALLER) --prefix="$(PREFIX)" --install-name=pragmarc --uninstall pragmarc.gpr
	- $(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga --uninstall gnoga.gpr
	- $(RM) "$(PREFIX)$(PATHSEP)bin$(PATHSEP)zbinfo"
	- $(RM) "$(PREFIX)$(PATHSEP)bin$(PATHSEP)zbmcompile"
	- $(RMS) "$(PREFIX)$(PATHSEP)include$(PATHSEP)zanyblue"
	- $(RMS) "$(PREFIX)$(PATHSEP)lib$(PATHSEP)zanyblue"
	- $(RM) "$(PREFIX)$(PATHSEP)lib$(PATHSEP)gnat$(PATHSEP)zanyblue.gpr"
	- $(RM) "$(PREFIX)$(PATHSEP)lib$(PATHSEP)libzanyblue.a"
	- $(RM) "$(PREFIX)$(PATHSEP)lib$(PATHSEP)libsqlite3.a"

demo: snake mine_detector connect_four chattanooga adaedit adablog password_gen linxtris random_int adaothello tic_tac_toe leaves db_maker

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
	- cd demo/connect_four && ..$(PATHSEP)..$(PATHSEP)deps$(PATHSEP)zanyblue$(PATHSEP)bin$(PATHSEP)zbmcompile -i -v -G strings connectfour_messages connectfour
	- cd demo/connect_four && $(BUILDER) -Pconnect_four.gpr -XPRJ_TARGET=${PRJ_TARGET}

linxtris:
	cd demo/linxtris && $(BUILDER) -Plinxtris.gpr -XPRJ_TARGET=${PRJ_TARGET}
	@echo "usage: bin/linxtris -data_dir demo/linxtris/"

password_gen: pragmarc
	cd demo/password_gen && $(BUILDER) -Ppassword_gen.gpr -XPRJ_TARGET=${PRJ_TARGET}

random_int:
	cd demo/random_int && $(BUILDER) -Prandom_int.gpr -XPRJ_TARGET=${PRJ_TARGET}

adaothello:
	cd demo/adaothello && $(BUILDER) -Padaothello.gpr -XPRJ_TARGET=${PRJ_TARGET}

tic_tac_toe:
	cd demo/tic_tac_toe && $(BUILDER) -Ptic_tac_toe.gpr -XPRJ_TARGET=${PRJ_TARGET}

leaves:
	cd demo/leaves && $(BUILDER) -Pleaves.gpr -XPRJ_TARGET=${PRJ_TARGET}

db_maker:
	cd demo/db_maker && $(BUILDER) -Pdb_maker.gpr -XPRJ_TARGET=${PRJ_TARGET}

tests:
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

clean_all: clean clean_deps
	- $(MAKE) -C components uninstall
	- $(MAKE) -C src clean
	- $(RM) docs$(PATHSEP)html$(PATHSEP)*.html
	- $(RMS) docs$(PATHSEP)html$(PATHSEP)gnoga_rm

clean_deps:
	- $(CLEANER) -P deps/simple_components/lib_components.gpr
	- $(CLEANER) -P deps/PragmARC/lib_pragmarc.gpr
	- cd deps/zanyblue && "$(MAKE)" -C src APPDIRS="zbmcompile zbinfo" clean
	- $(RMS) build
	- $(RMS) deps$(PATHSEP)MultiMarkdown-4
	- $(RMS) deps$(PATHSEP)electron-quick-start
	- $(RM) bin$(PATHSEP)multimarkdown
	- $(RM) lib$(PATHSEP)libsqlite3.a

clean: clean_demo clean_tutorials clean_tests
	- cd src && $(CLEANER) -Pgnoga.gpr
	- cd ssl && $(CLEANER) -Pgnoga_secure.gpr
	- cd tools && $(CLEANER) -Ptools.gpr
	- $(RM) bin$(PATHSEP)*.db
	- $(RM) bin$(PATHSEP)temp.txt
	- $(RM) obj$(PATHSEP)gnoga_gtk_window.o

clean_demo:
	- cd demo/snake && $(CLEANER) -P snake.gpr
	- cd demo/mine_detector && $(CLEANER) -P mine_detector.gpr
	- cd demo/chattanooga && $(CLEANER) -P chattanooga.gpr
	- cd demo/adaedit && $(CLEANER) -P adaedit.gpr
	- cd demo/adablog && $(CLEANER) -P adablog.gpr
	- cd demo/connect_four && $(CLEANER) -P connect_four.gpr
	- cd demo/connect_four && $(RM) connectfour_messages*
	- cd demo/linxtris && $(CLEANER) -P linxtris.gpr
	- cd demo/password_gen && $(CLEANER) -P password_gen.gpr
	- cd demo/random_int && $(CLEANER) -P random_int.gpr
	- cd demo/adaothello && $(CLEANER) -P adaothello.gpr
	- cd demo/tic_tac_toe && $(CLEANER) -P tic_tac_toe.gpr
	- cd demo/leaves && $(CLEANER) -P leaves.gpr
	- cd demo/db_maker && $(CLEANER) -P db_maker.gpr

clean_tutorials:
	- cd tutorial/tutorial-01 && $(CLEANER) -P tutorial_01.gpr
	- cd tutorial/tutorial-02 && $(CLEANER) -P tutorial_02.gpr
	- cd tutorial/tutorial-03 && $(CLEANER) -P tutorial_03.gpr
	- cd tutorial/tutorial-04 && $(CLEANER) -P tutorial_04.gpr
	- cd tutorial/tutorial-05 && $(CLEANER) -P tutorial_05.gpr
	- cd tutorial/tutorial-06 && $(CLEANER) -P tutorial_06.gpr
	- cd tutorial/tutorial-07 && $(CLEANER) -P tutorial_07.gpr
	- cd tutorial/tutorial-08 && $(CLEANER) -P tutorial_08.gpr
	- cd tutorial/tutorial-09 && $(CLEANER) -P tutorial_09.gpr
	- cd tutorial/tutorial-10 && $(CLEANER) -P tutorial_10.gpr
	- cd tutorial/tutorial-11 && $(CLEANER) -P tutorial_11.gpr

clean_tests:
	- cd test && $(CLEANER) -P test.gpr
	- cd test_ssl && $(CLEANER) -P test_ssl.gpr
	- cd test/tickets/001 && $(CLEANER) -P test.gpr
	- cd test/tickets/002 && $(CLEANER) -P test.gpr
	- cd test/tickets/005 && $(CLEANER) -P test.gpr
	- cd test/tickets/007 && $(CLEANER) -P test.gpr
	- cd test/tickets/011 && $(CLEANER) -P test.gpr
	- cd test/tickets/019 && $(CLEANER) -P test.gpr

rm-docs: gnoga
	gnatdoc -P src/gnoga.gpr --no-subprojects -XPRJ_TARGET=${PRJ_TARGET}

html-docs: bin/multimarkdown
	cd docs && ../bin/multimarkdown user_guide.md > html/user_guide.html
	cd docs && ../bin/multimarkdown api_summary.md > html/api_summary.html
	cd docs && ../bin/multimarkdown native_mac_apps.md > html/native_mac_apps.html
	cd docs && ../bin/multimarkdown native_gtk_apps.md > html/native_gtk_apps.html

gps:
ifeq ($(BUILD_OS),Windows)
	cmd /C start /B gps -P src/gnoga.gpr -XPRJ_TARGET=${PRJ_TARGET}
else
	gps -P src/gnoga.gpr -XPRJ_TARGET=${PRJ_TARGET} &
endif

# Use AdaControl to check rules/gnoga.aru
# Make sure AdaControl utilities are in your PATH
# https://sourceforge.net/projects/adacontrol
check_rules:
	$(BUILDER) -c -f -P src/gnoga.gpr -gnatct -XPRJ_TARGET=${PRJ_TARGET}
	adactl -f rules/gnoga.aru -p src/gnoga.gpr src/*.ads -- -Tobj
