PREFIX=$(shell echo $(dir $(shell which gnatls)) | sed "s:/cygdrive/\\(.\\):\\1\\::" )..
GPRCHECK=$(shell gprbuild --version)
TARGET=$(shell gcc -dumpmachine)
CWD=$(shell pwd)
DEPS_GPR_FLAGS=-aP$(CWD)/build/share/gpr -aP$(CWD)/build/lib/gnat

ATOMIC_ACCESS=GCC-long-offsets
#if using GNAT GPL prior to 2014 or earlier on a 32bit host (Windows or Linux)
#you need to change this to:
#
#ATOMIC_ACCESS=GCC-built-ins

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

ifdef ComSpec
	COPY=copy
	MOVE=move
	MKDIR=mkdir
	RM=del
	RMS=del /S /Q
	PATHSEP2=\\
else
	COPY=cp
	MOVE=mv
	MKDIR=mkdir
	RM=rm -f
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

all: deps $(BUILD_SQLITE3) setup basic_components gnoga gnoga_tools demo tutorials

setup:
	$(MAKE) -C src

basic_components:
	$(MAKE) -C components

# Mandatory dependances
deps : simple_components

simple_components:
	$(BUILDER) -P deps/simple_components/lib_components.gpr
	$(INSTALLER) --prefix=$(CWD)/build --install-name=components deps/simple_components/lib_components.gpr

lib/libsqlite3.a:
	cd deps/simple_components/sqlite-sources && gcc -s -c -O2 -o sqlite3.o sqlite3.c
	cd deps/simple_components/sqlite-sources && ar rcs libsqlite3.a sqlite3.o
	cd deps/simple_components/sqlite-sources && $(MOVE) libsqlite3.a ..$(PATHSEP)..$(PATHSEP)..$(PATHSEP)lib
	cd deps/simple_components/sqlite-sources && $(RM) sqlite3.o

sqlite3: lib/libsqlite3.a

# Zanyblue with DEBUG on
zanyblue:
	cd deps/zanyblue/src && make BUILD=Debug
	cd deps/zanyblue/src && make INSTALL_DIR=$(CWD)/build install

pragmarc:
	$(BUILDER) -P deps/PragmARC/lib_pragmarc.gpr
	$(INSTALLER) --prefix=$(CWD)/build --install-name=pragmarc deps/PragmARC/lib_pragmarc.gpr

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
	- cd deps/MultiMarkdown-4 && make
	- $(MKDIR) bin
	- $(MOVE) deps/MultiMarkdown-4/multimarkdown bin/

# Gnoga with DEBUG on by default
gnoga:
	$(BUILDER) -P src/gnoga.gpr -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS} $(DEPS_GPR_FLAGS)

gnoga_secure:
	$(BUILDER) -P ssl/gnoga_secure.gpr -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS} $(DEPS_GPR_FLAGS)

gnoga_tools:
	$(BUILDER) -P tools/tools.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

# Gnoga build with DEBUG off
release: deps setup basic_components
	$(BUILDER) -P src/gnoga.gpr -XPRJ_BUILD=Release -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS} $(DEPS_GPR_FLAGS)

# Install Gnoga with DEBUG off
install: release gnoga_tools
	$(INSTALLER) --prefix=$(PREFIX) --install-name=components deps/simple_components/lib_components.gpr
# TODO libsqlite3
	cd deps/zanyblue && $(INSTALLER) --prefix=$(PREFIX) --install-name=zanyblue src/zblib.gpr $(ZBGNATXDEFS)
	cd deps/zanyblue && $(INSTALLER) --prefix=$(PREFIX) --install-name=zanyblue -aP src -aP lib/gnat src/zbmcompile/zbmcompile.gpr $(ZBGNATXDEFS)
	$(INSTALLER) --prefix=$(PREFIX) --install-name=pragmarc deps/PragmARC/lib_pragmarc.gpr
	cd src && $(INSTALLER) --prefix=$(PREFIX) --install-name=gnoga gnoga.gpr -XPRJ_BUILD=Release -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS} $(DEPS_GPR_FLAGS)
	cd tools && $(INSTALLER) --prefix=$(PREFIX) --install-name=gnoga --mode=usage tools.gpr -XPRJ_BUILD=Release -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS} $(DEPS_GPR_FLAGS)

# Install Gnoga with DEBUG on
install_debug:
	$(INSTALLER) --prefix=$(PREFIX) --install-name=components deps/simple_components/lib_components.gpr
# TODO libsqlite3
	cd deps/zanyblue && $(INSTALLER) --prefix=$(PREFIX) --install-name=zanyblue src/zblib.gpr $(ZBGNATXDEFS)
	cd deps/zanyblue && $(INSTALLER) --prefix=$(PREFIX) --install-name=zanyblue -aP src -aP lib/gnat src/zbmcompile/zbmcompile.gpr $(ZBGNATXDEFS)
	$(INSTALLER) --prefix=$(PREFIX) --install-name=pragmarc deps/PragmARC/lib_pragmarc.gpr
	cd src && $(INSTALLER) --prefix=$(PREFIX) --install-name=gnoga gnoga.gpr -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS} $(DEPS_GPR_FLAGS)
	cd tools && $(INSTALLER) --prefix=$(PREFIX) --install-name=gnoga --mode=usage tools.gpr -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS} $(DEPS_GPR_FLAGS)

uninstall:
	- gprinstall -f --prefix=$(PREFIX) --uninstall lib_components.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall zblib.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall zbmcompile.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall pragmarc.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall gnoga.gpr
	- gprinstall -f --prefix=$(PREFIX) --uninstall tools.gpr

demo: snake mine_detector connect_four chattanooga adaedit adablog password_gen linxtris random_int adaothello tic_tac_toe

snake:
	cd demo/snake && $(BUILDER) -Psnake.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

mine_detector:
	cd demo/mine_detector && $(BUILDER) -Pmine_detector.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

chattanooga:
	cd demo/chattanooga && $(BUILDER) -Pchattanooga.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

adaedit:
	- cd demo/adaedit && $(BUILDER) -Padaedit.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

adablog:
	- cd demo/adablog && $(BUILDER) -Padablog.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

connect_four: zanyblue
	cd demo/connect_four && ../../deps/zanyblue/bin/zbmcompile -i -v -G strings connectfour_messages connectfour
	cd demo/connect_four && $(BUILDER) -Pconnect_four.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

linxtris:
	cd demo/linxtris && $(BUILDER) -Plinxtris.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	@echo "usage: bin/linxtris -data_dir demo/linxtris/"

password_gen: pragmarc
	cd demo/password_gen && $(BUILDER) -Ppassword_gen.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

random_int:
	cd demo/random_int && $(BUILDER) -Prandom_int.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

adaothello:
	cd demo/adaothello && $(BUILDER) -Padaothello.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

tic_tac_toe:
	cd demo/tic_tac_toe && $(BUILDER) -Ptic_tac_toe.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

tests:
	cd test && $(BUILDER) -Ptest.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

tests_ssl: gnoga_secure
	cd test_ssl && $(BUILDER) -Ptest_ssl.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

tutorials:
	cd tutorial/tutorial-01 && $(BUILDER) -Ptutorial_01.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-02 && $(BUILDER) -Ptutorial_02.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-03 && $(BUILDER) -Ptutorial_03.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-04 && $(BUILDER) -Ptutorial_04.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-05 && $(BUILDER) -Ptutorial_05.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-06 && $(BUILDER) -Ptutorial_06.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-07 && $(BUILDER) -Ptutorial_07.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-08 && $(BUILDER) -Ptutorial_08.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-09 && $(BUILDER) -Ptutorial_09.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-10 && $(BUILDER) -Ptutorial_10.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-11 && $(BUILDER) -Ptutorial_11.gpr -XPRJ_TARGET=${PRJ_TARGET} $(DEPS_GPR_FLAGS)

clean_all: clean clean_deps
	$(MAKE) -C components uninstall
	$(MAKE) -C src clean
	cd docs && $(RM) html/*.html
	cd docs && $(RMS) html/gnoga_rm

clean_deps:
	$(CLEANER) -P deps/simple_components/lib_components.gpr
	$(CLEANER) -P deps/PragmARC/lib_pragmarc.gpr
	cd deps/zanyblue && make -C src clean
	$(RMS) build
	cd deps && $(RMS) MultiMarkdown-4
	cd deps && $(RMS) electron-quick-start
	$(RM) bin/multimarkdown
	$(RM) lib/libsqlite3.a

clean: clean_demo clean_tutorials clean_tests
	cd src && $(CLEANER) -Pgnoga.gpr $(DEPS_GPR_FLAGS)
	cd ssl && $(CLEANER) -Pgnoga_secure.gpr $(DEPS_GPR_FLAGS)
	cd tools && $(CLEANER) -Ptools.gpr $(DEPS_GPR_FLAGS)
	$(RM) bin/*.db
	$(RM) bin/temp.txt
	$(RM) obj/gnoga_gtk_window.o

clean_demo:
	cd demo/snake && $(CLEANER) -P snake.gpr $(DEPS_GPR_FLAGS)
	cd demo/mine_detector && $(CLEANER) -P mine_detector.gpr $(DEPS_GPR_FLAGS)
	cd demo/chattanooga && $(CLEANER) -P chattanooga.gpr $(DEPS_GPR_FLAGS)
	cd demo/adaedit && $(CLEANER) -P adaedit.gpr $(DEPS_GPR_FLAGS)
	cd demo/adablog && $(CLEANER) -P adablog.gpr $(DEPS_GPR_FLAGS)
	cd demo/connect_four && $(CLEANER) -P connect_four.gpr $(DEPS_GPR_FLAGS)
	cd demo/connect_four && $(RM) connectfour_messages*
	cd demo/linxtris && $(CLEANER) -P linxtris.gpr $(DEPS_GPR_FLAGS)
	cd demo/password_gen && $(CLEANER) -P password_gen.gpr $(DEPS_GPR_FLAGS)
	cd demo/random_int && $(CLEANER) -P random_int.gpr $(DEPS_GPR_FLAGS)
	cd demo/adaothello && $(CLEANER) -P adaothello.gpr $(DEPS_GPR_FLAGS)
	cd demo/tic_tac_toe && $(CLEANER) -P tic_tac_toe.gpr $(DEPS_GPR_FLAGS)

clean_tutorials:
	cd tutorial/tutorial-01 && $(CLEANER) -P tutorial_01.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-02 && $(CLEANER) -P tutorial_02.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-03 && $(CLEANER) -P tutorial_03.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-04 && $(CLEANER) -P tutorial_04.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-05 && $(CLEANER) -P tutorial_05.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-06 && $(CLEANER) -P tutorial_06.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-07 && $(CLEANER) -P tutorial_07.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-08 && $(CLEANER) -P tutorial_08.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-09 && $(CLEANER) -P tutorial_09.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-10 && $(CLEANER) -P tutorial_10.gpr $(DEPS_GPR_FLAGS)
	cd tutorial/tutorial-11 && $(CLEANER) -P tutorial_11.gpr $(DEPS_GPR_FLAGS)

clean_tests:
	cd test && $(CLEANER) -P test.gpr $(DEPS_GPR_FLAGS)
	cd test_ssl && $(CLEANER) -P test_ssl.gpr $(DEPS_GPR_FLAGS)
	cd test/tickets/001 && $(CLEANER) -P test.gpr $(DEPS_GPR_FLAGS)
	cd test/tickets/002 && $(CLEANER) -P test.gpr $(DEPS_GPR_FLAGS)
	cd test/tickets/005 && $(CLEANER) -P test.gpr $(DEPS_GPR_FLAGS)
	cd test/tickets/007 && $(CLEANER) -P test.gpr $(DEPS_GPR_FLAGS)
	cd test/tickets/011 && $(CLEANER) -P test.gpr $(DEPS_GPR_FLAGS)
	cd test/tickets/019 && $(CLEANER) -P test.gpr $(DEPS_GPR_FLAGS)

rm-docs: gnoga
	GPR_PROJECT_PATH=$(DEPS_GPR_FLAGS) gnatdoc -P src/gnoga.gpr --no-subprojects -XAtomic_Access=${ATOMIC_ACCESS} -XPRJ_TARGET=${PRJ_TARGET}

html-docs: bin/multimarkdown
	cd docs && ../bin/multimarkdown user_guide.md > html/user_guide.html
	cd docs && ../bin/multimarkdown api_summary.md > html/api_summary.html
	cd docs && ../bin/multimarkdown native_mac_apps.md > html/native_mac_apps.html
	cd docs && ../bin/multimarkdown native_gtk_apps.md > html/native_gtk_apps.html

gps:
	GPR_PROJECT_PATH=$(DEPS_GPR_FLAGS) gps -XPRJ_TARGET=${PRJ_TARGET} -XAtomic_Access=${ATOMIC_ACCESS} &
