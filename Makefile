GPRCHECK=$(shell gprbuild --version)
TARGET=$(shell gcc -dumpmachine)
CWD=$(CURDIR)
PREFIX=$(CWD)/inst_folder

# Specify build mode "Debug", "Release" or "Profile"
BUILD_MODE=Debug

ifeq ($(strip $(findstring GPRBUILD, $(GPRCHECK))),GPRBUILD)
	BUILDER=gprbuild -p
	INSTALLER=gprinstall -p -f
	CLEANER=gprclean
	PRETTY_PRINTER=gnatpp
	STUDIO=gnatstudio
else
	BUILDER=gnatmake -p
	INSTALLER=gprinstall -p -f
	CLEANER=gnatclean
	PRETTY_PRINTER=gnatpp
	STUDIO=gnatstudio
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
ifeq ($(strip $(findstring windows, $(TARGET))),windows)
	PRJ_TARGET=Windows
else
ifeq ($(strip $(findstring freebsd, $(TARGET))),freebsd)
	PRJ_TARGET=FreeBSD
else
ifeq ($(strip $(findstring linux, $(TARGET))),linux)
	PRJ_TARGET=Linux
else
	PRJ_TARGET=UNIX
endif
endif
endif
endif
endif
endif

ifeq ($(strip $(findstring x86, $(TARGET))),x86)
	PRJ_ARCH=x86_64
else
ifeq ($(strip $(findstring i686, $(TARGET))),i686)
	PRJ_ARCH=i686
else
	PRJ_ARCH=auto
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
	PATHSEP=\\
	BUILD_SQLITE3=sqlite3
else
	COPY=cp -p
	MOVE=mv
	MKDIR=mkdir -p
	RM=rm -f
	RMS=rm -rf
	PATHSEP=/
	BUILD_SQLITE3=
endif

help :
	@echo "-----------------------------------------------------------------------------"
	@echo "--                                                                         --"
	@echo "-- Usage: make <entry>                                                     --"
	@echo "--                                                                         --"
	@echo "-- <entry> ::= help        -- print this message                           --"
	@echo "--         | all           -- build gnoga with demos and all dependencies  --"
	@echo "--         | install       -- install gnoga                                --"
	@echo "--         | uninstall     -- uninstall gnoga                              --"
	@echo "--         | demo          -- build all demos                              --"
	@echo "--         | tutorials     -- build all tutorials                          --"
	@echo "--         | gnoga_tools   -- build all tools                              --"
	@echo "--         | tests         -- build all tests                              --"
	@echo "--         | clean         -- clean build files                            --"
	@echo "--         | clean_all     -- clean build files and deps                   --"
	@echo "--         | rm-docs       -- build reference manual                       --"
	@echo "--         | html-docs     -- build html docs                              --"
	@echo "--         | studio        -- launch GNAT Studio with gnoga environnement  --"
	@echo "--         | check_rules   -- check gnoga with AdaControl                  --"
	@echo "--         | gnoga-config  -- create script with gnoga compilation options --"
	@echo "--                                                                         --"
	@echo "-- Configurable variables:                                                 --"
	@echo "--                                                                         --"
	@echo "--         PREFIX           = $(PREFIX)"
	@echo "--         GPRCHECK         = $(GPRCHECK)"
	@echo "--         TARGET           = $(TARGET)"
	@echo "--         BUILDER          = $(BUILDER)"
	@echo "--         PRJ_TARGET       = $(PRJ_TARGET)"
	@echo "--         BUILD_OS         = $(BUILD_OS)"
	@echo "--         BUILD_MODE       = $(BUILD_MODE)"
	@echo "--                                                                         --"
	@echo "-----------------------------------------------------------------------------"

SC_OPTIONS=-Xarch=${PRJ_ARCH} -XSC_OS=${PRJ_TARGET} -XDevelopment=${BUILD_MODE}
GN_OPTIONS=-XPRJ_BUILD=${BUILD_MODE} -XPRJ_TARGET=${PRJ_TARGET} ${SC_OPTIONS}
ifeq ($(PRJ_TARGET),Windows)
	ZB_MAKE=BUILD=$(subst Release,Production,${BUILD_MODE}) OS=Windows_NT
	ZB_OPTIONS=-XBUILD=$(subst Release,Production,${BUILD_MODE}) -XOS=Windows_NT
else
	ZB_MAKE=BUILD=$(subst Release,Production,${BUILD_MODE}) OS=unix
	ZB_OPTIONS=-XBUILD=$(subst Release,Production,${BUILD_MODE}) -XOS=unix
endif

all: deps $(BUILD_SQLITE3) basic_components gnoga gnoga_tools demo tutorials

basic_components:
	$(MAKE) -C components

# Mandatory dependances for Gnoga and demos
deps : simple_components uxstrings zanyblue pragmarc

simple_components:
	$(BUILDER) -P deps/simple_components/lib_components.gpr $(SC_OPTIONS)

sqlite3:
	- $(MKDIR) lib
	cd deps/simple_components/sqlite-sources && gcc -s -c -O2 -o sqlite3.o sqlite3.c
	cd deps/simple_components/sqlite-sources && ar rcs libsqlite3.a sqlite3.o
	cd deps/simple_components/sqlite-sources && $(MOVE) libsqlite3.a ..$(PATHSEP)..$(PATHSEP)..$(PATHSEP)lib
	cd deps/simple_components/sqlite-sources && $(RM) sqlite3.o

zanyblue:
	- cd deps/zanyblue/src && "$(MAKE)" $(ZB_MAKE) APPDIRS="zbmcompile zbinfo"

pragmarc:
	$(BUILDER) -P deps/PragmARC/lib_pragmarc.gpr

uxstrings:
	$(BUILDER) -P deps/uxstrings/lib_uxstrings.gpr

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

.IGNORE: bin/multimarkdown
bin/multimarkdown:
	cd deps && git clone git://github.com/fletcher/MultiMarkdown-4.git
	cd deps/MultiMarkdown-4 && git submodule init
	cd deps/MultiMarkdown-4 && git submodule update
	cd deps/MultiMarkdown-4 && "$(MAKE)"
	$(MKDIR) bin
	$(MOVE) deps/MultiMarkdown-4/multimarkdown bin/

gnoga:
	$(BUILDER) -P src/gnoga.gpr $(GN_OPTIONS)

gnoga_secure:
	$(BUILDER) -P ssl/gnoga_secure.gpr $(GN_OPTIONS)

gnoga_tools:
	$(BUILDER) -P tools/tools_agg.gpr $(GN_OPTIONS)

# Install Gnoga and deps
install: install_deps install_gnoga

# Install deps
install_deps: deps $(BUILD_SQLITE3)
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=components deps/simple_components/lib_components.gpr $(SC_OPTIONS)
	- $(COPY) lib$(PATHSEP)libsqlite3.a "$(PREFIX)$(PATHSEP)lib"
	- $(MAKE) -C deps/zanyblue/src $(ZB_MAKE) INSTALL_DIR="$(PREFIX)" install
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=pragmarc deps/PragmARC/lib_pragmarc.gpr
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=uxstrings deps/uxstrings/lib_uxstrings.gpr

# Install Gnoga without deps
install_gnoga: basic_components gnoga gnoga_tools
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga src/gnoga.gpr $(GN_OPTIONS)
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga --mode=usage tools/tools_agg.gpr $(GN_OPTIONS)
	$(MAKE) -C components INSTALL_DIR="$(PREFIX)"/share/gnoga

# Build and install Gnoga standalone with deps already installed in PREFIX
install_gnoga_sa:
	$(BUILDER) -P gpr_sa/gnoga.gpr $(GN_OPTIONS) -aP "$(PREFIX)"/share/gpr
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga gpr_sa/gnoga.gpr $(GN_OPTIONS) -aP "$(PREFIX)"/share/gpr
	$(BUILDER) -P tools/tools_agg_ext.gpr $(GN_OPTIONS) -XPREFIX="$(PREFIX)"
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga --mode=usage tools/tools_agg_ext.gpr $(GN_OPTIONS) -XPREFIX="$(PREFIX)"
	$(MAKE) -C components INSTALL_DIR="$(PREFIX)"/share/gnoga

.IGNORE: uninstall
uninstall:
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=components --uninstall lib_components.gpr
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=pragmarc --uninstall pragmarc.gpr
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=uxstrings --uninstall lib_uxstrings.gpr
	$(INSTALLER) --prefix="$(PREFIX)" --install-name=gnoga --uninstall gnoga.gpr
	$(MAKE) -C deps/zanyblue/src INSTALL_DIR="$(PREFIX)" uninstall
	$(RM) -fr "$(PREFIX)"/share/gnoga

demo: snake mine_detector connect_four chattanooga adaedit adablog password_gen linxtris random_int adaothello tic_tac_toe leaves db_maker logo localize

snake:
	$(BUILDER) -P demo/demo_agg.gpr $@-main $(GN_OPTIONS)

mine_detector:
	$(BUILDER) -P demo/demo_agg.gpr $@ $(GN_OPTIONS)

chattanooga:
	$(COPY) demo$(PATHSEP)chattanooga$(PATHSEP)glass.ogg html
	$(BUILDER) -P demo/demo_agg.gpr $@-program $(GN_OPTIONS)

adaedit:
	$(BUILDER) -P demo/demo_agg.gpr $@ $(GN_OPTIONS)

adablog:
	$(COPY) demo$(PATHSEP)adablog$(PATHSEP)adablog.css css
	- $(BUILDER) -P demo/demo_agg.gpr $@-main $(GN_OPTIONS)

connect_four: zanyblue
	- cd demo/connect_four && ..$(PATHSEP)..$(PATHSEP)deps$(PATHSEP)zanyblue$(PATHSEP)bin$(PATHSEP)zbmcompile -i -v -G strings connectfour_messages connectfour
	$(BUILDER) -P demo/demo_agg.gpr $@ $(GN_OPTIONS) $(ZB_OPTIONS)

linxtris:
	$(BUILDER) -P demo/demo_agg.gpr $@ $(GN_OPTIONS)
	@echo "usage: bin/linxtris -data_dir demo/linxtris/"

password_gen: pragmarc
	$(BUILDER) -P demo/demo_agg.gpr $@-program $(GN_OPTIONS)

random_int:
	$(BUILDER) -P demo/demo_agg.gpr $@-program $(GN_OPTIONS)

adaothello:
	$(BUILDER) -P demo/demo_agg.gpr othello_game $(GN_OPTIONS)

tic_tac_toe: pragmarc
	$(BUILDER) -P demo/demo_agg.gpr $@-program $(GN_OPTIONS)

leaves:
	$(COPY) demo$(PATHSEP)leaves$(PATHSEP)img$(PATHSEP)*.png img
	$(COPY) demo$(PATHSEP)leaves$(PATHSEP)img$(PATHSEP)*.jpg img
	$(BUILDER) -P demo/demo_agg.gpr $@_main $(GN_OPTIONS)

db_maker: pragmarc
	$(BUILDER) -P demo/demo_agg.gpr movies $(GN_OPTIONS)

logo: zanyblue
	$(COPY) demo$(PATHSEP)logo$(PATHSEP)*.png img
	- cd demo$(PATHSEP)logo && ..$(PATHSEP)..$(PATHSEP)deps$(PATHSEP)zanyblue$(PATHSEP)bin$(PATHSEP)zbmcompile -i -v -G strings logo_messages logo
	$(BUILDER) -P demo/demo_agg.gpr $@-main $(GN_OPTIONS) $(ZB_OPTIONS)

localize:
#	- cd demo$(PATHSEP)localize && ..$(PATHSEP)..$(PATHSEP)deps$(PATHSEP)zanyblue$(PATHSEP)bin$(PATHSEP)zbmcompile -i -v -G strings localize_messages localize
	$(BUILDER) -P demo/demo_agg.gpr $@-main $(GN_OPTIONS) $(ZB_OPTIONS)

tests:
	- $(BUILDER) -k -P test/test_agg.gpr $(GN_OPTIONS)

tests-%:
	- $(BUILDER) -k -P test/test_agg.gpr $(subst tests-,,$@) $(GN_OPTIONS)

tests_ssl: gnoga_secure
	- $(BUILDER) -P test_ssl/test_ssl.gpr $(GN_OPTIONS)

tutorials:
	- $(BUILDER) -P tutorial/tutorial_agg.gpr $(GN_OPTIONS)

.IGNORE: clean_all
clean_all: clean clean_deps
	$(MAKE) -C components uninstall
	$(RMS) css
	$(RMS) html
	$(RMS) img
	$(RMS) js
	$(RMS) upload
	$(RM) docs$(PATHSEP)html$(PATHSEP)*.html
	$(RMS) docs$(PATHSEP)html$(PATHSEP)gnoga_rm

.IGNORE: clean_deps
clean_deps:
	$(CLEANER) -P deps/simple_components/lib_components.gpr
	$(CLEANER) -P deps/uxstrings/lib_uxstrings.gpr
	$(CLEANER) -P deps/PragmARC/lib_pragmarc.gpr
	cd deps/zanyblue && "$(MAKE)" -C src clean
	$(RMS) deps$(PATHSEP)MultiMarkdown-4
	$(RMS) deps$(PATHSEP)electron-quick-start
	$(RM) bin$(PATHSEP)multimarkdown
	$(RM) lib$(PATHSEP)libsqlite3.a

.IGNORE: clean
clean: clean_demo clean_tutorials clean_tests
	$(CLEANER) -P src/gnoga.gpr
	$(CLEANER) -P ssl/gnoga_secure.gpr
	$(CLEANER) -P tools/tools_agg.gpr
	$(RM) bin$(PATHSEP)*.db
	$(RM) bin$(PATHSEP)temp.txt
	$(RM) obj$(PATHSEP)gnoga_gtk_window.o

.IGNORE: clean_demo
clean_demo:
	$(CLEANER) -P demo/demo_agg.gpr

.IGNORE: clean_tutorials
clean_tutorials:
	$(CLEANER) -P tutorial/tutorial_agg.gpr

.IGNORE: clean_tests
clean_tests:
	$(CLEANER) -P test/test_agg.gpr

rm-docs: gnoga
	gnatdoc -P src/gnoga.gpr --enable-build --no-subprojects $(GN_OPTIONS)

html-docs: bin/multimarkdown
	cd docs && ../bin/multimarkdown user_guide.md > html/user_guide.html
	cd docs && ../bin/multimarkdown api_summary.md > html/api_summary.html
	cd docs && ../bin/multimarkdown native_mac_apps.md > html/native_mac_apps.html
	cd docs && ../bin/multimarkdown native_gtk_apps.md > html/native_gtk_apps.html

studio:
ifeq ($(BUILD_OS),Windows)
	cmd /C start /B $(STUDIO) -P src/gnoga.gpr $(GN_OPTIONS) $(ZB_OPTIONS)
else
	$(STUDIO) -P src/gnoga.gpr $(GN_OPTIONS) $(ZB_OPTIONS) &
endif

# Use AdaControl to check rules/gnoga.aru
# Make sure AdaControl utilities are in your PATH
# https://sourceforge.net/projects/adacontrol
check_rules:
	$(BUILDER) -c -f -P src/gnoga.gpr -gnatct $(GN_OPTIONS)
	adactl -f rules/gnoga.aru -p src/gnoga.gpr @rules/gnoga.txt -s -S 3 -- -FT -Tobj

gnoga-config:
ifeq ($(BUILD_OS),Windows)
	- $(MKDIR) bin
	echo %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9 $(GN_OPTIONS) $(ZB_OPTIONS) -aP$(CWD)$(PATHSEP)src -aP$(CWD)$(PATHSEP)deps$(PATHSEP)zanyblue$(PATHSEP)src -aP$(CWD)$(PATHSEP)deps$(PATHSEP)PragmARC > bin$(PATHSEP)gnoga-config.cmd
else
	- $(MKDIR) bin
	echo echo $(GN_OPTIONS) $(ZB_OPTIONS) -aP$(CWD)$(PATHSEP)src -aP$(CWD)$(PATHSEP)deps$(PATHSEP)zanyblue$(PATHSEP)src -aP$(CWD)$(PATHSEP)deps$(PATHSEP)PragmARC > bin$(PATHSEP)gnoga-config
	chmod +x bin$(PATHSEP)gnoga-config
endif

pretty_print_gnoga:
	$(PRETTY_PRINTER) -P src/gnoga.gpr $(GN_OPTIONS)
pretty_print_test:
	GPR_PROJECT_PATH=$(CWD)$(PATHSEP)src $(PRETTY_PRINTER) -P test/test.gpr $(GN_OPTIONS)
pretty_print_demo:
	for i in snake mine_detector connect_four chattanooga adaedit adablog password_gen linxtris random_int adaothello tic_tac_toe leaves db_maker logo localize; do (GPR_PROJECT_PATH=$(CWD)$(PATHSEP)src:$(CWD)$(PATHSEP)deps$(PATHSEP)PragmARC:$(CWD)$(PATHSEP)deps$(PATHSEP)zanyblue$(PATHSEP)src $(PRETTY_PRINTER) -P demo/$$i/$$i.gpr $(GN_OPTIONS)); done
pretty_print_tutorial:
	for i in 01 02 03 04 05 06 07 08 09 10 11; do (GPR_PROJECT_PATH=$(CWD)$(PATHSEP)src:$(CWD)$(PATHSEP)deps$(PATHSEP)PragmARC:$(CWD)$(PATHSEP)deps$(PATHSEP)zanyblue$(PATHSEP)src $(PRETTY_PRINTER) -P tutorial/tutorial-$$i/tutorial_$$i.gpr $(GN_OPTIONS)); done
