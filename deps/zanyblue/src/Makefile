#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#
#    * Neither the name of ZanyBlue nor the names of its contributors may
#      be used to endorse or promote products derived from this software
#      without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

#
# Makefile for the ZanyBlue library and apps
#

TOP=..

GNATFLAGS+=-p
ZANYBLUEADB=$(SRCDIR)/root/zanyblue.adb
INCLUDE_SRC_DIRS=root os os/$(OS) parameters text utils
INCLUDE_SRCS=$(foreach d,$(INCLUDE_SRC_DIRS),\
                 $(patsubst $(SRCDIR)/$d/%,%,\
                     $(wildcard $(SRCDIR)/$d/*.ads) \
                     $(wildcard $(SRCDIR)/$d/*.adb) \
                     $(wildcard $(SRCDIR)/$d/*generic*.adb)))
INCLUDE_FILES=$(patsubst %,$(INCDIR)/%,$(INCLUDE_SRCS))
GENERATED=$(INCLUDE_FILES)
APPDIRS=zbmcompile zbtest
APPTARGETS=$(patsubst %,%.app,$(APPDIRS))
CLEAN_DEPS=$(patsubst %,%.clean,$(APPDIRS) root doc test)
CLEAN_FILES+=$(wildcard bin/pylib/*.pyc)
CLEAN_FILES+=$(wildcard bin/pylib/zb/*.pyc)
CLEAN_FILES+=$(wildcard gnatcheck.out)
CLEAN_FILES+=$(wildcard obj/*)
CLEAN_FILES+=$(wildcard ../lib/b~*)
CLEAN_FILES+=$(wildcard ../zanyblue-*)
CLEAN_FILES+=$(wildcard ../doc/*.*)
CLEAN_FILES+=$(wildcard ../doc/.htaccess)
CLEAN_DIRS+=$(wildcard ../doc/zanyblue)
CLEAN_DIRS+=$(STAGEDIR)
CLEAN_DEPS+=../examples.clean

# Include configuration definitions
include $(TOP)/src/mkfile/conf.mk

all: setup library applications

setup: $(ZANYBLUEADB) $(INCDIR) $(INCLUDE_FILES)

library:
	$(GNATMAKE) $(GNATFLAGS) -P zblib.gpr

applications:	$(APPTARGETS)

docs:
	$(MAKE) -C doc

website:
	$(MAKE) -C doc/website

changelog:
	$(SRCDIR)/bin/mkchangelog

distribution: clean all xcheck docs test.clean bundles website ws-bundle

verify-distribution:
	rm -rf $(STAGEDIR)
	mkdir -p $(STAGEDIR)
	tar -C $(STAGEDIR) -xvf $(DISTRIBUTION)
	cd $(STAGEDIR); md5sum --check --quiet */MANIFEST.txt
	cd $(DISTRIBDIR)/src; make distribution

%.app:
	$(MAKE) -C $*

clean::
	$(GNATCLEAN) -P zblib.gpr

clean::	$(CLEAN_DEPS)

%.clean:
	$(MAKE) -C $* clean

check:
	$(MAKE) -C test check

xcheck:
	$(MAKE) -C test xcheck

gcheck:
	$(info Expect 'use of equality operation for float values' in zanyblue-text-generic_floats.adb)
	$(GNATCHECK) -rules -from=admin/gnatcheck.rules -P zblib.gpr

force:

$(ZANYBLUEADB):
	$(MAKE) -C $(SRCDIR)/root

$(INCDIR):
	$(call MKDIR,$(INCDIR))

$(INCDIR)/%:	$(SRCDIR)/root/%
	$(call COPY,$(SRCDIR)/root/$*,$(INCDIR)/$*)

$(INCDIR)/%:	$(SRCDIR)/os/%
	$(call COPY,$(SRCDIR)/os/$*,$(INCDIR)/$*)

$(INCDIR)/%:	$(SRCDIR)/os/$(OS)/%
	$(call COPY,$(SRCDIR)/os/$(OS)/$*,$(INCDIR)/$*)

$(INCDIR)/%:	$(SRCDIR)/parameters/%
	$(call COPY,$(SRCDIR)/parameters/$*,$(INCDIR)/$*)

$(INCDIR)/%:	$(SRCDIR)/text/%
	$(call COPY,$(SRCDIR)/text/$*,$(INCDIR)/$*)

$(INCDIR)/%:	$(SRCDIR)/utils/%
	$(call COPY,$(SRCDIR)/utils/$*,$(INCDIR)/$*)

# Include general rules.
include $(TOP)/src/mkfile/rules.mk

# Generation of "source lines of code" data for Hudson trending.
include $(TOP)/src/mkfile/sloccount.mk

# Include the "gcov" target to build for coverage runs.
include $(TOP)/src/mkfile/gcov.mk

# Include the "gprof" target to build for profile runs.
include $(TOP)/src/mkfile/gprof.mk

# Include the "dist-bundles" target and support macros.
include $(TOP)/src/mkfile/bundles.mk

# Include the "install" target and support macros.
include $(TOP)/src/mkfile/install.mk
