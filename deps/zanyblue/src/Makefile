#  -*- coding: utf-8 -*-
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
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
                     $(wildcard $(SRCDIR)/$d/$(if $(filter-out Production,$(BUILD)),*,*generic*).adb)))
INCLUDE_FILES=$(patsubst %,$(INCDIR)/%,$(INCLUDE_SRCS))
GENERATED=$(INCLUDE_FILES)
APPDIRS=zbmcompile zbinfo zbtest
APPTARGETS=$(patsubst %,%.app,$(APPDIRS))
DOCLINKSRST=$(SRCDIR)/doc/source/links.rst
CLEAN_DEPS=$(patsubst %,%.clean,$(APPDIRS) root doc test)
CLEAN_FILES+=$(wildcard bin/pylib/*.pyc)
CLEAN_FILES+=$(wildcard bin/pylib/zb/*.pyc)
CLEAN_FILES+=$(wildcard gnatcheck.out)
CLEAN_FILES+=$(wildcard obj/*)
CLEAN_FILES+=$(wildcard $(TOP)/lib/b~*)
CLEAN_FILES+=$(wildcard $(TOP)/zanyblue-*.tar.gz)
CLEAN_FILES+=$(wildcard $(TOP)/zanyblue-*.zip)
CLEAN_FILES+=$(wildcard $(TOP)/doc/*.*)
CLEAN_FILES+=$(wildcard $(TOP)/doc/.buildinfo)
CLEAN_DIRS+=$(wildcard $(TOP)/doc/ref)
CLEAN_DIRS+=$(wildcard $(TOP)/doc/_images)
CLEAN_DIRS+=$(wildcard $(TOP)/doc/_sources)
CLEAN_DIRS+=$(wildcard $(TOP)/doc/_static)
CLEAN_DIRS+=$(wildcard $(TOP)/doc/text)
CLEAN_DIRS+=$(wildcard $(TOP)/doc/utilities)
CLEAN_DIRS+=$(wildcard $(TOP)/src/doc/build)
CLEAN_DIRS+=$(wildcard $(TOP)/src/text/cldr)
CLEAN_FILES+=$(wildcard $(TOP)/src/admin/inventory.lis)
CLEAN_FILES+=$(wildcard $(DOCLINKSRST))
CLEAN_DIRS+=$(STAGEDIR)
CLEAN_DEPS+=../examples.clean

# Include configuration definitions
include $(TOP)/src/mkfile/conf.mk

all: setup library applications

setup: $(ZANYBLUEADB) $(INCDIR) $(INCLUDE_FILES)

library:
	$(GPRBUILD) $(GNATFLAGS) -P zblib.gpr

applications:	$(APPTARGETS)

docs:	gnatdocs $(DOCLINKSRST)
	$(MAKE) -C doc html
	$(MAKE) -C doc latexpdf
	$(call COPY,doc/build/latex/ZanyBlue.pdf,$(DOCDIR)/ZanyBlue.pdf)

$(DOCLINKSRST):
	echo ".. _download: $(DOWNLOAD_URL)" > $(DOCLINKSRST)

gnatdocs:
	$(GNATDOC) -P$(SRCDIR)/zblib

changelog:
	$(SRCDIR)/bin/mkchangelog

distribution: clean inventory all xcheck docs test.clean bundles ws-bundle

verify-distribution:
	rm -rf $(STAGEDIR)
	mkdir -p $(STAGEDIR)
	$(TAR) -C $(STAGEDIR) -xvf $(DISTRIBUTION)
	cd $(STAGEDIR); $(MD5SUM) --check --quiet $(DIST_TLD)/MANIFEST.txt
	cd $(STAGEDIR)/$(DIST_TLD)/src; $(MAKE) clean all xcheck

%.app:
	$(MAKE) -C $*

clean::
	$(GNATCLEAN) $(GNATXDEFS) -P zblib.gpr

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
