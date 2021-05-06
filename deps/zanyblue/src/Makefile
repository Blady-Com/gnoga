#  -*- coding: utf-8 -*-
#
#  ZanyBlue, an Ada library and framework for finite element analysis.
#
#  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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

APPDIRS=zbmcompile zbinfo

CLEAN_FILES+=$(wildcard gnatcheck.out)
CLEAN_FILES+=$(wildcard obj/*)
CLEAN_FILES+=$(wildcard $(TOP)/lib/b~*)
CLEAN_FILES+=$(wildcard $(TOP)/zanyblue-*.tar.gz)
CLEAN_FILES+=$(wildcard $(TOP)/website.tar.gz)
CLEAN_FILES+=$(wildcard $(TOP)/src/admin/*.TXT)
CLEAN_DIRS+=$(STAGEDIR)
CLEAN_DIRS+=$(wildcard $(TOP)/src/text/cldr)
CLEAN_DIRS+=$(wildcard bin/pylib/__pycache__)
CLEAN_DIRS+=$(wildcard bin/pylib/zb/__pycache__)
CLEAN_DIRS+=$(wildcard bin/pylib/zb/scm/__pycache__)

DEV_CLEAN_DIRS+=$(TOP)/doc
DEV_CLEAN_DIRS+=$(TOP)/src/doc/_build
DEV_CLEAN_DIRS+=$(TOP)/src/obj/gnatdoc
DEV_CLEAN_FILES+=$(wildcard $(ZBVERSGPR))

# Include configuration definitions
include $(TOP)/src/mkfile/conf.mk

all: setup library applications

setup: $(ZANYBLUEADS) $(ZBVERSGPR)

library:
	$(GPRBUILD) $(GNATFLAGS) -P zanyblue.gpr

applications:	$(APPTARGETS)

docs:
	$(MAKE) -C doc all

distribution: clean
	$(MAKE) all xcheck docs
	$(ZBDEV) mkbundle $(GOOGLE_ANALYTICS_FLAG)
	$(MAKE) $(PLATFORM_TARGET)

verify-distribution:
	$(ZBDEV) verify -W $(TOP)/stage $(DISTRIBUTION)

%.app:
	$(MAKE) -C $*

clean:: $(ZBVERSGPR) examples-clean
	$(GNATCLEAN) $(GNATXDEFS) -P zanyblue.gpr

clean::	$(CLEAN_DEPS)

%.clean:
	$(MAKE) -C $* clean

examples-clean:
	GPR_PROJECT_PATH=$(realpath $(SRCDIR)) $(MAKE) -C ../examples clean

check:
	$(MAKE) -C test check

xcheck:
	$(MAKE) -C test xcheck

xcheck-%:
	$(MAKE) -C test/$* xcheck

gcheck:
	$(info Expect 'use of equality operation for float values' in zanyblue-text-generic_floats.adb)
	$(GNATCHECK) -rules -from=admin/gnatcheck.rules -P zanyblue.gpr

force:

$(ZANYBLUEADS):
	$(MAKE) -C $(SRCDIR)/root

$(ZBVERSGPR): $(ZBVERSGPRSRC)
	$(ZBDEV) docsubst $(DOCSUBSTDEFS) $< $@

# Include general rules.
include $(TOP)/src/mkfile/rules.mk

# Generation of "source lines of code" data for Hudson trending.
include $(TOP)/src/mkfile/sloccount.mk

# Include the "gcov" target to build for coverage runs.
include $(TOP)/src/mkfile/gcov.mk

# Include the "gprof" target to build for profile runs.
include $(TOP)/src/mkfile/gprof.mk

# Include the "install" target and support macros.
include $(TOP)/src/mkfile/install.mk
