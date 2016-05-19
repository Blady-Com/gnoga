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
# Configuation macros ...
# The choices are defined by the types in zanyblue_common.gpr, repeated here
# to simplify direct configuration (i.e., don't want to read another file).

# The choices for OS are "unix" and "Windows_NT".  This only changes the name
# returned by the ZanyBlue.OS.OS_Name function and, if ZanyBlue is built
# as a shared library, the package ZanyBlue.OS.Ld_Run_Path will add the
# ld.so runtime path "$ORIGIN/../lib", i.e., load shared libraries from
# the "lib" directory one from from the directory containing the executable.
# The value is defined to be "unix" if not already defined, i.e., Windows
# defines the environment variable OS with value "Windows_NT", if this is
# defined, use it.
OS ?= unix

# The choices for TYPE are "static", for a static library, and "relocatable",
# for a shared/dynamic library.
#TYPE=relocatable
TYPE=static

GPRBUILD=gprbuild
GNATCHECK=gnat check
GNATCLEAN=gnat clean
GNATDOC=gnatdoc

# HTML Preprocessor
HTP=htp
TAR=tar

include $(TOP)/src/mkfile/$(OS).mk

#
# Makefile definitions for the ZanyBlue version macros
#

include $(TOP)/src/mkfile/version.mk
PREPFLAGS+=-DV_MAJOR=$(V_MAJOR)
PREPFLAGS+=-DV_MINOR=$(V_MINOR)
PREPFLAGS+=-DV_PATCH=$(V_PATCH)
PREPFLAGS+=-DV_STATUS=$(V_STATUS)
PREPFLAGS+=-DVERSION=$(VERSION)
PREPFLAGS+=-DSVN_VERSION='"$(SVN_VERSION)"'
PREPFLAGS+=-DCOPYRIGHT_YEAR=$(COPYRIGHT_YEAR)

-include $(TOP)/src/mkfile/defs.mk

# If defs.mk doesn't exist, i.e., this is not a source tar ball
# snapshot, query the environment for the svn version and copyright info.
VERSION=$(V_MAJOR).$(V_MINOR).$(V_PATCH)
ifndef SVN_VERSION
SVN_VERSION=$(shell svnversion $(TOP))
COPYRIGHT_YEAR=$(CURRENT_YEAR)
endif

# Macro covers for the various directories
BINDIR=$(TOP)/bin
DOCDIR=$(TOP)/doc
HTMLDIR=$(DOCDIR)/zanyblue
HTMLREFDIR=$(HTMLDIR)/ref
INCDIR=$(TOP)/include/zanyblue
LIBDIR=$(TOP)/lib/zanyblue
GPRDIR=$(TOP)/lib/gnat
SRCDIR=$(TOP)/src
STAGEDIR=$(TOP)/stage
ADMINDIR=$(SRCDIR)/admin
DISTRIBUTION=$(wildcard $(TOP)/zanyblue-*.tar.gz)

# The choices for BUILD are "Debug", for a debug build, "Production" for an
# optimized production build, and  "Coverage" for a coverage enable build via
# "gcov".
# Build defaults to Debug
ifeq ($(BUILD),)
BUILD=Debug
endif

# ---------------------------------------------------------------------------
# Macros derived from the configuration macros.
GNATXDEFS+=-XOS=$(OS)
GNATXDEFS+=-XTYPE=$(TYPE)
GNATXDEFS+=-XBUILD=$(BUILD)
GNATXDEFS+=-XV_MAJOR=$(V_MAJOR)
GNATXDEFS+=-XV_MINOR=$(V_MINOR)
GNATXDEFS+=-XV_PATCH=$(V_PATCH)
GNATXDEFS+=-XV_STATUS=$(V_STATUS)
GNATFLAGS+=$(GNATXDEFS)
GNATFLAGS+=-aP$(TOP)/lib/gnat
GNATFLAGS+=-aP$(TOP)/src

# Add coverage generated files to the clean list
CLEAN_FILES+=$(wildcard $(TOP)/src/obj/*.gcno)
CLEAN_FILES+=$(wildcard $(TOP)/src/obj/*.gcda)
CLEAN_FILES+=$(wildcard $(TOP)/src/auto.cgpr)
CLEAN_FILES+=$(wildcard $(TOP)/src/b~*)
CLEAN_FILES+=$(wildcard $(TOP)/src/root/*.gcov)
CLEAN_FILES+=$(wildcard $(TOP)/src/os/*.gcov)
CLEAN_FILES+=$(wildcard $(TOP)/src/os/unix/*.gcov)
CLEAN_FILES+=$(wildcard $(TOP)/src/os/Windows_NT/*.gcov)
CLEAN_FILES+=$(wildcard $(TOP)/src/parameters/*.gcov)
CLEAN_FILES+=$(wildcard $(TOP)/src/text/*.gcov)
CLEAN_FILES+=$(wildcard $(TOP)/src/utils/*.gcov)

#
# Remove editor backup files
CLEAN_FILES+=$(wildcard *~)

#
# Generated files should be removed ...
CLEAN_FILES+=$(foreach i,$(GENERATED),$(wildcard $i))

#
# General cleaning rules
CLEAN_TARGS+=$(patsubst %,%.rmfile,$(CLEAN_FILES))
CLEAN_TARGS+=$(patsubst %,%.rmdir,$(CLEAN_DIRS))
