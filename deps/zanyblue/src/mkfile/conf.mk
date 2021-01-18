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
# Configuation macros ...
# The choices are defined by the types in zanyblue_common.gpr, repeated here
# to simplify direct configuration (i.e., don't want to read another file).
CURDIR=$(abspath $(dir $(firstword $(MAKEFILE_LIST))))

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

# Lowercase macro
lc = $(subst A,a,$(subst B,b,$(subst C,c,$(subst D,d,$(subst E,e,$(subst F,f,$(subst G,g,$(subst H,h,$(subst I,i,$(subst J,j,$(subst K,k,$(subst L,l,$(subst M,m,$(subst N,n,$(subst O,o,$(subst P,p,$(subst Q,q,$(subst R,r,$(subst S,s,$(subst T,t,$(subst U,u,$(subst V,v,$(subst W,w,$(subst X,x,$(subst Y,y,$(subst Z,z,$1))))))))))))))))))))))))))

GPRBUILD=gprbuild
GPRINSTALL=gprinstall
GNATCHECK=gnat check
GNATCLEAN=gnat clean
GNATPREP=gnatprep
GNATDOC=gnatdoc
TAR=tar
MD5SUM=md5sum
GNATFLAGS+=-p
ZANYBLUEADS=$(SRCDIR)/root/zanyblue.ads
ZBVERSGPR=$(SRCDIR)/zbvers.gpr
ZBVERSGPRSRC=$(SRCDIR)/admin/zbvers.gpr
PLATFORM_TARGET=noop
DOCSUBSTDEFS+=-d V_MAJOR=$(V_MAJOR)
DOCSUBSTDEFS+=-d V_MINOR=$(V_MINOR)
DOCSUBSTDEFS+=-d V_PATCH=$(V_PATCH)
SRCROOTDIR=root
SRCDOCDIR=doc
SRCTESTDIR=test

include $(TOP)/src/mkfile/$(OS).mk

#
# Makefile definitions for the ZanyBlue version macros
#

include $(TOP)/src/mkfile/version.mk

-include $(TOP)/src/mkfile/defs.mk

# Define the targets used to do a recursive build and clean
APPTARGETS=$(patsubst %,%.app,$(APPDIRS))
CLEAN_DEPS+=$(patsubst %,%.clean,$(APPDIRS) $(SRCROOTDIR) $(SRCDOCDIR) $(SRCTESTDIR))

# If defs.mk doesn't exist, i.e., this is not a source tar ball
# snapshot, query the environment for the svn version and copyright info.
VERSION=$(V_MAJOR).$(V_MINOR).$(V_PATCH)
VERSION_TLD=$(VERSION)
VERSION_S=$(VERSION_TLD)
DOWNLOAD_ROOT="http://sourceforge.net/projects/zanyblue/files"
DOWNLOAD_URL="$(DOWNLOAD_ROOT)/zanyblue-$(VERSION_S).tar.gz"
DIST_TLD=$(call lc,zanyblue-$(VERSION_TLD))
TARNAME=zanyblue-$(VERSION_S).tar.gz
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
SBINDIR=$(SRCDIR)/bin
STAGEDIR=$(TOP)/stage
ADMINDIR=$(SRCDIR)/admin
DISTRIBUTION=$(TOP)/$(TARNAME)
ZBDEV=$(TOP)/src/bin/zbdev

# The choices for BUILD are "Debug", for a debug build, "Production" for an
# optimized production build, and  "Coverage" for a coverage enable build via
# "gcov".
# Build defaults to Debug
ifeq ($(BUILD),)
BUILD=Debug
endif

# If doing a non-production build, add the DEV_CLEAN_FILES and DEV_CLEAN_DIRS
# to the CLEAN_FILES and CLEAN_DIRS macros
ifneq ($(BUILD),Production)
CLEAN_FILES+=$(DEV_CLEAN_FILES)
CLEAN_DIRS+=$(DEV_CLEAN_DIRS)
endif

# ---------------------------------------------------------------------------
# Macros derived from the configuration macros.
GNATXDEFS+=-XOS=$(OS)
GNATXDEFS+=-XTYPE=$(TYPE)
GNATXDEFS+=-XBUILD=$(BUILD)
GNATFLAGS+=$(GNATXDEFS)
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
# Remove auto.cgpr files
CLEAN_FILES+=$(wildcard auto.cgpr)

#
# Generated files should be removed ...
CLEAN_FILES+=$(foreach i,$(GENERATED),$(wildcard $i))

#
# General cleaning rules
CLEAN_TARGS+=$(patsubst %,%.rmdir,$(CLEAN_DIRS))
CLEAN_TARGS+=$(patsubst %,%.rmfile,$(CLEAN_FILES))
