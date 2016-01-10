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

TOP=../..

APPNAME=zbtest

ZBMCOMPILE=$(BINDIR)/zbmcompile
ZBTEST=$(BINDIR)/$(APPNAME)
MESGDIR=mesg
FACILTIES=ZBTest
PACKAGE=ZBTest_Messages
STAMPFILE=mesg/zbtest-messages.stamp
ZBMCOMPILE_FLAGS+=-p
ZBMCOMPILE_FLAGS+=-v
ZBMCOMPILE_FLAGS+=-m
ZBMCOMPILE_FLAGS+=-O
ZBMCOMPILE_FLAGS+=-G exceptions
ZBMCOMPILE_FLAGS+=-G wprints
ZBMCOMPILE_FLAGS+=-o mesg
ZBMCOMPILE_FLAGS+=-d mesg
ZBMCOMPILE_FLAGS+=-S $(STAMPFILE)

CLEAN_FILES+=$(wildcard *.gcno)
CLEAN_FILES+=$(wildcard *.gcda)
CLEAN_FILES+=$(wildcard *.gcov)
CLEAN_FILES+=$(wildcard gnatcheck.out)
CLEAN_FILES+=$(wildcard gnatcheck-source-list.out)
CLEAN_FILES+=$(wildcard mesg/*.ads)
CLEAN_FILES+=$(wildcard mesg/*.adb)
CLEAN_FILES+=$(wildcard $(STAMPFILE))
CLEAN_DIRS+=$(wildcard test-area)

# Include configuration definitions
include $(TOP)/src/mkfile/conf.mk

all:	$(STAMPFILE)
	$(GNATMAKE) -p $(GNATFLAGS) -P $(APPNAME)

messages:	$(STAMPFILE)

$(STAMPFILE):	$(wildcard $(MESGDIR)/*.properties)
	$(ZBMCOMPILE) $(ZBMCOMPILE_FLAGS) $(PACKAGE) $(FACILTIES)

gcheck:
	ADA_PROJECT_PATH=.:..:$(GPRDIR) gnat check -rules -from=$(SRCDIR)/admin/gnatcheck.rules -P $(APPNAME).gpr

gcov:
	$(MAKE) BUILD=Coverage

gprof:
	$(MAKE) BUILD=Profile

clean::
	$(GNATCLEAN) $(GNATFLAGS) -P $(APPNAME)

run:	$(ZBTEST)
	$(ZBTEST)

# Include general rules.
include $(TOP)/src/mkfile/rules.mk
