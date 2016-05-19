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

TEST_ARGS+=-T $(TOP)
GPR_FILE=$(TOP)/src/test/unittest/t_zanyblue.gpr
TEST_NAME=$(basename $(firstword $(wildcard t*.adb)))
CLEAN_TARGS+=$(patsubst %,%.clean,$(TEST_DIRS))
CLEAN_FILES+=$(wildcard *.gcno)
CLEAN_FILES+=$(wildcard *.gcda)
CLEAN_FILES+=$(wildcard *.gcov)
CLEAN_FILES+=$(wildcard b~*)
CLEAN_FILES+=$(wildcard GNAT-TEMP-*.TMP)
CLEAN_FILES+=$(wildcard $(TOP)/src/bin/$(TEST_NAME)$E)
CLEAN_TARGS+=$(patsubst %,%.clean,$(TEST_DIRS))
SALL_TARGS=$(patsubst %,%.sall,$(TEST_DIRS))
GNATFLAGS+=-XMAIN=$(TEST_NAME).adb
# Test subdirectories have Makefiles
TEST_DIRS=$(foreach i,$(wildcard *),$(if $(realpath $(wildcard $i/Makefile)),$i))

include $(TOP)/src/mkfile/conf.mk

ifneq ($(UNITTESTNAME),)
TEST_ARGS+=-P $(UNITTESTNAME)
endif

all::
	$(GPRBUILD) -p $(GNATFLAGS) -P $(GPR_FILE)

gcov:
	$(MAKE) BUILD=Coverage all

gprof:
	$(MAKE) BUILD=Profile all

check::	all
	$(realpath $(TOP)/src/bin/$(TEST_NAME)) $(AHVEN_ARGS) -i $(TEST_ARGS)

xcheck::	all
	$(realpath $(TOP)/src/bin/$(TEST_NAME)) -x -d $(realpath $(TOP)/src/test) $(AHVEN_ARGS) -i $(TEST_ARGS)

sall::	all $(SALL_TARGS)

clean::
	$(GNATCLEAN) $(GNATXDEFS) $(TEST_NAME).gpr

%.sall:
	$(MAKE) -C $* sall

include $(TOP)/src/mkfile/rules.mk
