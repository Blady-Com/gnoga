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
# Makefile for the message filtering example.
#

TOP=../../..
GNATFLAGS=-aP$(TOP)/lib
ZBMCOMPILE=$(TOP)/bin/zbmcompile
APPNAME=x_filtering
APPLICATION=$(TOP)/bin/$(APPNAME)
PACKAGE=Messages
FACILITY=Xmpl
STAMPFILE=messages.stamp
PROPERTIES=$(wildcard $(FACILITY)*.properties)
GENERATED=messages.ads messages.adb \
	$(wildcard messages-*.ads) $(wildcard messages-*.adb) \
	$(STAMPFILE)

include $(TOP)/src/mkfile/conf.mk

all:	$(STAMPFILE)
	$(GNATMAKE) -p $(GNATFLAGS) -P $(APPNAME).gpr

$(STAMPFILE):	$(PROPERTIES)
	$(ZBMCOMPILE) -G wprints -i -S $(STAMPFILE) -v $(PACKAGE) $(FACILITY)

run:	all
	$(APPLICATION)

run_v:	all
	$(APPLICATION) -v

clean::
	gnat clean $(GNATFLAGS) -P $(APPNAME).gpr

messages:	$(GENERATED)

include $(TOP)/src/mkfile/rules.mk
