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
# Makefile for the application based on the messages defined for the Apache
# Tomcat project.  This project has several facilities and the compilation
# to Ada optimizes the messages for locale based access.
#

TOP=../../..
GNATFLAGS=-aP$(TOP)/lib
ZBMCOMPILE=$(TOP)/bin/zbmcompile
ZBM_FLAGS=-O -F -i -v
APPNAME=zbx_tomcat
APPLICATION=$(TOP)/bin/$(APPNAME)
TC_VERSION=9.0.0.M8
TC_TLD=apache-tomcat-$(TC_VERSION)-src
TC_TGZ=apache-tomcat-$(TC_VERSION)-src.tar.gz
TC_URL=http://mirrors.sonic.net/apache/tomcat/tomcat-9/v$(TC_VERSION)/src/$(TC_TGZ)
TC_APPDIR=java/org/apache/catalina
TC_FACILITIES=authenticator connector core manager realm security servlets\
	startup users util valves
FACILITIES=$(TC_FACILITIES) zbtomcat
PACKAGE=Apache.Tomcat.Messages
GENERATED=apache-tomcat-messages.ads apache-tomcat-messages.adb
CLEAN_FILES+=$(wildcard $(TC_TGZ))
CLEAN_DIRS+=$(wildcard $(TC_TLD))

include $(TOP)/src/mkfile/conf.mk

all:	$(GENERATED)
	$(GPRBUILD) -p $(GNATFLAGS) -P $(APPNAME).gpr

generated:	$(GENERATED)
$(GENERATED):	$(wildcard mesg/*.properties)
	$(ZBMCOMPILE) $(ZBM_FLAGS) $(PACKAGE) -d mesg $(FACILITIES)

run:	all
	$(APPLICATION)

run_%:	all
	$(APPLICATION) -l$*

clean::
	gnat clean $(GNATFLAGS) -P $(APPNAME).gpr

messages:	$(GENERATED)

PROPERTIES=$(foreach f,$(TC_FACILITIES),$(patsubst $(TC_TLD)/$(TC_APPDIR)/$f/LocalStrings%,mesg/$f%,$(wildcard $(TC_TLD)/$(TC_APPDIR)/$f/LocalStrings*.properties)))

tomcat-sync: $(TC_TLD) $(PROPERTIES)

mesg/authenticator%:	$(TC_TLD)/$(TC_APPDIR)/authenticator/LocalStrings%
	cp $< $@

mesg/connector%:	$(TC_TLD)/$(TC_APPDIR)/connector/LocalStrings%
	cp $< $@

mesg/core%:	$(TC_TLD)/$(TC_APPDIR)/core/LocalStrings%
	cp $< $@

mesg/manager%:	$(TC_TLD)/$(TC_APPDIR)/manager/LocalStrings%
	cp $< $@

mesg/realm%:	$(TC_TLD)/$(TC_APPDIR)/realm/LocalStrings%
	cp $< $@

mesg/security%:	$(TC_TLD)/$(TC_APPDIR)/security/LocalStrings%
	cp $< $@

mesg/servlets%:	$(TC_TLD)/$(TC_APPDIR)/servlets/LocalStrings%
	cp $< $@

mesg/startup%:	$(TC_TLD)/$(TC_APPDIR)/startup/LocalStrings%
	cp $< $@

mesg/users%:	$(TC_TLD)/$(TC_APPDIR)/users/LocalStrings%
	cp $< $@

mesg/util%:	$(TC_TLD)/$(TC_APPDIR)/util/LocalStrings%
	cp $< $@

mesg/valves%:	$(TC_TLD)/$(TC_APPDIR)/valves/LocalStrings%
	cp $< $@

$(TC_TLD):	$(TC_TGZ)
	tar xzf $(TC_TGZ)

$(TC_TGZ):
	wget $(TC_URL)

include $(TOP)/src/mkfile/rules.mk
