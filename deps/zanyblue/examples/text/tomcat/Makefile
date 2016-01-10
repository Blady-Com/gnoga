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
# Makefile for the application based on the messages defined for the Apache
# Tomcat project.  This project has several facilities and the compilation
# to Ada optimizes the messages for locale based access.
#

TOP=../../..
GNATFLAGS=-aP$(TOP)/lib
ZBMCOMPILE=$(TOP)/bin/zbmcompile
ZBM_FLAGS=-O -F -i -v
APPNAME=x_tomcat
APPLICATION=$(TOP)/bin/$(APPNAME)
FACILITIES=ajp authenticator connector core ha ha-session ha-tcp host http11 \
	http jasper jsse loader manager mbeans membership naming naming-res \
	realm security servlet servlets session startup transport tribes users \
	util-buf util-http util-net util util-threads valves webapps zbtomcat
PACKAGE=Apache.Tomcat.Messages
GENERATED=apache-tomcat-messages.ads apache-tomcat-messages.adb

include $(TOP)/src/mkfile/conf.mk

all:	$(GENERATED)
	$(GNATMAKE) -p $(GNATFLAGS) -P $(APPNAME).gpr

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

include $(TOP)/src/mkfile/rules.mk
