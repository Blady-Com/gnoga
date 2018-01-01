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
# Makefile for the simple formatting example app using the ZanyBlue.Text
# package without a properties file defining the messages.
#

# If run from within the source tree, add the bin directory to the path
PATH:=../../../bin:$(PATH)
export PATH

RM=rm -f
ifeq ($(OS),Windows_NT)
RM=del
endif

zbx_gjenkins:	appmsg.ads appmsg.adb jenkins-messages.ads jenkins-messages.adb
	gprbuild -q -p -aP../../../src -P zbx_gjenkins.gpr

appmsg.ads appmsg.adb:	$(wildcard mesg/appmsg*.properties)
	zbmcompile -i -v AppMsg -d mesg appmsg

jenkins-messages.ads jenkins-messages.adb:	$(wildcard ../jenkins/mesg/Jenkins*.properties)
	zbmcompile -i -v Jenkins.Messages -d ../jenkins/mesg Jenkins

run:	zbx_gjenkins
	zbx_gjenkins

run_%:	zbx_gjenkins
	zbx_gjenkins -l$*

clean:
	-gprclean -aP../../../src -P zbx_gjenkins.gpr
	-$(RM) $(wildcard appmsg.*) $(wildcard jenkins-messages.*)
