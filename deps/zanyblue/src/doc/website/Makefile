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
# Makefile for the ZanyBlue website
#

TOP=../../..
include $(TOP)/src/mkfile/conf.mk

IMAGEDIR=$(SRCDIR)/doc/images
DATADIR=$(SRCDIR)/doc/website/data
INCLUDES=defs.hti header.hti title.hti trailer.hti
TEMPLATES+=$(wildcard *.htp)
HTMLPAGES+=$(patsubst %.htp,$(DOCDIR)/%.html,$(TEMPLATES))
IMAGEFILES+=$(wildcard $(IMAGEDIR)/*.png)
DATAFILES+=$(wildcard $(DATADIR)/*)
# The Makefile wildcard ignore dot files, explicitly add .htaccess
DATAFILES+=$(DATADIR)/.htaccess
WEBFILES+=$(patsubst %.htp,$(DOCDIR)/%.html,$(TEMPLATES))
WEBFILES+=$(patsubst $(IMAGEDIR)/%,$(DOCDIR)/%,$(IMAGEFILES))
WEBFILES+=$(patsubst $(DATADIR)/%,$(DOCDIR)/%,$(DATAFILES))
WEBFILES+=$(DOCDIR)/notices.html
WEBFILES+=$(DOCDIR)/zbxmpls.cgi
CLEAN_FILES+=$(WEBFILES)

all: $(DOCDIR) $(WEBFILES)

$(HTMLPAGES):	$(TEMPLATES)

$(DOCDIR)/%.html:	%.htp
	$(HTP) $*.htp $(DOCDIR)/$*.html

$(wildcard *.htp): defs.hti

$(DOCDIR)/zbxmpls.cgi:	zbxmpls.cgi
	$(call COPY,zbxmpls.cgi,$(DOCDIR)/zbxmpls.cgi)

$(DOCDIR)/%:	$(DATADIR)/%
	$(call COPY,$(DATADIR)/$*,$(DOCDIR)/$*)

$(DOCDIR)/%:	$(IMAGEDIR)/%
	$(call COPY,$(IMAGEDIR)/$*,$(DOCDIR)/$*)

$(DOCDIR):
	$(error Please build in the doc directory first)

mkr:
	@echo $(DATAFILES)

# Include general rules.
include $(TOP)/src/mkfile/rules.mk
