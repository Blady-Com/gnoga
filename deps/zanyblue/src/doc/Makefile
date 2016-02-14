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
# Makefile for the ZanyBlue documentation
#

TOP=../..
DOCSUBST=$(SRCDIR)/bin/docsubst.py
MKNOTICES=$(SRCDIR)/bin/mknotices.py
MKDEFSHTI=$(SRCDIR)/bin/mkdefshti.pl
DEFSHTI=$(SRCDIR)/doc/website/defs.hti
GNATHTMLFLAGS+=-I$(INCDIR)
GNATHTMLFLAGS+=-I$(LIBDIR)
GNATHTMLFLAGS+=-o $(HTMLREFDIR)
GNATHTMLFLAGS+=-ext html
GNATHTMLFLAGS+=-l 5
LABELSPL=$(TOP)/doc/zanyblue/labels.pl
DOCSUBSTFLAGS+=ZB_VERSION="$(VERSION)"
DOCSUBSTFLAGS+=ZB_STATUS="$(V_STATUS)"
DOCSUBSTFLAGS+=SVN_VERSION="$(SVN_VERSION)"
DOCSUBSTFLAGS+=COPYRIGHT_YEAR="$(COPYRIGHT_YEAR)"
ZBFILES+=$(patsubst $(INCDIR)/%,%,$(wildcard $(INCDIR)/*.ads))
CLEAN_FILES+=$(wildcard *.aux)
CLEAN_FILES+=$(wildcard *.log)
CLEAN_FILES+=$(wildcard *.dvi)
CLEAN_FILES+=$(wildcard *.toc)
CLEAN_FILES+=$(wildcard *.idx)
CLEAN_FILES+=$(wildcard *.ind)
CLEAN_FILES+=$(wildcard *.bbl)
CLEAN_FILES+=$(wildcard *.blg)
CLEAN_FILES+=$(wildcard *.ilg)
CLEAN_FILES+=$(wildcard *.out)
CLEAN_FILES+=$(wildcard *.pdf)
CLEAN_FILES+=$(wildcard zbdefs.tex)
CLEAN_FILES+=$(wildcard $(TOP)/NOTICES.txt)
CLEAN_FILES+=$(wildcard website/notices.htp)
CLEAN_FILES+=$(wildcard website/ganalytics.hti)
CLEAN_FILES+=$(wildcard $(DEFSHTI))

all::	pdffile html notices $(DEFSHTI)
	cp zanyblue.pdf $(DOCDIR)/zanyblue.pdf

defs defs.hti $(DEFSHTI):	$(LABELSPL)
	$(MKDEFSHTI) $(DEFSHTI) $(LABELSPL) "$(VERSION)" "$(V_STATUS)" "$(SVN_VERSION)"

$(LABELSPL):
	$(error Please compile the LaTeX docs first)

pdffile:	zbdefs.tex
	pdflatex zanyblue.tex
	bibtex zanyblue.aux
	pdflatex zanyblue.tex
	pdflatex zanyblue.tex

html:	zbdefs.tex $(HTMLREFDIR)
	rm -rf $(HTMLDIR)
	mkdir $(HTMLDIR)
	latex2html -local_icons -dir $(HTMLDIR) zanyblue.tex 
	$(GNATHTML) $(GNATHTMLFLAGS) $(ZBFILES)

notices:
	$(MKNOTICES)

$(HTMLREFDIR):
	mkdir -p $@

force:

include $(TOP)/src/mkfile/conf.mk

zbdefs.tex:
	$(DOCSUBST) $(DOCSUBSTFLAGS) zbdefs.tex-in zbdefs.tex

# Include general rules.
include $(TOP)/src/mkfile/rules.mk

