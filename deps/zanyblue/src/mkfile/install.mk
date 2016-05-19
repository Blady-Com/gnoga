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

# Target used to install the ZanyBlue build to an installation directory.

.PHONY:	install install-not-defined

ifndef INSTALL_DIR

install:
	$(warning Installation requires the definition of a)
	$(warning target directory via the INSTALL_DIR macro,)
	$(warning normally defined on command line for make,)
	$(warning  e.g.,)
	$(warning )
	$(warning     make INSTALL_DIR=/usr/gnat install)
	$(warning )
	$(error Please try again)

else

INCLUDE_FILES=$(wildcard $(INCDIR)/*)
INSTALL_INCDIR=$(INSTALL_DIR)/include/zanyblue
INSTALL_INCLUDE_FILES=$(patsubst $(INCDIR)/%,$(INSTALL_INCDIR)/%,$(INCLUDE_FILES))

LIB_FILES=$(wildcard $(LIBDIR)/*zanyblue*)
INSTALL_LIBDIR=$(INSTALL_DIR)/lib/zanyblue
INSTALL_LIB_FILES=$(patsubst $(LIBDIR)/%,$(INSTALL_LIBDIR)/%,$(LIB_FILES))

INSTALL_GPRDIR=$(INSTALL_DIR)/lib/gnat
INSTALL_GPRFILE=$(INSTALL_GPRDIR)/zanyblue.gpr
GPRFILE=$(GPRDIR)/zanyblue.gpr

BIN_FILES=$(wildcard $(BINDIR)/zb*)
INSTALL_BINDIR=$(INSTALL_DIR)/bin
INSTALL_BIN_FILES=$(patsubst $(BINDIR)/%,$(INSTALL_BINDIR)/%,$(BIN_FILES))

install:	includes libraries applications
	$(info OK, installed to $(INSTALL_DIR))

includes: $(INSTALL_INCDIR) $(INSTALL_INCLUDE_FILES)

$(INSTALL_INCDIR):
	$(call MKDIR,$(INSTALL_INCDIR))

$(INSTALL_INCDIR)/%:	$(INCDIR)/%
	$(call COPY,$(INCDIR)/$*,$(INSTALL_INCDIR)/$*)

libraries: $(INSTALL_LIBDIR) $(INSTALL_LIB_FILES) $(INSTALL_GPRFILE)

$(INSTALL_LIBDIR):
	$(call MKDIR,$(INSTALL_LIBDIR))

$(INSTALL_LIBDIR)/%:	$(LIBDIR)/%
	$(call COPY,$(LIBDIR)/$*,$(INSTALL_LIBDIR)/$*)

$(INSTALL_GPRFILE): $(GPRFILE)
	$(call MKDIR,$(INSTALL_GPRDIR))
	$(call COPY,$(GPRFILE),$(INSTALL_GPRFILE))

applications: $(INSTALL_BINDIR) $(INSTALL_BIN_FILES)

$(INSTALL_BINDIR):
	$(call MKDIR,$(INSTALL_BINDIR))

$(INSTALL_BINDIR)/%:	$(BINDIR)/%
	$(call COPY,$(BINDIR)/$*,$(INSTALL_BINDIR)/$*)

endif
