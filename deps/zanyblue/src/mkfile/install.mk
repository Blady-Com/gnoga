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

.PHONY:	install install-not-defined install-zb

ifndef INSTALL_DIR
INSTALL_DEP=install-not-defined
else
INSTALL_DEP=install-zb
endif
GPRINSTALL_FLAGS+=-f
GPRINSTALL_FLAGS+=-p
GPRINSTALL_FLAGS+=--prefix=$(INSTALL_DIR)
#GPRINSTALL_FLAGS+=--install-name=zanyblue
GPRINSTALL_FLAGS+=$(GNATFLAGS)

install:	$(INSTALL_DEP)

install-not-defined:
	$(warning Installation requires the definition of a)
	$(warning target directory via the INSTALL_DIR macro,)
	$(warning normally defined on command line for make,)
	$(warning  e.g.,)
	$(warning )
	$(warning     make INSTALL_DIR=/usr/gnat install)
	$(warning )
	$(error Please try again)

install-zb:
	$(GPRINSTALL) $(GPRINSTALL_FLAGS) zanyblue.gpr
	$(GPRINSTALL) $(GPRINSTALL_FLAGS) --mode=usage zbmcompile/zbmcompile.gpr
	$(GPRINSTALL) $(GPRINSTALL_FLAGS) --mode=usage zbinfo/zbinfo.gpr
	$(GPRINSTALL) $(GPRINSTALL_FLAGS) --mode=usage zbtest/zbtest.gpr
