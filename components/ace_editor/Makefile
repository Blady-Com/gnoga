ifdef ComSpec
	COPY=copy
	COPYS=xcopy /S /Y
	RM=del
	RMS=del /S /Q
	PATHSEP2=\\
else
	COPY=cp -p
	COPYS=cp -rp
	RM=rm
	RMS=rm -rf
	PATHSEP2=/
endif

PATHSEP=$(strip $(PATHSEP2))
INSTALL_DIR=..$(PATHSEP)..

all: install

install:
	$(COPY) html$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)html
	$(COPYS) js$(PATHSEP)ace-builds $(INSTALL_DIR)$(PATHSEP)js

uninstall:
	- $(RM) $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)boot_ace.html
	- $(RMS) $(INSTALL_DIR)$(PATHSEP)js$(PATHSEP)ace-builds
