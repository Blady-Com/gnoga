ifdef ComSpec
	COPY=copy
	RM=del
	RMS=del /S
	PATHSEP2=\\
else
	COPY=cp -p
	RM=rm
	RMS=rm -rf
	PATHSEP2=/
endif

PATHSEP=$(strip $(PATHSEP2))
INSTALL_DIR=..$(PATHSEP)..

all: install

install:
	$(COPY) html$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)html
	$(COPY) js$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)js

uninstall:
	- $(RM) $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)electron.html
	- $(RM) $(INSTALL_DIR)$(PATHSEP)js$(PATHSEP)electron_boot.js
