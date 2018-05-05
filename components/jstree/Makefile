ifdef ComSpec
	COPY=copy
	COPYS=xcopy /S /Y
	RM=del
	RMS=del /S
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
	$(COPYS) css$(PATHSEP)jstree_themes $(INSTALL_DIR)$(PATHSEP)css
	$(COPY) html$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)html
	$(COPY) js$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)js

uninstall:
	- $(RM) $(INSTALL_DIR)$(PATHSEP)css$(PATHSEP)mnmenu.css
	- $(RM) $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)boot_mnmenu.html
	- $(RM) $(INSTALL_DIR)$(PATHSEP)js$(PATHSEP)jquery.mnmenu.js
