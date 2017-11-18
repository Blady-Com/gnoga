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
	$(COPY) css$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)css
	$(COPY) html$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)html
	$(COPY) img$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)img
	$(COPY) js$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)js

uninstall:
	- $(RM) $(INSTALL_DIR)$(PATHSEP)css$(PATHSEP)mnmenu.css
	- $(RM) $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)boot_mnmenu.html
	- $(RM) $(INSTALL_DIR)$(PATHSEP)img$(PATHSEP)menu*.png
	- $(RM) $(INSTALL_DIR)$(PATHSEP)js$(PATHSEP)jquery.mnmenu.js
