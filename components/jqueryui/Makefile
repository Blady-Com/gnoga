ifdef ComSpec
	COPY=copy
	RM=del
	PATHSEP2=\\
else
	COPY=cp -p
	RM=rm
	PATHSEP2=/
endif

PATHSEP=$(strip $(PATHSEP2))
INSTALL_DIR=..$(PATHSEP)..

all: install

install:
	$(COPY) css$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)css
	$(COPY) html$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)html
	- mkdir $(INSTALL_DIR)$(PATHSEP)css$(PATHSEP)images
	$(COPY) css$(PATHSEP)images$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)css$(PATHSEP)images
	$(COPY) js$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)js

uninstall:
	- $(RM) $(INSTALL_DIR)$(PATHSEP)css$(PATHSEP)jquery-ui*
	- $(RM) $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)boot_jqueryui.html
	- $(RM) $(INSTALL_DIR)$(PATHSEP)css$(PATHSEP)images$(PATHSEP)*.png
	- rmdir $(INSTALL_DIR)$(PATHSEP)css$(PATHSEP)images
	- $(RM) $(INSTALL_DIR)$(PATHSEP)js$(PATHSEP)jquery-ui.min.js
