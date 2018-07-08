ifdef ComSpec
	COPY=copy
	RM=del
	PATHSEP2=\\
	MKDIR=mkdir
else
	COPY=cp -p
	RM=rm
	PATHSEP2=/
	MKDIR=mkdir -p
endif

PATHSEP=$(strip $(PATHSEP2))
INSTALL_DIR=..$(PATHSEP)..

all: install

install:
	$(COPY) css$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)css
	$(COPY) html$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)html
	- mkdir $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)fonts
	$(COPY) html$(PATHSEP)fonts$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)fonts
	$(COPY) js$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)js

uninstall:
	- $(RM) $(INSTALL_DIR)$(PATHSEP)css$(PATHSEP)bootstrap*
	- $(RM) $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)boot_bootstrap3.html
	- $(RM) $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)fonts$(PATHSEP)glyphicons-*
	- rmdir $(INSTALL_DIR)$(PATHSEP)html$(PATHSEP)fonts
	- $(RM) $(INSTALL_DIR)$(PATHSEP)js$(PATHSEP)bootstrap.min.js
