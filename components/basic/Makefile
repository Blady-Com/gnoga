ifdef ComSpec
	COPY=copy
	RM=del
	RMS=del /S
	PATHSEP2=\\
	MKDIR=mkdir
else
	COPY=cp -p
	RM=rm
	RMS=rm -rf
	PATHSEP2=/
	MKDIR=mkdir -p
endif

PATHSEP=$(strip $(PATHSEP2))
INSTALL_DIR=..$(PATHSEP)..

all: install

install:
	- $(MKDIR) $(INSTALL_DIR)$(PATHSEP)css
	- $(MKDIR) $(INSTALL_DIR)$(PATHSEP)html
	- $(MKDIR) $(INSTALL_DIR)$(PATHSEP)img
	- $(MKDIR) $(INSTALL_DIR)$(PATHSEP)js
	- $(MKDIR) $(INSTALL_DIR)$(PATHSEP)upload
	$(COPY) css$(PATHSEP)README $(INSTALL_DIR)$(PATHSEP)css
	$(COPY) html$(PATHSEP)README $(INSTALL_DIR)$(PATHSEP)html
	$(COPY) html$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)html
	$(COPY) img$(PATHSEP)README $(INSTALL_DIR)$(PATHSEP)img
	$(COPY) img$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)img
	$(COPY) js$(PATHSEP)README $(INSTALL_DIR)$(PATHSEP)js
	$(COPY) js$(PATHSEP)*.* $(INSTALL_DIR)$(PATHSEP)js
	$(COPY) upload$(PATHSEP)README $(INSTALL_DIR)$(PATHSEP)upload

uninstall:
	- $(RM) $(INSTALL_DIR)$(PATHSEP)css$(PATHSEP)README
	- $(RM) $(INSTALL_DIR)$(PATHSEP)upload$(PATHSEP)README
# Others files in html, img, js are removed in calling makefile
