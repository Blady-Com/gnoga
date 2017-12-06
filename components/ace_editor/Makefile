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

all: install

install:
	$(COPY) src$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)src
	$(COPY) html$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)html
	$(COPYS) js$(PATHSEP)ace-builds ..$(PATHSEP)..$(PATHSEP)js

uninstall:
	- $(RM) ..$(PATHSEP)..$(PATHSEP)src$(PATHSEP)gnoga-gui-plugin-ace_editor.ad?
	- $(RM) ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)boot_ace.html
	- $(RMS) ..$(PATHSEP)..$(PATHSEP)js$(PATHSEP)ace-builds
