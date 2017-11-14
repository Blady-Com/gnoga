ifdef ComSpec
	COPY=copy
	COPYS=xcopy /S
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

all: install

install:
	$(COPY) html$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)html
	$(COPYS) js$(PATHSEP)ace-builds ..$(PATHSEP)..$(PATHSEP)js

uninstall:
	- $(RM) ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)boot_ace.html
	- $(RMS) ..$(PATHSEP)..$(PATHSEP)js$(PATHSEP)ace-builds
