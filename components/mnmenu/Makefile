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

all: install

install:
	$(COPY) src$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)src
	$(COPY) css$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)css
	$(COPY) html$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)html
	$(COPY) img$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)img
	$(COPY) js$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)js

uninstall:
	- $(RM) ..$(PATHSEP)..$(PATHSEP)src$(PATHSEP)gnoga-gui-plugin-mnmenu*.ad?
	- $(RM) ..$(PATHSEP)..$(PATHSEP)css$(PATHSEP)mnmenu.css
	- $(RM) ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)boot_mnmenu.html
	- $(RM) ..$(PATHSEP)..$(PATHSEP)img$(PATHSEP)menu*.png
	- $(RM) ..$(PATHSEP)..$(PATHSEP)js$(PATHSEP)jquery.mnmenu.js
