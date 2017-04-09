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
	$(COPY) js$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)js

uninstall:
	- $(RM) ..$(PATHSEP)..$(PATHSEP)src$(PATHSEP)gnoga-gui-plugin-pixi*.ad?
	- $(RM) ..$(PATHSEP)..$(PATHSEP)js$(PATHSEP)pixi.js
	- $(RM) ..$(PATHSEP)..$(PATHSEP)js$(PATHSEP)pixi.js.map
