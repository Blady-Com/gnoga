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
	- cd ../../js && git clone https://github.com/ajaxorg/ace-builds.git
	$(COPY) src$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)src
	$(COPY) html$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)html

uninstall:
	- $(RM) ..$(PATHSEP)..$(PATHSEP)src$(PATHSEP)gnoga-gui-plugin-ace_editor.ad?
	- $(RM) ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)boot_ace.html
	- cd ../../js && ${RMS} ace-builds
