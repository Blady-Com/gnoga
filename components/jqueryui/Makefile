ifdef ComSpec
	COPY=copy
	RM=del
	PATHSEP2=\\
else
	COPY=cp
	RM=rm
	PATHSEP2=/
endif

PATHSEP=$(strip $(PATHSEP2))

all: install

install:
	$(COPY) src$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)src
	$(COPY) css$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)css
	$(COPY) html$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)html
	- mkdir ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)images
	$(COPY) html$(PATHSEP)images$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)images
	$(COPY) js$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)js

uninstall:
	$(RM) ..$(PATHSEP)..$(PATHSEP)src$(PATHSEP)gnoga-gui-plugin-jqueryui*
	$(RM) ..$(PATHSEP)..$(PATHSEP)css$(PATHSEP)jquery-ui*
	$(RM) ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)boot_jqueryui.html
	$(RM) ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)images$(PATHSEP)*.*
	- rmdir ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)images
	$(RM) ..$(PATHSEP)..$(PATHSEP)js$(PATHSEP)jquery-ui.min.js
