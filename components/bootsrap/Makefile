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
	- mkdir ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)fonts
	$(COPY) html$(PATHSEP)fonts$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)fonts
	$(COPY) js$(PATHSEP)*.* ..$(PATHSEP)..$(PATHSEP)js

uninstall:
	$(RM) ..$(PATHSEP)..$(PATHSEP)src$(PATHSEP)gnoga-gui-plugin-boot_strap.ad?
	$(RM) ..$(PATHSEP)..$(PATHSEP)css$(PATHSEP)bootstrap*
	$(RM) ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)boot_bootstrap3.html
	$(RM) ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)fonts$(PATHSEP)glyphicons-*
	- rmdir ..$(PATHSEP)..$(PATHSEP)html$(PATHSEP)fonts
	$(RM) ..$(PATHSEP)..$(PATHSEP)js$(PATHSEP)bootstrap.min.js
