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

uninstall:
	$(RM) ..$(PATHSEP)..$(PATHSEP)src$(PATHSEP)gnoga-gui-plugin-message_boxes.ad?
