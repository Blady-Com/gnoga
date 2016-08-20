PREFIX=$(dir $(shell which gnatls))..

TARGET=$(shell gcc -dumpmachine)

ifeq ($(strip $(findstring darwin, $(TARGET))),darwin)
	PRJ_TARGET=OSX
else
ifeq ($(strip $(findstring mingw32, $(TARGET))),mingw32)
	PRJ_TARGET=Windows
else
ifeq ($(strip $(findstring cygwin, $(TARGET))),cygwin)
	PRJ_TARGET=Windows
else
ifeq ($(strip $(findstring freebsd, $(TARGET))),freebsd)
	PRJ_TARGET=Freebsd
else
ifeq ($(strip $(findstring linux, $(TARGET))),linux)
	PRJ_TARGET=Linux
else
	PRJ_TARGET=Unix
endif
endif
endif
endif
endif

ifdef ComSpec
	COPY=copy
	RM=del
else
	COPY=cp -p
	RM=rm -f
endif

all: gnoga-application.adb

gnoga-application.adb:
ifeq (${PRJ_TARGET}, Windows)
	@echo "Setting up for Windows build"
	$(COPY) gnoga-application.windows gnoga-application.adb
else
ifeq (${PRJ_TARGET}, OSX)
	@echo "Setting up for Mac OS X build"
	cp gnoga-application.osx gnoga-application.adb
else
	@echo "Linux/FreeBSD or Unix build"
	cp gnoga-application.linux gnoga-application.adb
endif
endif

clean:
	$(RM) gnoga-application.adb
