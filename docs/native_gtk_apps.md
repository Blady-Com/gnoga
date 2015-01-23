# The Gnoga Native Gtk Application User Guide
## Gnoga - The GNU Omnificient GUI for Ada

(c) 2014 David Botton
    Permission is granted to copy, distribute and/or modify this document
    under the terms of the GNU Free Documentation License, Version 1.3
    or any later version published by the Free Software Foundation;
    with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
    A copy of the license is included in the section entitled "GNU
    Free Documentation License".

For more information about Gnoga see http://www.gnoga.com

* * *

### Table of Contents

* Introduction
   - How does it work?
* Getting Started

## Introduction

### How does it work?

Gnoga takes advantage of libwebkit2gtk to provide a simple Gtk window with an embedded WebKit View.

## Getting Started

1. Create a singleton app using Gnoga
2. Make sure you have installed the appropriate development libraries for Gtk 3.0 and libwebkit2gtk for example on Debian based GNU based operating systems can install with the following:
   ```
   sudo apt-get install libwebkit2gtk-3.0-dev
   sudo apt-get install libgtk-3-dev
   ```
3. After building Gnoga, build the native gtk support using:
   ```
   make native_gtk
   ```
4. In the singleton application before Initializing the connection use the following to open the gtk window:
   ``` ada
      Gnoga.Application.Gtk_Window.Initialize (Port   => 8080,
                                               Width  => 800,
                                               Height => 600);
   ```
5. Change the initialize call to include the parameter Verbose => False:
   ``` ada
      Gnoga.Application.Singleton.Initialize (Main_Window => M, Verbose => False);
   ```
6. Build your singleton as usual and now on launch will start it's own native Gtk Window. (Note: That you need to make sure that gnoga_gtk_window.o from gnoga/obj is in your projects obj path)
