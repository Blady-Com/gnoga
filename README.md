# GNOGA - The GNU Omnificent GUI for Ada
## http://www.gnoga.com

Please see the docs directory for documentation and the FAQ.

To get started:

Install GNAT (see http://GetAdaNow.com)

On Windows 32 bit or Linux 32 bit if using GNAT GPL prior to 2014 you need to
modify the Makfile ATOMIC_ACCESS=GCC-built-ins

On Windows - if using TDM-GCC replace make below with mingw32-make

Either download and unarchive the stable version of Gnoga from
http://www.gnoga.com or use git to get the latest development version:

```
git clone git://git.code.sf.net/p/gnoga/code gnoga-code
cd gnoga-code
make help # for all options and configuration values
make all
```

then run the snake demo or tutorials in bin directory. There are additional demos in demo directory.
Open a web browser with URL like: "http://127.0.0.1:8080".

See INSTALL for more information about installing Gnoga as a standard gcc/ada
library if desired.

GNAT Programming Studio (GPS) is available with 'make gps' which launch GPS with the
actual values of compiler switches for gnoga.gpr project.
For the demos: open in GPS the demo/demo_agg.gpr project.
For the tests: open in GPS the test/test_agg.gpr project.

See docs/html/user_guide.html to get started (make html-docs).
See docs/html/gnoga_rm/index.html for reference manual (make rm-docs).

Gnoga includes a copy of Dmitry A. Kazakov's Simple Components and uses it for http and websockets.
See deps/simple_components/components.htm for details.
It includes an incredible wealth of Ada components you can incorporate in your software freely and
is under the GMGPLv2.

Here is an overview of Gnoga's framework:

1. The communication platform between the Ada code and the browser / native
      Gnoga.Server.Connection
      Gnoga.Server.Connection.Secure - https / SSL Support

2. Binding to the HTML5 DOM and Browser
      Gnoga.Gui.Base (Not per se a binding of Node but takes its place)
      Gnoga.Gui.Element, Gnoga.Gui.Element.* (HTML Elements)
      Gnoga.Gui.Element.Canvas - HTML 5 Canvas bindings
      Gnoga.Gui.Element.SVG - HTML SVG vector graphics
      Gnoga.Gui.Element.Multimedia - HTML 5 Audio and Video
      Gnoga.Gui.Element.Style - CSS Style blocks
      Gnoga.Gui.Window, Gnoga.Gui.Navigator, Gnoga.Gui.Screen,
      Gnoga.Gui.Location
      Gnoga.Gui.Document

3. Application start up services
      Gnoga.Server.Application.Singleton - Desktop apps
      Gnoga.Server.Application.Multi_Connect - Multi user / Web apps

4. Gnoga higher level containers and GUI widgets
      Gnoga.Gui.Views.* - Auto layout of child elements and basis for
                          custom Gnoga Ada only widgets
      Gnoga.Gui.Views.Docker - Dock child views to view sides
      Gnoga.Gui.Views.Card - Stacks of views
      Gnoga.Gui.Views.Console - Views with auto scroll down
      Gnoga.Gui.Views.Grid - Grid of views
      Gnoga.Gui.Views.Modal_Dialog - Modal views

5. Gnoga client side application APIs
      Gnoga.Client.Storage - local persistent and session storage on browser
      Gnoga.Client.Bind_Page - Bind to all elements on pre-made HTML5 pages

6. Gnoga database bindings and server side APIs
      Gnoga.Server.Database - support for MySQL and SQLite 3
	(for ODBC bindings see deps/simple_components)
      Gnoga.Server.Model - Active Data models like in Rails
      Gnoga.Server.Migrations - Rails like database schema migrations
      Gnoga.Server.Template_Parser - Parse files with tokens or Python 2.7
           (install libpython27-dev to use the Python parser)

7. Gnoga development tools
      tool/gnoga_make - Generate application scaffolds
                        and front ends to database tables

8. Plugin bindings to existing JavaScript libraries
      Gnoga.Gui.Plugin.Ace_Editor - Full editor with Ada syntax highlighting
      Gnoga.Gui.Plugin.jQuery - jQuery support to access non-Gnoga Elements
      Gnoga.Gui.Plugin.jQueryUI - all the jQueryUI Interactions and Effects
      Gnoga.Gui.Plugin.jQueryUI.Widgets - the jQueryUI Widgets
      Gnoga.Gui.Plugin.Boot_Strap - general binding to Boot Strap
      Gnoga.Gui.Plugin.Messages_Boxes - dialog message boxes
      Gnoga.Gui.Plugin.MNMenu - menu construction based on list
      Gnoga.Gui.Plugin.PIXI - dynamic randering graphic elements such as sprites
      Gnoga.Gui.Plugin.JSTree - tree construction based on list
      Gnoga.Gui.Plugin.Ace_Editor.Console_IO - console emulation based on ACE

9. Native Desktop and Mobile Application Support coming:
      Gnoga.Server.Application.Gtk_Window - Native GTK front end
