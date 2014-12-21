# The Gnoga Native Mac Application User Guide
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
   - Is it Mac App Store Compatible?
* Getting Started
* The MacGap Plugin

## Introduction

### How does it work?

Gnoga takes advantage of a project called MacGap to provide a ready to go XCode project that contains a WebView which is a standard feature of native OS X apps and various extensions to it that provide JavaScript access to common native Mac OS X functionality. A Gnoga app is placed in to the MacGap project and all of its supporting files. When the MacGap application starts it loads the Gnoga application. Using the MacGap plugin for Gnoga the application has access to all the native features MacGap exposed view JavaScript.

### Is it Mac App Store Compatible?

MacGap which is used by Gnoga for native Mac apps makes use of WebViews which is a standard feature that is available when developing native OS X apps within XCode. The API that is added is also based on official API's provided by Apple. Therefore there are no dirty hacks, no embedded chrome browser or anything that would stand in your way for Mac App Store submission.

## Getting Started

1. Create a singleton app using Gnoga
2. Make native support for Mac using:
     make native_osx
3. Copy your project's individual bin, js, etc. directories to deps/MacGap2/public
4. Modify the index.html file in deps/MacGap2/public to contain the following lines:

   ``` html
   <script type="text/javascript" charset="utf-8">
   var p = MacGap.resourcePath + "/public/bin/YOUR_GNOGA_APP_NAME";
   MacGap.launch (p);
   window.open("http://127.0.0.1:8080","_self")
   </script>
   ```

   Note: The index.html page can be used to display some sort of "loading"
         message if desired, although usually loading is instant.
5. From the deps directory run - open MacGap2/MG.xcodeproj/
6. Build as you would any native Mac OS X application for XCode

## The MacGap Plugin

There is extensive support for native Mac OS X functionality via the MacGap
binding. See Gnoga.Gui.Plugin.MacGap
