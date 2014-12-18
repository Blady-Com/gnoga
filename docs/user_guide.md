# The Gnoga User Guide
## Gnoga - The GNU Omnificent GUI for Ada
## The Ada Open-Source Mission-Critical Cloud, Desktop and Mobile Application Development Framework

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

* Introduction to Gnoga
   - What is Gnoga?
   - How does Gnoga work?
   - Where can I use Gnoga?
   - What can I do with Gnoga?
   - When would I not use Gnoga?
   - Who wrote Gnoga?
* Getting Started
* Hello World in Gnoga
* Basic Gnoga concepts
* Getting around the Gnoga packages

## Introduction to Gnoga
### What is Gnoga

Defining Gnoga is an important first step to using it. Gnoga is best defined as a framework and tools to develop GUI applications for the Ada language using web technologies as a rendering engine. Gnoga should not be confused with web development frameworks. While Gnoga is very capable of creating web applications, even more capable to do so than web development frameworks, using Gnoga for web applications is only one possible use that is a byproduct of using web technologies to render the GUI.

### How does Gnoga work?

A Gnoga application can be divided in to three parts:

1. The application code written in Ada using the Gnoga framework and tools
2. The communication layer
3. The GUI rendering surface, an HTML 5 compliant browser or an embedded widget.

The communication layer is not passive as in typical web programming using http and perhaps Ajax calls to simulate a live active connection. It is rather an active connection using HTML5 websockets or direct access at the API level to an embedded widget.

Since the communication layer is always active there is a stateful constant connection to the rendering surface as there is in traditional desktop GUI development even if the rendering surface is a remote HTML 5 compliant browser.

The idea of using publishing technologies to display a GUI is not new. Next used Postscript, Mac OS X PDF and Gnoga uses HTML 5.

### Where can I use Gnoga?

Gnoga applications can be run anywhere that a suitable gcc/ada compiler version 4.7 or above compiler can target that provides a Gnat.Socket implementation or has direct API access to am HTML 5 client widget.

Gnoga is already being used on every major operating system and even in some embedded systems. At least one commercial appliance is being developed using Raspberry Pi boards and Gnoga to provide the user interface.

### What can I do with Gnoga?

Gnoga's use of web technologies, lite requirements and flexible communication layer allows for an incredible variety of potential uses.

1. Traditional cross platform GUI development that normally would be done in Gtk, Qt
2. In place of native GUI development that is with in the scope of available web technologies.
3. Thin client multiuser applications
4. Remote GUIs to embedded systems
5. Web base applications
6. Web site development
7. Collaborative realtime update applications
8. and so much more...

### When would I not use Gnoga?

Gnoga is ideal for almost all GUI application development. however it wouldn't be the best choice for writing an application that required close integration with the operating system's native UI. That doesn't mean to say you can't combine native UI development with Gnoga or that even in this case often Gnoga is still the ideal solution. 

For example, On Mac OS X, native Gnoga applications often use Mac Gap 2 (http://macgapproject.github.io/). Using Mac Gap Gnoga applications have direct access to most of the native GUI as well. It is also possible to create on any platform a native application with an embedded html 5 widget such as webkit and then have the best of both worlds.

### Who wrote Gnoga?

Gnoga's author is [David Botton](http://botton.com). Gnoga's http and websocket implementation is from Simple Components by [Dmitry A. Kazakov](http://www.dmitry-kazakov.de/).

However with out the good people on the [Gnoga e-mail list](https://lists.sourceforge.net/lists/listinfo/gnoga-list) using, testing and pushing for more Gnoga would not be a reality. I'm sorry for not mentioned everyone by name but the e-mail archives will bear your fame forever.
