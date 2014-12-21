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
      - Setting up your development environment
      - Building Gnoga
      - Installing Gnoga
      - How to get help using Gnoga
   * Writing Applications in Gnoga
      - A simple Hello World program
      - The gnoga_make tool
      - A singleton application
      - A multiconnect application
   * Getting around the Gnoga packages
   * Gnoga concepts
      - Multiconnect, data and exceptions
      - Views
      - Display, Visible, Hidden
      - Native applications
   
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

## Getting Started

### Setting up your development environment

Gnoga requies a development environment with gcc/ada 4.7 or above that also implements GNAT.Sockets. Just about every OS, platform and target is supported.

For most environments the instructions to get started can be found at GetAdaNow.com

Once you have set up a gcc/ada compiler you will need to install the git version control system. This is installed by default on Mac OS X and some versions of linux. For Windows there are excellent free versions available on Sourceforge.net

Use the following command with git to check out the latest version of Gnoga:

git clone git://git.code.sf.net/p/gnoga/code gnoga

Once you have cloned gnoga we are ready to starting building it.

### Building Gnoga

If your development environment includes gprtools then run:

```
make
```

If your development environment does now, open the Makefile and follow the directions in the Makefile to switch to use gnatmake in its place. Then run as above:

```
make
```

This should build the Gnoga framework a demo or two and the tutorials. You can give a try out now by typing:

```
bin/snake
```

This will run the snake demo. Open a browser and type http://127.0.0.1:8080 this will connect you to the demo and you can play the classic snake game.

### Installing Gnoga

There are many ways to incorporate Gnoga in to new projects. In environments with GPR tools it is possible install Gnoga as a "standard" library using:

```
make install
```

Once done it is possible to incorporate Gnoga in to projects by just adding with "gnoga". It also installs gnoga_make in to the same directory as the gcc/ada executable gnat.

It is not neccessary though to install Gnoga to use it. Only to place the correct path to gnoga in to the "with" in your gpr file. For example:

```
with "../../gnoga/src/gnoga.gpr";

project Gnoga_Web is
   for Languages use ("Ada");
   for Source_Dirs use (".");
   for Object_Dir use "../obj";
   for Exec_Dir use "../bin";
   for Main use ("gnoga_web-main.adb","source_view.adb");

   package Binder is
     for Default_Switches ("ada") use ("-E");
   end Binder;

   package Builder is
      for Executable ("gnoga_web-main.adb") use "gnoga_web";
   end Builder;
end Gnoga_Web;
```

### How to get help using Gnoga

There is a great community around Gnoga and the Ada language. Here are a few good resources for reaching out for help:

1. The Gnoga E-Mail Support List
   https://lists.sourceforge.net/lists/listinfo/gnoga-list

2. The Usenet Group - Comp.Lang.Ada
   https://groups.google.com/forum/#!forum/comp.lang.ada

3. The Freenode #Ada group

## Hello World in Gnoga

### A simple Hello World program

Gnoga provides a tool for setting basic Gnoga projects but for this chapter we are going to manually create our hello world application to learn the details about how a Gnoga application is structured and works.

We are going to assume for this little tutorial that you have checked out or unarchived gnoga in the same directory that we will be building this project.

So lets say it is ~/workspace

We can checkout and make gnoga using:

```
cd ~/workspace
git clone git://git.code.sf.net/p/gnoga/code gnoga
cd gnoga
make
```


Next lets create our directory structure, we will call our application hello.

```
cd ~/workspace
mkdir hello
cd hello
mkdir bin
mkdir src
mkdir html
mkdir js
```

Our root directory for our project we are calling hello after the name of our program, any name of course could be used.

Under our root directory we create a bin directory for our application. The binary for Gnoga applications can be in the bin director or in the root but it is cleaner to have everything in its own place.

We will be place our src files in the src directory, there is no rule that the source files need to be in their own directory, but again it is cleaner.

The additional two directories, html and js, are required. They will contain our "boot" html file in the html directory and the js directory will contain our boot.js and jquery.min.js files.

Let's copy the needed boot files now:

```
cd ~/workspace/hello
cp ../gnoga/html/* html/
cp ../gnoga/js/* js/
```

Next let's writ our "hello world" application in the src directory. We will create two files, our hello.adb file containing our application and hello.gpr a project file that describes what the compiler should do.

hello.adb:

``` ada
with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;

procedure hello is
   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.Console.Console_View_Type;
begin
   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080);
   --   Initialize Gnoga for a single connection application that will
   --   respond to port 8080
   
   Main_View.Create (Main_Window);
   --   Views are containers of elements and are the basic layout mechanism
   --   for UI objects in Gnoga. In this case our view laysout UI elements
   --   top to bottom left to right as they are created and offers some
   --   console like methods for easy output.
   
   Main_View.Put_Line ("Hello World!");
   --   The console view offers a convenient way to write text out to the UI
   --   as if it was a console application.
   
   Gnoga.Application.Singleton.Message_Loop;
   --   This tells Gnoga to wait until the user has closed the browser window
   --   or our application notifies it otherwise.
end hello;
```

hello.gpr

```
with "../../gnoga/src/gnoga.gpr";

project hello is
   for Languages use ("Ada");
   for Source_Dirs use (".");
   for Exec_Dir use "../bin";
   for Main use ("hello.adb");
end hello;
```

To build our application we simple run:

```
gprbuild
```

or if you do not have gprtools use

```
gnatmake -P hello.gpr
```

We can now executre our application

```
cd ~/workspace/hello
bin/hello
```

Open a browser and go to the URL: http://127.0.0.1:8080

### The gnoga_make tool

In order to make it easier to get started writing gnoga applications, a tool called gnoga_make is provided to quickly create the needed directories and provide the initial program structure including makefiles, etc.

The syntax for creating a new project is:

```
gnoga_make new NAME_OF_PROJECT NAME_OF_TEMPLATE
```

The templates current available are:

- hello_world
- singleton
- multi_connect


So to to create a simple hello_world program we can do (assuming gnoga was checked out or unarchived at ~/workspace/gnoga):

```
cd ~/workspace
gnoga/bin/gnoga_make new hello hello_world
```

If gnoga was installed use make install then you can skip the next step.

In our gpr file, let's modify where to find gnoga by editting ~/workspace/hello/src/hello.gpr and setting the first line to read

with "../../gnoga/src/gnoga.gpr";

now we can make and run our hello world application:

```
cd ~/workspace/hello
make
bin/hello
```

Open a browser to http://127.0.0.1:8080

