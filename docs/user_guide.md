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
      - A Simple Hello World program
      - The gnoga_make Tool
      - A Singleton Application
      - A Multi Connect Application
      - Advanced: The Connection Parameter and GUI elements on the Stack
      - Advanced: Per Connection App Data
      - Multi Connect Applications for a Single User
   * Getting around Gnoga
      - Gnoga Types
      - The Gnoga directory structure
      - Directory structure when developing apps
      - Directory structure when deploying apps
      - Application, Types, Gui, Server, Client
      - Plugin, Modules
      - Tags bound in Gnoga
   * Gnoga concepts
      - In and out of the DOM
      - Display, Visible, Hidden
      - Inner_HTML, Text and Value
   
## Introduction to Gnoga
### What is Gnoga

Defining Gnoga is an important first step to using it. Gnoga is a framework and associated tools for developing GUI applications using the Ada language, leveraging web technologies to provide a rendering engine. Gnoga should not be confused with a web development framework. While Gnoga is very capable of creating web applications and perform in that role and is even more capable than most web development frameworks, using Gnoga for web applications is only one possibility. Native applications for desktop and mobile are just as easy to create, all using the same code base.

### How does Gnoga work?

A Gnoga application can be divided into three parts:

1. The application code written in Ada using the Gnoga framework and tools
2. The communication layer
3. The GUI rendering surface, an HTML-5 compliant browser or an embedded widget for native platform development.

The communication layer is not passive as in typical web programming using HTTP, nor is it using Ajax calls to simulate a live-active connection. It is an active, open connection using HTML5 websockets or direct access at the API level to an embedded widget.

Since the communication layer is always active there is a stateful constant connection to the rendering surface as there is in traditional desktop GUI development even if the rendering surface is a remote browser.

The idea of using publishing technologies to display a GUI is not new. Next used Postscript; Mac OS X PDF; and Gnoga uses HTML 5.

### Where can I use Gnoga?

Gnoga applications can be run anywhere that a suitable gcc/ada compiler version 4.7 or above compiler can target that provides a Gnat.Socket implementation or has direct API access to an HTML-5 client widget.

Gnoga is already being used on every major operating system and even in some embedded systems. At least one commercial appliance is being developed using Raspberry Pi boards and Gnoga to provide the user interface.

### What can I do with Gnoga?

Gnoga's use of web technologies, its light requirements, and flexible communication layer allow for an incredible variety of potential uses.

1. Traditional cross platform GUI development that normally would be done in Gtk, Qt
2. In place of native GUI development that is within the scope of available web technologies
3. Thin-client multiuser applications
4. Remote GUIs to embedded systems
5. Web based applications
6. Web-site development
7. Collaborative real-time update applications
8. And so much more ...

### When would I not use Gnoga?

Gnoga is ideal for almost all GUI application development. However, it wouldn't be the best choice for writing an application that required close integration with the operating system's native UI. That doesn't mean to say you can't combine native UI development with Gnoga, and in those case Gnoga often remains a good solution. 

For example, on Mac OS X, native Gnoga applications use Mac Gap 2's APIs to access many native OS features (http://macgapproject.github.io/). It is also possible to create on any platform a native application with an embedded HTML 5 widget and then have the best of both worlds.

### Who wrote Gnoga?

Gnoga's author is [David Botton](http://botton.com). Gnoga's http and websockets implementation is from Simple Components by [Dmitry A. Kazakov](http://www.dmitry-kazakov.de/).

However with out the good people on the [Gnoga e-mail list](https://lists.sourceforge.net/lists/listinfo/gnoga-list) using, testing, and pushing for more, Gnoga would not be a reality. I'm sorry for not mentioning everyone by name but the e-mail archives will bear your fame forever.

## Getting Started
### Setting up your development environment

Gnoga requires a development environment with gcc/ada 4.7 or above that also implements GNAT.Sockets. Just about every OS, platform, and target is supported.

For most environments the instructions to get started can be found at GetAdaNow.com

Once you have set up a gcc/ada compiler you can either download the lastest release of Gnoga from http://www.gnoga.com, or you will need to install the git version control system and clone the current development repository. Git is installed by default on Mac OS X and some versions of Linux. For Windows there are excellent free versions available.

Use the following command with git to check out the latest version of Gnoga:

```
git clone git://git.code.sf.net/p/gnoga/code gnoga
```

### Building Gnoga

If your development environment includes gprtools then run:

```
make
```

If your development environment does not, open the Makefile and follow the directions in the Makefile to switch the build tools to use gnatmake. Then run as above:

```
make
```

This should build the Gnoga framework, the snake and mine_detector demo and the tutorials. You can give it a try now by typing:

```
bin/snake  or   bin/mine_detector
```

This will run the snake demo or mine-detector game. Open a browser and type http://127.0.0.1:8080; this will connect you to the demo and you can play the classic snake game or a mine detection simulator (read the instructions as there is less guess work used in this version and only numbered tiles are clickable).

### Installing Gnoga

There are many ways to "with" Gnoga in to new projects. In many environments with gprtools it is possible to install Gnoga as a "standard" library using:

```
make install
```

Once done it is possible to incorporate Gnoga in to a gpr project file by just adding:

```
with "gnoga";
```

The install system will also install gnoga_make in to the same directory as the gcc/ada executables.

It is not necessary though to install Gnoga. The only requirement is to place the correct path to gnoga into the "with" in your gpr file. For example:

``` ada
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

### How to Get Help Using Gnoga?

There is a great community around Gnoga and the Ada language. Here are a few good resources for reaching out for help:

1. The Gnoga E-Mail Support List
   https://lists.sourceforge.net/lists/listinfo/gnoga-list

2. The Usenet Group - Comp.Lang.Ada
   https://groups.google.com/forum/#!forum/comp.lang.ada

3. The Freenode #Ada group

4. Check GetAdaNow.com for more Ada resource

## Hello World in Gnoga

### A Simple Hello World Program

Gnoga provides a tool for setting up basic Gnoga projects (gnoga_make) but for this tutorial we are going to manually create our hello world application to learn the details about how a Gnoga application is structured and works.

We are going to assume for this tutorial that you have cloned or unarchived gnoga in the same directory that we will be creating the directory for this project.

So let's say it is ~/workspace

So to checkout and make gnoga it would be:

```
cd ~/workspace
git clone git://git.code.sf.net/p/gnoga/code gnoga
cd gnoga
make
```

Next let's create our directory structure; we will call our application hello.

```
cd ~/workspace
mkdir hello
cd hello
mkdir bin
mkdir src
mkdir html
mkdir js
```

We are calling our project directory hello, but any name could be used.

Under our root directory we create a bin directory for our executable. The executable for Gnoga applications during deployment can be in the root directory or the bin directory.

We will be placing our source files in the src directory; there is no rule that the source files need to be in their own directory.

The additional two directories, html and js, are required. They will contain our "boot" HTML file in the html directory and the js directory will contain our boot.js and jquery.min.js files.

Let's copy the needed boot files now:

```
cd ~/workspace/hello
cp ../gnoga/html/* html/
cp ../gnoga/js/* js/
```

Next let's starting writing our "hello world" application in the src directory. We will create two files, our hello.adb file containing our application and hello.gpr a project file that describes what the compiler should do.

~/workspace/hello/src/hello.adb:

``` ada
with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;

procedure Hello is
   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.Console.Console_View_Type;
begin
   Gnoga.Application.Open_URL;
   --  Open the browser to the default location of (http://127.0.0.1:8080)

   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080);
   --   Initialize Gnoga for a single connection application that will
   --   respond to port 8080
   
   Main_View.Create (Main_Window);
   --   Views are containers of elements and are the basic layout mechanism
   --   for UI objects in Gnoga. In this case our view lays out UI elements
   --   top to bottom left to right as they are created and offers some
   --   console like methods for easy output.
   
   Main_View.Put_Line ("Hello World!");
   --   The console view offers a convenient way to write text out to the UI
   --   as if it were a console application.
   
   Gnoga.Application.Singleton.Message_Loop;
   --   This tells Gnoga to wait until the user has closed the browser window
   --   or our application notifies it otherwise.
end Hello;
```

~/workspace/hello/src/hello.gpr

```
with "../../gnoga/src/gnoga.gpr";

project Hello is
   for Languages use ("Ada");
   for Source_Dirs use (".");
   for Exec_Dir use "../bin";
   for Main use ("hello.adb");
end Hello;
```

To build our application we run:

```
gnatmake -P hello.gpr
```

We can now execute our application

```
cd ~/workspace/hello
bin/hello
```

### The gnoga_make tool

In order to make it easier to get started writing gnoga applications, a tool called gnoga_make is provided to quickly create the needed directories and provide the initial program structure including a Makefile, etc.

The syntax for creating a new project is:

```
gnoga_make new NAME_OF_PROJECT NAME_OF_TEMPLATE
```

The templates currently available are:

- hello_world    - A simple hello world program using a single procedure
- singleton      - A singleton application
- multi_connect  - A mutli connection application

So to to create a simple hello_world program we could run (assuming gnoga was checked out or unarchived at ~/workspace/gnoga):

```
cd ~/workspace
gnoga/bin/gnoga_make new hello hello_world
```

If gnoga was installed using make install then you can skip the next step.

In our gpr file, let's modify where to find gnoga by editing ~/workspace/hello/src/hello.gpr and setting the first line to read

```
with "../../gnoga/src/gnoga.gpr";
```

now we can make and run our hello world application:

```
cd ~/workspace/hello
make
bin/hello
```

When hello executes it will also open the default operating system browser to http://127.0.0.1:8080.

### A Singleton Application

There are two basic application types in Gnoga, Singleton applications and Multi Connect applications. Singleton applications are ideal for single user desktop use. They allow only a single connection and exit when that connection is lost. Since the application will not be accessed in parallel by other connections implementation is easier in that there is no need to protect data except from parallel incoming events from a single user (In Gnoga events are not serialized and are fired as they are generated concurrently).

To demonstrate the use of a singleton application we will create a simple utility that will allow executing and displaying the results of a command to the operating system. This example is purposely going to be a bit more complex to help demonstrate various UI concepts with in Gnoga.

First let us generate the skeleton of our singleton application.

```
gnoga_make new GnogaCMD singleton
```

If Gnoga was not installed as a default library, we need to modify gnogacmd/src/gnogacmd.gpr with the path to the gnoga.gpr file.

```
with "../../gnoga/src/gnoga.gpr";
```

In the gnogacmd directory let's run make, since the first run of make will create our obj directory for our compiler temporaries, and run our application. If your development environment does not include gprtools, you can replace gprbuild in the Makefile with gnatmake.

```
cd ~/workspace/gnogacmd
make
bin/gnogacmd
```

Everything should build and your default browser should open with the application. Now let's close the browser window and that should cause the application to stop running as well.

We can now get started with creating our application.

First let's discuss how we would like our application to work. Let's mimic the way a shell works by displaying a command prompt, then the results of the command and once the results are returned offer another command prompt bellow the results.

In Gnoga the top most user interface object is the Window. This represents the physical browser window and connection to it. While it would be possible to place user interface elements directly in the browser window Gnoga applications usually use a container of a View_Type or child of it that will fill the entire window. View_Types are designed to help make the layout of user elements easier and more efficient, to make it easier to reuse the entire view as a user interface element itself, to make it easy to switch the entire contents of the browser window to another view if desired, and eliminate the need to create new connections to the browser, which is not only poor practice, it is not allowed in Singleton applications.

The singleton skeleton application creates a custom view called Default_View_Type and in our application that is contained in the package GnogaCMD.View. We are going to change the base Type of Default_View_Type to a Console_View_Type which will automatically provide a scroll bar and scrolling to the end of the last added element.

To do this we will switch to "with" Gnoga.Gui.View.Console in place of Gnoga.Gui.View and then change the base type of Default_View_Type to Gnoga.View.Console.Console_View_Type.

The skeleton had provided a generic label and button type, but we are going to replace those with form types for a label, text input and a default submit button. We will also replace the "with" for Gnoga.Gui.Element.Common with Gnoga.Gui.Element.Form and add a form and form elements to our view.

The resulting file should look like this now:

``` ada
with Gnoga.Gui.Base;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Element.Form;

package GnogaCMD.View is
   
   type Default_View_Type is new Gnoga.Gui.View.Console.Console_View_Type with
      record
         Entry_Form : Gnoga.Gui.Element.Form.Form_Type;
         Prompt     : Gnoga.Gui.Element.Form.Label_Type;
         Cmd_Line   : Gnoga.Gui.Element.Form.Text_Type;
         Go_Button  : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_to_Default_View_Class is access all Default_View_Type'Class;

   overriding
   procedure Create
     (View    : in out Default_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String  := "");
   
end GnogaCMD.View;
```

In gnogacmd-view.adb we will set up the user elements in the create procedure and handle our new view's functionality.

``` ada
with GNAT.OS_Lib;
with GNAT.Expect;

with Gnoga.Gui.Base;

package body GnogaCMD.View is
   
   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Handle submit from either hitting the submit button
   --  or pressing enter within the command line.
      
   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (View    : in out Default_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String  := "")
   is
   begin
      Gnoga.Gui.View.Console.Console_View_Type
        (View).Create (Parent, ID);
      
      View.Entry_Form.Create (Parent => View);
      
      View.Cmd_Line.Create (Form  => View.Entry_Form,
                            Size  => 40);
      
      View.Prompt.Create (Form       => View.Entry_Form,
                          Label_For  => View.Cmd_Line,
                          Contents   => "Command >",
                          Auto_Place => True);
      --  Labels can automatically place themselves before the associated
      --  control they are labeling. This is the default if Auto_Place is not
      --  specified.
      
      View.Go_Button.Create (Form  => View.Entry_Form,
                             Value => "Go");
      --  The "Value" of a submit button is the button's text. Go_Button is a submit
      --  button and will cause its Form to submit.
      
      View.On_Submit_Handler (On_Submit'Access);
      --  The submit event will bubble up if not handled at the form itself.
      --  In our case instead of placing On_Submit on the View.Entry_Form
      --  we have placed it on the parent view.
   end Create;

   ---------------
   -- On_Submit --
   ---------------
   
   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : Default_View_Type renames Default_View_Type (Object);
      --  Renaming is a convenient way to "upcast" in event handlers
      
      --  Our tutorial is focusing on Gnoga not the GNAT packages in gcc/ada
      --  so will will not go into length about using GNAT.Expect; it is
      --  left as an exercise to the reader to look at the specs.
      
      Args   : GNAT.OS_Lib.Argument_List_Access;
      Status : aliased Integer;
   begin
      Args := GNAT.OS_Lib.Argument_String_To_List (View.Cmd_Line.Value);
      --  A form element's content is contained in its Value property,
      --  not its Text propery.
      
      declare
         Result : String := Gnat.Expect.Get_Command_Output
           (Command    => Args (Args'First).all,
            Arguments  => Args (Args'First + 1 .. Args'Last),
            Input      => "",
            Status     => Status'Access,
            Err_To_Out => True);
      begin
         View.Put_HTML ("<pre>" & Result & "</pre>");
         --  Put_HTML is a convenient way to dump pure html into a view
         
         View.New_Line;
      end;
      
      View.Entry_Form.Place_Inside_Bottom_Of (View);
      --  This will move the entire Entry_Form (all of its children as well) to
      --  the bottom of the View.
      
      View.Cmd_Line.Focus;
      --  Place the text entry focus on Cmd_Line
      
      View.Cmd_Line.Select_Text;
      --  Select the entire contents of Cmd_Line
   end On_Submit;   

end GnogaCMD.View;
```

In addition we need to remove from gnogacmd-controller.adb the On_Click procedure and related parts as they no longer are needed.

We can now make and run our singleton example:

```
cd ~/workspace/gnogacmd
make
bin/gnogacmd
```

### A Multi Connect Application

Just like with the creation of the Singleton application we will start off using gnoga_make to generate a skeleton project. In this case we will build a multi-user white board.

```
gnoga_make new GnogaBoard multi_connect
```

As with the Singleton application example we will assume our project is now in ~/workspace/gnogaboard

Modify, if needed, as per the Singleton example gnogaboard/src/gnogaboard.gpr to "with" in the location of gnoga.gpr

Compile the project to make sure your changes (if you needed to make any) are correct:

```
cd ~/workspace/gnogaboard
make
bin/gnogaboard
```

When you are done testing the skeleton, press Ctr-C from the command line to close the application.

The actual layout of the files and basic structure is the same as the Singleton application. The most important difference is in the "controller".

In gnogaboard-controller.adb you will notice the procedure Default has an additional parameter called "Connection" and at the end of the body of the package there is a call to "On_Connect_Handler". (See below Advanced: The Connection Parameter and GUI elements on the Stack.) 

The On_Connect_Handler associates URLs with the "controller" procedure that will handle each incoming connection from the browser. The special URL of "default" tells Gnoga to call that handler as the default, i.e. for any URL not handled by another On_Connect_Handler. In this case it is our procedure called Default.

Our white board will be a simple example. It will allow any number of people to connect to the application and any one of the users can write with their mouse on the board. This will then display on every user's browser.

We are going to need a way to keep track of each user's connection. To do that we will keep a collection of the main views for each user in a Vector. Gnoga provides an instantiation of Ada.Containers.Vectors for Pointer_To_Base_Class, an access to Base_Type'Class (see below the section Gnoga Types)

``` ada
   Users : Gnoga.Gui.Base.Base_Type_Array;
```

We will also need to provide from the controller a procedure that can be called from the Views when a new segment needs to be drawn, we will call that procedure On_Change in the GnogaBoard.Controller package. On_Change can then access each view and call a Draw procedure that we will create for the views.

We will also need to track the destruction of user Views to remove them from our Users collection. For that we will create an event handler called On_Destroy and place it on each new view created.

Here is how our GnogaBoard.Controller will look:

``` ada
with Gnoga.Gui.Base;

with GnogaBoard.View;

package body GnogaBoard.Controller is
   
   Users : Gnoga.Gui.Base.Base_Type_Array;
   
   procedure On_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Called by a view when a new line segment is drawn by the user. It will
   --  update every user's view with the new segment.
   
   procedure On_Destroy (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Set to the On_Destroy event in each new event to remove it from that
   --  view from the Users collection.
   
   procedure On_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      use GnogaBoard.View;
      
      View : Default_View_Type renames Default_View_Type (Object);
   begin
      for i in Users.First_Index .. Users.Last_Index loop
         declare
            User_View : GnogaBoard.View.Default_View_Access :=
              GnogaBoard.View.Default_View_Access (Users.Element (i));
         begin
            User_View.Draw (View.X1, View.Y1, View.X2, View.Y2);
         end;
      end loop;
   end On_Change;   
   
   procedure On_Destroy (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      use Gnoga.Gui.Base.Base_Type_Arrays;
      
      N : Integer := Users.Find_Index (Object'Unchecked_Access);
   begin
      if N /= No_Index then
         Users.Delete (N);
      end if;
   end On_Destroy;
   
   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      View : GnogaBoard.View.Default_View_Access :=
               new GnogaBoard.View.Default_View_Type;
   begin
      View.Dynamic;
      View.Create (Main_Window);
      Users.Append (Gnoga.Gui.Base.Pointer_To_Base_Class (View));

      View.On_Change := On_Change'Access;
      --  Give the View access to our On_Change event in the controller

      View.On_Destroy_Handler (On_Destroy'Access);
   end Default;

begin
   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Default'Access, "default");   
end GnogaBoard.Controller;
```

For the View we will use the following for the spec which also includes our new Draw procedure that will be called from the controller:

``` ada
with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D;

package GnogaBoard.View is   
   
   type Default_View_Type is new Gnoga.Gui.View.View_Type with
      record
         On_Change : Gnoga.Gui.Base.Action_Event := null;
         --  Access to the controller's On_Change event to request broadcast
         --  to all children.
         X1, Y1    : Integer;
         X2, Y2    : Integer;
         Canvas    : Gnoga.Gui.Element.Canvas.Canvas_Type;
         Context   : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_to_Default_View_Class is access all Default_View_Type'Class;

   overriding
   procedure Create
     (View    : in out Default_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String  := "");     
   
   procedure Draw (View : in out Default_View_Type; X1, Y1, X2, Y2 : Integer);
   --  Draw a line from X1,Y1 to X2, Y2
   
end GnogaBoard.View;
```

In the view body we will handle the mouse events and drawing:

``` ada
package body GnogaBoard.View is

   procedure Mouse_Down
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);
   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);
   procedure Mouse_Up
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);

   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (View    : in out Default_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String  := "")
   is
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      
      View.Canvas.Create (Parent => View,
                          Width  => 400,
                          Height => 400);
      View.Canvas.Border;

      View.Canvas.On_Mouse_Down_Handler (Mouse_Down'Access);
      
      View.Context.Get_Drawing_Context_2D (View.Canvas);
   end Create;

   ----------
   -- Draw --
   ----------
   
   procedure Draw (View : in out Default_View_Type; X1, Y1, X2, Y2 : Integer)
   is
   begin
      View.Context.Begin_Path;
      View.Context.Stroke_Color ("Black");
      View.Context.Move_To (X1, Y1);
      View.Context.Line_To (X2, Y2);
      View.Context.Stroke;
   end Draw;
   
   ----------------
   -- Mouse_Down --
   ----------------
   
   procedure Mouse_Down
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      View : Default_View_Type renames Default_View_Type (Object.Parent.all);
      --  Since we place the mouse events on the Canvas, Object will always
      --  be the Canvas. We can get the parent view using Object.Parent.
   begin      
      View.X1 := Mouse_Event.X;
      View.Y1 := Mouse_Event.Y;
      View.X2 := Mouse_Event.X;
      View.Y2 := Mouse_Event.Y;

      View.Canvas.On_Mouse_Move_Handler (Mouse_Move'Access);
      View.Canvas.On_Mouse_Up_Handler (Mouse_Up'Access);
   end Mouse_Down;

   ----------------
   -- Mouse_Move --
   ----------------
   
   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      View : Default_View_Type renames Default_View_Type (Object.Parent.all);
   begin
      View.X1 := View.X2;
      View.Y1 := View.Y2;
      View.X2 := Mouse_Event.X;
      View.Y2 := Mouse_Event.Y;
      
      View.On_Change (View);      
   end Mouse_Move;

   --------------
   -- Mouse_Up --
   --------------
   
   procedure Mouse_Up
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      View : Default_View_Type renames Default_View_Type (Object.Parent.all);
   begin
      View.X1 := View.X2;
      View.Y1 := View.Y2;
      View.X2 := Mouse_Event.X;
      View.Y2 := Mouse_Event.Y;
      
      View.On_Change (View);

      View.Canvas.On_Mouse_Move_Handler (null);
      View.Canvas.On_Mouse_Up_Handler (null);
   end Mouse_Up;
end GnogaBoard.View;
```

You can now give it a try and open multiple browsers accessing the application (http://127.0.0.1:8080), and each will display instantaneously the drawing done on any browser.

```
cd ~/workspace/gnogaboard
make
bin/gnogaboard
```

While this example works, there is an issue. In Gnoga and in particular in Multi-Connect applications, two things must always be considered:

1. Concurrency
2. Exceptions

Given that:

While highly unlikely, it is possible for our Users vector to be accessed concurrently, something that that standard Ada containers are not designed to handle. If this were a production application, Users should be protected.

While again highly unlikely given our application, it is possible for a view to be destroyed during the On_Change event. This could result in trying to call User_View.Draw on an already deallocated object. Therefore, it would be a good idea to capture exceptions in the On_Change event at the very least, or as part of protecting the Users vector a means be included to insure the validity of the User_View before calling Draw.

Gnoga will handle most exceptional situations not handled in your code, but creating a solid Multi-Connect application should include considerations for the above in all designs.

Improving on our example is left as an exercise to the reader.


### Advanced: The "Connection" Parameter and GUI elements on the Stack

The extra parameter "Connection" in our controller procedure "Default" can be used when you wish to block the connection procedure until the connection is closed. The two common uses of Connection.Hold to block until connection loss are:

1. To add clean up code on connection loss to the connection procedure; this could also have been added to the On_Destroy event for Main_Window.

2. To prevent finalization of statically defined GUI elements within the connection procedure until the connection has been lost.

An example of this second method would allow us to rewrite the skeleton procedure as:

``` ada
   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      View : GnogaBoard.View.Default_View_Type;
   begin
      View.Create (Main_Window);
      View.Click_Button.On_Click_Handler (On_Click'Access);
      
      Connection.Hold;
   end Default;
```

### Advanced: Per Connection App Data

In the multi-connect example above we use the connection's main view to store data specific for each user connection. It is often convenient to have a data structure containing the data specific to a connection. Gnoga offers a way to associate data to a connection and allow access to those data through any GUI element on that connection.

The following is an example 

``` ada
   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Hello_World : aliased Common.DIV_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class;
                       Event  : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Hello_World.Text ("I've been clicked");
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;
      App.Hello_World.Create (Main_Window, "Click Me");
      --  By default Connection_Data is assumed to be a dynamic object
      --  and freed when the connection is closed. To use static app
      --  data pass Dynamic => False
      ...
   end On_Connect;
```

### Multi Connect Applications for a Single User

A Multi-Connect application allows multiple connections to the same application at the same time. This does not always imply multiple users, it could even be the same user with multiple browser windows connected to the same application. When using a multi-connect application as a single-user desktop application you simply need to restrict access to the application to the local machine and provide some way for the application to know it is time to shutdown.

Some tips:

1. In the On_Connect procedure check if there has already been a connection and if a reconnect is tried return a view that indicates application is already running.

2. If limiting to only one main window, use that window's On_Destroy event to tell the application to shut down using Gnoga.Application.Multi_Connect.End_Application if you do not provide some other way to exit the application, or track connections and end the application when all connections are closed.

3. Limit connections to the local machine only: in initialize use Initialize (Host => "127.0.0.1");

## Getting around Gnoga

### Gnoga Types

By convention in Gnoga all types are usually defined in the following way and using the following naming convention:

``` ada
type Some_Type is ....;
type Some_Access is access all Some_Type;
```

if Some_Type is a tagged type:

``` ada
type Pointer_to_Some_Class is access all Some_Type'Class
```

### The Gnoga directory structure

At the Gnoga distribution root directory there are a number of files with names in ALL CAPS that contain licensing information, the FAQ, and build and installation information.

The following is the layout of files in the Gnoga distribution:

```
Root Dir
  |
  |___ bin - gnoga_make, demo, and tutorial executables
  |
  |___ css - CSS files used by demos
  |
  |___ demo - Gnoga demos
  |
  |___ deps - Gnoga dependencies from other projects
  |    |
  |    |_ simple_components - Dmitry A. Kazakov - see components.htm
  |
  |___ docs - Gnoga documentation
  |
  |___ html - boot.html and other sample files useful for web Gnoga apps
  |
  |___ img - image files for demos
  |
  |___ js - jquery.min.js and boot.js (required for all Gnoga apps)
  |
  |___ lib - after make - libgnoga.a for linking to Gnoga applications
  |
  |___ obj - intermediate build objects created during make
  |
  |___ src - Gnoga source files
  |
  |___ templates - templates for gnoga_make and demo template files
  |
  |___ test - files used to test gnoga during Gnoga's development
  |
  |___ tools - source code for gnoga_make
  |
  |___ tutorial - tutorials for using Gnoga features
  |
  |___ upload - upload directory for demos and tests

```

### Directory structure when developing apps

If you use the gnoga_make tool it will set up a development directory structure in addition to creating a skeleton application. (See the Singleton and Multi-Connect examples in the chapters above.)

For reference the following directory structure is the basic structure during development:

```
App Dir
  |
  |___ bin - your gnoga app binary
  |
  |___ html - boot.html (or other boot loader used)
  |
  |___ js - must contain jquery.min.js and boot.js
  |
  |___ css - optional, all files served as CSS files
  |
  |___ img - optional, files served as graphics files
  |
  |___ src - Source code for your gnoga app
  |
  |___ obj - Build objects
  |
  |___ templates - optional, if using Gnoga.Server.Template_Parser
  |
  |___ upload - optional, optional directory for incoming files

```

### Directory structure when deploying apps

The ideal structure to deploy your apps for production is the following
directory structure:

```
App Dir
  |
  |___ bin - your gnoga app binary
  |
  |___ html - boot.html (or other boot loader used)
  |
  |___ js - must contain jquery.min.js
  |
  |___ css - optional, a directory for serving CSS files
  |
  |___ img - optional, files served as graphics files
  |
  |___ templates - optional, if using Gnoga.Server.Template_Parser
  |
  |___ upload - optional directory for incoming files
```

If any of the subdirectories are missing, the files expected to be in them are assumed to be in html. If the html subdirectory is also missing these files are assumed to be in App Dir. The executable can be in the bin directory or in App Dir.

### Application, Types, Gui, Server, Client

The Gnoga framework's root package is Gnoga. There are five child packages making up the five areas of Gnoga development.

   * Gnoga.Application and its children are related to initializing and managing the life cycle of Gnoga applications.
   * Gnoga.Types contains Gnoga specific types used throughout the framework.
   * Gnoga.Gui contains the user-interface portions of Gnoga. It is further divided into:
     - Gnoga.Gui.Base - Common base functionality and events to all UI objects
     - Gnoga.Gui.Document - Binding to root element of DOM in a window
     - Gnoga.Gui.Element - General binding to all UI objects
     - Gnoga.Gui.Element.Common - Common UI elements
     - Gnoga.Gui.Element.Form - Form-related UI elements
     - Gnoga.Gui.Ekement.Canvas - Binding to a drawing canvas
     - Gnoga.Gui.Element.Multimedia - Multimedia bindings
     - Gnoga.Gui.Element.SVG - SVG canvas binding
     - Gnoga.Gui.Location - Browser window location control
     - Gnoga.Gui.Navigator - Browser application control
     - Gnoga.Gui.Screen - Desktop screen properties
     - Gnoga.Gui.View - Layout control of UI elements
     - Gnoga.Gui.Window - Control of connection to UI
   * Gnoga.Server - Server side bindings and features
     - Gnoga.Server - Application settings and directories
     - Gnoga.Server.Connection - Low level control of connection to UI
     - Gnoga.Server.Database - Database bindings (MySQL and SQLite3)
     - Gnoga.Server.Migration - Database schema migration interface
     - Gnoga.Server.Model - Active Record implementation for Database access
     - Gnoga.Server.Template_Parser - Template parsing (Python or simple text)
   * Gnoga.Client - Non GUI client side bindings
     - Gnoga.Client.Storage - Local storage on client side
     - Gnoga.Client.Bind_Page - Dynamically create Gnoga objects for an HTML page

### Plugins, Modules

Users can write and publish to the Gnoga Marketplace two Gnoga-specific UI extension types, Plugins and Modules.

Plugins, including jQuery, jQueryUI, Boot_Strap, and Ace_Editor, are bindings to JavaScript libraries for use on the client side.

Modules are unique Gnoga-based UI elements written with Gnoga.


### Tags Bound in Gnoga

While Gnoga is not exactly HTML in Ada, knowing the relationships may be of assistance in developing your application:

```
*  HTML5 Tags Bound as Gui Elements in Gnoga

   <a>,<hr>,<br>,<button>,<div>,<img>,<meter>,<progress>,<p>
               - Element.Common, also see Gui.View for <div> and <span>
   <canvas>
               - Element.Canvas
   <svg>
               - Element.SVG

   <form>,<input>,<textarea>,<select>,<datalist>,<legend>,<label>,<option>,
   <optgroup>
               - Element.Form,
   <fieldset>
               - Element.Form.Fieldset

   <audio>,<video>,<source>*,<track>*
               - Element.Multimedia, * Not needed

   <iframe>
               - Element.IFrame

   <html>,<body>,<head>
               - Access through Window_Type.Document

   <ul>,<ol>,<li>,<dl>,<dd>,<dt>
               - Element.List

   <address>, <article>, <aside>, <header>, <main>, <nav>, <p>, <pre>,
   <section>
               - Element.Section

   <code>,<strong>,<em>,<dfn>,<samp>,<kbd>,<var>,<marked>,<del>,<ins>,
   <s>,<q>,<big>,<small>,<time>,<tt>,<wbr>
               - Element.Phrase

   <link>,<style>,<title>
               - Element.Style_Block, The Style property on Element_Type
               - Document.Load_CSS, Document.Title
               - Since content is generated by code

   <table>,<caption>,<td>,<tr>,<th>,<col>,<colgroup>,<tfoot>,<thead>
               - Element.Table


*  HTML5 Tags Unbound as Gui Elements in Gnoga
       Note: All tags can be bound and used with
             Element_Type.Create_With_HTML
             For various reasons as described here,
             they are not bound specifically.


   <map>,<area>
               - No specific bindings currently for image maps; best
               - generated with an automated tool as regular HTML.

   <bdi>,<bdo>,<ruby>,<rp>,<rt>,
   <details>,<output>,<figure>,<flgcaption>
               - Text formatting tags are not bound. Have no application
               - specific use. Span_Type should be used to contain text
               - that needs interaction or interactive styling.

   <object>,<embed>,<script>,<noscript>,<param>,<applet>
               - No bindings are made for external plugins or scripting
               - tags

   <base>,<meta>
               - base and meta only make sense for static pages

   <dialog>,<keygen>,<menu>,<menuitem>
               - No browsers support these tags in a way worth binding yet

   <frameset>,<frame>,<noframes>
               - No window level frame support; see Element.IFrame
```

## Gnoga Concepts
###  In and out of the DOM

An HTML document is a hierarchical collection of objects. In a browser window, the displayed document is the browser's DOM, but it is possible within JavaScript to have objects that are not in the main DOM and have their own hierarchical collections, DOMs.

Gnoga maintains on the browser side JavaScript references to GUI elements it creates; these elements may or may not also be in the browser's DOM. When creating a new object in Gnoga, if the parent is in the browser's DOM, the child will be there as well. If not, it will just be part of the parent's individual DOM and not be visible, and even changing the visibility of that object will not make it appear on the browser window since they are not in the Browser's DOM.

To take an object and all its children out of the DOM use Gnoga.Gui.Element.Remove; to place it back in the DOM use one of the Place_* methods in Gnoga.Gui.Element.

### Display, Visible, Hidden

HTML5 uses a few different independently working properties for visibility. 

Visible will turn on or off the visibility of an element and its children, but the objects will still take the same space on the page.

Hidden will turn off visibility and the object and its children will no longer take up space on the page, either.

Display changes how the elements are laid out by the browser. Using Display (None) acts in the same fashion as Hidden.

### Inner_HTML, Text and Value

Retrieving the contents of an Element in Gnoga differs depending on the type of Element. For form Elements the Value method is used. For others Text can be used to retrieve the text alone or Inner_HTML to retrieve the contents including any HTML tags present.

The reason there are different methods is based on the way the underlying HTML 5 works. Text and Inner\_HTML are retrieving all child nodes with in the element while Value is an attribute of Form elements. So Text or Inner_HTML will return the contents of every child.

