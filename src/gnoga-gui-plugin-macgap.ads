------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--               G N O G A . G U I . P L U G I N S . M A C G A P            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Gnoga.Gui.Window;

package Gnoga.Gui.Plugin.MacGap is

   procedure Activate_Application
     (Window : in out Gnoga.Gui.Window.Window_Type);

   procedure Hide_Application (Window : in out Gnoga.Gui.Window.Window_Type);

   procedure Unhide_Application (Window : in out Gnoga.Gui.Window.Window_Type);

   procedure System_Beep (Window : in out Gnoga.Gui.Window.Window_Type);

   procedure Bounce_Dock_Icon (Window : in out Gnoga.Gui.Window.Window_Type);

   procedure Launch_Application (Window : in out Gnoga.Gui.Window.Window_Type;
                                 Name   : in     String);
   --  Launch application with Name or Name is a full path to executable, e.g.
   --    Launch_Application (Window, "TextEdit");

   procedure Open_URL (Window : in out Gnoga.Gui.Window.Window_Type;
                       URL    : in     String);
   --  Open URL in default browser

   procedure Notify_User (Window  : in out Gnoga.Gui.Window.Window_Type;
                          Title   : in     String;
                          Message : in     String;
                          Sound   : in     Boolean := True);
   --  Create a system user notification

   procedure Display_Sheet (Window  : in out Gnoga.Gui.Window.Window_Type;
                            Title   : in     String;
                            Message : in     String;
                            Sound   : in     Boolean := True);
   --  Display a "sheet" notification on window

   function X_Position (Window : Gnoga.Gui.Window.Window_Type) return Integer;
   function Y_Position (Window : Gnoga.Gui.Window.Window_Type) return Integer;
   --  Position of window

   function Is_Maximized (Window : Gnoga.Gui.Window.Window_Type)
                          return Boolean;

   procedure Move (Window : in out Gnoga.Gui.Window.Window_Type;
                   X, Y   : in     Integer);
   --  Move window to position X, Y

   procedure Resize (Window : in out Gnoga.Gui.Window.Window_Type;
                     Width  : in     Integer;
                     Height : in     Integer);

   procedure Title (Window : in out Gnoga.Gui.Window.Window_Type;
                    Value  : in     String);

   procedure Maximize (Window : in out Gnoga.Gui.Window.Window_Type);
   procedure Minimize (Window : in out Gnoga.Gui.Window.Window_Type);
   procedure Restore (Window : in out Gnoga.Gui.Window.Window_Type);
   procedure Toggle_Full_Screen (Window : in out Gnoga.Gui.Window.Window_Type);

   function Application_Path (Window : Gnoga.Gui.Window.Window_Type)
                              return String;
   --  Path to Mac application

   function Resource_Path (Window : Gnoga.Gui.Window.Window_Type)
                           return String;
   --  Path to Mac application's resource path

   function Documents_Path (Window : Gnoga.Gui.Window.Window_Type)
                            return String;
   --  Path to user's documents directory

   function Library_Path (Window : Gnoga.Gui.Window.Window_Type)
                          return String;
   --  Path to applications library directory

   function Home_Path (Window : Gnoga.Gui.Window.Window_Type)
                       return String;
   --  Path to user's home directory or sandbox home directory

   function Temp_Path (Window : Gnoga.Gui.Window.Window_Type)
                       return String;
   --  Path to applications temp directory

   procedure Terminate_Application
     (Window : in out Gnoga.Gui.Window.Window_Type);
   --  Closes Mac OS X application

   procedure MacGap_Execute (Window : in out Gnoga.Gui.Window.Window_Type;
                             Method : in     String);
   function MacGap_Execute (Window : Gnoga.Gui.Window.Window_Type;
                            Method : String)
                            return String;
   --  Execute a method on the MacGap object for Window
   --  Method is eval'd JavaScript (quotes have to be escaped if any).

end Gnoga.Gui.Plugin.MacGap;
