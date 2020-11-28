------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--               G N O G A . G U I . P L U G I N S . M A C G A P            --
--                                                                          --
--                                 B o d y                                  --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Gnoga.Server.Connection;

package body Gnoga.Gui.Plugin.MacGap is

   --------------------------
   -- Activate_Application --
   --------------------------

   procedure Activate_Application (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "activate()");
   end Activate_Application;

   ----------------------
   -- Hide_Application --
   ----------------------

   procedure Hide_Application (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "hide()");
   end Hide_Application;

   ------------------------
   -- Unhide_Application --
   ------------------------

   procedure Unhide_Application (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "unhide()");
   end Unhide_Application;

   -----------------
   -- System_Beep --
   -----------------

   procedure System_Beep (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "beep()");
   end System_Beep;

   ----------------------
   -- Bounce_Dock_Icon --
   ----------------------

   procedure Bounce_Dock_Icon (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "bounce()");
   end Bounce_Dock_Icon;

   ------------------------
   -- Launch_Application --
   ------------------------

   procedure Launch_Application
     (Window : in out Gnoga.Gui.Window.Window_Type;
      Name   : in     String)
   is
   begin
      MacGap_Execute (Window, "launch('" & Name & "')");
   end Launch_Application;

   --------------
   -- Open_URL --
   --------------

   procedure Open_URL
     (Window : in out Gnoga.Gui.Window.Window_Type;
      URL    : in     String)
   is
   begin
      MacGap_Execute (Window, "openURL('" & Escape_Quotes (URL) & "')");
   end Open_URL;

   -----------------
   -- Notify_User --
   -----------------

   procedure Notify_User
     (Window  : in out Gnoga.Gui.Window.Window_Type;
      Title   : in     String;
      Message : in     String;
      Sound   : in     Boolean := True)
   is
   begin
      MacGap_Execute
        (Window,
         "notify({title: '" & Escape_Quotes (Title) & "', " & "content: '" & Escape_Quotes (Message) & "', " &
         "sound: " & Sound'Img & "})");
   end Notify_User;

   -------------------
   -- Display_Sheet --
   -------------------

   procedure Display_Sheet
     (Window  : in out Gnoga.Gui.Window.Window_Type;
      Title   : in     String;
      Message : in     String;
      Sound   : in     Boolean := True)
   is
   begin
      MacGap_Execute
        (Window,
         "notify({type: 'sheet', " & "title: '" & Escape_Quotes (Title) & "', " & "content: '" &
         Escape_Quotes (Message) & "', " & "sound: " & Sound'Img & "})");
   end Display_Sheet;

   ----------------
   -- X_Position --
   ----------------

   function X_Position
     (Window : Gnoga.Gui.Window.Window_Type)
      return Integer
   is
   begin
      return Integer'Value (MacGap_Execute (Window, "Window.x"));
   end X_Position;

   ----------------
   -- Y_Position --
   ----------------

   function Y_Position
     (Window : Gnoga.Gui.Window.Window_Type)
      return Integer
   is
   begin
      return Integer'Value (MacGap_Execute (Window, "Window.y"));
   end Y_Position;

   ------------------
   -- Is_Maximized --
   ------------------

   function Is_Maximized
     (Window : Gnoga.Gui.Window.Window_Type)
      return Boolean
   is
   begin
      return MacGap_Execute (Window, "Window.isMaximized") = "true";
   end Is_Maximized;

   ----------
   -- Move --
   ----------

   procedure Move
     (Window : in out Gnoga.Gui.Window.Window_Type;
      X, Y   : in     Integer)
   is
   begin
      MacGap_Execute (Window, "Window.move(" & X'Img & "," & Y'Img & ")");
   end Move;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Window : in out Gnoga.Gui.Window.Window_Type;
      Width  : in     Integer;
      Height : in     Integer)
   is
   begin
      MacGap_Execute (Window, "Window.resize(" & Width'Img & "," & Height'Img & ")");
   end Resize;

   -----------
   -- Title --
   -----------

   procedure Title
     (Window : in out Gnoga.Gui.Window.Window_Type;
      Value  : in     String)
   is
   begin
      MacGap_Execute (Window, "Window.title('" & Escape_Quotes (Value) & "')");
   end Title;

   --------------
   -- Maximize --
   --------------

   procedure Maximize (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "Window.maximize()");
   end Maximize;

   --------------
   -- Minimize --
   --------------

   procedure Minimize (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "Window.minimize()");
   end Minimize;

   -------------
   -- Restore --
   -------------

   procedure Restore (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "Window.restore()");
   end Restore;

   ------------------------
   -- Toggle_Full_Screen --
   ------------------------

   procedure Toggle_Full_Screen (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "Window.toggleFullscreen()");
   end Toggle_Full_Screen;

   ----------------------
   -- Application_Path --
   ----------------------

   function Application_Path
     (Window : Gnoga.Gui.Window.Window_Type)
      return String
   is
   begin
      return MacGap_Execute (Window, "applicationPath");
   end Application_Path;

   -------------------
   -- Resource_Path --
   -------------------

   function Resource_Path
     (Window : Gnoga.Gui.Window.Window_Type)
      return String
   is
   begin
      return MacGap_Execute (Window, "resourcePath");
   end Resource_Path;

   --------------------
   -- Documents_Path --
   --------------------

   function Documents_Path
     (Window : Gnoga.Gui.Window.Window_Type)
      return String
   is
   begin
      return MacGap_Execute (Window, "documentsPath");
   end Documents_Path;

   ------------------
   -- Library_Path --
   ------------------

   function Library_Path
     (Window : Gnoga.Gui.Window.Window_Type)
      return String
   is
   begin
      return MacGap_Execute (Window, "libraryPath");
   end Library_Path;

   ---------------
   -- Home_Path --
   ---------------

   function Home_Path
     (Window : Gnoga.Gui.Window.Window_Type)
      return String
   is
   begin
      return MacGap_Execute (Window, "homePath");
   end Home_Path;

   ---------------
   -- Temp_Path --
   ---------------

   function Temp_Path
     (Window : Gnoga.Gui.Window.Window_Type)
      return String
   is
   begin
      return MacGap_Execute (Window, "tempPath");
   end Temp_Path;

   ---------------------------
   -- Terminate_Application --
   ---------------------------

   procedure Terminate_Application (Window : in out Gnoga.Gui.Window.Window_Type) is
   begin
      MacGap_Execute (Window, "terminate()");
   end Terminate_Application;

   --------------------
   -- MacGap_Execute --
   --------------------

   procedure MacGap_Execute
     (Window : in out Gnoga.Gui.Window.Window_Type;
      Method : in     String)
   is
   begin
      Gnoga.Server.Connection.Execute_Script (Window.Connection_ID, "MacGap." & Method);
   end MacGap_Execute;

   function MacGap_Execute
     (Window : Gnoga.Gui.Window.Window_Type;
      Method : String)
      return String
   is
   begin
      return Gnoga.Server.Connection.Execute_Script (Window.Connection_ID, "MacGap." & Method);
   end MacGap_Execute;

end Gnoga.Gui.Plugin.MacGap;
