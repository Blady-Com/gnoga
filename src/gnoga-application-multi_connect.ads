------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--       G N O G A . A P P L I C A T I O N . M U L T _ C O N N E C T        --
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

--  This package allows for the creation of GUI applications with multiple
--  connections to the same app.

with Gnoga.Types;
with Gnoga.Server.Connection;
with Gnoga.Gui.Window;

package Gnoga.Application.Multi_Connect is

   subtype Connection_Holder_Type is
     Gnoga.Server.Connection.Connection_Holder_Type;

   type Application_Connect_Event is access
     procedure (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
                Connection  : access Connection_Holder_Type);
   --  On each connection an Application_Connect_Event registered in
   --  On_Connect_Handler will be called. Main_Window will be attached
   --  to the window of the browser, Connection can optionally be used
   --  to allow for "clean up" upon close of connection, or to prevent
   --  finalization of the called Application_Connect_Event procedure
   --  until the connection is closed. This allows for Gnoga objects to
   --  be created on the stack with in the procedure and not be finalized
   --  prematurely and still able to respond to events, etc.
   --  Use: Connection.Hold;

   procedure Initialize
     (Event   : in Application_Connect_Event := null;
      Host    : in String                    := "";
      Port    : in Integer                   := 8080;
      Boot    : in String                    := "boot.html";
      Verbose : in Boolean                   := True);
   --  Initialize an applicaiton for multiple connections using
   --  Event for the default Connection Handler and Boot for bootstrap html.
   --  If Host = "" then will listen on all netwrok interfaces.
   --  Use Host = "localhost" to constrain to local use only.

   procedure On_Connect_Handler (Event : in Application_Connect_Event;
                                 Path  : in String := "default");
   --  Set an event handler for new application connections with Path. If
   --  Path = "default" then Event will be the default handler for any
   --  connection not matching another Path. Note that http://myapp:8080/abc
   --  and http://myapp:8080/abc/ will both match Path = "/abc" or Path="abc"
   --  or Path="/abc/"
   --  This can be used to set or change connection handlers during application
   --  execution for future connections or to to set the default handler if it
   --  was not set in Initialize.

   procedure Message_Loop;
   --  Start serving connections to application and continue until
   --  End_Application is called.

   procedure End_Application;
   --  Terminate application.
end Gnoga.Application.Multi_Connect;
