------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . A P P L I C A T I O N . S I N G L E T O N           --
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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------                                                                          --

with Gnoga.Connections;

package body Gnoga.Application.Singleton is
   Connection_ID : Gnoga.Types.Connection_ID := Gnoga.Types.No_Connection;
   --  Set after Initialization

   Application_Holder : Gnoga.Connections.Connection_Holder_Type;
   --  Used to block Initialize until On_Connect is called

   Connection_Holder : Gnoga.Connections.Connection_Holder_Type;
   --  Used to hold the single incoming connection

   procedure On_Connect
     (ID         : in     Gnoga.Types.Connection_ID;
      Connection : access Gnoga.Connections.Connection_Holder_Type);

   procedure On_Connect
     (ID         : in     Gnoga.Types.Connection_ID;
      Connection : access Gnoga.Connections.Connection_Holder_Type)
   is
   begin
      if Connection_ID = Gnoga.Types.No_Connection then
         Connection_ID := ID;
         Application_Holder.Release;
         Connection_Holder.Hold;
      else
         Gnoga.Connections.Execute_Script
           (ID, "document.writeln ('Only one connection permitted.');");
      end if;
   end On_Connect;

   task Web_Server_Task is
      entry Start;
   end Web_Server_Task;

   task body Web_Server_Task is
   begin
      accept Start;
      Gnoga.Connections.Run (Wait_For_Q => False);
      Connection_Holder.Release;
   end Web_Server_Task;

   procedure Initialize
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Host        : in  String  := "";
      Port        : in  Integer := 8080;
      Boot        : in  String  := "boot.html")
   is
   begin
      Gnoga.Connections.Initialize (Host, Port, Boot);

      Gnoga.Connections.On_Connect_Handler
        (Event => On_Connect'Access);

      Web_Server_Task.Start;

      Application_Holder.Hold;

      Main_Window.Attach (Connection_ID => Connection_ID);
      Gnoga.Connections.HTML_On_Close (Connection_ID, HTML_On_Close);

      Main_Window.Document.Title (Application_Name);
   end Initialize;

   procedure Message_Loop is
   begin
      Connection_Holder.Hold;
   end Message_Loop;

   procedure End_Application is
   begin
      Connection_Holder.Release;
      Gnoga.Connections.Stop;
   end End_Application;
end Gnoga.Application.Singleton;
