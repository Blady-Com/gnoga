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
------------------------------------------------------------------------------

with Ada.Task_Identification;

with Gnoga.Server.Connection;
with Gnoga.Types;

package body Gnoga.Application.Singleton is
   Connection_ID : Gnoga.Types.Connection_ID := Gnoga.Types.No_Connection;
   --  Set after Initialization

   Application_Holder : Gnoga.Server.Connection.Connection_Holder_Type;
   --  Used to block Initialize until On_Connect is called

   Connection_Holder : Gnoga.Server.Connection.Connection_Holder_Type;
   --  Used to hold the single incoming connection

   procedure On_Connect
     (ID         : in     Gnoga.Types.Connection_ID;
      Connection : access Gnoga.Server.Connection.Connection_Holder_Type);
   --  Connection On_Connect handler

   ----------------
   -- On_Connect --
   ----------------

   procedure On_Connect
     (ID         : in     Gnoga.Types.Connection_ID;
      Connection : access Gnoga.Server.Connection.Connection_Holder_Type)
   is
   begin
      if Connection_ID = Gnoga.Types.No_Connection then
         Connection_ID := ID;

         Application_Holder.Release;

         Connection.Hold;

         Connection_Holder.Release;

         Gnoga.Server.Connection.Stop;
      else
         Gnoga.Server.Connection.Execute_Script
           (ID, "document.writeln ('Only one connection permitted.');");
      end if;
   end On_Connect;

   ---------------------
   -- Web_Server_Task --
   ---------------------

   task type Web_Server_Task is
      entry Start;
   end Web_Server_Task;
   type Web_Server_Task_Access is access all Web_Server_Task;

   Web_Server : Web_Server_Task_Access := null;

   task body Web_Server_Task is
   begin
      accept Start;
      Gnoga.Server.Connection.Run;
   end Web_Server_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Host        : in     String  := "";
      Port        : in     Integer := 8080;
      Boot        : in     String  := "boot.html";
      Verbose     : in     Boolean := True)
   is
   begin
      Gnoga.Activate_Exception_Handler (Ada.Task_Identification.Current_Task);
      Gnoga.Server.Connection.Initialize (Host, Port, Boot, Verbose);

      if Verbose then
         Gnoga.Write_To_Console ("Singleton application.");
         Gnoga.Write_To_Console ("Press Ctrl-C to close server.");
      end if;

      Gnoga.Server.Connection.On_Connect_Handler
        (Event => On_Connect'Access);

      Web_Server := new Web_Server_Task;
      Web_Server.Start;

      Application_Holder.Hold;

      Main_Window.Attach (Connection_ID => Connection_ID);
      Gnoga.Server.Connection.HTML_On_Close (Connection_ID, HTML_On_Close);

      Main_Window.Document.Title (Title);
   end Initialize;

   ------------------
   -- Message_Loop --
   ------------------

   procedure Message_Loop is
   begin
      Connection_Holder.Hold;
   end Message_Loop;

   ---------------------
   -- End_Application --
   ---------------------

   procedure End_Application is
   begin
      Gnoga.Server.Connection.Close (Connection_ID);
   end End_Application;
end Gnoga.Application.Singleton;
