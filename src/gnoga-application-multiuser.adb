------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . A P P L I C A T I O N . M U L T I U S E R           --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;
with Gnoga.Types;

package body Gnoga.Application.Multiuser is

   package Path_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Application_Connect_Event);

   Path_Map : Path_Maps.Map;

   procedure On_Connect
     (ID         : in     Gnoga.Types.Connection_ID;
      Connection : access Gnoga.Connections.Connection_Holder_Type);
   --  Handle connections by creating Main_Winow object and dispatching
   --  to correct On_Connect_Handler based on Path

   ----------------
   -- On_Connect --
   ----------------

   procedure On_Connect
     (ID         : in     Gnoga.Types.Connection_ID;
      Connection : access Gnoga.Connections.Connection_Holder_Type)
   is
      Main_Window : Gnoga.Window.Window_Type;
   begin
      Main_Window.Attach (Connection_ID => ID);
      Main_Window.Document.Title (Application_Name);
      Connections.HTML_On_Close (ID, HTML_On_Close);

      declare
         Path : String :=
           Right_Trim_Slashes (Left_Trim_Slashes
                               (Main_Window.Location.Path_Name));
      begin
         if Path_Map.Contains (Path) then
            Path_Map.Element (Path) (Main_Window, Connection);
         elsif Path_Map.Contains ("default") then
            Path_Map.Element ("default") (Main_Window, Connection);
         else
            Connections.HTML_On_Close (ID, "No route to path.");
         end if;
      end;
   end On_Connect;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Event : in Application_Connect_Event := null;
      Host  : in String                    := "";
      Port  : in Integer                   := 8080;
      Boot  : in String                    := "boot.html")
   is
   begin
      Gnoga.Connections.Initialize (Host, Port, Boot);

      Gnoga.Connections.On_Connect_Handler
        (Event => On_Connect'Access);

      if Event /= null then
         On_Connect_Handler (Event);
      end if;
   end Initialize;

   ------------------------
   -- On_Connect_Handler --
   ------------------------

   procedure On_Connect_Handler (Event : in Application_Connect_Event;
                                 Path  : in String := "default")
   is
   begin
      Path_Map.Include (Right_Trim_Slashes (Left_Trim_Slashes (Path)), Event);
   end On_Connect_Handler;

   ---------------------
   -- Connection_Data --
   ---------------------

   procedure Connection_Data
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Data        : access Gnoga.Types.Connection_Data_Type'Class)
   is
   begin
      Gnoga.Connections.Connection_Data (ID   => Main_Window.Connection_ID,
                                         Data => Data);
   end Connection_Data;

   ------------------
   -- Message_Loop --
   ------------------

   procedure Message_Loop is
   begin
      Gnoga.Connections.Run (Wait_For_Q => False);
   end Message_Loop;

   ---------------------
   -- End_Application --
   ---------------------

   procedure End_Application is
   begin
      Gnoga.Connections.Stop;
   end End_Application;

end Gnoga.Application.Multiuser;
