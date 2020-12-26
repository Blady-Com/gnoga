------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . S E R V E R . M I G R A T I O N               --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
with Ada.Exceptions;

with Gnoga.Server.Model.Table;

package body Gnoga.Server.Migration is

   ----------------------
   -- Add_Migration_Up --
   ----------------------

   procedure Add_Migration_Up
     (Collection : in out Migration_Collection;
      SQL        : in     String)
   is
   begin
      Collection.Migrations_Up.Append (SQL);
   end Add_Migration_Up;

   ------------------------
   -- Add_Migration_Down --
   ------------------------

   procedure Add_Migration_Down
     (Collection : in out Migration_Collection;
      SQL        : in     String)
   is
   begin
      Collection.Migrations_Down.Append (SQL);
   end Add_Migration_Down;

   ----------------
   -- Migrate_To --
   ----------------

   procedure Migrate_To
     (Collection : in out Migration_Collection;
      Connection : in out Gnoga.Server.Database.Connection'Class;
      Level      : in     Natural)
   is
      package Migration_Model is new Gnoga.Server.Model.Table ("gnogaparams", Connection'Access);

      Actual_Level         : Natural := Level;
      Current_Level        : Natural := 0;
      Dummy_Migration_Info : Migration_Model.Active_Record;
   begin
      Dummy_Migration_Info.Find_Where ("name='migration_level'", Create_New => True);

      if Dummy_Migration_Info.Exists ("name") then
         Current_Level := Value (Dummy_Migration_Info.Value ("value"));
      else
         Dummy_Migration_Info.Value ("value", "0");
      end if;

      Gnoga.Write_To_Console ("Current migration level =" & Image (Current_Level));

      if Level > Collection.Migrations_Up.Last_Index then
         Actual_Level := Collection.Migrations_Up.Last_Index;
      end if;

      if Actual_Level > Current_Level then
         Gnoga.Write_To_Console ("Requested migration level up to" & Image (Actual_Level));
         for i in Current_Level + 1 .. Actual_Level loop
            Gnoga.Write_To_Console ("Migrating to level" & Image (i));
            Dummy_Migration_Info.Value ("value", Image (i));

            Gnoga.Write_To_Console ("Running : " & Collection.Migrations_Up.Element (i));
            Connection.Execute_Query (Collection.Migrations_Up.Element (i));
         end loop;
         Gnoga.Write_To_Console ("Done migration");
      end if;

      if Actual_Level < Current_Level then
         Gnoga.Write_To_Console ("Requested migration level down to" & Image (Actual_Level));
         for i in reverse Actual_Level + 1 .. Current_Level loop
            Gnoga.Write_To_Console ("Migrating from level" & Image (i));
            Dummy_Migration_Info.Value ("value", Image (i - 1));
            Gnoga.Write_To_Console ("Running : " & Collection.Migrations_Down.Element (i));
            Connection.Execute_Query (Collection.Migrations_Down.Element (i));
         end loop;
         Gnoga.Write_To_Console ("Done migration");
      end if;

      Gnoga.Write_To_Console ("Saving new migration_level = " & Dummy_Migration_Info.Value ("value"));
      Dummy_Migration_Info.Value ("name", "migration_level");
      Dummy_Migration_Info.Save;
   end Migrate_To;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Collection : in out Migration_Collection;
      Connection : in out Gnoga.Server.Database.Connection'Class)
   is
   begin
      Create_Param_Table :
      begin
         Connection.Execute_Query
           ("CREATE TABLE gnogaparams" & " (" & Connection.ID_Field_String & "," & "  name VARCHAR(80)," &
            "  value VARCHAR(80))");
         Gnoga.Write_To_Console ("gnogaparams table created");
      exception
         when E : Gnoga.Server.Database.Query_Error =>
            --  table already exists
            Log ("Error Create_Param_Table table already exists.");
            Log (From_Latin_1 (Ada.Exceptions.Exception_Information (E)));
      end Create_Param_Table;

      Collection.Migrate_To (Connection, Collection.Migrations_Up.Last_Index);
   end Setup;

   -------------------------------------
   -- Migrations_Handled_Command_Line --
   -------------------------------------

   function Migrations_Handled_Command_Line
     (Connection          : in Gnoga.Server.Database.Connection_Access;
      Migration_Procedure :    access procedure (Collection : in out Migration_Collection))
      return Boolean
   is
      use Ada.Command_Line;
      use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;

      M : Migration_Collection;
   begin
      if Argument_Count > 0 then
         declare
            Command : constant String := Translate (From_Latin_1 (Argument (1)), Lower_Case_Map);
         begin
            if Command = "setup" then
               Migration_Procedure (M);
               M.Setup (Connection.all);
               return True;
            elsif Command = "migrate" then
               Migration_Procedure (M);
               M.Migrate_To (Connection.all, Natural'Value (Ada.Command_Line.Argument (2)));
               return True;
            end if;
         end;
      end if;
      return False;
   end Migrations_Handled_Command_Line;

end Gnoga.Server.Migration;
