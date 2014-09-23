------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                    G N O G A . S E R V E R . M O D E L                   --
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

with Ada.Strings.Unbounded;
with Ada.Calendar.Formatting;

package body Gnoga.Server.Model is
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Active_Record) is
      use Gnoga.Server.Database;
   begin
      if Object.Connection = null then
         raise Connection_Error;
      end if;

      Object.Fields :=
        Object.Connection.List_Fields_Of_Table
          (Object.Table_Name.all);
   end Initialize;

   ----------------
   -- Set_Values --
   ----------------

   procedure Set_Values
     (A                   : in out Active_Record;
      Map                 : in     Gnoga.Types.Data_Maps.Map)
   is
      procedure foreach (Position : in Gnoga.Types.Data_Arrays.Cursor);

      procedure foreach (Position : in Gnoga.Types.Data_Arrays.Cursor) is
      begin
         if Map.Contains (Gnoga.Types.Data_Arrays.Element (Position)) then
            A.Set_Value
              (Gnoga.Types.Data_Arrays.Element (Position),
               Map.Element (Gnoga.Types.Data_Arrays.Element (Position)));
         end if;
      end foreach;
   begin
      A.Fields.Iterate (foreach'Access);
   end Set_Values;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (A          : in out Active_Record;
                        Field_Name : in     String;
                        Value      : in     String)
   is
   begin
      A.Values.Include (Field_Name, Value);
   end Set_Value;

   procedure Set_Value (A             : in out Active_Record;
                        Field_Name    : in     String;
                        Integer_Value : in     Integer)
   is
      Value : String := Integer'Image (Integer_Value);
   begin
      Set_Value (A, Field_Name, Value (Value'First + 1 .. Value'Last));
   end Set_Value;

   procedure Set_Value (A          : in out Active_Record;
                        Field_Name : in     String;
                        Date_Value : in     Ada.Calendar.Time)
   is
      Value : String := Ada.Calendar.Formatting.Image (Date_Value);
   begin
      Set_Value (A, Field_Name, Value);
   end Set_Value;

   -----------------
   -- Field_Names --
   -----------------

   function Field_Names (A : Active_Record)
                         return Gnoga.Types.Data_Array_Type
   is
   begin
      return A.Fields;
   end Field_Names;

   ------------
   -- Values --
   ------------

   function Values (A : Active_Record) return Gnoga.Types.Data_Maps.Map is
   begin
      return A.Values;
   end Values;

   -----------
   -- Value --
   -----------

   function Value (A : Active_Record; Field_Name : String) return String is
   begin
      return A.Values.Element (Field_Name);
   end Value;

   ------------
   -- Exists --
   ------------

   function Exists (A : Active_Record; Field_Name : String) return Boolean is
   begin
      return A.Values.Contains (Field_Name);
   end Exists;

   ----------
   -- Save --
   ----------

   procedure Save (A : in out Active_Record) is
      use Ada.Strings.Unbounded;

      fields : Unbounded_String;
      values : Unbounded_String;

      procedure foreach (Position : in Gnoga.Types.Data_Maps.Cursor);

      procedure foreach (Position : in Gnoga.Types.Data_Maps.Cursor) is
      begin
         if (Gnoga.Types.Data_Maps.Key (Position) /= "id") then
            if (A.Is_New) then
               fields := fields & "`" & Gnoga.Types.Data_Maps.Key (Position) & "` ,";
               values := values & "'" &
               A.Connection.Escape_String
                 (Gnoga.Types.Data_Maps.Element (Position)) &
               "',";
            else
               fields := fields & "`" &
               Gnoga.Types.Data_Maps.Key (Position) & "`=" &
               "'" &
               A.Connection.Escape_String
                 (Gnoga.Types.Data_Maps.Element (Position)) &
               "',";
            end if;
         end if;
      end foreach;

   begin
      A.Values.Iterate (foreach'Access);
      if A.Is_New then
         declare
            f : String := To_String (fields);
            v : String := To_String (values);

            Insert_String : String := "insert into " &
            A.Table_Name.all &
            " (" & f (f'First .. f'Last - 1) & ") VALUES (" &
            v (v'First .. v'Last - 1) & ")";
         begin
            A.Connection.Execute_Query (Insert_String);
            declare
               New_ID : String := A.Connection.Insert_ID'Img;
            begin
               A.Set_Value ("id", New_ID (New_ID'First + 1 .. New_ID'Last));
               A.Is_New := False;
            end;
         end;
      else
         declare
            f : String := To_String (fields);

            Update_String : String := "update " & A.Table_Name.all &
            " set " & f (f'First .. f'Last - 1) &
            " where id=" & A.Values.Element ("id");
         begin
            A.Connection.Execute_Query (Update_String);
         end;
      end if;
   end Save;

   ------------
   -- Delete --
   ------------

   procedure Delete (A : in out Active_Record) is
      use Ada.Strings.Unbounded;

      SQL : String := "delete from " & A.Table_Name.all &
         " where id=" & A.Value ("id");
   begin
      A.Connection.Execute_Query (SQL);
      A.Clear;
   end Delete;

   -----------
   -- Clear --
   -----------

   procedure Clear (A : in out Active_Record) is
   begin
      A.Is_New := True;
      A.Values.Clear;
   end Clear;

   ----------
   -- Find --
   ----------

   procedure Find (A : in out Active_Record; ID : in Positive) is
      use Ada.Strings.Unbounded;
      use Ada.Strings;

      Key : String := ID'Img;
      RS  : Gnoga.Server.Database.Recordset'Class :=
        A.Connection.Query ("select * from " & A.Table_Name.all &
                            " where id=" & Key (Key'First + 1 .. Key'Last));
   begin
      RS.Next;
      A.Is_New := False; -- If no exception is raised then this is not new
      A.Values := RS.Field_Values;
      RS.Close;
   end Find;

   procedure Find (A : in out Active_Record; ID : in String) is
   begin
      Find (A, Positive'Value (ID));
   end Find;

   ----------------
   -- Find_Where --
   ----------------

   procedure Find_Where (A          : in out Active_Record;
                         Where      : in     String;
                         Create_New : in     Boolean := True)
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings;

      RS : Gnoga.Server.Database.Recordset'Class :=
        A.Connection.Query ("select * from " & A.Table_Name.all &
                            " where " & Where & " LIMIT 1");
   begin
      RS.Next;
      A.Is_New := False;  -- If no exception is raised then this is not new
      A.Values := RS.Field_Values;
      RS.Close;
   exception
      when Gnoga.Server.Database.End_Of_Recordset =>
         if Create_New then
            A.Set_Value ("id", "");
            RS.Close;
         else
            raise Gnoga.Server.Database.End_Of_Recordset;
         end if;
   end Find_Where;

   ---------------
   -- Find_Item --
   ---------------

   procedure Find_Item (A          : in out Active_Record;
                        Parent     : in     Active_Record'Class;
                        Create_New : in     Boolean := True)
   is
      Remove_s : String := Parent.Table_Name.all;
      Where_Clause : String := Remove_s (Remove_s'First .. Remove_s'Last - 1)
        & "_id = " & Parent.Value ("id");
   begin
      A.Find_Where (Where_Clause, Create_New);
   end Find_Item;

end Gnoga.Server.Model;
