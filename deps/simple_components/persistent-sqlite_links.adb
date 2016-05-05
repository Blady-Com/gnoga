--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.SQLite_Links                     Luebeck            --
--  Implementation                                 Winter, 2009       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Interfaces;         use Interfaces;

package body Persistent.SQLite_Links is

   procedure Create_Table
             (  Base       : Data_Base;
                Table_Name : String;
                Data_Type  : String := "INTEGER"
             )  is
   begin
      Exec
      (  Base,
         (  "CREATE TABLE IF NOT EXISTS "
         &  Table_Name
         &  " (dependant "
         &  Data_Type
         &  ", referent "
         &  Data_Type
         &  ")"
      )  );
   end Create_Table;

   procedure Delete
             (  Base       : Data_Base;
                Table_Name : String;
                Object     : Object_ID
             )  is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Base,
            (  "DELETE FROM "
            &  Table_Name
            &  " WHERE dependant = ? OR referent = ?"
         )  );
      Bind (Command, 1, Object);
      Bind (Command, 2, Object);
      Step (Command);
   exception
      when End_Error =>
         null;
   end Delete;

   function Depends_On
            (  Base       : Data_Base;
               Table_Name : String;
               Dependant  : Object_ID;
               Referent   : Object_ID
            )  return Boolean is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Base,
            (  "SELECT dependant FROM "
            &  Table_Name
            &  " WHERE dependant ="
            &  Object_ID'Image (Dependant)
            &  " AND referent ="
            &  Object_ID'Image (Referent)
         )  );
      return Step (Command);
   exception
      when End_Error =>
         return False;
   end Depends_On;

   procedure Get_Set
             (  Base       : Data_Base;
                Table_Name : String;
                Key_Name   : String;
                Data_Name  : String;
                Key_Value  : Object_ID;
                Data_Set   : in out Sets.Set
             )  is
      use Sets;
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Base,
            (  "SELECT "
            &  Data_Name
            &  " FROM "
            &  Table_Name
            &  " WHERE "
            &  Key_Name
            &  " = ?"
         )  );
      Bind (Command, 1, Key_Value);
      while Step (Command) loop
         declare
            Data_ID : constant Object_ID := Column (Command, 1);
         begin
            if Data_ID /= Key_Value then
               Add (Data_Set, Data_ID);
            end if;
         end;
      end loop;
   exception
      when End_Error =>
         null;
   end Get_Set;

   procedure Get_Dependants
             (  Base       : Data_Base;
                Table_Name : String;
                Referent   : Object_ID;
                Dependants : in out Sets.Set
             )  is
   begin
      Get_Set
      (  Base       => Base,
         Table_Name => Table_Name,
         Key_Name   => "referent",
         Data_Name  => "dependant",
         Key_Value  => Referent,
         Data_Set   => Dependants
      );
   end Get_Dependants;

   procedure Get_References
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referents  : in out Sets.Set
             )  is
   begin
      Get_Set
      (  Base       => Base,
         Table_Name => Table_Name,
         Key_Name   => "dependant",
         Data_Name  => "referent",
         Key_Value  => Dependant,
         Data_Set   => Referents
      );
   end Get_References;

   procedure Get_References
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referents  : in out Unbounded_Array;
                Pointer    : in out Integer
             )  is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Base,
            (  "SELECT referent FROM "
            &  Table_Name
            &  " WHERE dependant = ?"
         )  );
      Bind (Command, 1, Dependant);
      while Step (Command) loop
         declare
            Data_ID : constant Object_ID := Column (Command, 1);
         begin
            if Data_ID /= Dependant then
               Put (Referents, Pointer, Data_ID);
               Pointer := Pointer + 1;
            end if;
         end;
      end loop;
   exception
      when End_Error =>
         null;
   end Get_References;

   function Has_Dependants
            (  Base       : Data_Base;
               Table_Name : String;
               Referent   : Object_ID
            )  return Boolean is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Base,
            (  "SELECT dependant FROM "
            &  Table_Name
            &  " WHERE referent = ?"
         )  );
      Bind (Command, 1, Referent);
      while Step (Command) loop
         if Referent /= Column (Command, 1) then
            return True;
         end if;
      end loop;
      return False;
   exception
      when End_Error =>
         return False;
   end Has_Dependants;

   procedure Reference
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referent   : Object_ID
             )  is
      Command : Statement;
   begin
      if not Depends_On
             (  Base,
                Table_Name,
                Dependant,
                Referent
             )
      then
         Command :=
            Prepare
            (  Base,
               "INSERT INTO " & Table_Name & " VALUES (?, ?)"
            );
         Bind (Command, 1, Dependant);
         Bind (Command, 2, Referent);
         Step (Command);
      end if;
   end Reference;

   procedure Unreference
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID;
                Referent   : Object_ID
             )  is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Base,
            (  "DELETE FROM "
            &  Table_Name
            &  " WHERE dependant = ? AND Referent = ?"
         )  );
      Bind (Command, 1, Dependant);
      Bind (Command, 2, Referent);
      Step (Command);
   end Unreference;

   procedure Unreference
             (  Base       : Data_Base;
                Table_Name : String;
                Dependant  : Object_ID
             )  is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Base,
            (  "DELETE FROM "
            &  Table_Name
            &  " WHERE dependant = ?"
         )  );
      Bind (Command, 1, Dependant);
      Step (Command);
   end Unreference;

end Persistent.SQLite_Links;
