--                                                                    --
--  package Persistent.SQLite       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2009       --
--                                                                    --
--                                Last revision :  07:53 21 Jul 2016  --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Persistent.SQLite_Links;  use Persistent.SQLite_Links;
with Strings_Edit.Floats;      use Strings_Edit.Floats;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Ada.Unchecked_Deallocation;

package body Persistent.SQLite is
   use type Object_ID;

   Data_Error : exception renames Ada.IO_Exceptions.Data_Error;
--
-- Delete -- Delete object by ID
--
--    Storage - The data base
--    ID      - The object's ID
--
   procedure Delete
             (  Storage : in out Data_Base_Object;
                ID      : Object_ID
             );
--
-- Create_Links -- Creates links of a newly created object
--
--    Storage    - The data base
--    Table_Name - The links table name
--    Dependant  - The object's ID
--    Links      - The set of objects it refers
--
   procedure Create_Links
             (  Storage    : in out Data_Base_Object;
                Table_Name : String;
                Dependant  : Object_ID;
                Links      : Deposit_Set
             );
--
-- Has -- Prefix check
--
--    Name   - A string
--    Prefix - To check
--
-- Returns :
--
--    True if Prefix is a prefix of Name
--
   function Has (Name : String; Prefix : String) return Boolean;
   pragma Inline (Has);
--
-- Sync_Links -- Updates links of an existing object
--
--    Storage    - The data base
--    Table_Name - The links table name
--    Dependant  - The object's ID
--    Links      - The set of objects it refers to
--
   procedure Sync_Links
             (  Storage    : in out Data_Base_Object;
                Table_Name : String;
                Dependant  : Object_ID;
                Links      : Deposit_Set
             );

   function Image (Date : Time) return String;
   function Value (Stamp : String) return Time;

   procedure Check_Existent
             (  Storage : in out Data_Base_Object;
                Name    : String;
                Parent  : Object_Key
             )  is
      pragma Inline (Check_Existent);
      function Indexed return Boolean is
         pragma Inline (Indexed);
      begin
         if Parent = Null_Key then
            return Is_Indexed (Storage'Access, Name);
         else
            return Is_Indexed
                   (  Storage'Access,
                      Name,
                      Get (Storage'Access, Parent)
                   );
         end if;
      end Indexed;
   begin
      if (  Indexed
         or else
            Null_Key /= Object_Key (Find (Storage'Access, Name, Parent))
         )
      then
         Raise_Exception
         (  Name_Error'Identity,
            "Object " & Quote (Name) & " already exists"
         );
      end if;
   exception
      when End_Error =>
         null;
   end Check_Existent;

   procedure Commit (Storage : in out Data_Base_Object) is
   begin
      if Storage.Owner = Current_Task then
         if Storage.Count > 1 then
            Storage.Count := Storage.Count - 1;
         else
            Storage.Count   := 0;
            Storage.Owner   := Null_Task_ID;
            Storage.Sharing := Fully;
            Exec (Storage.Connection, "COMMIT");
         end if;
      else
         Raise_Exception
         (  Use_Error'Identity,
            (  "Attemped other's transaction commit by "
            &  Image (Current_Task)
         )  );
      end if;
   end Commit;

   function Create
            (  File_Name : String;
               Erase     : Boolean := False
            )  return Storage_Handle is
      Result  : constant Storage_Handle :=
                Ref (new Data_Base_Object (File_Name'Length));
      Storage : Data_Base_Object renames
                Data_Base_Object (Ptr (Result).all);
   begin
      Storage.Connection :=
         Open
         (  File_Name,
            READWRITE or Standard.SQLite.CREATE or FULLMUTEX
         );
      Storage.File_Name := File_Name;
      if Erase then
         --
         -- Deleting all tables we know about
         --
         Exec
         (  Storage.Connection,
            "DROP TABLE IF EXISTS " & Objects_Table
         );
         Exec
         (  Storage.Connection,
            "DROP TABLE IF EXISTS " & Direct_Links_Table
         );
         Exec
         (  Storage.Connection,
            "DROP TABLE IF EXISTS " & Backward_Links_Table
         );
         declare
            List : String_List.Unbounded_Ptr_Array;
            Size : Natural := 0;
         begin
            declare
               Command : constant Statement :=
                         Prepare
                         (  Storage.Connection,
                            (  "SELECT name FROM sqlite_master "
                            &  "WHERE type='table'"
                         )  );
            begin
               while Step (Command) loop
                  declare
                     Name : constant String := Column (Command, 1);
                  begin
                     if (  Has (Name, Has_In_Table)
                        or else
                           Has (Name, Has_Out_Table)
                        or else
                           Has (Name, Has_Not_Table)
                        or else
                           Has (Name, Has_Not_Out_Table)
                        )
                     then
                        Size := Size + 1;
                        Put (List, Size, new String'(Name));
                     end if;
                  end;
               end loop;
            end;
            for Index in 1..Size loop
               Exec
               (  Storage.Connection,
                  "DROP TABLE " & Get (List, Index).all
               );
            end loop;
         end;
      end if;
      --
      -- Creating tables when necessary
      --
      Exec
      (  Storage.Connection,
         (  "CREATE TABLE IF NOT EXISTS "
         &  Objects_Table
         &  " ("
         &  ID_Column         & " INTEGER PRIMARY KEY, "
         &  Name_Column       & " TEXT, "
         &  Class_Column      & " TEXT, "
         &  Data_Column       & " TEXT, "
         &  Parameters_Column & " TEXT, "
         &  Time_Column       & " TEXT, "
         &  Parent_ID_Column  & " INTEGER"
         &  ")"
      )  );
      Create_Table (Storage.Connection, Direct_Links_Table);
      Create_Table (Storage.Connection, Backward_Links_Table);
      return Result;
   end Create;

   procedure Create_Links
             (  Storage    : in out Data_Base_Object;
                Table_Name : String;
                Dependant  : Object_ID;
                Links      : Deposit_Set
             )  is
   begin
      for Item in 1..Get_Size (Links) loop
         Reference
         (  Base       => Storage.Connection,
            Table_Name => Table_Name,
            Dependant  => Dependant,
            Referent   => Get_Key (Storage'Access, Ref (Links, Item)).ID
         );
      end loop;
   end Create_Links;

   procedure Delete
             (  Storage : in out Data_Base_Object;
                ID      : Object_ID
             )  is
      ID_Text : constant String := Image (ID);
   begin
      Delete (Storage.Connection, Direct_Links_Table,   ID);
      Delete (Storage.Connection, Backward_Links_Table, ID);
      Exec
      (  Storage.Connection,
         "DROP TABLE IF EXISTS has_in_" & ID_Text
      );
      Exec
      (  Storage.Connection,
         "DROP TABLE IF EXISTS has_out_" & ID_Text);
      Exec
      (  Storage.Connection,
         "DROP TABLE IF EXISTS has_not_" & ID_Text
      );
      Exec
      (  Storage.Connection,
         "DROP TABLE IF EXISTS has_not_out_" & ID_Text
      );
      declare
         Command : constant Statement :=
                   Prepare
                   (  Storage.Connection,
                      (  "DELETE FROM "
                      &  Objects_Table
                      &  " WHERE "
                      &  ID_Column
                      &  " = ?"
                   )  );
      begin
         Bind (Command, 1, ID);
         Step (Command);
      end;
   exception
      when End_Error =>
         null;
   end Delete;

   procedure Delete
             (  Storage : in out Data_Base_Object;
                Key     : Persistent_Key'Class
             )  is
   begin
      if Key in Object_Key'Class then
         Delete (Storage, Object_Key'Class (Key).ID);
      end if;
   end Delete;

   function Find
            (  Storage : access Data_Base_Object;
               Name    : String;
               Parent  : Persistent_Key'Class
            )  return Persistent_Key'Class is
   begin
      if Parent not in Object_Key'Class then
         return Object_Key'(Null_Key);
      end if;
      declare
         Command : constant Statement :=
                   Prepare
                   (  Storage.Connection,
                      (  "SELECT "
                      &  ID_Column
                      &  " FROM "
                      &  Objects_Table
                      &  " WHERE "
                      &  Name_Column
                      &  " = ? AND "
                      &  Parent_ID_Column
                      &  " = ?"
                   )  );
      begin
         Bind (Command, 1, Name);
         Bind (Command, 2, Object_Key'Class (Parent).ID);
         if Step (Command) then
            return Object_Key'(Persistent_Key with Column (Command, 1));
         else
            return Object_Key'(Null_Key);
         end if;
      end;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            "Table " & Quote (Objects_Table) & " does not exist"
         );
   end Find;

   procedure Finalize (Storage : in out Data_Base_Object) is
   begin
      Purge (Storage);
      Finalize (Indexed_Storage_Object (Storage));
   end Finalize;

   function Get (Container : Object_Key_Array; Index : Integer)
      return Persistent_Key'Class is
   begin
      return
         Object_Key'(Persistent_Key with Get (Container.list, Index));
   end Get;

   function Get_Access_Mode (Storage : Data_Base_Object)
      return Sharing_Type is
   begin
      return Storage.Sharing;
   end Get_Access_Mode;

   procedure Get_Children
             (  Storage  : in out Data_Base_Object;
                Key      : Object_ID;
                Children : in out Unbounded_Array;
                Pointer  : in out Integer
             )  is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Storage.Connection,
            (  "SELECT "
            &  ID_Column
            &  " FROM "
            &  Objects_Table
            &  " WHERE "
            &  Parent_ID_Column
            &  " = ?"
         )  );
      Bind (Command, 1, Key);
      while Step (Command) loop
         Put (Children, Pointer, Column (Command, 1));
         Pointer := Pointer + 1;
      end loop;
   exception
      when End_Error =>
         null;
   end Get_Children;

   procedure Get_Children
             (  Storage  : in out Data_Base_Object;
                Key      : Persistent_Key'Class;
                Children : in out Persistent_Key_Array'Class;
                Pointer  : in out Integer
             )  is
   begin
      Get_Children
      (  Storage,
         Object_Key (Key).ID,
         Object_Key_Array (Children).List,
         Pointer
      );
   end Get_Children;

   function Get_Class
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return String is
   begin
      if Key not in Object_Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Invalid key"
         );
      end if;
      declare
         Command : Statement;
      begin
         Command :=
            Prepare
            (  Storage.Connection,
               (  "SELECT "
               &  Class_Column
               &  " FROM "
               &  Objects_Table
               &  " WHERE "
               &  ID_Column
               &  " = ?"
            )  );
         Bind (Command, 1, Object_Key'Class (Key).ID);
         if Step (Command) then
            return Column (Command, 1);
         end if;
      exception
         when End_Error =>
            Raise_Exception
            (  End_Error'Identity,
               "Table " & Quote (Objects_Table) & " does not exist"
            );
      end;
      Raise_Exception
      (  End_Error'Identity,
         "Object " & Image (Storage.all, Key) & " does not exist"
      );
   end Get_Class;

   function Get_Creation_Time
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Time is
   begin
      if Key not in Object_Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Invalid key"
         );
      end if;
      declare
         Command : Statement;
      begin
         Command :=
            Prepare
            (  Storage.Connection,
               (  "SELECT "
               &  Time_Column
               &  " FROM "
               &  Objects_Table
               &  " WHERE "
               &  ID_Column
               &  " = ?"
            )  );
         Bind (Command, 1, Object_Key'Class (Key).ID);
         if Step (Command) then
            return Value (Column (Command, 1));
         end if;
      exception
         when End_Error =>
            Raise_Exception
            (  End_Error'Identity,
               "Table " & Quote (Objects_Table) & " does not exist"
            );
      end;
      Raise_Exception
      (  End_Error'Identity,
         "Object " & Image (Storage.all, Key) & " does not exist"
      );
   end Get_Creation_Time;

   procedure Get_Data
             (  Storage    : in out Data_Base_Object;
                Key        : Persistent_Key'Class;
                Class      : out Unbounded_String;
                Data       : out Unbounded_String;
                Parameters : out Unbounded_String
             )  is
   begin
      if Key not in Object_Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Invalid key"
         );
      end if;
      declare
         Command : Statement;
      begin
         Command :=
            Prepare
            (  Storage.Connection,
               (  "SELECT "
               &  Class_Column
               &  ", "
               &  Data_Column
               &  ", "
               &  Parameters_Column
               &  " FROM "
               &  Objects_Table
               &  " WHERE "
               &  ID_Column
               &  " = ?"
            )  );
         Bind (Command, 1, Object_Key'Class (Key).ID);
         if Step (Command) then
            Class := Null_Unbounded_String;
            Append (Class, Column (Command, 1));
            Data := Null_Unbounded_String;
            Append (Data, Column (Command, 2));
            Parameters := Null_Unbounded_String;
            Append (Parameters, Column (Command, 3));
            return;
         end if;
      exception
         when End_Error =>
            Raise_Exception
            (  End_Error'Identity,
               "Table " & Quote (Objects_Table) & " does not exist"
            );
      end;
      Raise_Exception
      (  End_Error'Identity,
         "Object " & Image (Storage, Key) & " does not exist"
      );
   end Get_Data;

   function Get_Dependant
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class;
               No      : Positive
            )  return Persistent_Key'Class is
      ID : Object_ID;
   begin
      if Key not in Object_Key'Class then
         raise End_Error;
      end if;
      ID := Object_Key'Class (Key).ID;
      if ID /= Storage.Cached_Key then
         Storage.Cached_Key := No_ID;
         Get_Dependants
         (  Storage.Connection,
            Backward_Links_Table,
            ID,
            Storage.Cached_Set
         );
         Storage.Cached_Key := ID;
      end if;
      if No > Get_Size (Storage.Cached_Set) then
         raise End_Error;
      end if;
      return
         Object_Key'
         (  Persistent_Key
         with
            Get (Storage.Cached_Set, No)
         );
   end Get_Dependant;

   function Get_File_Name (Storage : Storage_Handle) return String is
   begin
      return Data_Base_Object'Class (Ptr (Storage).all).File_Name;
   end Get_File_Name;

   function Get_List
            (  Storage     : access Data_Base_Object;
               Prefix      : String := "";
               Suffix      : String := "";
               Equivalence : Unicode_Mapping_Function := null;
               Parent      : Deposit_Handle := Root_Directory
            )  return Catalogue.Set is
      Length : constant Integer := Prefix'Length + Suffix'Length;

      function Match (Name : String) return Boolean is
      begin
         if Name'Length < Length then
            return False;
         end if;
         declare
            From : Integer := Name'First;
         begin
            if Prefix'Length > 0 then
               declare
                  Pointer : Integer := Prefix'First;
                  This    : UTF8_Code_Point;
                  That    : UTF8_Code_Point;
               begin
                   while Pointer <= Prefix'Last loop
                      if From > Name'Last then
                         return False;
                      end if;
                      Get (Name,   From,    This);
                      Get (Prefix, Pointer, That);
                      if (  Equivalence /= null
                         and then
                            Equivalence (This) /= Equivalence (That)
                         )
                      then
                         return False;
                      end if;
                   end loop;
               end;
            end if;
            if Suffix'Length > 0 then
               declare
                  Pointer : Integer := Suffix'Last + 1;
                  To      : Integer := Name'Last   + 1;
                  This    : UTF8_Code_Point;
                  That    : UTF8_Code_Point;
               begin
                   while Pointer > Suffix'First loop
                      if To <= From then
                         return False;
                      end if;
                      Get_Backwards (Name, To, This);
                      Get_Backwards (Suffix, Pointer, That);
                      if (  Equivalence /= null
                         and then
                            Equivalence (This) /= Equivalence (That)
                         )
                      then
                         return False;
                      end if;
                   end loop;
               end;
            end if;
         exception
            when Data_Error | Layout_Error =>
               return False;
         end;
         return True;
      end Match;
      Result  : Catalogue.Set;
      Command : Statement;
      Mutex   : Read_Mutex (Storage);
   begin
      Command :=
         Prepare
         (  Storage.Connection,
            (  "SELECT "
            &  Name_Column
            &  " FROM "
            &  Objects_Table
            &  " WHERE "
            &  Parent_ID_Column
            &  " = ? AND NOT "
            &  Name_Column
            &  " IS NULL"
         )  );
      Bind (Command, 1, Get_New_Parent_Key (Storage, Parent).ID);
      while Step (Command) loop
         declare
            Name : String renames Column (Command, 1);
         begin
            if Match (Name) then
               Catalogue.Add (Result, Name);
            end if;
         end;
      end loop;
      Commit (Mutex);
      return Result;
   exception
      when End_Error =>
         return Result;
   end Get_List;

   function Get_Name
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class;
               Parent  : access Persistent_Key'Class
            )  return String is
   begin
      if Key not in Object_Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Invalid key"
         );
      end if;
      if Parent.all not in Object_Key then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The type of requested parent key is unsupported"
         );
      end if;
      declare
         Command : Statement;
      begin
         Command :=
            Prepare
            (  Storage.Connection,
               (  "SELECT "
               &  Parent_ID_Column
               &  ", "
               &  Name_Column
               &  " FROM "
               &  Objects_Table
               &  " WHERE "
               &  ID_Column
               &  " = ?"
            )  );
         Bind (Command, 1, Object_Key'Class (Key).ID);
         if Step (Command) then
            Object_Key'Class (Parent.all).ID := Column (Command, 1);
            declare
               Result : String renames Column (Command, 2);
            begin
               if Result'Length > 0 then
                  return Result;
               else
                  Raise_Exception (Name_Error'Identity, "No name");
               end if;
            end;
         end if;
      exception
         when End_Error =>
            Raise_Exception
            (  End_Error'Identity,
               "Table " & Quote (Objects_Table) & " does not exist"
            );
      end;
      Raise_Exception
      (  End_Error'Identity,
         "Object " & Image (Storage.all, Key) & " does not exist"
      );
   end Get_Name;

   procedure Get_References
             (  Storage    : in out Data_Base_Object;
                Key        : Persistent_Key'Class;
                References : in out Persistent_Key_Array'Class;
                Pointer    : in out Integer
             )  is
   begin
      Get_References
      (  Storage.Connection,
         Direct_Links_Table,
         Object_Key (Key).ID,
         Object_Key_Array (References).List,
         Pointer
      );
   end Get_References;

   function Has (Name : String; Prefix : String) return Boolean is
   begin
      return
      (  Name'Length > Prefix'Length
      and then
         Name (Name'First..Name'First + Prefix'Length - 1) = Prefix
      );
   end Has;

   function Has_Dependants
            (  Storage   : access Data_Base_Object;
               Key       : Persistent_Key'Class;
               All_Links : Boolean
            )  return Boolean is
   begin
      if Key not in Object_Key'Class then
         return False;
      end if;
      if All_Links then
         return
         (  Has_Dependants
            (  Storage.Connection,
               Direct_Links_Table,
               Object_Key'Class (Key).ID
            )
         or else
            Has_Dependants
            (  Storage.Connection,
               Backward_Links_Table,
               Object_Key'Class (Key).ID
         )  );
      else
         return
            Has_Dependants
            (  Storage.Connection,
               Direct_Links_Table,
               Object_Key'Class (Key).ID
            );
      end if;
   end Has_Dependants;

   function Image (Date : Time) return String is
      use Strings_Edit;
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Second  : Day_Duration;
      Text    : String  := "YYYY-MM-DD HH:MM:SS.SSS";
      Pointer : Integer := Text'First;
   begin
      Split (Date, Year, Month, Day, Second);
      Put
      (  Text,
         Pointer,
         Integer (Year),
         Field   => 4,
         Justify => Right,
         Fill    => '0'
      );
      Pointer := Pointer + 1;
      Put
      (  Text,
         Pointer,
         Integer (Month),
         Field   => 2,
         Justify => Right,
         Fill    => '0'
      );
      Pointer := Pointer + 1;
      Put
      (  Text,
         Pointer,
         Integer (Day),
         Field   => 2,
         Justify => Right,
         Fill    => '0'
      );
      Pointer := Pointer + 1;
      Put
      (  Text,
         Pointer,
         Integer (Second / 3_600.0),
         Field   => 2,
         Justify => Right,
         Fill    => '0'
      );
      Pointer := Pointer + 1;
      Put
      (  Text,
         Pointer,
         Integer (Second / 60.0) mod 60,
         Field   => 2,
         Justify => Right,
         Fill    => '0'
      );
      Pointer := Pointer + 1;
      Put
      (  Text,
         Pointer,
         Float (Second - Second / 60.0),
         Field    => 6,
         Justify  => Right,
         AbsSmall => -3,
         Fill     => '0'
      );
      return Text;
   end Image;

   function Is_In
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Boolean is
   begin
      if Key not in Object_Key'Class then
         return False;
      end if;
      declare
         Command : Statement;
      begin
         Command :=
            Prepare
            (  Storage.Connection,
               (  "SELECT "
               &  Name_Column
               &  " FROM "
               &  Objects_Table
               &  " WHERE "
               &  ID_Column
               &  " = ?"
            )  );
         Bind (Command, 1, Object_Key'Class (Key).ID);
         return Step (Command);
      end;
   exception
      when End_Error =>
         return False;
   end Is_In;

   function Is_SQLite (Storage : Storage_Handle) return Boolean is
   begin
      return
      (  Is_Valid (Storage)
      and then
         Ptr (Storage).all in Data_Base_Object'Class
      );
   end Is_SQLite;

   procedure Put
             (  Container : in out Object_Key_Array;
                Index     : Integer;
                Key       : Persistent_Key'Class
             )  is
   begin
      Put (Container.List, Index, Object_Key'Class (Key).ID);
   end Put;

   procedure Rename
             (  Storage : in out Data_Base_Object;
                Key     : Persistent_Key'Class;
                Name    : String;
                Parent  : Persistent_Key'Class
             )  is
      Parent_Key : Object_Key renames Object_Key (Parent);
      ID         : constant Object_ID := Object_Key'Class (Key).ID;
      New_Parent : constant Object_ID := Parent_Key.ID;
      Old_Parent : Object_ID := No_ID;
   begin
      if Name'Length = 0 then
         Raise_Exception
         (  Name_Error'Identity,
            "Object's name cannot be empty"
         );
      end if;
      if Parent not in Object_Key'Class then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid parent key"
         );
      end if;
      if Key not in Object_Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Invalid key"
         );
      end if;
      declare
         Unnamed : Boolean := False;
      begin
         if New_Parent = ID then
            Raise_Exception
            (  Name_Error'Identity,
               (  "Object "
               &  Image (ID)
               &  " is renamed to a self-descendant"
            )  );
         end if;
         --
         -- Checking if the object  already  has  the  name  and  parent
         -- specified.
         --
         declare
            Command : Statement;
         begin
            Command :=
               Prepare
               (  Storage.Connection,
                  (  "SELECT "
                  &  Parent_ID_Column
                  &  ", "
                  &  Name_Column
                  &  " FROM "
                  &  Objects_Table
                  &  " WHERE "
                  &  ID_Column
                  &  " = ?"
               )  );
            Bind (Command, 1, ID);
            if Step (Command) then
               Old_Parent := Column (Command, 1);
               if (  Old_Parent = New_Parent
                  and then
                     Name = Column (Command, 2)
                  )
               then
                  return;
               end if;
            else
               Unnamed := True;
            end if;
         exception
            when End_Error =>
               Raise_Exception
               (  End_Error'Identity,
                  "Table " & Quote (Objects_Table) & " does not exist"
               );
         end;
         if Old_Parent /= New_Parent then
            --
            -- Checking  if  the   new  parent  is  one of  the object's
            -- descendants.
           --
            declare
               Children : Unbounded_Array;
               This     : Object_ID;
               From     : Integer := Integer'First;
               To       : Integer := From;
            begin
               Get_Children (Storage, ID, Children, To);
               while From < To loop
                  This := Get (Children, From);
                  if This = ID then
                     Raise_Exception
                     (  Name_Error'Identity,
                        (  "Object "
                        &  Image (ID)
                        &  " is renamed to a self-descendant"
                     )  );
                  end if;
                  From := From + 1;
                  Get_Children (Storage, This, Children, To);
               end loop;
            end;
         end if;
            -- Checking if the name is already in use
         Check_Existent (Storage, Name, Parent_Key);
         --
         -- Changing the object's record
         --
         declare
            Command : Statement;
         begin
            Command :=
               Prepare
               (  Storage.Connection,
                  (  "UPDATE "
                  &  Objects_Table
                  &  " SET "
                  &  Name_Column
                  &  " = ?, "
                  &  Parent_ID_Column
                  &  " ="
                  &  Image (New_Parent)
                  &  " WHERE "
                  &  ID_Column
                  &  " = ?"
               )  );
            Bind (Command, 1, Name);
            Bind (Command, 2, ID);
            Step (Command);
         end;
         if Unnamed then
            Reference (Storage.Connection, Direct_Links_Table, ID, ID);
         end if;
      end;
   end Rename;

   procedure Roll_Back (Storage : in out Data_Base_Object) is
   begin
      if Storage.Owner = Current_Task then
         if Storage.Count > 1 then
            Storage.Count := Storage.Count - 1;
         else
            Storage.Count   := 0;
            Storage.Owner   := Null_Task_ID;
            Storage.Sharing := Fully;
            Exec (Storage.Connection, "ROLLBACK");
         end if;
      end if;
   exception
      when others =>
         null;
   end Roll_Back;

   procedure Seize_Read (Storage : in out Data_Base_Object) is
   begin
      if Storage.Owner = Null_Task_ID then
         Storage.Owner := Current_Task;
      elsif Storage.Owner /= Current_Task then
         Raise_Exception
         (  Use_Error'Identity,
            "A concurrent transaction requested"
         );
      end if;
      case Storage.Sharing is
         when Fully =>
            Exec (Storage.Connection, "BEGIN");
            Storage.Sharing := Read_Only;
            Storage.Count   := 1;
         when Read_Only | Read_Write =>
            Storage.Count := Storage.Count + 1;
      end case;
   end Seize_Read;

   procedure Seize_Write (Storage : in out Data_Base_Object) is
   begin
      if Storage.Owner = Null_Task_ID then
         Storage.Owner := Current_Task;
      elsif Storage.Owner /= Current_Task then
         Raise_Exception
         (  Use_Error'Identity,
            "A concurrent transaction requested"
         );
      end if;
      case Storage.Sharing is
         when Fully =>
            Exec (Storage.Connection, "BEGIN EXCLUSIVE");
            Storage.Sharing := Read_Write;
            Storage.Count   := 1;
         when Read_Only =>
            Raise_Exception
            (  Use_Error'Identity,
               "A transaction upgrade requested"
            );
         when Read_Write =>
            Storage.Count := Storage.Count + 1;
      end case;
   end Seize_Write;

   function Unicode (Storage : Storage_Handle) return Boolean is
   begin
      return True;
   end Unicode;

   function Store
            (  Storage        : access Data_Base_Object;
               Name           : String;
               Parent         : Persistent_Key'Class;
               Class          : String;
               Data           : String;
               Parameters     : String;
               Direct_Links   : Deposit_Set;
               Backward_Links : Deposit_Set
            )  return Persistent_Key'Class is
   begin
      if Name'Length = 0 then
         Raise_Exception
         (  Name_Error'Identity,
            "No empty object names are allowed"
         );
      end if;
      if Parent not in Object_Key'Class then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid parent key"
         );
      end if;
      declare
         Parent_Key : Object_Key renames Object_Key (Parent);
         ID         : Object_ID;
         Command    : Statement;
      begin
         Check_Existent (Storage.all, Name, Parent_Key);
         Command :=
            Prepare
            (  Storage.Connection,
               (  "INSERT INTO "
               &  Objects_Table
               &  " VALUES (NULL, ?, ?, ?, ?,"
               &  " strftime('%Y-%m-%d %H:%M:%f','now'), ?)"
            )  );
         Bind (Command, 1, Name);
         Bind (Command, 2, Class);
         Bind (Command, 3, Data);
         Bind (Command, 4, Parameters);
         Bind (Command, 5, Parent_Key.ID);
         Step (Command);
         ID := Last_Insert_Row (Storage.Connection);
         Create_Links
         (  Storage.all,
            Direct_Links_Table,
            ID,
            Direct_Links
         );
         Create_Links
         (  Storage.all,
            Backward_Links_Table,
            ID,
            Backward_Links
         );
         Reference (Storage.Connection, Direct_Links_Table, ID, ID);
         return Object_Key'(Persistent_Key with ID);
      end;
   end Store;

   function Store
            (  Storage        : access Data_Base_Object;
               Class          : String;
               Data           : String;
               Parameters     : String;
               Direct_Links   : Deposit_Set;
               Backward_Links : Deposit_Set
            )  return Persistent_Key'Class is
      Command : Statement;
      ID      : Object_ID := No_ID;
   begin
      Command :=
         Prepare
         (  Storage.Connection,
            (  "INSERT INTO "
            &  Objects_Table
            &  " VALUES (NULL, NULL, ?, ?, ?,"
            &  " strftime('%Y-%m-%d %H:%M:%f','now'), ?)"
         )  );
      Bind (Command, 1, Class);
      Bind (Command, 2, Data);
      Bind (Command, 3, Parameters);
      Bind (Command, 4, ID);
      Step (Command);
      ID := Last_Insert_Row (Storage.Connection);
      Create_Links
      (  Storage.all,
         Direct_Links_Table,
         ID,
         Direct_Links
      );
      Create_Links
      (  Storage.all,
         Backward_Links_Table,
         ID,
         Backward_Links
      );
      return Object_Key'(Persistent_Key with ID);
   end Store;

   procedure Sync_Links
             (  Storage    : in out Data_Base_Object;
                Table_Name : String;
                Dependant  : Object_ID;
                Links      : Deposit_Set
             )  is
      To_Remove : SQLite_Keys.Sets.Set;
      Referent  : Object_ID;
   begin
      Get_References
      (  Base       => Storage.Connection,
         Table_Name => Table_Name,
         Dependant  => Dependant,
         Referents  => To_Remove
      );
      for Item in 1..Get_Size (Links) loop
         Referent := Get_Key (Storage'Access, Ref (Links, Item)).ID;
         if Is_In (To_Remove, Referent) then
            Remove (To_Remove, Referent);
         else
            Reference
            (  Base       => Storage.Connection,
               Table_Name => Table_Name,
               Dependant  => Dependant,
               Referent   => Referent
            );
         end if;
      end loop;
      for Item in 1..Get_Size (To_Remove) loop
         Unreference
         (  Base       => Storage.Connection,
            Table_Name => Table_Name,
            Dependant  => Dependant,
            Referent   => Get (To_Remove, Item)
         );
      end loop;
   end Sync_Links;

   function Unicode (Storage : Data_Base_Object) return Boolean is
   begin
      return True;
   end Unicode;

   procedure Unname
             (  Storage : in out Data_Base_Object;
                Key     : Persistent_Key'Class
             )  is
   begin
      if Key not in Object_Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Invalid key"
         );
      end if;
      declare
         ID : constant Object_ID := Object_Key'Class (Key).ID;
      begin
         declare
            Command : Statement;
         begin
            Command :=
               Prepare
               (  Storage.Connection,
                  (  "SELECT "
                  &  Name_Column
                  &  " FROM "
                  &  Objects_Table
                  &  " WHERE "
                  &  ID_Column
                  &  " = ?"
               )  );
            Bind (Command, 1, ID);
            if Step (Command) then
               if "" = Column (Command, 1) then
                  return;
               end if;
            else
               Raise_Exception
               (  End_Error'Identity,
                  (  "Object "
                  &  Image (Storage, Key)
                  &  " does not exist"
               )  );
            end if;
         end;
         declare
            Command : Statement;
         begin
            Command :=
               Prepare
               (  Storage.Connection,
                  (  "UPDATE "
                  &  Objects_Table
                  &  " SET "
                  &  Name_Column
                  &  " = NULL, "
                  &  Parent_ID_Column
                  &  " ="
                  &  Image (No_ID)
                  &  " WHERE "
                  &  ID_Column
                  &  " = ?"
               )  );
            Bind (Command, 1, ID);
            Step (Command);
         end;
         Unreference (Storage.Connection, Direct_Links_Table, ID);
      end;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            "Table " & Quote (Objects_Table) & " does not exist"
         );
   end Unname;

   procedure Update
             (  Storage        : in out Data_Base_Object;
                Key            : Persistent_Key'Class;
                Class          : String;
                Data           : String;
                Parameters     : String;
                Direct_Links   : Deposit_Set;
                Backward_Links : Deposit_Set
             )  is
   begin
      if Key not in Object_Key'Class then
         Raise_Exception
         (  Data_Error'Identity,
            "Invalid key"
         );
      end if;
      declare
         ID      : constant Object_ID := Object_Key'Class (Key).ID;
         Command : Statement;
      begin
         Command :=
            Prepare
            (  Storage.Connection,
               (  "UPDATE "
               &  Objects_Table
               &  " SET "
               &  Class_Column
               &  " = ?, "
               &  Data_Column
               &  " = ?, "
               &  Parameters_Column
               &  " = ? WHERE "
               &  ID_Column
               &  " = ?"
            )  );
         Bind (Command, 1, Class);
         Bind (Command, 2, Data);
         Bind (Command, 3, Parameters);
         Bind (Command, 4, ID);
         Step (Command);
         Sync_Links (Storage, Direct_Links_Table,   ID, Direct_Links);
         Sync_Links (Storage, Backward_Links_Table, ID, Backward_Links);
      exception
         when End_Error =>
            Raise_Exception
            (  End_Error'Identity,
               "Table " & Quote (Objects_Table) & " does not exist"
            );
      end;
   end Update;

   function Value (Stamp : String) return Time is
      Pointer  : Integer := Stamp'First;
      Year     : Integer;
      Month    : Integer;
      Day      : Integer;
      Hour     : Integer;
      Minute   : Integer;
      Second   : Float;
   begin
      Get (Stamp, Pointer, Year, First => 0, Last => 9999);
      Pointer := Pointer + 1;
      Get (Stamp, Pointer, Month, First => 1, Last => 12);
      Pointer := Pointer + 1;
      Get (Stamp, Pointer, Day, First => 1, Last => 31);
      Pointer := Pointer + 1;
      Get (Stamp, Pointer, Hour, First => 0, Last => 23);
      Pointer := Pointer + 1;
      Get (Stamp, Pointer, Minute, First => 0, Last => 59);
      Pointer := Pointer + 1;
      Get (Stamp, Pointer, Second, First => 0.0, Last => 60.0);
      Second := Float ((Hour * 60 + Minute) * 60) + Second;
      if Second >= Float (Day_Duration'Last) then
         return
            Time_Of
            (  Year    => Year_Number  (Year),
               Month   => Month_Number (Month),
               Day     => Day_Number   (Day),
               Seconds => Day_Duration'Last
            );
      else
         return
            Time_Of
            (  Year    => Year_Number  (Year),
               Month   => Month_Number (Month),
               Day     => Day_Number   (Day),
               Seconds => Day_Duration (Second)
            );
      end if;
   exception
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Wrong time stamp:'"
            &  Stamp
            &  "': "
            &  Exception_Message (Error)
         )  );
   end Value;

   function Value
            (  Storage : Data_Base_Object;
               Key     : String
            )  return Persistent_Key'Class is
   begin
      return Object_Key'(Persistent_Key with Object_ID'Value (Key));
   exception
      when others =>
         raise Data_Error;
   end Value;

end Persistent.SQLite;
