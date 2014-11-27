--                                                                    --
--  package Persistent.APQ          Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  15:03 28 Mar 2009  --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with APQ.Common;               use APQ.Common;
with APQ.Links;                use APQ.Links;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Ada.Unchecked_Deallocation;

package body Persistent.APQ is
   Data_Error : exception renames Ada.IO_Exceptions.Data_Error;

   type On_Object_Visited is access
      procedure (Storage : in out Data_Base_Object; Object : Object_ID);
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
-- Sync_Links -- Updates links of an existing object
--
--    Storage    - The data base
--    Table_Name - The links table name
--    Dependant  - The object's ID
--    Links      - The set of objects it refers
--
   procedure Sync_Links
             (  Storage    : in out Data_Base_Object;
                Table_Name : String;
                Dependant  : Object_ID;
                Links      : Deposit_Set
             );

   procedure Free is new
      Ada.Unchecked_Deallocation
      (  Storage_Object'Class,
         Storage_Object_Ptr
      );

   procedure Child_Unname
             (  Storage : in out Data_Base_Object;
                Object  : Object_ID
             )  is
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         (  "UPDATE "
         &  Objects_Table
         &  " SET "
         &  Name_Column
         &  " = NULL, "
         &  Parent_ID_Column
         &  " ="
      )  );
      Append (Command, No_ID, " WHERE " & ID_Column  & " =");
      Append (Command, Object);
      Execute (Storage.Data_Base);
      Unreference
      (  Storage.Data_Base,
         Direct_Links_Table,
         Object,
         Object
      );
   end Child_Unname;

   procedure Commit (Storage : in out Data_Base_Object) is
   begin
      Commit (Storage.Data_Base);
   end Commit;

   procedure Connect
             (  Storage        : in out Data_Base_Object'Class;
                Server_Type    : Database_Type;
                Data_Base_Name : String;
                User_Name      : String;
                Password       : String;
                Host_Name      : String  := "localhost";
                Port_Number    : Natural := 0;
                Erase          : Boolean := False
             )  is
      Create : Boolean := Erase;
   begin
      Connect
      (  Data_Base      => Storage.Data_Base,
         Server_Type    => Server_Type,
         Data_Base_Name => Data_Base_Name,
         User_Name      => User_Name,
         Password       => Password,
         Host_Name      => Host_Name,
         Port_Number    => Port_Number
      );
      if not Create then
         declare
            Mutex : Read_Mutex (Storage'Unchecked_Access);
         begin
            Create :=
               not Table_Exists (Storage.Data_Base, Objects_Table);
            Commit (Mutex);
         end;
      end if;
      if Create then
         Drop (Storage.Data_Base, Objects_Table);
         Drop (Storage.Data_Base, Direct_Links_Table);
         Drop (Storage.Data_Base, Backward_Links_Table);
         declare
            Mutex : Write_Mutex (Storage'Unchecked_Access);
         begin
            --
            -- Deleting all tables we know about
            --
            --
            -- Creating tables new
            --
            Prepare
            (  Storage.Data_Base.Query.all,
               (  "CREATE TABLE "
               &  Objects_Table
               &  " ("
               &  ID_Column
               &     " " & ID_Type (Storage.Data_Base) & ","
               &  Name_Column
               &     " " & String_Type (Storage.Data_Base) & ","
               &  Class_Column
               &     " " & String_Type (Storage.Data_Base) & ","
               &  Data_Column
               &     " " & String_Type (Storage.Data_Base) & ","
               &  Parameters_Column
               &     " " & String_Type (Storage.Data_Base) & ","
               &  Time_Column
               &     " " & Timestamp_Type (Storage.Data_Base) & ","
               &  Parent_ID_Column
               &     " " & ID_Type (Storage.Data_Base)
               &  ")"
            )  );
            Execute (Storage.Data_Base);
            Links.Create_Table (Storage.Data_Base, Direct_Links_Table);
            Links.Create_Table
            (  Storage.Data_Base,
               Backward_Links_Table
            );
            Commit (Mutex);
         end;
      end if;
   end Connect;

   function Create
            (  Server_Type    : Database_Type;
               Data_Base_Name : String;
               User_Name      : String;
               Password       : String;
               Host_Name      : String  := "localhost";
               Port_Number    : Natural := 0;
               Erase          : Boolean := False
            )  return Storage_Handle is
      Result : Storage_Object_Ptr;
   begin
      Result := new Data_Base_Object;
      Connect
      (  Storage        => Data_Base_Object (Result.all),
         Server_Type    => Server_Type,
         Data_Base_Name => Data_Base_Name,
         User_Name      => User_Name,
         Password       => Password,
         Host_Name      => Host_Name,
         Port_Number    => Port_Number,
         Erase          => Erase
      );
      return Ref (Result);
   exception
      when others =>
         Free (Result);
         raise;
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
         (  Data_Base  => Storage.Data_Base,
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
   begin
      if Storage.Cached_Key = ID then
         Storage.Cached_Key := No_ID;
      end if;
      Delete (Storage.Data_Base, Direct_Links_Table,   ID);
      Delete (Storage.Data_Base, Backward_Links_Table, ID);
      Prepare
      (  Storage.Data_Base.Query.all,
         (  "DELETE FROM "
         &  Objects_Table
         &  " WHERE "
         &  ID_Column
         &  " ="
      )  );
      Append (Storage.Data_Base.Query.all, ID);
      Execute (Storage.Data_Base);
   exception
      when Ada.IO_Exceptions.Use_Error | Data_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Storage.Data_Base.Query.all)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
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

   procedure Disable_Tracing (Storage : in out Storage_Handle) is
   begin
      Close_DB_Trace
      (  Data_Base_Object'Class (Ptr (Storage).all).Data_Base
      );
   end Disable_Tracing;

   procedure Enable_Tracing
             (  Storage : in out Storage_Handle;
                Name    : String;
                Mode    : Trace_Mode_Type := Trace_APQ
             )  is
   begin
      Open_DB_Trace
      (  Data_Base_Object'Class (Ptr (Storage).all).Data_Base,
         Name,
         Mode
      );
    end Enable_Tracing;

   function Find
            (  Storage : access Data_Base_Object;
               Name    : String;
               Parent  : Persistent_Key'Class
            )  return Persistent_Key'Class is
      Result  : Object_Key;
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
   begin
      if Parent not in Object_Key'Class then
         return Object_Key'(Null_Key);
      end if;
      Prepare
      (  Command,
         (  "SELECT "
         &  ID_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  Name_Column
         &  " ="
      )  );
      Append_Quoted (Storage.Data_Base, Name, " AND ");
      Append (Command, Parent_ID_Column, " =");
      Append (Command, Object_Key'Class (Parent).ID);
      Execute (Storage.Data_Base);
      Fetch (Command);
      Result.ID := Value (Command, 1);
      Clear (Command);
      return Result;
   exception
      when No_Tuple | Null_Value =>
         Raise_Exception
         (  End_Error'Identity,
            (  "Table "
            &  Quote (Objects_Table)
            &  " does not contain object "
            &  Quote (Name)
         )  );
      when Ada.IO_Exceptions.Use_Error | Data_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Storage.Data_Base.Query.all)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Find;

   procedure Finalize (Storage : in out Data_Base_Object) is
   begin
      Purge (Storage);
      Close_DB_Trace (Storage.Data_Base);
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
      return Storage.Shared;
   end Get_Access_Mode;

   procedure Get_Children
             (  Storage  : in out Data_Base_Object;
                Key      : Object_ID;
                Children : in out Unbounded_Array;
                Pointer  : in out Integer
             )  is
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         (  "SELECT "
         &  ID_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  Parent_ID_Column
         &  " ="
      )  );
      Append (Command, Key);
      Execute (Storage.Data_Base);
      loop
         Fetch (Command);
         Put (Children, Pointer, Value (Command, 1));
         Pointer := Pointer + 1;
      end loop;
   exception
      when No_Tuple =>
         Clear (Command);
      when Data_Error | Ada.IO_Exceptions.Use_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Command)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
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
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Invalid key"
         );
      end if;
      Prepare
      (  Command,
         (  "SELECT "
         &  Class_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  ID_Column
         &  " ="
      )  );
      Append (Command, Object_Key'Class (Key).ID);
      Execute (Storage.Data_Base);
      Fetch (Command);
      declare
         Class : String renames Value (Command, 1);
      begin
         Clear (Command);
         return Class;
      end;
   exception
      when End_Error =>
         raise;
      when No_Tuple =>
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Image (Storage.all, Key) & " does not exist"
         );
      when Ada.IO_Exceptions.Use_Error | Data_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Command)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get_Class;

   function Get_Creation_Time
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Time is
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
      Result  : Time;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Invalid key"
         );
      end if;
      Prepare
      (  Command,
         (  "SELECT "
         &  Time_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  ID_Column
         &  " ="
      )  );
      Append (Command, Object_Key'Class (Key).ID);
      Execute (Storage.Data_Base);
      Fetch (Command);
      Result := Value (Command, 1);
      Clear (Command);
      return Result;
   exception
      when End_Error =>
         raise;
      when No_Tuple =>
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Image (Storage.all, Key) & " does not exist"
         );
      when Ada.IO_Exceptions.Use_Error | Data_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Command)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get_Creation_Time;

   procedure Get_Data
             (  Storage    : in out Data_Base_Object;
                Key        : Persistent_Key'Class;
                Class      : out Unbounded_String;
                Data       : out Unbounded_String;
                Parameters : out Unbounded_String
             )  is
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Invalid key"
         );
      end if;
      Prepare
      (  Command,
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
         &  " ="
      )  );
      Append (Command, Object_Key'Class (Key).ID);
      Execute (Storage.Data_Base);
      Fetch (Command);
      Class      := To_Unbounded_String (String'(Value (Command, 1)));
      Data       := To_Unbounded_String (String'(Value (Command, 2)));
      Parameters := To_Unbounded_String (String'(Value (Command, 3)));
      Clear (Command);
   exception
      when End_Error =>
         raise;
      when No_Tuple =>
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Image (Storage, Key) & " does not exist"
         );
      when Ada.IO_Exceptions.Use_Error | Data_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Command)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
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
         (  Storage.Data_Base,
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

   function Get_List
            (  Storage     : access Data_Base_Object;
               Prefix      : String := "";
               Suffix      : String := "";
               Equivalence : Unicode_Mapping_Function := null;
               Parent      : Deposit_Handle := Root_Directory
            )  return Catalogue.Set is
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
      Length  : constant Integer := Prefix'Length + Suffix'Length;

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
      Result : Catalogue.Set;
      Mutex  : Read_Mutex (Storage);
   begin
      Prepare
      (  Command,
         (  "SELECT "
         &  Name_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  Parent_ID_Column
         &  " ="
      )  );
      Append (Command, Get_New_Parent_Key (Storage, Parent).ID);
      Append (Command, " AND NOT " & Name_Column, " IS NULL ");
      Execute (Storage.Data_Base);
      loop
         Fetch (Command);
         declare
            Name : String renames Value (Storage.Data_Base, 1);
         begin
            if Match (Name) then
               Catalogue.Add
               (  Result,
                  To_Unbounded_String (Name)
               );
            end if;
         end;
      end loop;
   exception
      when No_Tuple =>
         Clear (Command);
         return Result;
      when Data_Error | Ada.IO_Exceptions.Use_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Command)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get_List;

   function Get_Name
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class;
               Parent  : access Persistent_Key'Class
            )  return String is
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
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
      Prepare
      (  Command,
         (  "SELECT "
         &  Parent_ID_Column
         &  ", "
         &  Name_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  ID_Column
         &  " ="
      )  );
      Append (Command, Object_Key'Class (Key).ID);
      Execute (Storage.Data_Base);
      Fetch (Command);
      declare
         ID   : Object_ID := Value (Command, 1);
         Name : String    := Value (Command, 2);
      begin
         Clear (Command);
         Object_Key (Parent.all).ID := ID;
         if Name'Length = 0 then
            Raise_Exception (Name_Error'Identity, "No name");
         else
            return Name;
         end if;
      end;
      return Value (Storage.Data_Base, 2);
   exception
      when Null_Value =>
         Clear (Command);
         Raise_Exception (Name_Error'Identity, "No name");
      when No_Tuple =>
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Image (Storage.all, Key) & " does not exist"
         );
      when Data_Error | Ada.IO_Exceptions.Use_Error | Name_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Command)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
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
      (  Storage.Data_Base,
         Direct_Links_Table,
         Object_Key (Key).ID,
         Object_Key_Array (References).List,
         Pointer
      );
   end Get_References;

   function Has_Dependants
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Boolean is
   begin
      if Key not in Object_Key'Class then
         return False;
      end if;
      declare
         ID : Object_ID := Object_Key'Class (Key).ID;
      begin
         return
            Has_Dependants (Storage.Data_Base, Direct_Links_Table, ID);
      end;
   end Has_Dependants;

   function Is_APQ (Storage : Storage_Handle) return Boolean is
   begin
      return
      (  Is_Valid (Storage)
      and then
         Ptr (Storage).all in Data_Base_Object'Class
      );
   end Is_APQ;

   function Is_In
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Boolean is
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
   begin
      if Key not in Object_Key'Class then
         return False;
      end if;
      Prepare
      (  Command,
         (  "SELECT "
         &  Name_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  ID_Column
         &  " ="
      )  );
      Append (Command, Object_Key'Class (Key).ID);
      Execute (Storage.Data_Base);
      Fetch (Command);
      Clear (Command);
      return True;
   exception
      when No_Tuple | Null_Value =>
         return False;
      when Data_Error | Ada.IO_Exceptions.Use_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Command)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Is_In;

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
      ID         : Object_ID := Object_Key'Class (Key).ID;
      New_Parent : Object_ID := Object_Key'Class (Parent).ID;
      Old_Parent : Object_ID;
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
         Unnamed : Boolean   := False;
         Command : Root_Query_Type'Class renames
                      Storage.Data_Base.Query.all;
      begin
         if New_Parent = ID then
            Raise_Exception
            (  Name_Error'Identity,
               (  "Object "
               &  Image (Storage, Key)
               &  " is renamed to a self-descendant"
            )  );
         end if;
         --
         -- Checking if the object  already  has  the  name  and  parent
         -- specified.
         --
         Prepare
         (  Command,
            (  "SELECT "
            &  Parent_ID_Column
            &  ", "
            &  Name_Column
            &  " FROM "
            &  Objects_Table
            &  " WHERE "
            &  ID_Column
            &  " ="
         )  );
         Append (Command, ID);
         Execute (Storage.Data_Base);
         Fetch (Command);
         Old_Parent := Value (Command, 1);
         if Is_Null (Command, 2) then
            Unnamed := True;
         elsif (  New_Parent = Old_Parent
               and then
                  Name = String'(Value (Command, 2))
               )
         then
            Clear (Command);
            return;
         end if;
         Clear (Command);
         if Old_Parent /= New_Parent then
            --
            -- Checking if parent is the object itself or  else  one  of
            -- its descendants.
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
                        &  Image (Storage, Key)
                        &  " is renamed to a self-descendant"
                     )  );
                  end if;
                  From := From + 1;
                  Get_Children (Storage, This, Children, To);
               end loop;
            end;
         end if;
         --
         -- Changing the object's record
         --
         Prepare
         (  Command,
            (  "UPDATE "
            &  Objects_Table
            &  " SET "
            &  Name_Column
            &  " ="
         )  );
         Append_Quoted (Storage.Data_Base, Name, ", ");
         Append (Command, Parent_ID_Column, " =");
         Append (Command, New_Parent, " WHERE " & ID_Column & " =");
         Append (Command, ID);
         Execute (Storage.Data_Base);
         if Unnamed then
            Reference
            (  Storage.Data_Base,
               Direct_Links_Table,
               ID,
               ID
            );
         end if;
      exception
         when No_Tuple | Null_Value =>
            Raise_Exception
            (  End_Error'Identity,
               "Object " & Image (Storage, Key) & " does not exist"
            );
         when Data_Error | Ada.IO_Exceptions.Use_Error =>
            raise;
         when SQL_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               Error_Message (Command)
            );
         when Error : others =>
            Raise_Exception
            (  Data_Error'Identity,
               Exception_Message (Error)
            );
      end;
   end Rename;

   procedure Roll_Back (Storage : in out Data_Base_Object) is
   begin
      Roll_Back (Storage.Data_Base);
   end Roll_Back;

   procedure Seize_Read (Storage : in out Data_Base_Object) is
   begin
      Seize_Read (Storage.Data_Base);
   end Seize_Read;

   procedure Seize_Write (Storage : in out Data_Base_Object) is
   begin
      Seize_Write (Storage.Data_Base);
   end Seize_Write;

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
      ID        : Object_ID := No_ID;
      Parent_ID : Object_ID;
      Command   : Root_Query_Type'Class renames
                     Storage.Data_Base.Query.all;
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
      Parent_ID := Object_Key (Parent).ID;
      begin
         if not Is_Indexed (Storage, Name) then
            declare
               Key : Persistent_Key'Class :=
                        Find (Storage, Name, Parent);
            begin
               null;
            end;
         end if;
         Raise_Exception
         (  Name_Error'Identity,
            "Object " & Quote (Name) & " already exists"
         );
      exception
         when End_Error =>
            null;
      end;
      Prepare
      (  Command,
         (  "INSERT INTO "
         &  Objects_Table
         &  " VALUES ("
         &  New_ID (Storage.Data_Base)
         &  ","
      )  );
      Append_Quoted (Storage.Data_Base, Name,       ",");
      Append_Quoted (Storage.Data_Base, Class,      ",");
      Append_Quoted (Storage.Data_Base, Data,       ",");
      Append_Quoted (Storage.Data_Base, Parameters, ",");
      Append
      (  Command,
         Timestamp_Now (Storage.Data_Base),
          ","
      );
      Append (Command, Parent_ID, ")");
      Execute (Storage.Data_Base);
      ID := Get_ID (Storage.Data_Base, Objects_Table, ID_Column);
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
      Reference (Storage.Data_Base, Direct_Links_Table, ID, ID);
      return Object_Key'(Persistent_Key with ID);
   exception
      when Name_Error | Ada.IO_Exceptions.Use_Error =>
         raise;
      when Data_Error =>
         if ID /= No_ID then
            Delete (Storage.all, ID);
         end if;
         raise;
      when SQL_Error =>
         declare
            Error : constant String := Error_Message (Command);
         begin
            if ID /= No_ID then
               Delete (Storage.all, ID);
            end if;
            Raise_Exception (Data_Error'Identity, Error);
         end;
      when Error : others =>
         if ID /= No_ID then
            Delete (Storage.all, ID);
         end if;
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Store;

   function Store
            (  Storage        : access Data_Base_Object;
               Class          : String;
               Data           : String;
               Parameters     : String;
               Direct_Links   : Deposit_Set;
               Backward_Links : Deposit_Set
            )  return Persistent_Key'Class is
      ID      : Object_ID := No_ID;
      Command : Root_Query_Type'Class renames
                   Storage.Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         (  "INSERT INTO "
         &  Objects_Table
         &  " VALUES ("
         &  New_ID (Storage.Data_Base)
         &  ",NULL,"
      )  );
      Append_Quoted (Storage.Data_Base, Class,      ",");
      Append_Quoted (Storage.Data_Base, Data,       ",");
      Append_Quoted (Storage.Data_Base, Parameters, ",");
      Append
      (  Storage.Data_Base.Query.all,
         Timestamp_Now (Storage.Data_Base),
         ")"
      );
      Execute (Storage.Data_Base);
      ID := Get_ID (Storage.Data_Base, Objects_Table, ID_Column);
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
   exception
      when Ada.IO_Exceptions.Use_Error =>
         raise;
      when Data_Error =>
         if ID /= No_ID then
            Delete (Storage.all, ID);
         end if;
         raise;
      when SQL_Error =>
         declare
            Error : constant String := Error_Message (Command);
         begin
            if ID /= No_ID then
               Delete (Storage.all, ID);
            end if;
            Raise_Exception (Data_Error'Identity, Error);
         end;
      when Error : others =>
         if ID /= No_ID then
            Delete (Storage.all, ID);
         end if;
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Store;

   procedure Sync_Links
             (  Storage    : in out Data_Base_Object;
                Table_Name : String;
                Dependant  : Object_ID;
                Links      : Deposit_Set
             )  is
      To_Remove : Sets.Set;
      Referent  : Object_ID;
   begin
      Get_References
      (  Storage.Data_Base,
         Table_Name,
         Dependant,
         To_Remove
      );
      for Item in 1..Get_Size (Links) loop
         Referent := Get_Key (Storage'Access, Ref (Links, Item)).ID;
         if Is_In (To_Remove, Referent) then
            Remove (To_Remove, Referent);
         else
            Reference
            (  Data_Base  => Storage.Data_Base,
               Table_Name => Table_Name,
               Dependant  => Dependant,
               Referent   => Referent
            );
         end if;
      end loop;
      for Item in 1..Get_Size (To_Remove) loop
         Unreference
         (  Data_Base  => Storage.Data_Base,
            Table_Name => Table_Name,
            Dependant  => Dependant,
            Referent   => Get (To_Remove, Item)
         );
      end loop;
   end Sync_Links;

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
         ID      : Object_ID := Object_Key'Class (Key).ID;
         Command : Root_Query_Type'Class renames
                      Storage.Data_Base.Query.all;
      begin
         Prepare
         (  Command,
            (  "SELECT "
            &  Name_Column
            &  " FROM "
            &  Objects_Table
            &  " WHERE "
            &  ID_Column
            &  " ="
         )  );
         Append (Command, ID);
         begin
            Execute (Storage.Data_Base);
            Fetch (Command);
            if Is_Null (Command, 1) then
               Clear (Command);
               return;
            end if;
         exception
            when No_Tuple | Null_Value =>
               Clear (Command);
               return;
         end;
         Prepare
         (  Command,
            (  "UPDATE "
            &  Objects_Table
            &  " SET "
            &  Name_Column
            &  " = NULL, "
            &  Parent_ID_Column
            &  " ="
         )  );
         Append (Command, No_ID, " WHERE " & ID_Column  & " =");
         Append (Command, ID);
         Execute (Storage.Data_Base);
         Unreference
         (  Storage.Data_Base,
            Direct_Links_Table,
            ID,
            ID
         );
      exception
         when Data_Error | Ada.IO_Exceptions.Use_Error =>
            raise;
         when SQL_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               Error_Message (Command)
            );
         when Error : others =>
            Raise_Exception
            (  Data_Error'Identity,
               Exception_Message (Error)
            );
      end;
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
         ID      : Object_ID := Object_Key'Class (Key).ID;
         Command : Root_Query_Type'Class renames
                      Storage.Data_Base.Query.all;
      begin
         if Storage.Cached_Key = ID then
            Storage.Cached_Key := No_ID;
         end if;
         Prepare
         (  Command,
            (  "UPDATE "
            &  Objects_Table
            &  " SET "
            &  Class_Column
            &  "="
         )  );
         Append_Quoted (Storage.Data_Base, Class, ",");
         Append (Command, Data_Column & "=");
         Append_Quoted (Storage.Data_Base, Data, ",");
         Append (Command, Parameters_Column & "=");
         Append_Quoted (Storage.Data_Base, Parameters);
         Append (Command, " WHERE " & ID_Column & "=");
         Append (Command, ID);
         Execute (Storage.Data_Base);
         Sync_Links (Storage, Direct_Links_Table,   ID, Direct_Links);
         Sync_Links (Storage, Backward_Links_Table, ID, Backward_Links);
      exception
         when Data_Error | Ada.IO_Exceptions.Use_Error =>
            raise;
         when SQL_Error =>
            Raise_Exception
            (  Data_Error'Identity,
               Error_Message (Command)
            );
         when Error : others =>
            Raise_Exception
            (  Data_Error'Identity,
               Exception_Message (Error)
            );
      end;
   end Update;

   function Value
            (  Storage : Data_Base_Object;
               Key     : String
            )  return Persistent_Key'Class is
   begin
      return Object_Key'(Persistent_Key with Object_ID'Value (Key));
   exception
      when End_Error =>
         raise Data_Error;
   end Value;

end Persistent.APQ;
