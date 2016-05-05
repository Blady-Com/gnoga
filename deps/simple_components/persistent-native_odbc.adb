--                                                                    --
--  package Persistent.Native_ODBC  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2012       --
--                                                                    --
--                                Last revision :  10:00 09 Apr 2016  --
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
with ODBC.API.Keys.Edit;       use ODBC.API.Keys.Edit;
with ODBC.API.Links;           use ODBC.API.Links;
with ODBC.Bound_Parameters;    use ODBC.Bound_Parameters;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Ada.Unchecked_Deallocation;

package body Persistent.Native_ODBC is
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
-- New -- Generate a new object's ID
--
--    Storage  - The data base
--
-- Returns :
--
--    The ID generated
--
   function New_ID (Storage : access Data_Base_Object) return Object_ID;
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
            End_Transaction (Storage.Connection.all);
            if Storage.Serializable then
               Release (Storage.Connection.all);
            end if;
            Set_Execution_Mode (Storage.Connection.all, None);
         end if;
      else
         Raise_Exception
         (  Use_Error'Identity,
            (  "Attemped other's transaction commit by "
            &  Image (Current_Task)
         )  );
      end if;
   end Commit;

   procedure Free is new
      Ada.Unchecked_Deallocation
      (  Storage_Object'Class,
         Storage_Object_Ptr
      );
   procedure Free is new
      Ada.Unchecked_Deallocation
      (  ODBC_Connection'Class,
         ODBC_Connection_Ptr
      );
   procedure Free is new
      Ada.Unchecked_Deallocation
      (  ODBC_Command'Class,
         ODBC_Command_Ptr
      );

   procedure Do_Connect
             (  Storage     : in out Data_Base_Object'Class;
                Server_Name : String;
                User_Name   : String;
                Password    : String
             )  is
   begin
      Connect
      (  Storage.Connection.all,
         Server_Name,
         User_Name,
         Password,
         False
      );
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Use_Error'Identity,
            "No Unicode support beyond UCS-2"
         );
   end Do_Connect;

   procedure Connect
             (  Storage     : in out Data_Base_Object'Class;
                Server_Name : String;
                User_Name   : String;
                Password    : String;
                Erase       : Boolean := False
             )  is
      type Data_Type_Array is
         array (Positive range <>) of SQL_DATA_TYPE;
      procedure Query_Type
                (  Data : in out SQL_Type_Info;
                   List : Data_Type_Array;
                   Text : String
                )  is
      begin
         for Index in List'Range loop
            begin
               declare
                  Info : constant Type_Info :=
                         Get_Type_Info (Storage.Command, List (Index));
               begin
                  Data.Data_Type := Info.Data_Type;
                  Data.Name := To_Unbounded_String (Info.Type_Name);
                  return;
               end;
            exception
               when Constraint_Error => -- The type is not supported
                  null;
            end;
         end loop;
         Raise_Exception
         (  Data_Error'Identity,
            "The data base does not support " & Text
         );
      end Query_Type;
   begin
      Storage.Connection := new ODBC_Connection (Environment'Access);
      Do_Connect (Storage, Server_Name, User_Name, Password);
      Storage.Command :=
         new ODBC_Command (Storage.Connection.all'Unchecked_Access);
      Storage.Serializable := Serializable (Storage.Connection.all);
      declare
         Mutex   : Write_Mutex (Storage'Unchecked_Access);
         Command : ODBC_Command'Class renames Storage.Command.all;
      begin
         if Erase then
            --
            -- Deleting all tables we know about
            --
            Drop (Command, Objects_Table);
            Drop (Command, Direct_Links_Table);
            Drop (Command, Backward_Links_Table);
            declare
               List : String_List.Unbounded_Ptr_Array;
               Size : Natural := 0;
            begin
               Get_Tables (Command);
               loop
                  Fetch (Command);
                  declare
                     Name : constant String :=
                                     Get_Data
                                     (  Command'Unchecked_Access,
                                        3, -- The table name column
                                        Never
                                     );
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
            exception
               when End_Error =>
                  Close_Cursor (Command);
                  for Index in 1..Size loop
                     Execute
                     (  Command,
                        "DROP TABLE " & Get (List, Index).all
                     );
                  end loop;
               when Error : others =>
                  Close_Cursor (Command);
                  raise;
            end;
         end if;
         --
         -- Going to determine the ODBC driver capacities
         --
         Query_Type
         (  Storage.Integer_SQL_Type,
            (1 => SQL_INTEGER),
            "32-bit integers"
         );
         Query_Type
         (  Storage.Stamp_SQL_Type,
            (SQL_TYPE_TIMESTAMP, SQL_TIMESTAMP),
            "time stamps"
         );
         Query_Type
         (  Storage.ID_SQL_Type,
            (SQL_UBIGINT, SQL_BIGINT, SQL_ULONG, SQL_INTEGER),
            "big integers"
         );
         Storage.Has_Bigint :=
            (  Storage.ID_SQL_Type.Data_Type = SQL_UBIGINT
            or else
               Storage.ID_SQL_Type.Data_Type = SQL_BIGINT
            );
         Query_Type
         (  Storage.Data_SQL_Type,
            (  SQL_LONGVARCHAR, SQL_WLONGVARCHAR,
               SQL_VARCHAR,     SQL_WVARCHAR
            ),
            "variable-length binary data"
         );
         Storage.Name_SQL_Type := Storage.Data_SQL_Type;
         --
         -- Creating tables when necessary
         --
         if not Table_Exists (Storage.Connection, Objects_Table) then
            declare
               Prefix : constant String :=
                  (  "CREATE TABLE "
                  &  Objects_Table
                  &  " ("
                  &  ID_Column & " "
                  &     To_String (Storage.ID_SQL_Type.Name)
                  );
               Suffix : constant String :=
                  (  Name_Column & " "
                  &     To_String (Storage.Name_SQL_Type.Name)  & ", "
                  &  Class_Column & " "
                  &     To_String (Storage.Data_SQL_Type.Name)  & ", "
                  &  Data_Column & " "
                  &     To_String (Storage.Data_SQL_Type.Name)  & ", "
                  &  Parameters_Column & " "
                  &     To_String (Storage.Data_SQL_Type.Name)  & ", "
                  &  Time_Column & " "
                  &     To_String (Storage.Stamp_SQL_Type.Name) & ", "
                  &  Parent_ID_Column & " "
                  &     To_String (Storage.ID_SQL_Type.Name)
                  &  ")"
                  );
            begin
               Execute
               (  Storage.Command.all,
                  Prefix & " PRIMARY KEY, " & Suffix
               );
            exception
               when others => -- Maybe it does not support PRIMARY KEY?
                  Execute
                  (  Storage.Command.all,
                     Prefix & ", " & Suffix
                  );
            end;
         end if;
         Links.Create_Table
         (  Command    => Command,
            Table_Name => Direct_Links_Table,
            Data_Type  => To_String (Storage.ID_SQL_Type.Name)
         );
         Links.Create_Table
         (  Command    => Command,
            Table_Name => Backward_Links_Table,
            Data_Type  => To_String (Storage.ID_SQL_Type.Name)
         );
         --
         -- Initializing object's ID generator
         --
         declare
            ID : aliased Object_ID := 0;
         begin
            Prepare
            (  Command,
               "SELECT MAX(" & ID_Column & ") FROM " & Objects_Table
            );
            Bind_Result
            (  Command,
               1,
               ID'Unchecked_Access
            );
            Execute (Command);
            Fetch (Command);
            Close_Cursor (Command);
            Storage.Next_ID := Object_ID'Succ (ID);
         exception
            when End_Error =>
               Close_Cursor (Command);
               Storage.Next_ID := Object_ID'Succ (No_ID);
            when Error : others =>
               Close_Cursor (Command);
               Raise_Exception
               (  Data_Error'Identity,
                  Exception_Message (Error)
               );
         end;
         Commit (Mutex);
      end;
   end Connect;

   function Create
            (  Server_Name : String;
               User_Name   : String;
               Password    : String;
               Erase       : Boolean := False
            )  return Storage_Handle is
      Result : Storage_Object_Ptr;
   begin
      Result := new Data_Base_Object (Server_Name'Length);
      Data_Base_Object (Result.all).Server_Name := Server_Name;
      Connect
      (  Data_Base_Object (Result.all),
         Server_Name,
         User_Name,
         Password,
         Erase
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
      Command : ODBC_Command'Class renames Storage.Command.all;
   begin
      for Item in 1..Get_Size (Links) loop
         Reference
         (  Command    => Command,
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
      Command : ODBC_Command'Class renames Storage.Command.all;
      ID_Text : constant String := Image (ID);
   begin
      if Storage.Cached_Key = ID then
         Storage.Cached_Key := No_ID;
      end if;
      Drop (Command, "has_in_"      & ID_Text);
      Drop (Command, "has_out_"     & ID_Text);
      Drop (Command, "has_not_"     & ID_Text);
      Drop (Command, "has_not_out_" & ID_Text);
      Delete (Command, Direct_Links_Table,   ID);
      Delete (Command, Backward_Links_Table, ID);
      Execute
      (  Command,
         (  "DELETE FROM "
         &  Objects_Table
         &  " WHERE "
         &  ID_Column
         &  " =" & ID_Text
      )  );
   exception
      when End_Error =>
         null;
      when Data_Error =>
         raise;
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
      Disable_Tracing
      (  Data_Base_Object'Class (Ptr (Storage).all).Connection.all
      );
   end Disable_Tracing;

   procedure Enable_Tracing
             (  Storage : in out Storage_Handle;
                Name    : String
             )  is
   begin
      Enable_Tracing
      (  Data_Base_Object'Class (Ptr (Storage).all).Connection.all,
         Name
      );
   exception
      when Constraint_Error | Data_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Enable_Tracing;

   function Find
            (  Storage : access Data_Base_Object;
               Name    : String;
               Parent  : Persistent_Key'Class
            )  return Persistent_Key'Class is
      Command   : ODBC_Command'Class renames Storage.Command.all;
      ID        : aliased Object_ID;
      Name_Copy : aliased String_Parameter (Name'Length);
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
         &  " = ? AND "
         &  Parent_ID_Column
         &  " = "
         &  Image (Object_Key'Class (Parent).ID)
      )  );
      ODBC.Bound_Parameters.Set (Name_Copy, Name);
      Bind_Parameter
      (  Command,
         1,
         Name_Copy'Unchecked_Access,
         Storage.Name_SQL_Type.Data_Type
      );
      Bind_Result (Command, 1, ID'Unchecked_Access);
      Execute (Command);
      Fetch (Command);
      Close_Cursor (Command);
      return Object_Key'(Persistent_Key with ID);
   exception
      when End_Error =>
         Close_Cursor (Command);
         return Object_Key'(Null_Key);
      when Data_Error =>
         Close_Cursor (Command);
         raise;
      when Error : others =>
         Close_Cursor (Command);
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Find;

   procedure Finalize (Storage : in out Data_Base_Object) is
   begin
      Purge (Storage);
      Free (Storage.Command);
      Free (Storage.Connection);
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
      Command : ODBC_Command'Class renames Storage.Command.all;
   begin
      Execute
      (  Command,
         (  "SELECT "
         &  ID_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  Parent_ID_Column
         &  " ="
         &  Image (Key)
      )  );
      loop
         Fetch (Storage.Command.all);
         Put
         (  Children,
            Pointer,
            Get_Data (Storage.Command, 1, Never)
         );
         Pointer := Pointer + 1;
      end loop;
   exception
      when End_Error =>
         Close_Cursor (Command);
      when Data_Error =>
         Close_Cursor (Command);
         raise;
      when Error : others =>
         Close_Cursor (Command);
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
      Command : ODBC_Command'Class renames Storage.Command.all;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      Execute
      (  Command,
         (  "SELECT "
         &  Class_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  ID_Column
         &  " ="
         &  Image (Object_Key'Class (Key).ID)
      )  );
      Fetch (Command);
      return Get_Data (Storage.Command, 1, Always);
   exception
      when End_Error =>
         Close_Cursor (Command);
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Image (Storage.all, Key) & " does not exist"
         );
      when Data_Error =>
         Close_Cursor (Command);
         raise;
      when Error : others =>
         Close_Cursor (Command);
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get_Class;

   function Get_Connection (Storage : Storage_Handle)
      return ODBC_Connection_Ptr is
   begin
      return Data_Base_Object'Class (Ptr (Storage).all).Connection;
   end Get_Connection;

   function Get_Creation_Time
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Time is
      Command : ODBC_Command'Class renames Storage.Command.all;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      Execute
      (  Command,
         (  "SELECT "
         &  Time_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  ID_Column
         &  " ="
         &  Image (Object_Key'Class (Key).ID)
      )  );
      Fetch (Command);
      return Get_Data (Storage.Command, 1, Always);
   exception
      when End_Error =>
         Close_Cursor (Command);
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Image (Storage.all, Key) & " does not exist"
         );
      when Data_Error =>
         Close_Cursor (Command);
         raise;
      when Error : others =>
         Close_Cursor (Command);
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
      Command : ODBC_Command'Class renames Storage.Command.all;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      Execute
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
         &  Image (Object_Key'Class (Key).ID)
      )  );
      Fetch (Command);
      Class :=
         To_Unbounded_String (Get_Data (Storage.Command, 1, Never));
      Data  :=
         To_Unbounded_String (Get_Data (Storage.Command, 2, Never));
      Parameters :=
         To_Unbounded_String (Get_Data (Storage.Command, 3, Always));
   exception
      when End_Error =>
         Close_Cursor (Command);
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Image (Storage, Key) & " does not exist"
         );
      when Data_Error =>
         Close_Cursor (Command);
         raise;
      when Error : others =>
         Close_Cursor (Command);
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
         (  Storage.Command.all,
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
      Command : ODBC_Command'Class renames Storage.Command.all;
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
      Result  : Catalogue.Set;
      Mutex   : Read_Mutex (Storage);
   begin
      Execute
      (  Command,
         (  "SELECT "
         &  Name_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  Parent_ID_Column
         &  " ="
         &  Image (Get_New_Parent_Key (Storage, Parent).ID)
         &  " AND NOT "
         &  Name_Column
         &  " IS NULL"
      )  );
      loop
         Fetch (Storage.Command.all);
         declare
            Name : String renames Get_Data (Storage.Command, 1, Never);
         begin
            if Match (Name) then
               Catalogue.Add (Result, Name);
            end if;
         end;
      end loop;
   exception
      when End_Error =>
         Close_Cursor (Command);
         Commit (Mutex);
         return Result;
      when Data_Error =>
         Close_Cursor (Command);
         raise;
      when Error : others =>
         Close_Cursor (Command);
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
      Command : ODBC_Command'Class renames Storage.Command.all;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      if Parent.all not in Object_Key then
         Raise_Exception
         (  Constraint_Error'Identity,
            "The type of requested parent key is unsupported"
         );
      end if;
      Execute
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
         &  Image (Object_Key'Class (Key).ID)
      )  );
      Fetch (Command);
      begin
         Object_Key'Class (Parent.all).ID :=
            Get_Data (Storage.Command, 1, Never);
         declare
            Result : constant String :=
                     Get_Data (Storage.Command, 2, Never);
         begin
            if Result'Length > 0 then
               Close_Cursor (Command);
               return Result;
            else
               Raise_Exception (Name_Error'Identity, "No name");
            end if;
         end;
      exception
         when End_Error =>
            Raise_Exception (Name_Error'Identity, "No name");
      end;
   exception
      when Name_Error =>
         Close_Cursor (Command);
         raise;
      when End_Error =>
         Close_Cursor (Command);
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Image (Storage.all, Key) & " does not exist"
         );
      when Data_Error =>
         Close_Cursor (Command);
         raise;
      when Error : others =>
         Close_Cursor (Command);
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
      Command : ODBC_Command'Class renames Storage.Command.all;
   begin
      Get_References
      (  Command,
         Direct_Links_Table,
         Object_Key (Key).ID,
         Object_Key_Array (References).List,
         Pointer
      );
   end Get_References;

   function Get_Server_Name (Storage : Storage_Handle) return String is
   begin
      return Data_Base_Object'Class (Ptr (Storage).all).Server_Name;
   end Get_Server_Name;

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
            (  Storage.Command,
               Direct_Links_Table,
               Object_Key'Class (Key).ID
            )
         or else
            Has_Dependants
            (  Storage.Command,
               Backward_Links_Table,
               Object_Key'Class (Key).ID
         )  );
      else
         return
            Has_Dependants
            (  Storage.Command,
               Direct_Links_Table,
               Object_Key'Class (Key).ID
            );
      end if;
   end Has_Dependants;

   function Is_In
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Boolean is
      Command : ODBC_Command'Class renames Storage.Command.all;
   begin
      if Key not in Object_Key'Class then
         return False;
      end if;
      Execute
      (  Command,
         (  "SELECT "
         &  Name_Column
         &  " FROM "
         &  Objects_Table
         &  " WHERE "
         &  ID_Column
         &  " ="
         &  Image (Object_Key'Class (Key).ID)
      )  );
      Fetch (Storage.Command.all);
      Close_Cursor (Command);
      return True;
   exception
      when End_Error =>
         Close_Cursor (Command);
         return False;
      when Data_Error =>
         Close_Cursor (Command);
         raise;
      when Error : others =>
         Close_Cursor (Command);
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Is_In;

   function Is_ODBC (Storage : Storage_Handle) return Boolean is
   begin
      return
      (  Is_Valid (Storage)
      and then
         Ptr (Storage).all in Data_Base_Object'Class
      );
   end Is_ODBC;

   function New_ID (Storage : access Data_Base_Object)
      return Object_ID is
      Result : constant Object_ID := Storage.Next_ID;
   begin
      Storage.Next_ID := Object_ID'Succ (Result);
      return Result;
   end New_ID;

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
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      declare
         Command : ODBC_Command'Class renames Storage.Command.all;
         Unnamed : Boolean   := False;
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
         begin
            Execute
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
               &  Image (ID)
            )  );
            Fetch (Command);
            Old_Parent := Get_Data (Storage.Command, 1, Never);
            if (  Old_Parent = New_Parent
               and then
                  Name = Get_Data (Storage.Command, 2, Never)
               )
            then
               Close_Cursor (Command);
               return;
            end if;
            Close_Cursor (Command);
         exception
            when End_Error =>
               Close_Cursor (Command);
               Unnamed := True;
            when others =>
               Close_Cursor (Command);
               raise;
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
            New_Name : aliased String_Parameter (Name'Length);
         begin
            ODBC.Bound_Parameters.Set (New_Name, Name);
            Prepare
            (  Command,
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
               &  " ="
               &  Image (ID)
            )  );
            Bind_Parameter
            (  Command,
               1,
               New_Name'Unchecked_Access,
               Storage.Name_SQL_Type.Data_Type
            );
            if Execute (Command'Unchecked_Access) = 0 then
               Raise_Exception
               (  End_Error'Identity,
                  "No object " & Image (Storage, Key) & " exists"
               );
            end if;
         end;
         if Unnamed then
            Reference (Command, Direct_Links_Table, ID, ID);
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
            RollBack (Storage.Connection.all);
            if Storage.Serializable then
               Release (Storage.Connection.all);
            end if;
            Set_Execution_Mode (Storage.Connection.all, None);
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
            if Storage.Serializable then
               Seize (Storage.Connection.all);
            end if;
            Storage.Sharing := Read_Only;
            Storage.Count   := 1;
            Set_Execution_Mode (Storage.Connection.all, Read_Only);
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
            if Storage.Serializable then
               Seize (Storage.Connection.all);
            end if;
            Storage.Sharing := Read_Write;
            Storage.Count   := 1;
            Set_Execution_Mode (Storage.Connection.all, Read_Write);
         when Read_Only =>
            Raise_Exception
            (  Use_Error'Identity,
               "A transaction upgrade requested"
            );
         when Read_Write =>
            Storage.Count := Storage.Count + 1;
      end case;
   end Seize_Write;

   function Serializable (Storage : Storage_Handle) return Boolean is
   begin
      return Data_Base_Object (Ptr (Storage).all).Serializable;
   end Serializable;

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
      Command    : ODBC_Command'Class renames Storage.Command.all;
      ID         : aliased Object_ID := No_ID;
      Name_Copy  : aliased String_Parameter (Name'Length);
      Class_Copy : aliased String_Parameter (Class'Length);
      Data_Copy  : aliased String_Parameter (Data'Length);
      Param_Copy : aliased String_Parameter (Parameters'Length);
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
      ODBC.Bound_Parameters.Set (Name_Copy,  Name);
      ODBC.Bound_Parameters.Set (Class_Copy, Class);
      ODBC.Bound_Parameters.Set (Data_Copy,  Data);
      ODBC.Bound_Parameters.Set (Param_Copy, Parameters);
      declare
         Parent_Key : Object_Key renames Object_Key (Parent);
         Parent_ID  : constant Object_ID := Parent_Key.ID;
      begin
         Check_Existent (Storage.all, Name, Parent_Key);
         ID := New_ID (Storage);
         Prepare
         (  Command,
            (  "INSERT INTO "
            &  Objects_Table
            &  " VALUES ("
            &  Image (ID)
            &  ", ?, ?, ?, ?, Now(), "
            &  Image (Parent_ID)
            &  ")"
         )  );
         Bind_Parameter
         (  Command,
            1,
            Name_Copy'Unchecked_Access,
            Storage.Name_SQL_Type.Data_Type
         );
         Bind_Parameter
         (  Command,
            2,
            Class_Copy'Unchecked_Access,
            Storage.Data_SQL_Type.Data_Type
         );
         Bind_Parameter
         (  Command,
            3,
            Data_Copy'Unchecked_Access,
            Storage.Data_SQL_Type.Data_Type
         );
         Bind_Parameter
         (  Command,
            4,
            Param_Copy'Unchecked_Access,
            Storage.Data_SQL_Type.Data_Type
         );
         Execute (Command);
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
         Reference (Command, Direct_Links_Table, ID, ID);
         return Object_Key'(Persistent_Key with ID);
      exception
         when Error : others =>
            if ID /= No_ID then
               Delete (Storage.all, ID);
            end if;
            Raise_Exception
            (  Data_Error'Identity,
               Exception_Message (Error)
            );
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
      Command    : ODBC_Command'Class renames Storage.Command.all;
      ID         : Object_ID := No_ID;
      Class_Copy : aliased String_Parameter (Class'Length);
      Data_Copy  : aliased String_Parameter (Data'Length);
      Param_Copy : aliased String_Parameter (Parameters'Length);
   begin
      ID := New_ID (Storage);
      Prepare
      (  Command,
         (  "INSERT INTO "
         &  Objects_Table
         &  " VALUES ("
         &  Image (ID)
         &  ", NULL, ?, ?, ?, Now(), "
         &  Image (No_ID)
         &  ")"
      )  );
      ODBC.Bound_Parameters.Set (Class_Copy, Class);
      ODBC.Bound_Parameters.Set (Data_Copy,  Data);
      ODBC.Bound_Parameters.Set (Param_Copy, Parameters);
      Bind_Parameter
      (  Command,
         1,
         Class_Copy'Unchecked_Access,
         Storage.Data_SQL_Type.Data_Type
      );
      Bind_Parameter
      (  Command,
         2,
         Data_Copy'Unchecked_Access,
         Storage.Data_SQL_Type.Data_Type
      );
      Bind_Parameter
      (  Command,
         3,
         Param_Copy'Unchecked_Access,
         Storage.Data_SQL_Type.Data_Type
      );
      Execute (Command);
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
      Command   : ODBC_Command'Class renames Storage.Command.all;
      To_Remove : Keys.Sets.Set;
      Referent  : Object_ID;
   begin
      Get_References (Command, Table_Name, Dependant, To_Remove);
      for Item in 1..Get_Size (Links) loop
         Referent := Get_Key (Storage'Access, Ref (Links, Item)).ID;
         if Is_In (To_Remove, Referent) then
            Remove (To_Remove, Referent);
         else
            Reference
            (  Command    => Command,
               Table_Name => Table_Name,
               Dependant  => Dependant,
               Referent   => Referent
            );
         end if;
      end loop;
      for Item in 1..Get_Size (To_Remove) loop
         Unreference
         (  Command    => Command,
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
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      declare
         Command : ODBC_Command'Class renames Storage.Command.all;
         ID      : constant Object_ID := Object_Key'Class (Key).ID;
      begin
         begin
            Execute
            (  Command,
               (  "SELECT "
               &  Name_Column
               &  " FROM "
               &  Objects_Table
               &  " WHERE "
               &  ID_Column
               &  " ="
               &  Image (ID)
            )  );
            Fetch (Command);
            if "" = Get_Data (Storage.Command, 1, Always) then
               return;
            end if;
         exception
            when End_Error =>
               return;
            when others =>
               Close_Cursor (Command);
               raise;
         end;
         Close_Cursor (Command);
         if 0 = Execute
               (  Command'Unchecked_Access,
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
                  &  " ="
                  &  Image (ID)
               )  )
         then
            Raise_Exception
            (  End_Error'Identity,
               (  "Object "
               &  Image (Storage, Key)
               &  " does not exist"
            )  );
         end if;
         Unreference (Command, Direct_Links_Table, ID);
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
         Raise_Exception (Data_Error'Identity, "Invalid key");
      end if;
      declare
         Command    : ODBC_Command'Class renames Storage.Command.all;
         ID         : constant Object_ID := Object_Key'Class (Key).ID;
         Class_Copy : aliased String_Parameter (Class'Length);
         Data_Copy  : aliased String_Parameter (Data'Length);
         Param_Copy : aliased String_Parameter (Parameters'Length);
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
            &  " = ?, "
            &  Data_Column
            &  " = ?, "
            &  Parameters_Column
            &  " = ? WHERE "
            &  ID_Column
            &  " ="
            &  Image (ID)
         )  );
         ODBC.Bound_Parameters.Set (Class_Copy, Class);
         ODBC.Bound_Parameters.Set (Data_Copy,  Data);
         ODBC.Bound_Parameters.Set (Param_Copy, Parameters);
         Bind_Parameter
         (  Command,
            1,
            Class_Copy'Unchecked_Access,
            Storage.Data_SQL_Type.Data_Type
         );
         Bind_Parameter
         (  Command,
            2,
            Data_Copy'Unchecked_Access,
            Storage.Data_SQL_Type.Data_Type
         );
         Bind_Parameter
         (  Command,
            3,
            Param_Copy'Unchecked_Access,
            Storage.Data_SQL_Type.Data_Type
         );
         Execute (Command);
         Sync_Links (Storage, Direct_Links_Table,   ID, Direct_Links);
         Sync_Links (Storage, Backward_Links_Table, ID, Backward_Links);
      end;
   end Update;

   function Value
            (  Storage : Data_Base_Object;
               Key     : String
            )  return Persistent_Key'Class is
   begin
      return Object_Key'(Persistent_Key with Value (Key));
   exception
      when End_Error =>
         raise Data_Error;
   end Value;

end Persistent.Native_ODBC;
