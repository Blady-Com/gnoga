--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Single_File                      Luebeck            --
--  Implementation                                 Autumn, 2014       --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Persistent.Blocking_Files.Text_IO;
with Persistent.Memory_Pools.Streams;
with Strings_Edit.Streams.Naturals;

package body Persistent.Single_File is
   use ID_To_Object_Maps;
   use Name_To_ID_Maps;
   use Persistent.Blocking_Files.Text_IO;
   use Persistent.Memory_Pools.Streams;
   use Persistent.Single_File_Keys.Sets;
   use Strings_Edit.Streams.Naturals;

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
            Set_Root_Index
            (  Storage.Pool.all,
               Directory_Index,
               Get_Root_Address (Storage.Directory.all)
            );
            Set_Root_Index
            (  Storage.Pool.all,
               Object_Index,
               Get_Root_Address (Storage.Map.all)
            );
            Set_Root_Index
            (  Storage.Pool.all,
               Links_Index,
               Get_Root_Address (Storage.Links.all)
            );
            Commit (Storage.Pool.all);
         end if;
      else
         Raise_Exception
         (  Use_Error'Identity,
            (  "Attempted other's transaction commit by "
            &  Image (Current_Task)
         )  );
      end if;
   end Commit;

   function Create
            (  File_Name : String;
               Erase     : Boolean  := False;
               Hash_Size : Positive := 256;
               Map_Size  : Positive := 100
            )  return Storage_Handle is
      Result : Storage_Handle;
   begin
      Result := Ref (new Data_Base_Object);
      declare
         This : Data_Base_Object'Class renames
                Data_Base_Object'Class (Ptr (Result).all);
      begin
         if Erase then
            Open
            (  Container => This.File,
               Name      => File_Name,
               Mode      => Create_Mode,
               Hash_Size => Hash_Size,
               Map_Size  => Map_Size
            );
            This.Pool :=
               new Persistent_Pool (This.File'Unchecked_Access);
            This.Map       := new ID_To_Object_Maps.B_Tree (This.Pool);
            This.Links     := new ID_To_Object_Maps.B_Tree (This.Pool);
            This.Directory := new Name_To_ID_Maps.B_Tree   (This.Pool);
         else
            Open
            (  Container => This.File,
               Name      => File_Name,
               Mode      => Read_Write_Mode,
               Hash_Size => Hash_Size,
               Map_Size  => Map_Size
            );
            This.Pool :=
               new Persistent_Pool (This.File'Unchecked_Access);
            This.Map := new ID_To_Object_Maps.B_Tree (This.Pool);
            ID_To_Object_Maps.Set_Root_Address
            (  This.Map.all,
               Get_Root_Index (This.Pool.all, Object_Index)
            );
            This.Links := new ID_To_Object_Maps.B_Tree (This.Pool);
            ID_To_Object_Maps.Set_Root_Address
            (  This.Links.all,
               Get_Root_Index (This.Pool.all, Links_Index)
            );
            This.Directory := new Name_To_ID_Maps.B_Tree (This.Pool);
            Name_To_ID_Maps.Set_Root_Address
            (  This.Directory.all,
               Get_Root_Index (This.Pool.all, Directory_Index)
            );
         end if;
      end;
      return Result;
   end Create;

   procedure Delete
             (  Storage : in out Data_Base_Object;
                ID      : Object_ID
             )  is
      Stream : aliased Input_Stream (Storage.Pool);
      This   : Object_Ptr := Find (Storage.Map.all, Byte_Index (ID));
   begin
      if This /= No_Object then
         Open (Stream, Get_Value (This));
         declare
            Object : constant Object_Record := Input (Stream'Access);
         begin
            Delete_Links (Storage, Object.ID, Object.References);
            Deallocate (Storage.Pool.all, Object.Data);
            Deallocate (Storage.Pool.all, Object.Parameters);
            if Object.References > 0 then
               Deallocate (Storage.Pool.all, Object.References);
            end if;
            Deallocate (Storage.Pool.all, Get_Value (This));
            Remove (This);
         end;
         loop -- Delete all links to the object
            This := Sup (Storage.Links.all, Get_Byte_Index (ID, 0));
            exit when This = No_Object;
            declare
               Key : constant Byte_Index := Get_Key (This);
            begin
               exit when Get_To (Key) > ID;
            end;
            Remove (This);
         end loop;
      end if;
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

   procedure Delete_Links
             (  Storage    : in out Data_Base_Object;
                ID         : Object_ID;
                References : Byte_Index
             )  is
   begin
      if References > 0 then
         declare
            Stream : aliased Input_Stream (Storage.Pool);
         begin
            Open (Stream, References);
            for Index in Natural range 1..Input (Stream'Access) loop
               Remove
               (  Storage.Links.all,
                  Get_Byte_Index (Input (Stream'Access), ID)
               );
            end loop;
         end;
      end if;
   end Delete_Links;

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
         Child : Name_Ptr;
         ID    : constant Object_ID := Object_Key'Class (Parent).ID;
      begin
         Child := Find (Storage.Directory.all, (Name'Length, ID, Name));
         if Child = No_Name then
            return Object_Key'(Null_Key);
         else
            return Object_Key'
                   (  Persistent_Key
                   with
                      Object_ID (Get_Pointer (Child))
                   );
         end if;
      end;
   end Find;

   procedure Finalize (Storage : in out Data_Base_Object) is
   begin
      Purge (Storage);
      Finalize (Indexed_Storage_Object (Storage));
      if Storage.Pool /= null then
         Set_Root_Index
         (  Storage.Pool.all,
            Directory_Index,
            Get_Root_Address (Storage.Directory.all)
         );
         Set_Root_Index
         (  Storage.Pool.all,
            Object_Index,
            Get_Root_Address (Storage.Map.all)
         );
         Set_Root_Index
         (  Storage.Pool.all,
            Links_Index,
            Get_Root_Address (Storage.Links.all)
         );
         Free (Storage.Map);
         Free (Storage.Links);
         Free (Storage.Directory);
         Free (Storage.Pool);
      end if;
   exception
      when Error : others =>
         On_Error
         (  Data_Bank_Object'Class (Storage),
            "Finalizing single file storage",
            Error
         );
   end Finalize;

   function Get (Container : Object_Key_Array; Index : Integer)
      return Persistent_Key'Class is
   begin
      return
         Object_Key'(Persistent_Key with Get (Container.List, Index));
   end Get;

   function Get_Access_Mode (Storage : Data_Base_Object)
      return Sharing_Type is
   begin
      return Storage.Sharing;
   end Get_Access_Mode;

   function Get_By_ID
            (  Storage : Data_Base_Object;
               ID      : Object_ID
            )  return Byte_Index is
      Item : constant Object_Ptr :=
             Find (Storage.Map.all, Byte_Index (ID));
   begin
      if Item = No_Object then
         Raise_Exception
         (  End_Error'Identity,
            "Non-existing object ID " & Image (ID)
         );
      else
         return Get_Value (Item);
      end if;
   end Get_By_ID;

   function Get_By_ID
            (  Storage : Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Byte_Index is
      Item : Object_Ptr;
      ID   : Object_ID;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      ID := Object_Key'Class (Key).ID;
      if ID = No_ID then
         Raise_Exception (End_Error'Identity, "Null key");
      end if;
      Item := Find (Storage.Map.all, Byte_Index (ID));
      if Item = No_Object then
         Raise_Exception
         (  End_Error'Identity,
            "Non-existing object " & Image (Object_Key'Class (Key).ID)
         );
      else
         return Get_Value (Item);
      end if;
   end Get_By_ID;

   function Get_Byte_Index (From, To : Object_ID) return Byte_Index is
   begin
      return Byte_Index (From) + Byte_Index (To) * 2**32;
   end Get_Byte_Index;

   procedure Get_Children
             (  Storage  : in out Data_Base_Object;
                Parent   : Object_ID;
                Children : in out Unbounded_Array;
                Pointer  : in out Integer
             )  is
      Child : Name_Ptr;
   begin
      Child := Sup (Storage.Directory.all, (0, Parent, ""));
      while Child /= No_Name loop
         declare
            Token : constant Name_Token := Get_Key (Child);
         begin
            if Token.Parent = Parent then
               Put (Children, Pointer, Object_ID (Get_Pointer (Child)));
               Pointer := Pointer + 1;
            elsif Token.Parent > Parent then
               return;
            end if;
         end;
         Child := Get_Next (Child);
      end loop;
   end Get_Children;

   procedure Get_Children
             (  Storage  : in out Data_Base_Object;
                Key      : Persistent_Key'Class;
                Children : in out Persistent_Key_Array'Class;
                Pointer  : in out Integer
             )  is
   begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      Get_Children
      (  Storage,
         Object_Key'Class (Key).ID,
         Object_Key_Array (Children).List,
         Pointer
      );
   end Get_Children;

   function Get_Class
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return String is
   begin
      return Get_Record (Storage.all, Key).Class;
   end Get_Class;

   function Get_Creation_Time
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Time is
   begin
      return Get_Record (Storage.all, Key).Created;
   end Get_Creation_Time;

   procedure Get_Data
             (  Storage    : in out Data_Base_Object;
                Key        : Persistent_Key'Class;
                Class      : out Unbounded_String;
                Data       : out Unbounded_String;
                Parameters : out Unbounded_String
             )  is
      Object : constant Object_Record := Get_Record (Storage, Key);
      Stream : aliased Input_Stream (Storage.Pool);
   begin
      Class := To_Unbounded_String (Object.Class);
      Open (Stream, Object.Data);
      Data := To_Unbounded_String (String'(Input (Stream'Access)));
      Close (Stream);
      Open (Stream, Object.Parameters);
      Parameters := To_Unbounded_String (String'(Input (Stream'Access)));
   end Get_Data;

   function Get_Dependant
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class;
               No      : Positive
            )  return Persistent_Key'Class is
      Count : Positive := No;
      ID    : Object_ID;
      This  : Object_Ptr;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      ID   := Object_Key'Class (Key).ID;
      This := Sup (Storage.Links.all, Get_Byte_Index (ID, 0));
      loop
         exit when This = No_Object;
         declare
            Key : constant Byte_Index := Get_Key (This);
         begin
            exit when Get_To (Key) > ID;
            if Get_Value (This) = 1 then -- A backward link
               if Count = 1 then
                  return
                     Object_Key'(Persistent_Key with Get_From (Key));
               else
                 Count := Count - 1;
               end if;
            end if;
         end;
         This := Get_Next (This);
      end loop;
      Raise_Exception (End_Error'Identity, "No such dependant");
   end Get_Dependant;

   function Get_File_Name (Storage : Storage_Handle) return String is
   begin
      if not (  Is_Valid (Storage)
             and then
                Ptr (Storage).all in Data_Base_Object'Class
             )
      then
         Raise_Exception (Constraint_Error'Identity, "Invalid handle");
      end if;
      return Get_Name
             (  Data_Base_Object'Class (Ptr (Storage).all).File
             );
   end Get_File_Name;

   function Get_From (Index : Byte_Index) return Object_ID is
   begin
      return Object_ID (Index mod 2**32);
   end Get_From;

   function Get_Key
            (  Storage : Data_Base_Object;
               Token   : Byte_Index
            )  return Name_Token is
   begin
      if Token = 0 then
         return (0, 0, "");
      else
         declare
            Stream : aliased Input_Stream (Storage.Pool);
         begin
            Open (Stream, Token);
            return Input (Stream'Access);
         end;
      end if;
   end Get_Key;

   function Get_List
            (  Storage     : access Data_Base_Object;
               Prefix      : String := "";
               Suffix      : String := "";
               Equivalence : Unicode_Mapping_Function := null;
               Parent      : Deposit_Handle := Root_Directory
            )  return Catalogue.Set is
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
      Child  : Name_Ptr;
      ID     : constant Object_ID :=
               Get_New_Parent_Key (Storage, Parent).ID;
      Mutex  : Read_Mutex (Storage);
   begin
      Child := Sup (Storage.Directory.all, (0, ID, ""));
      while Child /= No_Name loop
         declare
            Token : constant Name_Token := Get_Key (Child);
         begin
            if Token.Parent = ID then
               if Match (Token.Name) then
                  Catalogue.Add (Result, Token.Name);
               end if;
            elsif Token.Parent > ID then
               exit;
            end if;
         end;
         Child := Get_Next (Child);
      end loop;
      Commit (Mutex);
      return Result;
   end Get_List;

   function Get_Name
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class;
               Parent  : access Persistent_Key'Class
            )  return String is
   begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      declare
         Token : constant Name_Token :=
                 Get_Key
                 (  Storage.all,
                    Get_Record (Storage.all, Key).Token
                 );
      begin
         Parent.all :=
            Persistent_Key'Class
            (  Object_Key'(Persistent_Key with Token.Parent)
            );
         return Token.Name;
      end;
   end Get_Name;

   function Get_Record
            (  Storage : Data_Base_Object;
               ID      : Object_ID
            )  return Object_Record is
      Stream : aliased Input_Stream (Storage.Pool);
   begin
      Open (Stream, Get_By_ID (Storage, ID));
      return Input (Stream'Access);
   end Get_Record;

   function Get_Record
            (  Storage : Data_Base_Object;
               Index   : Byte_Index
            )  return Object_Record is
      Stream : aliased Input_Stream (Storage.Pool);
   begin
      Open (Stream, Index);
      return Input (Stream'Access);
   end Get_Record;

   function Get_Record
            (  Storage : Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Object_Record is
    begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      declare
         Stream : aliased Input_Stream (Storage.Pool);
      begin
         Open (Stream, Get_By_ID (Storage, Object_Key'Class (Key).ID));
         return Input (Stream'Access);
      end;
   end Get_Record;

   procedure Get_References
             (  Storage    : in out Data_Base_Object;
                Key        : Persistent_Key'Class;
                References : in out Persistent_Key_Array'Class;
                Pointer    : in out Integer
             )  is
      Object : constant Object_Record := Get_Record (Storage, Key);
   begin
      if Object.References > 0 then
         declare
            Stream : aliased Input_Stream (Storage.Pool);
         begin
            Open (Stream, Object.References);
            for Index in Natural range 1..Input (Stream'Access) loop
               Put
               (  References,
                  Pointer,
                  Object_Key'(Persistent_Key with Input (Stream'Access))
               );
               Pointer := Pointer + 1;
            end loop;
         end;
      end if;
   end Get_References;

   function Get_To (Index : Byte_Index) return Object_ID is
   begin
      return Object_ID (Index / 2**32);
   end Get_To;

   function Has_Dependants
            (  Storage   : access Data_Base_Object;
               Key       : Persistent_Key'Class;
               All_Links : Boolean
            )  return Boolean is
      ID   : Object_ID;
      This : Object_Ptr;
   begin
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid key");
      end if;
      ID   := Object_Key'Class (Key).ID;
      This := Sup (Storage.Links.all, Get_Byte_Index (0, ID));
      loop
         exit when This = No_Object;
         declare
            Key : constant Byte_Index := Get_Key (This);
         begin
            exit when Get_To (Key) > ID;
            if All_Links or else Get_Value (This) = 0 then
               return True;
            end if;
         end;
         This := Get_Next (This);
      end loop;
      return False;
   end Has_Dependants;

   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Byte_Index is
      Index : constant Unsigned_64 := Input (Stream);
   begin
      return Byte_Index (Index);
   end Input;

   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Object_Record is
      Result : Object_Record (Input (Stream));
   begin
      Result.ID         := Input (Stream);
      Result.Token      := Input (Stream);
      Result.Data       := Input (Stream);
      Result.Parameters := Input (Stream);
      Result.Created    := Input (Stream);
      Result.References := Input (Stream);
      String'Read (Stream, Result.Class);
      return Result;
   end Input;

   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return String is
      Length : constant Natural := Input (Stream);
      Result : String (1..Length);
   begin
      if Length > 0 then
         String'Read (Stream, Result);
      end if;
      return Result;
   end Input;

   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Time is
      Year         : Year_Number;
      Month        : Month_Number;
      Day          : Day_Number;
      Milliseconds : Unsigned_64;
   begin
      Year         := Input (Stream);
      Month        := Input (Stream);
      Day          := Input (Stream);
      Milliseconds := Input (Stream);
      return
         Time_Of
         (  Year    => Year,
            Month   => Month,
            Day     => Day,
            Seconds => Day_Duration (Long_Float (Milliseconds) / 1000.0)
         );
   exception
      when Time_Error =>
         Raise_Exception (Data_Error'Identity, "Wrong time");
   end Input;

   function Is_In
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Boolean is
   begin
      if Key not in Object_Key'Class then
         return False;
      else
         return True;
      end if;
   end Is_In;

   function Is_Single_File (Storage : Storage_Handle) return Boolean is
   begin
      return
      (  Is_Valid (Storage)
      and then
         Ptr (Storage).all in Data_Base_Object'Class
      );
   end Is_Single_File;

   function New_ID (Storage : access Data_Base_Object)
      return Object_ID is
      Pool : Persistent_Pool'Class renames Storage.Pool.all;
      ID   : constant Object_ID :=
             Object_ID (Get_Root_Index (Pool, Last_Index)) + 1;
   begin
      Set_Root_Index (Pool, Last_Index, Byte_Index (ID));
      return ID;
   end New_ID;

   procedure On_Error
             (  Storage : Data_Base_Object;
                Text    : String;
                Error   : Exception_Occurrence
             )  is
   begin
      null;
   end On_Error;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Byte_Index
             )  is
   begin
      Output (Stream, Unsigned_64 (Value));
   end Output;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Object_Record
             )  is
   begin
      Output (Stream, Value.Length);
      Output (Stream, Value.ID);
      Output (Stream, Value.Token);
      Output (Stream, Value.Data);
      Output (Stream, Value.Parameters);
      Output (Stream, Value.Created);
      Output (Stream, Value.References);
      String'Write (Stream, Value.Class);
   end Output;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : String
             )  is
   begin
      Output (Stream, Natural (Value'Length));
      if Value'Length > 0 then
         String'Write (Stream, Value);
      end if;
   end Output;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Time
             )  is
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin
      Split (Value, Year, Month, Day, Seconds);
      Output (Stream, Year);
      Output (Stream, Month);
      Output (Stream, Day);
      Output (Stream, Unsigned_64 (Long_Float (Seconds) * 1000.0));
   end Output;

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
                Child   : Object_ID;
                Name    : String;
                Parent  : Object_ID
             )  is
      Index   : constant Byte_Index := Get_By_ID (Storage, Child);
      Object  : Object_Record := Get_Record (Storage, Index);
      New_Key : constant Name_Token := (Name'Length, Parent, Name);
   begin
      if Object.Token /= 0 then -- Named object
         declare
            Old_Key : constant Name_Token :=
                      Get_Key (Storage, Object.Token);
         begin
            if Old_Key.Parent = Parent and then Name = Old_Key.Name then
               return; -- Same parent, same name
            elsif Parent /= No_ID then
               declare     -- Check if self-decendant
                  Children : Unbounded_Array;
                  This     : Object_ID;
                  From     : Integer := Integer'First;
                  To       : Integer := From;
               begin
                  Get_Children (Storage, Parent, Children, To);
                  while From < To loop
                     This := Get (Children, From);
                     if This = Child then
                        Raise_Exception
                        (  Name_Error'Identity,
                           (  "Object "
                           &  Image (Child)
                           &  " is renamed to a self-descendant"
                        )  );
                     end if;
                     From := From + 1;
                     Get_Children (Storage, This, Children, To);
                  end loop;
               end;
            end if;
            begin
               Add (Storage.Directory.all, New_Key, Byte_Index (Child));
            exception
               when Constraint_Error =>
                  Raise_Exception
                  (  Name_Error'Identity,
                     "Object " & Quote (Name) & " already exists"
                  );
            end;
            Remove (Storage.Directory.all, Old_Key);
         end;
      else
         begin
            Add (Storage.Directory.all, New_Key, Byte_Index (Child));
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Name_Error'Identity,
                  "Object " & Quote (Name) & " already exists"
               );
         end;
      end if;
      Object.Token :=
         Get_Key_Address (Find (Storage.Directory.all, New_Key));
      Set_Record (Storage, Index, Object);
      Replace -- Reference to itself to prevent removal
      (  Storage.Links.all,
         Get_Byte_Index (Object.ID, Object.ID),
         0
      );
   end Rename;

   procedure Rename
             (  Storage : in out Data_Base_Object;
                Key     : Persistent_Key'Class;
                Name    : String;
                Parent  : Persistent_Key'Class
             )  is
   begin
      if Name'Length = 0 then
         Raise_Exception
         (  Name_Error'Identity,
            "Object's name cannot be empty"
         );
      end if;
      if Key not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid child key");
      end if;
      if Parent not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid parent key");
      end if;
      Rename
      (  Storage => Storage,
         Child   => Object_Key'Class (Key).ID,
         Name    => Name,
         Parent  => Object_Key'Class (Parent).ID
      );
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

            Free (Storage.Directory);
            Free (Storage.Map);
            Free (Storage.Pool);
            Rollback (Storage.File);
            Storage.Pool :=
               new Persistent_Pool (Storage.File'Unchecked_Access);
            Storage.Map := new ID_To_Object_Maps.B_Tree (Storage.Pool);
            ID_To_Object_Maps.Set_Root_Address
            (  Storage.Map.all,
               Get_Root_Index (Storage.Pool.all, Object_Index)
            );
            Storage.Links := new ID_To_Object_Maps.B_Tree (Storage.Pool);
            ID_To_Object_Maps.Set_Root_Address
            (  Storage.Links.all,
               Get_Root_Index (Storage.Pool.all, Links_Index)
            );
            Storage.Directory :=
               new Name_To_ID_Maps.B_Tree (Storage.Pool);
            Name_To_ID_Maps.Set_Root_Address
            (  Storage.Directory.all,
               Get_Root_Index (Storage.Pool.all, Directory_Index)
            );
         end if;
      end if;
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

   procedure Set_Record
             (  Storage : in out Data_Base_Object;
                Index   : Byte_Index;
                Object  : Object_Record
             )  is
      Stream : aliased Output_Stream (Storage.Pool);
   begin
      Open (Stream, Index);
      Output (Stream'Access, Object);
   end Set_Record;

   procedure Store
             (  Storage        : access Data_Base_Object;
                ID             : Object_ID;
                Class          : String;
                Data           : String;
                Parameters     : String;
                Direct_Links   : Deposit_Set;
                Backward_Links : Deposit_Set;
                Key            : Name_Token := (0, 0, "")
             )  is
      Object : Object_Record (Class'Length);
      Stream : aliased Output_Stream (Storage.Pool);
   begin
      if Key.Length > 0 then
         Object.Token :=
            Get_Key_Address (Find (Storage.Directory.all, Key));
      else
         Object.Token := 0;
      end if;
      Object.ID := ID;
      Object.Created := Clock;
         -- Data
      Output (Stream'Access, Data);
      Object.Data := Get_First (Stream);
      Close (Stream);
         -- Parameters
      Output (Stream'Access, Parameters);
      Object.Parameters := Get_First (Stream);
      Close (Stream);
         -- References
      if Get_Size (Direct_Links) > 0 then
         Output (Stream'Access, Get_Size (Direct_Links));
         for Index in 1..Get_Size (Direct_Links) loop
            declare
               ID : constant Object_ID :=
                    Get_Key (Storage, Ref (Direct_Links, Index)).ID;
            begin
               Output (Stream'Access, ID);
               Replace
               (  Storage.Links.all,
                  Get_Byte_Index (ID, Object.ID),
                  0
               );
            end;
         end loop;
         Object.References := Get_First (Stream);
         Close (Stream);
      else
         Object.References := 0;
      end if;
         -- Class
      Object.Class := Class;
         -- Object
      Output (Stream'Access, Object);
      Sync_Links (Storage.all, Object.ID, Backward_Links);
      if Key.Length > 0 then -- Self reference named object
         Replace
         (  Storage.Links.all,
            Get_Byte_Index (Object.ID, Object.ID),
            0
         );
      end if;
      Add (Storage.Map.all, Byte_Index (Object.ID), Get_First (Stream));
   end Store;

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
      elsif Parent not in Object_Key'Class then
         Raise_Exception (End_Error'Identity, "Invalid parent key");
      end if;
      declare
         ID  : constant Object_ID := New_ID (Storage);
         Key : constant Name_Token :=
                 (Name'Length, Object_Key'Class (Parent).ID, Name);
      begin
         begin
            Add (Storage.Directory.all, Key, Byte_Index (ID));
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Name_Error'Identity,
                  "Object " & Quote (Name) & " already exists"
               );
         end;
         Store
         (  Storage        => Storage,
            ID             => ID,
            Key            => Key,
            Class          => Class,
            Data           => Data,
            Parameters     => Parameters,
            Direct_Links   => Direct_Links,
            Backward_Links => Backward_Links
         );
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
      ID : constant Object_ID := New_ID (Storage);
   begin
      Store
      (  Storage        => Storage,
         ID             => ID,
         Class          => Class,
         Data           => Data,
         Parameters     => Parameters,
         Direct_Links   => Direct_Links,
         Backward_Links => Backward_Links
      );
      return Object_Key'(Persistent_Key with ID);
   end Store;

   function Unicode (Storage : Data_Base_Object) return Boolean is
   begin
      return True;
   end Unicode;

   procedure Unname
             (  Storage : in out Data_Base_Object;
                Key     : Persistent_Key'Class
             )  is
      Index  : constant Byte_Index := Get_By_ID (Storage, Key);
      Object : Object_Record := Get_Record (Storage, Index);
      Token  : constant Name_Token := Get_Key (Storage, Object.Token);
   begin
      if Token.Length /= 0 then -- Named object
         Remove (Storage.Directory.all, Token);
         Remove -- Delete self-reference
         (  Storage.Links.all,
            Get_Byte_Index (Object.ID, Object.ID)
         );
         Object.Token := 0;
         Set_Record (Storage, Index, Object);
      end if;
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
      Index : constant Byte_Index := Get_By_ID (Storage, Key);

      procedure Update (Object : in out Object_Record) is
         Stream : aliased Output_Stream (Storage.Pool);
      begin
            -- Data
         Open (Stream, Object.Data);
         Output (Stream'Access, Data);
         Object.Data := Get_First (Stream);
         Close (Stream);
            -- Parameters
         Open (Stream, Object.Parameters);
         Output (Stream'Access, Parameters);
         Object.Parameters := Get_First (Stream);
         Close (Stream);
            -- References
         Delete_Links (Storage, Object.ID, Object.References);
         if Get_Size (Direct_Links) > 0 then
            if Object.References /= 0 then
               Open (Stream, Object.References);
            end if;
            Output (Stream'Access, Get_Size (Direct_Links));
            for Index in 1..Get_Size (Direct_Links) loop
               declare
                  ID : constant Object_ID :=
                          Get_Key
                          (  Storage'Access,
                             Ref (Direct_Links, Index)
                          ) .ID;
               begin
                  Output (Stream'Access, ID);
                  Replace
                  (  Storage.Links.all,
                     Get_Byte_Index (Object.ID, ID),
                     0
                  );
               end;
            end loop;
            Object.References := Get_First (Stream);
            Close (Stream);
         else
            if Object.References /= 0 then
               Deallocate (Storage.Pool.all, Object.References);
               Object.References := 0;
            end if;
         end if;
         Open (Stream, Index);
         Output (Stream'Access, Object);
      end Update;

      Old_Object : Object_Record := Get_Record (Storage, Index);
   begin
      if Class'Length = Old_Object.Length then
         Update (Old_Object);
      else
         declare
            New_Object : Object_Record (Class'Length);
         begin
            New_Object.ID         := Old_Object.ID;
            New_Object.Token      := Old_Object.Token;
            New_Object.Data       := Old_Object.Data;
            New_Object.Parameters := Old_Object.Parameters;
            New_Object.Created    := Old_Object.Created;
            New_Object.References := Old_Object.References;
            New_Object.Class      := Old_Object.Class;
            Update (New_Object);
         end;
      end if;
      Sync_Links (Storage, Old_Object.ID, Backward_Links);
   end Update;

   procedure Sync_Links
             (  Storage : in out Data_Base_Object;
                ID      : Object_ID;
                Links   : Deposit_Set
             )  is
      Old   : Single_File_Keys.Sets.Set;
      Other : Object_ID;
      This  : Object_Ptr;
   begin
      This := Sup (Storage.Links.all, Get_Byte_Index (ID, 0));
      while This /= No_Object loop
         Other := Get_To (Get_Key (This));
         exit when Other > ID;
         if Get_Value (This) = 1 then
            Add (Old, Other);
         end if;
         This := Get_Next (This);
      end loop;
      Remove (Old, ID);
      for Index in 1..Get_Size (Links) loop
         Other := Get_Key (Storage'Access, Ref (Links, Index)).ID;
         Remove (Old, Other);
         Replace (Storage.Links.all, Get_Byte_Index (ID, Other), 1);
      end loop;
      for Index in 1..Get_Size (Old) loop
         Remove
         (  Storage.Links.all,
            Get_Byte_Index (ID, Get (Old, Index))
         );
      end loop;
   end Sync_Links;

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

end Persistent.Single_File;
