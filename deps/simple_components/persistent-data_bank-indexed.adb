--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Data_Bank.Indexed                Luebeck            --
--  Implementation                                 Autumn, 2004       --
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

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Persistent.Data_Bank.Reference; use Persistent.Data_Bank.Reference;
with Persistent.Handle;              use Persistent.Handle;
with Strings_Edit;                   use Strings_Edit;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Strings_Edit.Quoted;            use Strings_Edit.Quoted;

package body Persistent.Data_Bank.Indexed is
   use Storage_Index;

   Backward_Link_Quote : constant Character := '?';
   Direct_Link_Quote   : constant Character := '/';
--
-- Restore_Class -- Restore object's class
--
--    Storage - Persistent data storage (a pointer to)
--    Key     - The key of the object
--
   function Restore_Class
            (  Storage    : access Indexed_Storage_Object'Class;
               Key        : Persistent_Key'Class
            )  return String;
--
-- Restore_Creation_Time -- Restore object's creation time
--
--    Storage - Persistent data storage (a pointer to)
--    Key     - The key of the object
--
   function Restore_Creation_Time
            (  Storage    : access Indexed_Storage_Object'Class;
               Key        : Persistent_Key'Class
            )  return Time;
--
-- Restore_By_Key -- Recursively restore object by its key
--
--    Storage - Persistent data storage (a pointer to)
--    Key     - The key of the object being restored
--    Object  - The result
--
   procedure Restore_By_Key
             (  Storage : in out Indexed_Storage_Object'Class;
                Key     : Persistent_Key'Class;
                Object  : in out Deposit_Handle
             );
--
-- Restore_Object -- Recursively restore object
--
--    Storage    - Persistent data storage (a pointer to)
--    Key        - The key of the object being restored
--    Class      - The object class
--    Data       - The object description
--    Parameters - Additional parameters
--    Object     - The result
--
   procedure Restore_Object
             (  Storage    : in out Indexed_Storage_Object'Class;
                Key        : Persistent_Key'Class;
                Class      : String;
                Data       : String;
                Parameters : String;
                Object     : in out Deposit_Handle
             );
--
-- Store_Dependencies
--
--    Storage        - Persistent data storage
--    Data           - To write
--    Parameters     - To write
--    Object         - The object
--    Direct_Links   - Of the object (output)
--    Backward_Links - Of the object (output)
--
   procedure Store_Dependencies
             (  Storage        : in out Indexed_Storage_Object'Class;
                Data           : in out Unbounded_String;
                Parameters     : in out Unbounded_String;
                Object         : Deposit'Class;
                Direct_Links   : in out Deposit_Set;
                Backward_Links : in out Deposit_Set
             );

   procedure Create
             (  Storage        : in out Indexed_Storage_Object;
                Object         : Deposit_Handle;
                ID             : out Key;
                Name           : String;
                Class          : String;
                Data           : String;
                Parameters     : String;
                Direct_Links   : Deposit_Set;
                Backward_Links : Deposit_Set;
                Parent         : Deposit_Handle := Root_Directory
            )   is
      Lock : Holder (Storage.Lock'Access);
   begin
      if not Is_Valid (Object) then
         raise Constraint_Error;
      end if;
      declare
         Where : Key renames Get_Key (Storage.Index, Ptr (Parent));
      begin
         ID :=
            Key
            (  Store
               (  Storage        => Storage.Index.Storage,
                  Name           => Name,
                  Parent         => Where,
                  Class          => Class,
                  Data           => Data,
                  Parameters     => Parameters,
                  Direct_Links   => Direct_Links,
                  Backward_Links => Backward_Links
            )  );
         Add (Storage.Index, Ptr (Object), ID, Name, Where);
      end;
   end Create;

   procedure Create
             (  Storage        : in out Indexed_Storage_Object;
                Object         : Deposit_Handle;
                ID             : out Key;
                Class          : String;
                Data           : String;
                Parameters     : String;
                Direct_Links   : Deposit_Set;
                Backward_Links : Deposit_Set
            )   is
      Lock : Holder (Storage.Lock'Access);
   begin
      if not Is_Valid (Object) then
         raise Constraint_Error;
      end if;
      ID :=
         Key
         (  Store
            (  Storage        => Storage.Index.Storage,
               Class          => Class,
               Data           => Data,
               Parameters     => Parameters,
               Direct_Links   => Direct_Links,
               Backward_Links => Backward_Links
         )  );
      Add (Storage.Index, Ptr (Object), ID);
   end Create;

   function Get
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Deposit_Handle is
      Lock : Holder (Storage.Lock'Access);
   begin
      return Ref (Get (Storage.Index, Name, Ptr (Parent)));
   exception
      when End_Error =>
         declare
            Result     : Deposit_Handle;
            Parent_Key : Key renames
                             Get_Key (Storage.Index, Ptr (Parent));
            Mutex      : Read_Mutex (Storage.Index.Storage);
            Object_Key : Key renames
               Key (Find (Storage.Index.Storage, Name, Parent_Key));
         begin
            if Object_Key = Null_Key then
               Raise_Exception
               (  End_Error'Identity,
                  "Object " & Name & " does not exist"
               );
            end if;
            Restore_By_Key (Storage.all, Object_Key, Result);
            Add
            (  Storage.Index,
               Ptr (Result),
               Object_Key,
               Name,
               Parent_Key
            );
            Commit (Mutex);
            return Result;
         end;
   end Get;

   function Get
            (  Storage : access Indexed_Storage_Object;
               Key     : Persistent_Key'Class
            )  return Deposit_Handle is
      Lock : Holder (Storage.Lock'Access);
   begin
      if Key not in Persistent.Data_Bank.Indexed.Key'Class then
         Raise_Exception
         (  End_Error'Identity,
            "Unsupported key type"
         );
      end if;
      declare
         Object     : Deposit_Handle;
         Object_Key : Persistent.Data_Bank.Indexed.Key renames
                         Persistent.Data_Bank.Indexed.Key (Key);
      begin
         Set (Object, Get (Storage.Index, Object_Key));
         return Object;
      exception
         when End_Error =>
            begin
               --
               -- Get the object's name and parent if it has them
               --
               declare
                  Parent_Key : aliased Persistent.Data_Bank.Indexed.Key;
                  Name : constant String :=
                            Get_Name
                            (  Storage.Index.Storage,
                               Key,
                               Parent_Key'Access
                            );
                  Parent : Deposit_Handle;
               begin
                  if Parent_Key /= Null_Key then
                     --
                     -- Ensure that the parent is resident before trying
                     -- to restore the child
                     --
                     Parent := Get (Storage, Parent_Key);
                  end if;
                  Restore_By_Key (Storage.all, Object_Key, Object);
                  if Name = "" then
                     Add (Storage.Index, Ptr (Object), Object_Key);
                  else
                     Add
                     (  Storage.Index,
                        Ptr (Object),
                        Object_Key,
                        Name,
                        Parent_Key
                     );
                  end if;
               end;
            exception
               when Name_Error =>
                  --
                  -- The object is anonymous
                  --
                  Restore_By_Key (Storage.all, Object_Key, Object);
                  Add (Storage.Index, Ptr (Object), Object_Key);
            end;
            return Object;
      end;
   end Get;

   function Get_Class
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return String is
      Lock       : Holder (Storage.Lock'Access);
      Mutex      : Read_Mutex (Storage.Index.Storage);
      Object_Key : constant Key :=
                       Key
                       (  Find
                          (  Storage.Index.Storage,
                             Name,
                             Get_Key (Storage.Index, Ptr (Parent))
                       )  );
   begin
      if Object_Key = Null_Key then
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Name & " does not exist"
         );
      end if;
      declare
         Result : constant String := Restore_Class (Storage, Object_Key);
      begin
         Commit (Mutex);
         return Result;
      end;
   end Get_Class;

   function Get_Creation_Time
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Time is
      Lock       : Holder (Storage.Lock'Access);
      Mutex      : Read_Mutex (Storage.Index.Storage);
      Result     : Time;
      Object_Key : constant Key :=
                            Key
                            (  Find
                               (  Storage.Index.Storage,
                                  Name,
                                  Get_Key (Storage.Index, Ptr (Parent))
                            )  );
   begin
      if Object_Key = Null_Key then
         Raise_Exception
         (  End_Error'Identity,
            "Object " & Name & " does not exist"
         );
      end if;
      Result := Restore_Creation_Time (Storage, Object_Key);
      Commit (Mutex);
      return Result;
   end Get_Creation_Time;

   function Get_Key
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return Key is
      Lock : Holder (Storage.Lock'Access);
   begin
      return Get (Storage.Index, Ptr (Object));
   exception
      when End_Error =>
         raise Constraint_Error;
   end Get_Key;

   function Get_Name
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return String is
      Lock : Holder (Storage.Lock'Access);
   begin
      if not Is_Valid (Object) then
         raise Constraint_Error;
      end if;
      declare
         Name : String renames Get (Storage.Index, Ptr (Object));
      begin
         if Name'Length > 0 then
            return Name;
         end if;
         raise Name_Error;
      end;
   exception
      when End_Error =>
         raise Constraint_Error;
   end Get_Name;

   function Get_New_Parent_Key
            (  Storage : access Indexed_Storage_Object;
               Parent  : Deposit_Handle
            )  return Key is
      Lock : Holder (Storage.Lock'Access);
   begin
      return Get_New_Parent_Key (Storage.Index, Ptr (Parent));
   end Get_New_Parent_Key;

   function Get_Parent
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return Deposit_Handle is
      Lock : Holder (Storage.Lock'Access);
   begin
      if not Is_Valid (Object) then
         raise Constraint_Error;
      end if;
      return Ref (Get_Parent (Storage.Index, Ptr (Object)));
   exception
      when End_Error =>
         raise Constraint_Error;
   end Get_Parent;

   function Is_Descendant
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle;
               Parent  : Deposit_Handle
            )  return Boolean is
      Lock : Holder (Storage.Lock'Access);
   begin
      return Is_Descendant (Storage.Index, Ptr (Object), Ptr (Parent));
   end Is_Descendant;

   function Is_In
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Boolean is
      Lock : Holder (Storage.Lock'Access);
   begin
      if Is_In (Storage.Index, Name, Ptr (Parent)) then
         return True;
      end if;
      declare
         Mutex  : Read_Mutex (Storage.Index.Storage);
         Result : Boolean;
      begin
         Result := (  Null_Key
                   /= Key
                      (  Find
                         (  Storage.Index.Storage,
                            Name,
                            Get_Key (Storage.Index, Ptr (Parent))
                   )  )  );
         Commit (Mutex);
         return Result;
      end;
   exception
      when End_Error | Name_Error =>
         return False;
   end Is_In;

   function Is_In
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return Boolean is
      Lock : Holder (Storage.Lock'Access);
   begin
      return Is_In (Storage.Index, Ptr (Object));
   end Is_In;

   function Is_Indexed
            (  Storage : access Indexed_Storage_Object;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Boolean is
      Lock : Holder (Storage.Lock'Access);
   begin
      return Is_In (Storage.Index, Name, Ptr (Parent));
   exception
      when Name_Error =>
         return False;
   end Is_Indexed;

   function Is_Named
            (  Storage : access Indexed_Storage_Object;
               Object  : Deposit_Handle
            )  return Boolean is
      Lock : Holder (Storage.Lock'Access);
   begin
      if not Is_Valid (Object) then
         return False;
      end if;
      declare
         Name : String renames Get (Storage.Index, Ptr (Object));
      begin
         return Name'Length > 0;
      end;
   exception
      when End_Error =>
         return False;
   end Is_Named;

   function Is_Named
            (  Storage : access Indexed_Storage_Object;
               Object  : Key
            )  return Boolean is
      Lock : Holder (Storage.Lock'Access);
   begin
      declare
         Name : String renames Get (Storage.Index, Object);
      begin
         return Name'Length > 0;
      end;
   exception
      when End_Error =>
         return False;
   end Is_Named;

   procedure Purge (Storage : in out Indexed_Storage_Object'Class) is
   begin
      Erase (Storage.Index);
   end Purge;

   procedure Put
             (  Storage : in out Indexed_Storage_Object;
                Object  : in out Deposit_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
      Direct_Links   : Deposit_Set;
      Backward_Links : Deposit_Set;
      Lock           : Holder (Storage.Lock'Access);
   begin
      if not Is_Valid (Object) then
         raise Constraint_Error;
      elsif Is_In (Storage.Index, Ptr (Object)) then
         --
         -- The object is already in the data base, the only thing to do
         -- is to check that its name and parent are same.
         --
         if Get_Parent (Storage.Index, Ptr (Object)) /= Ptr (Parent)
         then
            Raise_Exception
            (  Name_Error'Identity,
               (  "Object "
               &  Quote (Name)
               &  " persists under a different parent"
            )  );
         elsif Get (Storage.Index, Ptr (Object)) /= Name then
            Raise_Exception
            (  Name_Error'Identity,
               (  "Object "
               &  Quote (Name)
               &  " persists as "
               &  Quote (Get (Storage.Index, Ptr (Object)))
            )  );
         end if;
      else
         --
         -- The  object  is not there. We have to check that the name is
         -- free and then store it.
         --
         declare
            Parent_Key : Key renames
               Get_New_Parent_Key (Storage.Index, Ptr (Parent));
            Data       : Unbounded_String;
            Parameters : Unbounded_String;
            Mutex      : Write_Mutex (Storage.Index.Storage);
         begin
            if (  Null_Key
               /= Key
                  (  Find
                     (  Storage.Index.Storage,
                        Name,
                        Parent_Key
               )  )  )
            then
               Raise_Exception
               (  Name_Error'Identity,
                  "Object " & Quote (Name) & " already exists"
               );
            end if;
            Store_Dependencies
            (  Storage        => Storage,
               Data           => Data,
               Parameters     => Parameters,
               Object         => Ptr (Object).all,
               Direct_Links   => Direct_Links,
               Backward_Links => Backward_Links
            );
            Add
            (  List     => Storage.Index,
               Object   => Ptr (Object),
               Name     => Name,
               Parent   => Parent_Key,
               External =>
                  Key
                  (  Store
                     (  Storage        => Storage.Index.Storage,
                        Name           => Name,
                        Class          => Get_Class (Object),
                        Data           => To_String (Data),
                        Parameters     => To_String (Parameters),
                        Direct_Links   => Direct_Links,
                        Backward_Links => Backward_Links,
                        Parent         => Parent_Key
            )    )  );
            Commit (Mutex);
            Reset_Modified (Ptr (Object).all);
         end;
      end if;
   end Put;

   procedure Put
             (  Storage : in out Indexed_Storage_Object;
                Object  : in out Deposit_Handle
             )  is
      This : constant Deposit_Ptr := Ptr (Object);
      Lock : Holder (Storage.Lock'Access);
   begin
      if This = null then
         raise Constraint_Error;
      else
         declare
            Mutex : Write_Mutex (Storage.Index.Storage);
         begin
            Store_Object (Storage, This);
            Commit (Mutex);
         end;
      end if;
   end Put;

   procedure Put
             (  Storage : in out Indexed_Storage_Object;
                Key     : Persistent_Key'Class;
                Object  : Deposit'Class
             )  is
      Direct_Links   : Deposit_Set;
      Backward_Links : Deposit_Set;
      Data           : Unbounded_String;
      Parameters     : Unbounded_String;
      Lock           : Holder (Storage.Lock'Access);
   begin
      Store_Dependencies
      (  Storage        => Storage,
         Data           => Data,
         Parameters     => Parameters,
         Object         => Object,
         Direct_Links   => Direct_Links,
         Backward_Links => Backward_Links
      );
      Update
      (  Storage        => Storage.Index.Storage.all,
         Key            => Key,
         Class          => Get_Class (Object),
         Data           => To_String (Data),
         Parameters     => To_String (Parameters),
         Direct_Links   => Direct_Links,
         Backward_Links => Backward_Links
      );
   end Put;

   procedure Rename
             (  Storage    : in out Indexed_Storage_Object;
                Old_Name   : String;
                Old_Parent : Deposit_Handle := Root_Directory;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             )  is
      Lock  : Holder (Storage.Lock'Access);
      Mutex : Write_Mutex (Storage.Index.Storage);
   begin
      Rename
      (  Storage.Index.Storage.all,
         Storage.Index,
         Old_Name,
         Ptr (Old_Parent),
         New_Name,
         Ptr (New_Parent)
      );
      Commit (Mutex);
   end Rename;

   procedure Rename
             (  Storage    : in out Indexed_Storage_Object;
                Object     : in out Deposit_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle := Root_Directory
             )  is
      Lock  : Holder (Storage.Lock'Access);
      Mutex : Write_Mutex (Storage.Index.Storage);
   begin
      Rename
      (  Storage.Index.Storage.all,
         Storage.Index,
         Ptr (Object),
         New_Name,
         Ptr (New_Parent)
      );
      Commit (Mutex);
   exception
      when Error : End_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Rename;

   function Restore_Class
            (  Storage    : access Indexed_Storage_Object'Class;
               Key        : Persistent_Key'Class
            )  return String is
   begin
      return Get_Class (Storage, Key);
   end Restore_Class;

   function Restore_Creation_Time
            (  Storage    : access Indexed_Storage_Object'Class;
               Key        : Persistent_Key'Class
            )  return Time is
   begin
      return Get_Creation_Time (Storage, Key);
   end Restore_Creation_Time;

   procedure Restore_By_Key
             (  Storage : in out Indexed_Storage_Object'Class;
                Key     : Persistent_Key'Class;
                Object  : in out Deposit_Handle
             )  is
      Class      : Unbounded_String;
      Data       : Unbounded_String;
      Parameters : Unbounded_String;
   begin
      Get_Data (Storage, Key, Class, Data, Parameters);
      Restore_Object
      (  Storage,
         Key,
         To_String (Class),
         To_String (Data),
         To_String (Parameters),
         Object
      );
   exception
      when Error : End_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
      when Name_Error =>
         Raise_Exception
         (  Use_Error'Identity,
            (  Quote (To_String (Class))
            &  " is not a registered object class"
         )  );
   end Restore_By_Key;

   procedure Restore_Object
             (  Storage    : in out Indexed_Storage_Object'Class;
                Key        : Persistent_Key'Class;
                Class      : String;
                Data       : String;
                Parameters : String;
                Object     : in out Deposit_Handle
             )  is
      Referent : Deposit_Handle;
      Pointer  : aliased Integer := Parameters'First;
      List     : Deposit_List;
   begin
      loop
         Get (Parameters, Pointer);
         exit when Pointer > Parameters'Last;
         case Parameters (Pointer) is
            when '@' => -- Reference to the data base
               if (  Pointer + 1 > Parameters'Last
                  or else                            -- Backward
                     Parameters (Pointer + 1) = ' '  -- compatibility
                  )
               then
                  Add
                  (  List,
                     Ref
                     (  To_Persistent_Storage_Ptr
                        (  Storage'Unchecked_Access
                     )  ),
                     Key
                  );
               else
                  Referent :=
                     Value
                     (  Storage => Ref
                                   (  To_Persistent_Storage_Ptr
                                      (  Storage'Unchecked_Access
                                   )  ),
                        Key     => Key,
                        Class   => Class,
                        Source  => Get_Quoted
                                   (  Parameters,
                                      Pointer'Unchecked_Access,
                                      '@'
                     )             );
                  Add (List, Ptr (Referent), False);
               end if;
            when Direct_Link_Quote => -- Key of (direct link)
               Referent :=
                  Get
                  (  Storage'Unchecked_Access,
                     Value
                     (  Storage,
                        Get_Quoted
                        (  Parameters,
                           Pointer'Unchecked_Access,
                           Direct_Link_Quote
                  )  )  );
               Add (List, Ptr (Referent), False);
            when Backward_Link_Quote => -- Key of (backward link)
               Referent :=
                  Get
                  (  Storage'Unchecked_Access,
                     Value
                     (  Storage,
                        Get_Quoted
                        (  Parameters,
                           Pointer'Unchecked_Access,
                           Backward_Link_Quote
                  )  )  );
               Add (List, Ptr (Referent), True);
            when others => -- Number of an object in the list to use
               declare
                  Position : Integer;
               begin
                  Get (Parameters, Pointer, Position);
                  Add (List, Get (List, Position), False);
               exception
                  when Error : others =>
                     Raise_Exception
                     (  Data_Error'Identity,
                        Exception_Message (Error)
                     );
               end;
         end case;
      end loop;
      Pointer := Data'First;
      Get (Data, Pointer);
      declare
         Result : Deposit_Ptr;
      begin
         Create
         (  Source  => Data,
            Pointer => Pointer,
            Class   => Class,
            List    => List,
            Object  => Result
         );
         Set (Object, Result);
      end;
      Get (Data, Pointer);
      if Pointer /= Data'Last + 1 then
         Raise_Exception
         (  Data_Error'Identity,
            "Unmatched data in " & Quote (Class)
         );
      end if;
   end Restore_Object;

   procedure Store_Object
             (  Storage : in out Indexed_Storage_Object'Class;
                Object  : Deposit_Ptr
             )  is
   begin
      if not Is_In (Storage.Index, Object) then
         declare
            Direct_Links   : Deposit_Set;
            Backward_Links : Deposit_Set;
            Data           : Unbounded_String;
            Parameters     : Unbounded_String;
         begin
            Store_Dependencies
            (  Storage        => Storage,
               Data           => Data,
               Parameters     => Parameters,
               Object         => Object.all,
               Direct_Links   => Direct_Links,
               Backward_Links => Backward_Links
            );
            Add
            (  List     => Storage.Index,
               Object   => Object,
               External =>
                  Key
                  (  Store
                     (  Storage        => Storage.Index.Storage,
                        Class          => Get_Class (Object.all),
                        Data           => To_String (Data),
                        Parameters     => To_String (Parameters),
                        Direct_Links   => Direct_Links,
                        Backward_Links => Backward_Links
            )    )  );
            Reset_Modified (Object.all);
         end;
      end if;
   end Store_Object;

   procedure Store_Dependencies
             (  Storage        : in out Indexed_Storage_Object'Class;
                Data           : in out Unbounded_String;
                Parameters     : in out Unbounded_String;
                Object         : Deposit'Class;
                Direct_Links   : in out Deposit_Set;
                Backward_Links : in out Deposit_Set
             )  is
      Referent_Key : Key;
      Mark         : Character;
      Item         : Deposit_Ptr;
      List         : Deposit_List;
      Self         : Boolean := False;
   begin
      Get_Referents (Object, List);
      for Position in 1..Get_Size (List) loop
         if Position > 1 then
            Append (Parameters, ' ');
         end if;
         Item := Get (List, Position);
         if Item.all in Self_Reference'Class then
            Append
            (  Parameters,
               '@' & Image (Self_Reference'Class (Item.all)) & '@'
            );
            Self := True;
         else
            if Is_Backward (List, Item) then
               Mark := Backward_Link_Quote;
            else
               Mark := Direct_Link_Quote;
            end if;
            if Is_In (Storage.Index, Item) then
               Append
               (  Parameters,
                  Quote
                  (  Image (Storage, Get (Storage.Index, Item)),
                     Mark
               )  );
            else
               declare
                  Referent_Param : Unbounded_String;
                  Referent_Data  : Unbounded_String;
               begin
                  Store_Dependencies
                  (  Storage        => Storage,
                     Data           => Referent_Data,
                     Parameters     => Referent_Param,
                     Object         => Item.all,
                     Direct_Links   => Direct_Links,
                     Backward_Links => Backward_Links
                  );
                  Referent_Key :=
                     Key
                     (  Store
                        (  Storage        => Storage.Index.Storage,
                           Class          => Get_Class (Item.all),
                           Data           => To_String (Referent_Data),
                           Parameters     => To_String (Referent_Param),
                           Direct_Links   => Direct_Links,
                           Backward_Links => Backward_Links
                     )  );
                  Reset_Modified (Item.all);
               end;
               Add (Storage.Index, Item, Referent_Key);
               Append
               (  Parameters,
                  Quote (Image (Storage, Referent_Key), Mark)
               );
            end if;
         end if;
      end loop;
      if not Self then
         declare
            Size : Positive := 1024;
         begin
            loop
               declare
                  Buffer  : String (1..Size);
                  Pointer : Positive := Buffer'First;
               begin
                  Store (Buffer, Pointer, Object);
                  Append (Data, Buffer (Buffer'First..Pointer - 1));
                  exit;
               exception
                  when Ada.IO_Exceptions.Layout_Error =>
                     Size := Size * 2;
               end;
            end loop;
         end;
      end if;
      Erase (Direct_Links);
      Erase (Backward_Links);
      for Position in 1..Get_Size (List) loop
         Item := Get (List, Position);
         if Item.all not in Self_Reference'Class then
            if Is_Backward (List, Item) then
               Add (Backward_Links, Item);
            else
               Add (Direct_Links, Item);
            end if;
         end if;
      end loop;
   end Store_Dependencies;

   procedure Unname
             (  Storage : in out Indexed_Storage_Object;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             )  is
      Lock  : Holder (Storage.Lock'Access);
      Mutex : Write_Mutex (Storage.Index.Storage);
   begin
      Unname
      (  Storage.Index.Storage.all,
         Storage.Index,
         Name,
         Ptr (Parent)
      );
      Commit (Mutex);
   end Unname;

   procedure Unname
             (  Storage : in out Indexed_Storage_Object;
                Object  : in out Deposit_Handle
             )  is
      Lock  : Holder (Storage.Lock'Access);
      Mutex : Write_Mutex (Storage.Index.Storage);
   begin
      Unname (Storage.Index.Storage.all, Storage.Index, Ptr (Object));
      Commit (Mutex);
   end Unname;

end Persistent.Data_Bank.Indexed;
