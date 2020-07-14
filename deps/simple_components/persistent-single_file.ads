--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Single_File                      Luebeck            --
--  Interface                                      Autumn, 2014       --
--                                                                    --
--                                Last revision :  10:09 24 May 2020  --
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
--
--  This package provides an implementation of persistent storage  based
--  on transactional blocking files.
--
with Ada.Streams;                  use Ada.Streams;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Task_Identification;      use Ada.Task_Identification;
with Object.Archived.Sets;         use Object.Archived.Sets;
with Persistent.Blocking_Files;    use Persistent.Blocking_Files;
with Persistent.Data_Bank;         use Persistent.Data_Bank;
with Persistent.Handle;            use Persistent.Handle;
with Persistent.Memory_Pools;      use Persistent.Memory_Pools;
with Persistent.Single_File_Keys;  use Persistent.Single_File_Keys;

with Ada.Unchecked_Deallocation;
with Persistent.Blocking_Files.Transactional;
with Persistent.Data_Bank.Indexed;
with Persistent.Memory_Pools.Streams.External_B_Tree;
with Persistent.Memory_Pools.Streams.Generic_External_Ptr_B_Tree;
with Persistent.Single_File_Keys.Arrays;

package Persistent.Single_File is
--
-- Create -- A single file persistent storage interface object
--
--    File_Name - To open (UTF-8 encoded database file name)
--    Erase     - Erase the data base upon opening
--    Hash_Size - Number of blocks kept stored in the memory
--    Map_Size  - Number of virtual block map stored in the memory
--
-- This  function  creates  a persistent  storage  interface  object and
-- returns a handle to it.
--
-- Returns :
--
--    Handle to the created storage object
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - Password or other data might be wrong
--
   function Create
            (  File_Name : String;
               Erase     : Boolean  := False;
               Hash_Size : Positive := 256;
               Map_Size  : Positive := 100
            )  return Storage_Handle;
--
-- Get_File_Name -- Get file name
--
--    Storage - A handle to persistent storage
--
-- Returns :
--
--    The server name
--
-- Exceptions :
--
--    Constraint_Error - Not a valid handle
--
   function Get_File_Name (Storage : Storage_Handle) return String;
--
-- Is_Single_File -- If the storage handle refers to single file storage
--
--    Storage - A handle to persistent storage
--
-- Returns :
--
--    True if Storage refers to a single file one
--
   function Is_Single_File (Storage : Storage_Handle) return Boolean;

private
   use Persistent.Blocking_Files.Transactional;
   use Persistent.Single_File_Keys.Arrays;
--
-- Object_Key_Array -- Unbounded array of Object_Key
--
   type Object_Key_Array is new Persistent_Key_Array with record
      List : Unbounded_Array;
   end record;
--
-- Get -- Overrides Persistent.Data_Bank...
--
   function Get (Container : Object_Key_Array; Index : Integer)
      return Persistent_Key'Class;
--
-- Put -- Overrides Persistent.Data_Bank...
--
   procedure Put
             (  Container : in out Object_Key_Array;
                Index     : Integer;
                Key       : Persistent_Key'Class
             );

   package Indexed is
      new Persistent.Data_Bank.Indexed
          (  Data_Bank_Object,
             Object_Key,
             Object_Key_Array
          );
   use Indexed;
   type Persistent_Pool_Ptr is access Persistent_Pool'Class;

   Directory_Index : constant Root_Index := 1;
   Last_Index      : constant Root_Index := 2;
   Object_Index    : constant Root_Index := 3;
   Links_Index     : constant Root_Index := 4;

   function Get_Byte_Index (From, To : Object_ID) return Byte_Index;
   function Get_From (Index : Byte_Index) return Object_ID;
   function Get_To (Index : Byte_Index) return Object_ID;

   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Byte_Index;
   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return String;
   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Time;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Byte_Index
             );
   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : String
             );
   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Time
             );

   package ID_To_Object_Maps
      renames Persistent.Memory_Pools.Streams.External_B_Tree;

   subtype Object_Ptr is ID_To_Object_Maps.Item_Ptr;
   No_Object : constant Object_Ptr := ID_To_Object_Maps.No_Item;
   type Object_Tree_Ptr is access ID_To_Object_Maps.B_Tree;

   package Name_To_ID_Maps is
      new Persistent.Memory_Pools.Streams.Generic_External_Ptr_B_Tree
          (  Key_Type   => Name_Token,
             Input_Key  => Input,
             Output_Key => Output
          );
   subtype Name_Ptr is Name_To_ID_Maps.Item_Ptr;
   No_Name : constant Name_Ptr := Name_To_ID_Maps.No_Item;
   type Name_Tree_Ptr is access Name_To_ID_Maps.B_Tree;
--
-- Object_Record -- Object data
--
   type Object_Record (Length : Natural) is record
      ID         : Object_ID;    -- Object ID
      Token      : Byte_Index;   -- Name_Token allocated
      Data       : Byte_Index;   -- String allocated
      Parameters : Byte_Index;   -- String allocated
      Created    : Time;         -- Object creation time
      References : Byte_Index;   -- List of referenced object
      Class      : String (1..Length);
   end record;
   function Input
            (  Stream : access Root_Stream_Type'Class
            )  return Object_Record;
   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Object_Record
             );
--
-- Data_Base_Object -- Single file persistent storage
--
   type Data_Base_Object is new Indexed_Storage_Object with record
      Pool      : Persistent_Pool_Ptr;
      Map       : Object_Tree_Ptr;
      Links     : Object_Tree_Ptr;
      Directory : Name_Tree_Ptr;
      Sharing   : Sharing_Type := Fully;
      Owner     : Task_ID      := Null_Task_ID;
      Count     : Natural      := 0;
      File      : aliased Persistent_Transactional_Array;
   end record;
   type Data_Base_Ptr is access Data_Base_Object'Class;
--
-- Commit -- Overrides Persistent.Data_Bank...
--
   procedure Commit (Storage : in out Data_Base_Object);
--
-- Delete -- Overrides Persistent.Data_Bank...
--
   procedure Delete
             (  Storage : in out Data_Base_Object;
                Key     : Persistent_Key'Class
             );
--
-- Delete_References -- Delete direct references of an object
--
--    Storage    - The data base
--    ID         - The object ID
--    References - Address of the references list
--
   procedure Delete_Links
             (  Storage    : in out Data_Base_Object;
                ID         : Object_ID;
                References : Byte_Index
             );
--
-- Finalize -- Destruction
--
--    Storage - To finalize
--
   procedure Finalize (Storage : in out Data_Base_Object);
--
-- Find -- Overrides Persistent.Data_Bank...
--
   function Find
            (  Storage : access Data_Base_Object;
               Name    : String;
               Parent  : Persistent_Key'Class
            )  return Persistent_Key'Class;
--
-- Get_Access_Mode -- Overrides Persistent.Data_Bank...
--
   function Get_Access_Mode (Storage : Data_Base_Object)
      return Sharing_Type;
--
-- Get_By_ID -- Object record
--
--    Storage  - The data base
--    ID / Key - Of the object
--
-- Returns :
--
--    The object's address
--
-- Exceptions :
--
--    End_Error - Invalid object ID
--
   function Get_By_ID
            (  Storage : Data_Base_Object;
               ID      : Object_ID
            )  return Byte_Index;
   function Get_By_ID
            (  Storage : Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Byte_Index;
--
-- Get_Children -- Parent's children
--
--    Storage  - The data base
--    Parent   - The ID of
--    Children - To store children ID's
--    Pointer  - To place the first child at (incremented)
--
   procedure Get_Children
             (  Storage  : in out Data_Base_Object;
                Parent   : Object_ID;
                Children : in out Unbounded_Array;
                Pointer  : in out Integer
             );
--
-- Get_Children -- Overrides Persistent.Data_Bank...
--
   procedure Get_Children
             (  Storage  : in out Data_Base_Object;
                Key      : Persistent_Key'Class;
                Children : in out Persistent_Key_Array'Class;
                Pointer  : in out Integer
             );
--
-- Get_Class -- Overrides Persistent.Data_Bank...
--
   function Get_Class
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return String;
--
-- Get_Creation_Time -- Overrides Persistent.Data_Bank...
--
   function Get_Creation_Time
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Time;
--
-- Get_Data -- Overrides Persistent.Data_Bank...
--
   procedure Get_Data
             (  Storage    : in out Data_Base_Object;
                Key        : Persistent_Key'Class;
                Class      : out Unbounded_String;
                Data       : out Unbounded_String;
                Parameters : out Unbounded_String
             );
--
-- Get_Dependant -- Overrides Persistent.Data_Bank...
--
   function Get_Dependant
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class;
               No      : Positive
            )  return Persistent_Key'Class;
--
-- Get_Key -- The object's key
--
--    Storage - The data base
--    Token   - The byte index of
--
-- Returns :
--
--    The key
--
   function Get_Key
            (  Storage : Data_Base_Object;
               Token   : Byte_Index
            )  return Name_Token;
--
-- Get_List -- Overrides Persistent...
--
-- Effects :
--
--    Seize_Read
--
   function Get_List
            (  Storage     : access Data_Base_Object;
               Prefix      : String := "";
               Suffix      : String := "";
               Equivalence : Unicode_Mapping_Function := null;
               Parent      : Deposit_Handle := Root_Directory
            )  return Catalogue.Set;
--
-- Get_Name -- Overrides Persistent.Data_Bank...
--
   function Get_Name
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class;
               Parent  : access Persistent_Key'Class
            )  return String;
--
-- Get_Record -- Object record
--
--    Storage          - The data base
--    ID / Index / Key - Of the object
--
-- Returns :
--
--    The object's record
--
   function Get_Record
            (  Storage : Data_Base_Object;
               ID      : Object_ID
            )  return Object_Record;
   function Get_Record
            (  Storage : Data_Base_Object;
               Index   : Byte_Index
            )  return Object_Record;
   function Get_Record
            (  Storage : Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Object_Record;
--
-- Get_References -- Overrides Persistent.Data_Bank...
--
   procedure Get_References
             (  Storage    : in out Data_Base_Object;
                Key        : Persistent_Key'Class;
                References : in out Persistent_Key_Array'Class;
                Pointer    : in out Integer
             );
--
-- Has_Dependants -- Overrides Persistent.Data_Bank...
--
   function Has_Dependants
            (  Storage   : access Data_Base_Object;
               Key       : Persistent_Key'Class;
               All_Links : Boolean
            )  return Boolean;
--
-- Is_In -- Overrides Persistent.Data_Bank...
--
   function Is_In
            (  Storage : access Data_Base_Object;
               Key     : Persistent_Key'Class
            )  return Boolean;
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
-- On_Error -- Overrides Persistent.Data_Bank...
--
   procedure On_Error
             (  Storage : Data_Base_Object;
                Text    : String;
                Error   : Exception_Occurrence
             );
--
-- Rename -- Child of a parent object
--
--    Storage - The data base
--    Child   - To rename
--    Name    - The child name
--    Parent  - The index of
--
   procedure Rename
             (  Storage : in out Data_Base_Object;
                Child   : Object_ID;
                Name    : String;
                Parent  : Object_ID
             );
--
-- Rename -- Overrides Persistent.Data_Bank...
--
   procedure Rename
             (  Storage : in out Data_Base_Object;
                Key     : Persistent_Key'Class;
                Name    : String;
                Parent  : Persistent_Key'Class
             );
--
-- RollBack -- Overrides Persistent.Data_Bank...
--
   procedure Roll_Back (Storage : in out Data_Base_Object);
--
-- Seize_Read -- Overrides Persistent.Data_Bank...
--
   procedure Seize_Read (Storage : in out Data_Base_Object);
--
-- Seize_Write -- Overrides Persistent.Data_Bank...
--
   procedure Seize_Write (Storage : in out Data_Base_Object);
--
-- Set_Record -- Object record
--
--    Storage - The data base
--    Index   - Of the object
--    Object  - The object record
--
   procedure Set_Record
             (  Storage : in out Data_Base_Object;
                Index   : Byte_Index;
                Object  : Object_Record
             );
--
-- Store -- Object record
--
--    Storage        - The data base
--    ID             - Object ID
--    Class          - The object's class
--    Data           - The object's data
--    Parameters     - The object's parameters
--    Direct_Links   - Direct links
--    Backward_Links - Backward links
--    Key            - The new object's key
--
   procedure Store
             (  Storage        : access Data_Base_Object;
                ID             : Object_ID;
                Class          : String;
                Data           : String;
                Parameters     : String;
                Direct_Links   : Deposit_Set;
                Backward_Links : Deposit_Set;
                Key            : Name_Token := (0, 0, "")
             );
--
-- Store -- Overrides Persistent.Data_Bank...
--
   function Store
            (  Storage        : access Data_Base_Object;
               Name           : String;
               Parent         : Persistent_Key'Class;
               Class          : String;
               Data           : String;
               Parameters     : String;
               Direct_Links   : Deposit_Set;
               Backward_Links : Deposit_Set
            )  return Persistent_Key'Class;
   function Store
            (  Storage        : access Data_Base_Object;
               Class          : String;
               Data           : String;
               Parameters     : String;
               Direct_Links   : Deposit_Set;
               Backward_Links : Deposit_Set
            )  return Persistent_Key'Class;
--
-- Sync_Links -- Synchronize backward links
--
--    Storage - A pesistent storage object
--    ID      - The object ID
--    Links   - The list of objects referencing to this object
--
   procedure Sync_Links
             (  Storage : in out Data_Base_Object;
                ID      : Object_ID;
                Links   : Deposit_Set
             );
--
-- Unicode -- Overrides Persistent...
--
--    Storage - A pesistent storage object
--
-- Returns :
--
--    True if Unicode is supported
--
   function Unicode (Storage : Data_Base_Object) return Boolean;
--
-- Unname -- Overrides Persistent.Data_Bank...
--
   procedure Unname
             (  Storage : in out Data_Base_Object;
                Key     : Persistent_Key'Class
             );
--
-- Update -- Overrides Persistent.Data_Bank...
--
   procedure Update
             (  Storage        : in out Data_Base_Object;
                Key            : Persistent_Key'Class;
                Class          : String;
                Data           : String;
                Parameters     : String;
                Direct_Links   : Deposit_Set;
                Backward_Links : Deposit_Set
             );
--
-- Value -- Overrides Persistent.Data_Bank...
--
   function Value
            (  Storage : Data_Base_Object;
               Key     : String
            )  return Persistent_Key'Class;

   pragma Inline (Get_By_ID);
   pragma Inline (Get_Record);
   pragma Inline (Is_Single_File);

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Persistent_Pool'Class,
             Persistent_Pool_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Name_To_ID_Maps.B_Tree,
             Name_Tree_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  ID_To_Object_Maps.B_Tree,
             Object_Tree_Ptr
          );
end Persistent.Single_File;
