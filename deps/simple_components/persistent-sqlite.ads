--                                                                    --
--  package Persistent.SQLite       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2009       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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
--  on  SQLite  data  base.  Though SQLite is also supported through the
--  ODBC, this package is provided because SQLite is simpliest to use as
--  single  file database with no administration overhead. The data base
--  sturcture is defined as follows:
--
--  The table of objects has the following columns:
--
--  object_id      : a unique object identifier, primary key
--  catalogue_name : a unique object name, Unicode string
--  class_name     : object class, UTF-8 string
--  object_data    : object representation, UTF-8 string
--  parameter_list : object parameters, UTF-8 string
--  created_at     : creation time
--
--  The table of links represent depends on relation:
--
--  dependant : dependent object identifier
--  referent  : reference object identifier
--
--  Each record represents dependency of the  object  identified  by  id
--  from the object identified by reference. Objects that  are  excluded
--  from  garbage  collection  contain  records  referencing the objects
--  themselves.
--
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Task_Identification;        use Ada.Task_Identification;
with Object.Archived.Sets;           use Object.Archived.Sets;
with Persistent.Data_Bank;           use Persistent.Data_Bank;
with Persistent.Handle;              use Persistent.Handle;
with Persistent.SQLite_Keys;         use Persistent.SQLite_Keys;
with Persistent.SQLite_Keys.Arrays;  use Persistent.SQLite_Keys.Arrays;
with Persistent.SQLite_Keys.Sets;    use Persistent.SQLite_Keys.Sets;
with SQLite;                         use SQLite;

with Ada.Finalization;
with Generic_Unbounded_Ptr_Array;
with Persistent.Data_Bank.Indexed;

package Persistent.SQLite is
   pragma Elaborate_Body (Persistent.SQLite);
--
-- Create -- A SQLite persistent storage interface object
--
--    File_Name - To open (UTF-8 encoded database file name)
--    Erase     - Erase the data base upon connecting
--
-- This function creates a SQLite persistent  storage  interface  object
-- and returns a handle to it. The object is responsible for interacting
-- with  the  database.  A  connection  is  established  to  the  server
-- specified by the parameter Server_Name.
--
-- Returns :
--
--    Handle to the created SQLite storage object
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - Password or other data might be wrong
--
   function Create
            (  File_Name : String;
               Erase     : Boolean := False
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
--    Constraint_Error - Not a valid handle to an SQLite storage object
--
   function Get_File_Name (Storage : Storage_Handle) return String;
--
-- Is_SQLite -- Test if the storage handle refers SQLite storage
--
--    Storage - A handle to persistent storage
--
-- Returns :
--
--    True if Storage refers an SQLite one
--
   function Is_SQLite (Storage : Storage_Handle) return Boolean;

private
   pragma Inline (Is_SQLite);
--
-- Table names:
--
   Direct_Links_Table   : aliased String := "direct_links";
   Backward_Links_Table : aliased String := "backward_links";
   Objects_Table        : aliased String := "objects";
--
-- Columns:
--
   ID_Column            : constant String := "object_id";
   Name_Column          : constant String := "catalogue_name";
   Class_Column         : constant String := "class_name";
   Data_Column          : constant String := "object_data";
   Parameters_Column    : constant String := "parameters_list";
   Time_Column          : constant String := "created_at";
   Parent_ID_Column     : constant String := "parent_id";
--
-- Prefixes of training set's table names:
--
   Has_In_Table         : constant String := "has_in_";
   Has_Out_Table        : constant String := "has_out_";
   Has_Not_Table        : constant String := "has_not_";
   Has_Not_Out_Table    : constant String := "has_not_out_";
--
-- Arrays of string pointers
--
   type String_Ptr is access String;
   type String_Ptr_Array is array (Integer range <>) of String_Ptr;
   package String_List is
      new Generic_Unbounded_Ptr_Array
          (  Index_Type            => Integer,
             Object_Type           => String,
             Object_Ptr_Type       => String_Ptr,
             Object_Ptr_Array_Type => String_Ptr_Array
          );
   use String_List;
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
--
-- Data_Base_Object -- SQLite data base as persistent storage
--
   type Data_Base_Object (File_Name_Length : Natural) is
      new Indexed_Storage_Object with
   record
      Connection  : Data_Base;
      Sharing     : Sharing_Type := Fully;
      Owner       : Task_ID      := Null_Task_ID;
      Count       : Natural      := 0;
      Cached_Key  : Object_ID    := No_ID;
      Cached_Last : Integer      := 0;
      Cached_Set  : Sets.Set;
      File_Name   : String (1..File_Name_Length);
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

end Persistent.SQLite;
