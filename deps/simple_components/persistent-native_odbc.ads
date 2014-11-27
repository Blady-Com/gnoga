--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Native_ODBC                      Luebeck            --
--  Interface                                      Autumn, 2012       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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
--  on an ODBC conform data base. The data base sturcture is defined  as
--  follows:
--
--  The table of objects has the following columns:
--
--  object_id      : a unique object identifier, primary key
--  catalogue_name : a unique object name, Unicode string
--  class_name     : object class, Latin-1 string
--  object_data    : object representation, Latin-1 string
--  parameter_list : object parameters, Latin-1 string
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
with Ada.Task_Identification;  use Ada.Task_Identification;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with ODBC;                     use ODBC;
with ODBC.API;                 use ODBC.API;
with ODBC.API.Keys;            use ODBC.API.Keys;
with ODBC.API.Keys.Arrays;     use ODBC.API.Keys.Arrays;
with ODBC.API.Keys.Sets;       use ODBC.API.Keys.Sets;
with ODBC.SQLTypes;            use ODBC.SQLTypes;
with Object.Archived.Sets;     use Object.Archived.Sets;
with Persistent.Data_Bank;     use Persistent.Data_Bank;
with Persistent.Handle;        use Persistent.Handle;

with Ada.Finalization;
with Generic_Unbounded_Ptr_Array;
with Persistent.Data_Bank.Indexed;

package Persistent.Native_ODBC is
   type ODBC_Connection_Ptr is access all ODBC_Connection'Class;
   type ODBC_Command_Ptr is access all ODBC_Command'Class;
--
-- Create -- An ODBC persistent storage interface object
--
--    Server_Name - To connect to (UTF-8 encoded)
--    User_Name   - To connect as (UTF-8 encoded)
--    Password    - Of the user (UTF-8 encoded)
--    Erase       - Erase the data base upon connecting
--
-- This function creates an ODBC persistent storage interface object and
-- returns  a  handle  to  it. The object is responsible for interacting
-- with  an  ODBC  conform data base. A connection is established to the
-- server  specified  by  the  parameter  Server_Name.  The   parameters
-- User_Name and Password identify the data base user. Refer to the ODBC
-- driver  configuration for further information about these parameters.
-- The parameter Erase when  True  erases  the  data  base  contents  by
-- dropping  all  the tables used for storing persistent objects. If the
-- data base contains any additional tables, they remain untouched.
--
-- Returns :
--
--    Handle to the created ODBC storage object
--
-- Exceptions :
--
--    Use_Error  - Password or other data might be wrong
--    Data_Error - Data base error
--
   function Create
            (  Server_Name : String;
               User_Name   : String;
               Password    : String;
               Erase       : Boolean := False
            )  return Storage_Handle;
--
-- Disable_Tracing -- Stop tracing
--
--    Storage - A handle to persistent storage
--
-- Exceptions :
--
--    Constraint_Error - Not a valid handle to an ODBC storage
--
   procedure Disable_Tracing
             (  Storage : in out Storage_Handle
             );
--
-- Enable_Tracing -- Set a trace file
--
--    Storage - A handle to persistent storage
--    Name    - Of the trace file
--
-- Exceptions :
--
--    Constraint_Error - Not a valid handle to an ODBC storage
--    Data_Error       - Any other error
--
   procedure Enable_Tracing
             (  Storage : in out Storage_Handle;
                Name    : String
             );
--
-- Get_Connection -- Get a handle to the ODBC connection
--
--    Storage - A handle to persistent storage
--
-- Returns :
--
--    Pointer to the connection object
--
-- Exceptions :
--
--    Constraint_Error - Not a valid handle to an ODBC storage
--
   function Get_Connection (Storage : Storage_Handle)
      return ODBC_Connection_Ptr;
--
-- Get_Server_Name -- Get server name
--
--    Storage - A handle to persistent storage
--
-- Returns :
--
--    The server name
--
-- Exceptions :
--
--    Constraint_Error - Not a valid handle to an ODBC storage
--
   function Get_Server_Name (Storage : Storage_Handle) return String;
--
-- Is_ODBC -- Test if the storage handle refers ODBC storage
--
--    Storage - A handle to persistent storage
--
-- Returns :
--
--    True if Storage refers an ODBC one
--
   function Is_ODBC (Storage : Storage_Handle) return Boolean;
--
-- Serializable -- Check if concurrent access is safe
--
--    Storage - A handle to persistent storage
--
-- When  the  result  is  false  it  is  unsafe  to  have  more than one
-- connection to the data base.
--
-- Returns :
--
--    True if the driver supports interlocking
--
-- Exceptions :
--
--    Constraint_Error - Not a valid handle to an ODBC storage
--
   function Serializable (Storage : Storage_Handle)
      return Boolean;
--
-- The ODBC environment for all connections
--
   Environment : aliased ODBC_Environment;

private
   pragma Inline (Is_ODBC);
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
-- SQL_Type_Info -- Description of the SQL type available for use
--
   type SQL_Type_Info is record
      Data_Type : SQL_Data_Type;
      Name      : Unbounded_String;
   end record;
--
-- Data_Base_Object -- ODBC data base as persistent storage
--
   type Data_Base_Object (Server_Name_Length : Natural) is
      new Indexed_Storage_Object with
   record
      Connection   : ODBC_Connection_Ptr;
      Command      : ODBC_Command_Ptr;
      Sharing      : Sharing_Type := Fully;
      Owner        : Task_ID      := Null_Task_ID;
      Count        : Natural      := 0;
      Serializable : Boolean      := True;
      Has_Bigint   : Boolean      := False;
      Cached_Key   : Object_ID    := No_ID;
      Next_ID      : Object_ID;
      Cached_Last  : Integer      := 0;
      Cached_Set   : Sets.Set;
   --
   -- Driver specifics :
   --
      Integer_SQL_Type : SQL_Type_Info;   -- Integer type
      ID_SQL_Type      : SQL_Type_Info;   -- Objects IDs
      Name_SQL_Type    : SQL_Type_Info;   -- Objects names
      Data_SQL_Type    : SQL_Type_Info;   -- Variable strings
      Stamp_SQL_Type   : SQL_Type_Info;   -- Time stamps
   --
   -- Server name :
   --
      Server_Name : String (1..Server_Name_Length);
   end record;
   type Data_Base_Ptr is access Data_Base_Object'Class;
--
-- Commit -- Overrides Persistent.Data_Bank...
--
   procedure Commit (Storage : in out Data_Base_Object);
--
-- Connect -- Make ODBC persistent storage interface object ready
--
--    Storage     - To prepare to work
--    Server_Name - To connect to (UTF-8 encoded)
--    User_Name   - To connect as (UTF-8 encoded)
--    Password    - Of the user (UTF-8 encoded)
--    Erase       - Erase the data base upon connecting
--
-- This procedure is called from Create to make Storage ready to use. If
-- any type is derived from Data_Base_Object, it should call Connect  at
-- some point after Initialize, but before any use.
--
-- Exceptions :
--
--    Use_Error  - Password or other data might be wrong
--    Data_Error - Data base error
--
   procedure Connect
             (  Storage     : in out Data_Base_Object'Class;
                Server_Name : String;
                User_Name   : String;
                Password    : String;
                Erase       : Boolean := False
             );
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

end Persistent.Native_ODBC;
