--                                                                    --
--  package SQLite                  Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2009       --
--                                                                    --
--                                Last revision :  08:30 04 Aug 2022  --
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
--  This package provides bindings  to  SQLite  3.x.  The  bindings  are
--  incomplete,   only   the  things  necessary  for  persistence  layer
--  implementation are included.
--
with Ada.Streams;           use Ada.Streams;
with Interfaces;            use Interfaces;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

with Object.Handle;

package SQLite is
   pragma Elaborate_Body (SQLite);

   type Open_Flags is new Unsigned;
   READONLY       : constant Open_Flags := 16#00001#;
   READWRITE      : constant Open_Flags := 16#00002#;
   CREATE         : constant Open_Flags := 16#00004#;
   DELETEONCLOSE  : constant Open_Flags := 16#00008#;
   EXCLUSIVE      : constant Open_Flags := 16#00010#;
   AUTOPROXY      : constant Open_Flags := 16#00020#;
   URI            : constant Open_Flags := 16#00040#;
   MEMORY         : constant Open_Flags := 16#00080#;
   MAIN_DB        : constant Open_Flags := 16#00100#;
   TEMP_DB        : constant Open_Flags := 16#00200#;
   TRANSIENT_DB   : constant Open_Flags := 16#00400#;
   MAIN_JOURNAL   : constant Open_Flags := 16#00800#;
   TEMP_JOURNAL   : constant Open_Flags := 16#01000#;
   SUBJOURNAL     : constant Open_Flags := 16#02000#;
   MASTER_JOURNAL : constant Open_Flags := 16#04000#;
   NOMUTEX        : constant Open_Flags := 16#08000#;
   FULLMUTEX      : constant Open_Flags := 16#10000#;
   SHAREDCACHE    : constant Open_Flags := 16#20000#;
   PRIVATECACHE   : constant Open_Flags := 16#40000#;
   WAL            : constant Open_Flags := 16#80000#;

   type Datatype is new int range 1..5;
   SQLITE_INTEGER : constant Datatype := 1;
   SQLITE_FLOAT   : constant Datatype := 2;
   SQLITE_BLOB    : constant Datatype := 3;
   SQLITE_TEXT    : constant Datatype := 4;
   SQLITE_NULL    : constant Datatype := 5;

   subtype Row_ID is Integer_64;
------------------------------------------------------------------------
-- Data_Base -- The type encapsulating a SQLite data base
--
   type Data_Base is tagged private;
--
-- Close -- Close the database.
--
--   Base - The data-base object
--
-- This closes the data-base and finalizes all relevant objects.
--
   procedure Close (Base : in out Data_Base);
--
-- Filename -- The database file name
--
--    Base - The data base
--    Name - The data base name, e.g. main
--
-- This  function returns the name of  the file associated with the data
-- base. It is always an absolute name path.
--
-- Returns :
--
--    The file name
--
-- Exceptions :
--
--    Constraint_Error - Base is an invalid handle
--
   function Filename
            (  Base : Data_Base;
               Name : String := "main"
            )  return String;
--
-- Open -- The database file
--
--    File_Name - To open (UTF-8 encoded)
--    Flags     - Open flags
--
-- Returns :
--
--    The data base object
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - File open error
--
   function Open
            (  File_Name : String;
               Flags     : Open_Flags :=
                              READWRITE or CREATE or FULLMUTEX
            )  return Data_Base;
------------------------------------------------------------------------
-- Statement -- To be executed
--
   type Statement is tagged private;
--
-- Bind -- Set a parameter of statement
--
--    Command   - The prepared command
--    Parameter - The position of
--    Value     - To be bound when missing then null is bound
--
-- The parameters to be bound are usually specified as ? in the  command
-- text  (see  Prepare). Each such parameter has to be bound to a value.
-- The position of a parameter is specified by its index,  i.e.  by  the
-- position  of  ?  in  the  command  text.  The first parameter has the
-- position 1. The variant with Value of access String type is used when
-- the  caller  takes  the  responsibility not to destroy string all the
-- time  the binding is used. In this case an attempt is made to prevent
-- extra copying of the string contents.
--
-- Exceptions :
--
--    Constraint_Error - Command or Parameter is invalid
--    Data_Error       - Data base error
--    End_Error        - Not found (table does not exist)
--    Status_Error     - Access errors
--    Use_Error        - File access related errors
--
   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : double
             );
   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : int
             );
   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : Integer_64
             );
   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : String
             );
   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : Stream_Element_Array
             );
   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : access String
             );
   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : access Stream_Element_Array
             );
   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : chars_ptr;
                Length    : size_t
             );
   procedure Bind
             (  Command   : Statement;
                Parameter : Positive
             );
--
-- Column -- Get result row's column
--
--    Command  - The statement being execured
--    Position - The column number
--
-- Returns :
--
--    The value of the column Position in the current row
--
-- Exceptions :
--
--    Constraint_Error - Command is an invalid handle
--
   function Column
            (  Command  : Statement;
               Position : Positive
            )  return double;
   function Column
            (  Command  : Statement;
               Position : Positive
            )  return int;
   function Column
            (  Command  : Statement;
               Position : Positive
            )  return Integer_64;
   function Column
            (  Command  : Statement;
               Position : Positive
            )  return String;
   function Column
            (  Command  : Statement;
               Position : Positive
            )  return Stream_Element_Array;
--
-- Column_Count -- Get the number of columns in the result row
--
--    Command  - The statement being execured
--
-- Returns :
--
--    The number of columns
--
-- Exceptions :
--
--    Constraint_Error - Command is an invalid handle
--
   function Column_Count (Command : Statement) return Natural;
--
-- Column_Type -- Get result row's column type
--
--    Command  - The statement being execured
--    Position - The column number
--
-- Returns :
--
--    The type of the result set's column
--
-- Exceptions :
--
--    Constraint_Error - Command is an invalid handle
--
   function Column_Type
            (  Command  : Statement;
               Position : Positive
            )  return Datatype;
--
-- Exec -- Execute a SQL command
--
--    Base    - The database
--    Command - To be prepared (UTF-8 encoded)
--
-- Exceptions :
--
--    Constraint_Error - Base is an invalid handle
--    Data_Error       - Data base error
--    End_Error        - Not found (table does not exist)
--    Status_Error     - Access error
--    Use_Error        - File open error
--
   procedure Exec (Base : Data_Base; Command : String);
--
-- Is_Null -- Check if the result row's column has a defined value
--
--    Command  - The statement being execured
--    Position - The column number
--
-- Returns :
--
--    True if the value is NULL
--
-- Exceptions :
--
--    Constraint_Error - Command is an invalid handle
--
   function Is_Null
            (  Command  : Statement;
               Position : Positive
            )  return Boolean;
--
-- Is_Valid -- Check if the statement is valid
--
--    Command - The statement
--
-- Returns :
--
--    True if statement refers to a valid command
--
   function Is_Valid (Command : Statement) return Boolean;
--
-- Last_Insert_Row -- The last inserted row
--
--    Base - The database
--
-- Since this operation is not bound to the statement it  is  inherently
-- unsafe when concurrent INSERT queries are processed.
--
-- Returns :
--
--    ID of the the last inserted row
--
   function Last_Insert_Row (Base : Data_Base'Class) return Row_ID;
--
-- Prepare -- Create a SQL command
--
--    Base    - The database
--    Command - To be prepared (UTF-8 encoded)
--
-- Returns :
--
--    The statement object
--
-- Exceptions :
--
--    Constraint_Error - Base is an invalid handle
--    Data_Error       - Data base error
--    End_Error        - Not found (table does not exist)
--    Status_Error     - Access error
--    Use_Error        - File open error
--
   function Prepare
            (  Base    : Data_Base'Class;
               Command : String
            )  return Statement;
--
-- Reset -- Complete command execution, make it ready to execute again
--
--    Command - Prepared command
--
-- Exceptions :
--
--    Constraint_Error - Command is an invalid handle
--
   procedure Reset (Command : Statement);
--
-- SQL -- The original SQL command text of the prepared statement
--
--    Command - The statement being execured
--
-- This  function returns an  UTF-8 string  containing  the SQL text  of
-- the statement without bound parameters expanded.
--
-- Returns :
--
--    The statement text
--
-- Exceptions :
--
--    Constraint_Error - Command is an invalid handle
--    Status_Error     - Access error
--
   function SQL (Command : Statement) return String;
--
-- SQL_Show -- The original SQL command text of the prepared statement
--
--    Command - The statement being execured
--
-- This  function returns an  UTF-8 string  containing  the SQL text  of
-- the statement with bound parameters expanded.
--
-- Returns :
--
--    The statement text
--
-- Exceptions :
--
--    Constraint_Error - Command is an invalid handle
--    Status_Error     - Access error
--
   function SQL_Show (Command : Statement) return String;
--
-- Step -- Execue prepared command
--
--    Command   - The statement being execured
--    Completed - The execution state
--
-- When  the  result is False, the Command execution has been completed.
-- In  this  case the next operation should be Reset. When the result is
-- True  there  is  a  row  of  data  produced  by the command. The next
-- operation can be Step to get another row or else Reset to  reset  the
-- statement. The statement life-cycle looks like this:
--
--    declare
--       Command : Statement := Prepare (DB, "SQL command");
--    begin
--       Bind (Command, ...); -- Binding parameters
--       while Step (Command) loop
--          ... Column (Command) ... -- Taking the results out
--       end loop;
--       Reset (Command);
--
-- After Reset the parameters can be rebound before another execution of
-- the parameter is initiated by doing Step.
--
-- Returns :
--
--    False when the execution was completed. The next
--
-- Exceptions :
--
--    Constraint_Error - Command is an invalid handle
--    Data_Error       - Data base error
--    End_Error        - Not found (table does not exist)
--    Status_Error     - Access error
--    Use_Error        - File open error
--
   procedure Step (Command : Statement);
   function Step (Command : Statement) return Boolean;
--
-- Table_Exists -- Test if a table exists
--
--    Base - The database
--    Name - Table name
--
-- Returns :
--
--    True if the there is the table Name
--
-- Exceptions :
--
--    Constraint_Error - Base is an invalid handle
--    Data_Error       - Data base error
--    Status_Error     - Access error
--    Use_Error        - File open error
--
   function Table_Exists
            (  Base : Data_Base;
               Name : String
            )  return Boolean;
--
-- Version -- The library version
--
-- Returns :
--
--    The library version, e.g. 3.25.3
--
   function Version return String;
------------------------------------------------------------------------
-- Backup interface
--
   type Backup is tagged private;
--
-- Copy -- The data base
--
--    Base      - The data base to copy
--    Name      - The data base name
--    File_Name - The file name to backup the data base into
--
-- Exceptions :
--
--    Constraint_Error - Base is an invalid handle
--    Data_Error       - Data base error
--    Status_Error     - Access error
--    Use_Error        - File open error
--
   procedure Copy
             (  Base      : Data_Base;
                Name      : String := "main";
                File_Name : String
             );
--
-- Init -- Initiate backup
--
--    Destination      - The destination data base
--    Destination_Name - Its name
--    Source           - The source data base
--    Source_Name      - Its name
--
-- Exceptions :
--
--    Constraint_Error - Base is an invalid handle
--    Data_Error       - Data base error
--    Status_Error     - Access error
--    Use_Error        - File open error
--
   function Init
            (  Destination      : Data_Base'Class;
               Destination_Name : String := "main";
               Source           : Data_Base'Class;
               Source_Name      : String := "main"
            )  return Backup;
--
-- Pagecount -- The total number of backup pages
--
--    Object - The backup object
--
-- Returns :
--
--    The total number of pages
--
-- Exceptions :
--
--    Constraint_Error - Object is an invalid handle
--    Data_Error       - Data base error
--
   function Pagecount (Object : Backup) return int;
--
-- Remaining -- The number of pages remaining to backup
--
--    Object - The backup object
--
-- Returns :
--
--    The number of pages to backup
--
-- Exceptions :
--
--    Constraint_Error - Object is an invalid handle
--    Data_Error       - Data base error
--
   function Remaining (Object : Backup) return int;
--
-- Step -- Perform backup step
--
--    Object - The backup object
--    Pages  - The number of pages to backup
--
-- Exceptions :
--
--    Constraint_Error - Object is an invalid handle
--    Data_Error       - Data base error
--    Status_Error     - Access error
--    Use_Error        - File open error
--
   procedure Step
             (  Object : Backup;
                Pages  : int := -1
             );
------------------------------------------------------------------------
-- Tracing interface
--
-- Get_User_Data -- Get user data
--
--    Base - The database
--
-- Returns :
--
--    The user data or null if not set
--
-- Exceptions :
--
--    Constraint_Error - Base is an invalid handle
--
   function Get_User_Data (Base : Data_Base) return Object.Entity_Ptr;
--
-- On_Close -- Connection closing callback
--
   type On_Close is access procedure
        (  User_Data : Object.Entity_Ptr
        );
--
-- On_Profile -- Profiling callback
--
--    Command - Statement being executed
--    Elapsed - The time required to execute the statement
--
   type On_Profile is access procedure
        (  Command   : Statement'Class;
           Elapsed   : Duration;
           User_Data : Object.Entity_Ptr
        );
--
-- On_Row -- Row set callback
--
--    Command - Statement being executed
--
   type On_Row is access procedure
        (  Command   : Statement'Class;
           User_Data : Object.Entity_Ptr
        );
--
-- On_Statement -- Trace
--
--    Command - Statement being executed
--    Query   - The query
--
   type On_Statement is access procedure
        (  Command   : Statement'Class;
           Query     : String;
           User_Data : Object.Entity_Ptr
        );
--
-- Set_Trace -- Enable or disable tracing
--
--    Base   - The database
--    Tracer - The callback procedure
--
-- When the argument is null the corresponding tracing is disabled. Note
-- that  the statement passed into  the callback is not the original Ada
-- object.  It is a new object referencing  the original SQLite prepared
-- statement.
--
-- Exceptions :
--
--    Constraint_Error - Base is an invalid handle
--
   procedure Set_Trace (Base : Data_Base; Tracer : On_Statement);
   procedure Set_Trace (Base : Data_Base; Tracer : On_Profile);
   procedure Set_Trace (Base : Data_Base; Tracer : On_Row);
   procedure Set_Trace (Base : Data_Base; Tracer : On_Close);
--
-- Set_User_Data -- Set user data
--
--    Base - The database
--    Data - The user data, reference-counted object
--
-- If the pointer is not  null the object reference is incremented until
-- another object is set.
--
-- Exceptions :
--
--    Constraint_Error - Base is an invalid handle
--
   procedure Set_User_Data
             (  Base : Data_Base;
                Data : Object.Entity_Ptr
             );

private
   pragma Inline (Bind);
   pragma Inline (Column);
   pragma Inline (Column_Count);
   pragma Inline (Is_Null);
   pragma Inline (Is_Valid);
   pragma Inline (Last_Insert_Row);
   pragma Inline (Open);
   pragma Inline (Prepare);
   pragma Inline (Reset);

   type SQLite_Object is null record;
   type SQLite_Handle is access all SQLite_Object;
   for SQLite_Handle'Storage_Size use 0;
   pragma Convention (C, SQLite_Handle);

   type Data_Base_Object is new Object.Entity with record
      Handle       : aliased SQLite_Handle;
      User_Data    : Object.Entity_Ptr;
      Do_Close     : On_Close     := null;
      Do_Statement : On_Statement := null;
      Do_Profile   : On_Profile   := null;
      Do_Row       : On_Row       := null;
   end record;
   type Data_Base_Object_Ptr is access Data_Base_Object'Class;
--
-- Finalize -- Overrides Object...
--
   procedure Finalize (Object : in out Data_Base_Object);

   package Data_Base_Handles is
      new Object.Handle (Data_Base_Object, Data_Base_Object_Ptr);
   type Data_Base is tagged record
      Handle : Data_Base_Handles.Handle;
   end record;

   type Statement_Object is new Object.Entity with record
      Handle : aliased SQLite_Handle;
      Base   : Data_Base_Handles.Handle;
   end record;
   type Statement_Object_Ptr is access Statement_Object'Class;
--
-- Finalize -- Overrides Object...
--
   procedure Finalize (Object : in out Statement_Object);

   type Immutable_Statement_Object is
      new Statement_Object with null record;
   procedure Finalize (Object : in out Immutable_Statement_Object);

   package Statement_Handles is
      new Object.Handle (Statement_Object, Statement_Object_Ptr);
   type Statement is tagged record
      Handle : Statement_Handles.Handle;
   end record;

   type SQLite_Backup_Object is null record;
   type SQLite_Backup_Handle is access all SQLite_Backup_Object;
   for SQLite_Backup_Handle'Storage_Size use 0;
   pragma Convention (C, SQLite_Backup_Handle);

   type Backup_Object is new Object.Entity with record
      Handle : aliased SQLite_Backup_Handle;
   end record;
   type Backup_Object_Ptr is access Backup_Object'Class;
--
-- Finalize -- Overrides Object...
--
   procedure Finalize (Object : in out Backup_Object);
   package Backup_Handles is
      new Object.Handle (Backup_Object, Backup_Object_Ptr);
   type Backup is tagged record
      Handle : Backup_Handles.Handle;
   end record;

   function Errcode (Data_Base : SQLite_Handle) return int;
   pragma Import (C, Errcode, "sqlite3_errcode");

end SQLite;
