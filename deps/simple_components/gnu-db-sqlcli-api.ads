--                                                                    --
--  package GNU.DB.SQLCLI.API       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  11:56 13 Oct 2012  --
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
--  This  package  provides  a  thicker bindings to ODBC. The handles to
--  ODBC resources  are  encapsulated  into  controlled  types  ensuring
--  resource   release   under  all  circumstances.  If  not  explicitly
--  specified, the ODBC exceptions are propagated out of all subroutines
--  if something goes wrong.  Though  note  that  if  SQLRETURN  is  the
--  result, then probably some of errors will be rather indicated by the
--  result.
--
with Ada.Calendar;           use Ada.Calendar;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
--with GNU.DB.SQLCLI.Desc;   use GNU.DB.SQLCLI.Desc;

with Ada.Finalization;

package GNU.DB.SQLCLI.API is
   pragma Elaborate_Body (GNU.DB.SQLCLI.API);
--
-- Cursor_Disposition -- When cursor has to be closed
--
--    Never        - The cursor is never closed
--    On_Error     - On errors
--    On_No_Result - On errors and when no result returned
--    Always       - It is always closed
--
   type Cursor_Disposition is (Never, On_Error, On_No_Result, Always);
--
-- SQLGUID -- Globally unique identifier
--
   type SQLGUID_Octet is array (1..8) of Interfaces.Unsigned_8;
   pragma Pack (SQLGUID_Octet);
   pragma Convention (C, SQLGUID_Octet);
   for SQLGUID_Octet'Size use 64;

   type SQLGUID is record
      Data1 : Interfaces.Unsigned_32;
      Data2 : Interfaces.Unsigned_16;
      Data3 : Interfaces.Unsigned_16;
      Data4 : SQLGUID_Octet;
   end record;
   pragma Convention (C, SQLGUID);
   pragma Pack (SQLGUID);
   for SQLGUID'Size use 128;

   function "<"  (Left, Right : SQLGUID) return Boolean;
   function "<=" (Left, Right : SQLGUID) return Boolean;
   function ">"  (Left, Right : SQLGUID) return Boolean;
   function ">=" (Left, Right : SQLGUID) return Boolean;

   Null_GUID : constant SQLGUID := (0, 0, 0, (others => 0));
--
-- SQLGUID_Array -- Array of globally unique identifiers
--
   type SQLGUID_Array is array (Integer range <>) of SQLGUID;
-- type SEARCHABLE_ATTRIBUTE is new SQLSMALLINT;
--
-- Type_Info -- The type information (see GetTypeInfo)
--
   type Type_Info is record
      Type_Name          : Unbounded_String;
      Data_Type          : SQL_DATA_TYPE;
      Column_Size        : Natural;
      Literal_Prefix     : Unbounded_String;
      Literal_Suffix     : Unbounded_String;
      Create_Parameters  : Unbounded_String;
      Nullable           : Boolean;
      Case_Sensitive     : Boolean;
      Searchable         : SQLSMALLINT; --SEARCHABLE_ATTRIBUTE;
      Unsigned_Attribute : Boolean;
      Fixed_Prec_Scale   : Boolean;
      Auto_Unique_Value  : Boolean;
      Local_Name         : Unbounded_String;
   end record;
--
-- Access_Mode -- Of a connection
--
-- When the access mode is None a call to Execute causes propagation  of
-- Use_Error. The  Read_Only  and  Read_Write  modes  do  not  influence
-- SQL_ATTR_ACCESS_MODE,  basically,  because  ODBC  standard  does  not
-- recommend  to  change  it  on an established connection. Furthermore,
-- SQL_ATTR_ACCESS_MODE  does  not prevent driver from updating the data
-- base.  So the difference here is just for convenience. The default is
-- Read_Write in which everything is allowed. In which case it  is  also
-- recommended to call Seize to switch into serialized transaction mode.
-- The access mode None  is  useful  in  manual-commit  mode  to  detect
-- potential bugs caused by executing statements outside transactions.
--
   type Access_Mode is (None, Read_Only, Read_Write);
--
-- ODBC_Environment -- The type encapsulating ODBC environment
--
   type ODBC_Environment is
      new Ada.Finalization.Limited_Controlled with private;
   type ODBC_Environment_Ptr is access ODBC_Environment'Class;
--
-- Finalize -- Destructor
--
--    Environment - The ODBC environment object
--
   procedure Finalize (Environment: in out ODBC_Environment);
--
-- Initialize -- Constructor
--
--    Environment - The ODBC environment object
--
   procedure Initialize (Environment: in out ODBC_Environment);
--
-- ODBC_Connection -- The type encapsulating an ODBC connection
--
--    Environment - An ODBC environment
--
   type ODBC_Connection
        (  Environment : access ODBC_Environment'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
   type ODBC_Connection_Ptr is access ODBC_Connection'Class;
--
-- Connect -- Initialize the connection
--
--    Connection  - To initialize
--    Server_Name - To connect to
--    User_Name   - To connect as
--    Password    - Of the user
--    Auto_Commit - Use auto-commit mode
--
-- This procedure should be called once  immediately  after  the  object
-- construction.  No_Unicode_Support  is  propagated  from a wide-string
-- version when the driver does not support Unicode. When the  parameter
-- Auto_Commit is false, the objects of the type Session can be used  to
-- enclose separate transactions.
--
--    No_Unicode_Support - No unicode support
--    others             - Other error
--
   procedure Connect
             (  Connection  : in out ODBC_Connection;
                Server_Name : String;
                User_Name   : String;
                Password    : String;
                Auto_Commit : Boolean
             );
   procedure Connect
             (  Connection  : in out ODBC_Connection;
                Server_Name : Wide_String;
                User_Name   : Wide_String;
                Password    : Wide_String;
                Auto_Commit : Boolean
             );
--
-- EndTran -- Commit a transaction
--
--    Connection - The data base connection object
--
   procedure EndTran (Connection : in out ODBC_Connection);
--
-- Finalize -- Destructor
--
--    Connection - The data base connection object
--
-- The destructor drops the connection to the data base.
--
   procedure Finalize (Connection : in out ODBC_Connection);
--
-- Initialize -- Constructor
--
--    Connection - The data base connection object
--
-- The constructor establishes a connection to the data base.
--
   procedure Initialize (Connection : in out ODBC_Connection);
--
-- Release -- Unlock the data base
--
--    Connection - The data base connection object
--
-- The opposite to Seize.
--
   procedure Release (Connection : ODBC_Connection);
--
-- RollBack -- Discard a transaction
--
--    Connection - The data base connection object
--
   procedure RollBack (Connection : in out ODBC_Connection);
--
-- Seize -- Lock the data base
--
--    Connection - The data base connection object
--
-- The access to the data base becomes serialized.  This  procedure  may
-- fail  if  the  driver  does  not support serialized transactions. See
-- Serializable.
--
   procedure Seize (Connection : ODBC_Connection);
--
-- Serializable -- Check if the driver supports locking
--
--    Connection - The data base connection object
--
-- When the result is false, then it is unsafe to access the  data  base
-- concurrently,  because  the  drives  provides no interlocking. If the
-- result is true, Seize can be called to start a safe transaction.
--
-- Returns :
--
--    True if the driver supports serialized access
--
   function Serializable (Connection : ODBC_Connection) return Boolean;
--
-- Set_Access_Mode -- Change the access mode
--
--    Mode - The new access mode
--
   procedure Set_Access_Mode
             (  Connection : in out ODBC_Connection;
                Mode       : Access_Mode
             );
--
-- Table_Exists -- The command
--
--    Connection - To be used
--    Name       - The table name
--
-- Returns :
--
--    True if the table exists
--
   function Table_Exists
            (  Connection : access ODBC_Connection;
               Name       : String
            )  return Boolean;
   function Table_Exists
            (  Connection : access ODBC_Connection;
               Name       : Wide_String
            )  return Boolean;
--
-- ODBC_Command -- The base type of an ODBC command
--
--    Connection - To use
--
   type ODBC_Command
        (  Connection : access ODBC_Connection'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
   type ODBC_Command_Ptr is access ODBC_Command'Class;
--
-- Bind_Parameter -- Of a command
--
--    Command     - To be prepared
--    Index       - The parameter number 1..
--    Parameter   - To be bound
--    Length      - Of the data (pointer to)
--  [ Data_Type ] - The column data type
--
-- The  parameters  to  be  bound are specified as ? in the command text
-- (see Prepare). Each such parameter has to be bound to a variable. The
-- position  of  a  parameter  is  specified  by  its index, i.e. by the
-- position  of  ?  in  the  command  text.  The first parameter has the
-- position 1.
--
-- Side effects :
--
--    Length is set to the Parameter size in bytes
--
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access String;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_LONGVARCHAR
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access Wide_String;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_WLONGVARCHAR
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLSMALLINT;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_SMALLINT
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLINTEGER;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_INTEGER
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLBIGINT;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_BIGINT
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLDOUBLE;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_DOUBLE
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLGUID;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_GUID
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQL_TIMESTAMP_STRUCT;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_TIMESTAMP
             );
--
-- Bind_Result -- Of a command
--
--    Command   - To be prepared
--    Column    - The column number 1..
--    Parameter - To be bound
--    Length    - Of the data (pointer to)
--
-- The first column has the number 1. See also Get_Data which offers  an
-- alternative way for retrieving unbounded data.
--
-- Side effects :
--
--    Length is set to the Parameter size in bytes
--
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLSMALLINT;
                Length    : access SQLINTEGER
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLINTEGER;
                Length    : access SQLINTEGER
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLBIGINT;
                Length    : access SQLINTEGER
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLDOUBLE;
                Length    : access SQLINTEGER
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLGUID;
                Length    : access SQLINTEGER
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQL_TIMESTAMP_STRUCT;
                Length    : access SQLINTEGER
             );
--
-- Close_Cursor -- Drop the results of a command
--
--    Command - To be which result has to be dropped
--
-- All errors on cursor closing are ignored.
--
   procedure Close_Cursor (Command : in out ODBC_Command);
--
-- Drop -- Delete a table
--
--    Command    - A scratch command to use
--    Table_Name - To delete
--
-- Nothing happens if the table does not exist.
--
-- Exceptions :
--
--    Use_Error  - Wrong access mode
--    Data_Error - A data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Drop
             (  Command    : in out ODBC_Command'Class;
                Table_Name : String
             );
   procedure Drop
             (  Command    : in out ODBC_Command'Class;
                Table_Name : Wide_String
             );
--
-- Execute -- The command
--
--    Command - To be executed
--  [ Text  ] - The SQL command
--
-- This procedure executes a command. When the parameter Text is omitted
-- the command to be executed is  one  prepared  before  using  Prepare.
-- Otherwise it is prepared from Text.
--
-- Exceptions :
--
--    Use_Error - No access connection mode
--    Any other is a data base error
--
   procedure Execute (Command : in out ODBC_Command);
   procedure Execute (Command : in out ODBC_Command; Text : String);
   procedure Execute
             (  Command : in out ODBC_Command;
                Text    : Wide_String
             );
--
-- Execute -- The command
--
--    Command - To be executed
--  [ Text  ] - The SQL command
--
-- This  function is an equivalent of SQLExecute with SQLRowCount called
-- after it. It can be used to execute  the  commands  such  as  DELETE,
-- INSERT and UPDATE.
--
-- Returns :
--
--    The number of affected rows, which can be 0
--
-- Exceptions :
--
--    Use_Error - No access connection mode
--    Any other is a data base error
--
   function Execute (Command : access ODBC_Command) return Natural;
   function Execute
            (  Command : access ODBC_Command;
               Text    : String
            )  return Natural;
   function Execute
            (  Command : access ODBC_Command;
               Text    : Wide_String
            )  return Natural;
--
-- Fetch -- The results of a command
--
--    Command - To be which result has to be fetched
--
   procedure Fetch (Command : in out ODBC_Command);
--
-- Fetch -- The results of a command
--
--    Command - Of which result has to be fetched
--
-- Returns :
--
--    The result of execution
--
   function Fetch (Command : access ODBC_Command)
      return SQLRETURN;
--
-- Finalize -- Destructor
--
--    Command - To be finalized
--
-- A statement handle is freed
--
   procedure Finalize (Command : in out ODBC_Command);
--
-- Get_Data -- Of a command
--
--    Command - The command which result is requested
--    Column  - The result column number
--    Finish  - Close cursor before return
--
-- The data of the specified column are read  from  the  result  of  the
-- command  execution. The data are returned as a string. Note that some
-- ODBC  drivers  might  require  accessing  the result columns in their
-- number  order.  When  cursor  remains unclosed the command processing
-- should  be  either  continued  using  new  Fetch  or finished calling
-- Close_Cursor explicitly. The parameter Finish determine when Get_Data
-- closes  the cursor. On_Error and On_No_Result differs in what happens
-- on NULL value. On_Error does not close the cursor on NULL. Note  that
-- some data bases do not support NULL values for some data types, these
-- could  for  example  return  an  empty  string,  and so the result of
-- Get_Data will be an empty string instead of End_Error.
--
-- Returns :
--
--    The value of the result
--
-- Exceptions :
--
--    Data_Error - A data base error
--    End_Error  - No result, the value requested is NULL (unset)
--
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return String;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return Time;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return Wide_String;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLINTEGER;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLBIGINT;
--
-- Get_Tables -- Enumerates tables
--
--    Command - The command which result will be the table list
--
-- Fetch  and  Get_Data  can  be  used to retrieve the table names after
-- successful completion of this procedure.
--
-- Exceptions :
--
--    Data_Error - A data base error
--
    procedure Get_Tables (Command : in out ODBC_Command);
--
-- Get_Type_Info -- Query type information
--
--    Command   - The command
--    Data_Type - The data type
--
-- This function returns information about supporting the specified data
-- type by the data base.
--
-- Returns :
--
--    The data type information structure
--
-- Exceptions :
--
--    Constraint_Error - The data type is not supported
--    Data_Error       - Data base error
--
   function Get_Type_Info
            (  Command   : access ODBC_Command;
               Data_Type : SQL_DATA_TYPE
            )  return Type_Info;
--
-- Get_Message -- Get error message of a failed command
--
--    Command - The command
--
-- This  function  is  used  to  generate   an   appropriate   exception
-- information before raising Data_Error.  It  makes  sense  immediately
-- after a command returning SQLRETURN other than successful.
--
-- Returns :
--
--    The message
--
   function Get_Message (Command : ODBC_Command) return String;
--
-- Initialize -- Constructor
--
--    Command - To be initialized
--
-- A statement handle is allocated as a result of execution
--
   procedure Initialize (Command : in out ODBC_Command);
--
-- Prepare -- The command
--
--    Command - To be prepared
--    Request - The text of the request
--
-- Carefully observe that  prepared statements  might  get  lost between
-- transactions.
--
   procedure Prepare
             (  Command : in out ODBC_Command;
                Request : String
             );
   procedure Prepare
             (  Command : in out ODBC_Command;
                Request : Wide_String
             );

private
   pragma Inline ("<", "<=", ">=", ">");
   pragma Inline (Bind_Parameter);
   pragma Inline (Bind_Result);
   pragma Inline (Close_Cursor);
   pragma Inline (Execute);
   pragma Inline (Execute);
   pragma Inline (Fetch);
   pragma Inline (Prepare);

   type ODBC_Environment is
      new Ada.Finalization.Limited_Controlled with
   record
      Handle : SQLHENV;
   end record;

   type ODBC_Connection
        (  Environment : access ODBC_Environment'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Handle : SQLHDBC;
      Mode   : Access_Mode := Read_Write;
   end record;

   type ODBC_Command
        (  Connection : access ODBC_Connection'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Handle : SQLHSTMT;
   end record;

end GNU.DB.SQLCLI.API;
