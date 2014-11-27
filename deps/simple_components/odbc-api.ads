--                                                                    --
--  package ODBC.API                Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2012       --
--                                                                    --
--                                Last revision :  17:24 20 Oct 2012  --
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

with Ada.Calendar;           use Ada.Calendar;
with Ada.Streams;            use Ada.Streams;
with Interfaces.C;           use Interfaces.C;
with ODBC.Bound_Parameters;  use ODBC.Bound_Parameters;
with ODBC.SQLTypes;          use ODBC.SQLTypes;

with Ada.Finalization;

package ODBC.API is
--
-- Used_ODBC_Version -- The ODBC version requested from the environment
--
   Used_ODBC_Version : constant SQL_OV := SQL_OV_ODBC3;
--                                        SQL_OV_ODBC3_80;
--
-- Default_Block_Size -- The default size of blocks by which string data
--                       are fetched
--
   Default_Block_Size : constant := 1024 * 2;
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
-- Execution_Mode -- Of a connection
--
-- When the access mode is None a call to Execute causes propagation  of
-- Use_Error.  The Read_Only  and  Read_Write  modes  do  not  influence
-- SQL_ATTR_ACCESS_MODE,  basically,  because  ODBC  standard  does  not
-- recommend  to  change  it  on an established connection. Furthermore,
-- SQL_ATTR_ACCESS_MODE  does  not prevent driver from updating the data
-- base.  So the difference here is just for convenience. The default is
-- Read_Write in which everything is allowed. In which case it  is  also
-- recommended to call Seize to switch into serialized transaction mode.
-- The access mode None  is  useful  in  manual-commit  mode  to  detect
-- potential bugs caused by executing statements outside transactions.
--
   type Execution_Mode is (None, Read_Only, Read_Write);
--
-- Type_Info -- The type information (see Get_Type_Info)
--
   type Type_Info
        (  Type_Name_Length         : Natural;
           Literal_Prefix_Length    : Natural;
           Literal_Suffix_Length    : Natural;
           Create_Parameters_Length : Natural;
           Local_Name_Length        : Natural
        )  is
   record
      Data_Type          : SQL_DATA_TYPE;
      Column_Size        : SQLINTEGER;
      Searchable         : SQL_COLUMN_SEARCHABLE;
      Nullable           : Boolean;
      Case_Sensitive     : Boolean;
      Unsigned_Attribute : Boolean;
      Fixed_Prec_Scale   : Boolean;
      Auto_Unique_Value  : Boolean;
      Type_Name          : String (1..Type_Name_Length);
      Local_Name         : String (1..Local_Name_Length);
      Literal_Prefix     : String (1..Literal_Prefix_Length);
      Literal_Suffix     : String (1..Literal_Suffix_Length);
      Create_Parameters  : String (1..Create_Parameters_Length);
   end record;
--
-- Param_Description -- Parameter description (see Describe_Param)
--
   type Param_Description is record
      Data_Type      : SQL_DATA_TYPE;
      Parameter_Size : SQLULEN;
      Decimal_Digits : Natural;
      Nullable       : SQL_NULLABLE_FIELD;
   end record;
--
-- Column_Description -- Description of a table column
--
   type Column_Description (Name_Length : Natural) is record
      Data_Type      : SQL_DATA_TYPE;
      Column_Size    : SQLULEN;
      Decimal_Digits : Natural;
      Nullable       : SQL_NULLABLE_FIELD;
      Column_Name    : String (1..Name_Length);
   end record;
--
-- DSN_Descrition -- Name and description string of a DSN
--
   type DSN_Type is (Any_DSN, System_DSN, User_DSN);
   type DSN_Description
        (  Name_Length        : Natural;
           Description_Length : Natural
        )  is record
      Name        : String (1..Name_Length);
      Description : String (1..Description_Length);
   end record;
------------------------------------------------------------------------
-- ODBC_Environment -- The type encapsulating ODBC environment
--
   type ODBC_Environment is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Finalize -- Destructor
--
--    Environment - The ODBC environment object
--
-- A derived type must call this procedure if it overrides it.
--
   procedure Finalize (Environment : in out ODBC_Environment);
--
-- Get_Connection_Pooling -- Get environment attribute
--
--    Environment - The ODBC environment object
--
-- Returns :
--
--    Current connection pooling attribute
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Connection_Pooling
            (  Environment : ODBC_Environment
            )  return SQL_CP;
--
-- Get_Class_Origin -- Get diag record field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    A string  that  indicates the  document  that  defines  the  class
--    portion of the SQLSTATE value in this record
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Class_Origin
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_Connection_Name -- Get diag record field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    The name of the connection that the diagnostic record relates to
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Connection_Name
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_Column_Number -- Get diag record field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    The value that represents  the column number in the result  set or
--    the parameter number in the set of parameters
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Column_Number
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_CP_Match -- Get environment attribute
--
--    Environment - The ODBC environment object
--
-- Returns :
--
--    How a connection is chosen from a connection pool
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_CP_Match
            (  Environment : ODBC_Environment
            )  return SQL_CP_MATCH;
--
-- Get_Next_DSN -- Enumerate DSNs
--
--    Environment   - The ODBC environment object
--    DSN           - The DSN type to enumerate
--    Buffer_Length - The maximal length of DSN description
--
-- This function returns a description of  the  first DSN  of  the  type
-- specified by the parameter DSN.
--
-- Returns :
--
--    A DSN description
--
-- Exceptions :
--
--    Constraint_Error - Buffer is too small for DSN description
--    Data_Error       - ODBC driver error
--    End_Error        - No more DSNs of this type
--
   function Get_First_DSN
            (  Environment   : ODBC_Environment;
               DSN           : DSN_Type;
               Buffer_Length : Positive := Default_Block_Size
            )  return DSN_Description;
--
-- Get_Message_Text -- Get diag record field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    An informational message on the error or warning
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Message_Text
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String;
   function Get_Message_Text
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return Wide_String;
--
-- Get_Number -- Diag field
--
--    Environment - The ODBC environment object
--
-- Returns :
--
--    The number of status records available
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Number
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_Native -- Get diag record field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    A driver/data  source-specific  native error code.  If there is no
--    native error code, the driver returns 0
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Native
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_Next_DSN -- Enumerate DSNs
--
--    Environment   - The ODBC environment object
--    Buffer_Length - The maximal length of DSN description
--
-- This function returns a description of  the  next  DSN  of  the  type
-- specified in the call to Get_First_DSN.
--
-- Returns :
--
--    A DSN description
--
-- Exceptions :
--
--    Constraint_Error - Buffer is too small for DSN description
--    Data_Error       - ODBC driver error
--    End_Error        - No more DSNs of this type
--
   function Get_Next_DSN
            (  Environment   : ODBC_Environment;
               Buffer_Length : Positive := Default_Block_Size
            )  return DSN_Description;
--
-- Get_ODBC_Version -- Get environment attribute
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    The ODBC version
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_ODBC_Version
            (  Environment : ODBC_Environment
            )  return SQL_OV;
--
-- Get_Output_NTS -- Get environment attribute
--
--    Environment - The ODBC environment object
--
-- Returns :
--
--    Whether the driver retunrs strings NUL-terminated
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Output_NTS
            (  Environment : ODBC_Environment
            )  return Boolean;
--
-- Get_Returncode -- Diag field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    Return code returned by the function
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Returncode
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLRETURN;
--
-- Get_Row_Number -- Get diag record field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    The row number in the rowset,  or the parameter  number in the set
--    of parameters, with which the status record is associated
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Row_Number
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLLEN;
--
-- Get_Server_Name -- Get diag record field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    A string that indicates the server name that the diagnostic record
--    relates to
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Server_Name
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String;
   function Get_Server_Name
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return Wide_String;
--
-- Get_SQLSTATE -- Get diag record field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    A five-character SQLSTATE diagnostic code. For more information
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_SQLSTATE
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_Subclass_Origin -- Get diag record field
--
--    Environment   - The ODBC environment object
--    Record_Number - The record number
--
-- Returns :
--
--    Identifies  the defining  portion  of the  subclass portion of the
--    SQLSTATE code
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Subclass_Origin
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String;
--
-- Initialize -- Constructor
--
--    Environment - The ODBC environment object
--
-- A derived type must call this procedure if it overrides it.  The ODBC
-- version is set to ODBC 3.x.
--
   procedure Initialize (Environment : in out ODBC_Environment);
--
-- Set_Connection_Pooling -- Set environment attribute
--
--    Environment - The ODBC environment object
--    Value       - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Set_Connection_Pooling
             (  Environment : in out ODBC_Environment;
                Value       : SQL_CP
             );
--
-- Set_CP_Match -- Set environment attribute
--
--    Environment - The ODBC environment object
--    Value       - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Set_CP_Match
             (  Environment : in out ODBC_Environment;
                Value       : SQL_CP_MATCH
             );
--
-- Set_ODBC_Version -- Set environment attribute
--
--    Environment - The ODBC environment object
--    Value       - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Set_ODBC_Version
             (  Environment : in out ODBC_Environment;
                Value       : SQL_OV
             );
--
-- Set_Output_NTS -- Set environment attribute
--
--    Environment - The ODBC environment object
--    Value       - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Set_Output_NTS
             (  Environment : in out ODBC_Environment;
                Value       : Boolean
             );
------------------------------------------------------------------------
-- ODBC_Connection -- The type encapsulating an ODBC connection
--
--    Environment - An ODBC environment
--
   type ODBC_Connection
        (  Environment : access ODBC_Environment'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
--
-- Connect -- Initialize the connection
--
--    Connection  - To initialize
--    Server_Name - To connect to
--    User_Name   - To connect as
--    Password    - Of the user
--    Auto_Commit - Use auto-commit mode
--
-- This procedure should be called once to connect to the data base.
--
--    Data_Error - Data base error
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
-- Disable_Tracing -- Stop tracing
--
--    Connection - The connection
--
-- This procedure stops tracing previously enabled by Enable_Tracing
--
   procedure Disable_Tracing (Connection : in out ODBC_Connection);
--
-- Drop -- Delete a table
--
--    Connection - The connection
--    Table_Name - To delete
--
-- Nothing happens if the table does not exist.
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Wrong access mode
--
   procedure Drop
             (  Connection : in out ODBC_Connection;
                Table_Name : String
             );
   procedure Drop
             (  Connection : in out ODBC_Connection;
                Table_Name : Wide_String
             );
--
-- Enable_Tracing -- Set a trace file
--
--    Connection - The connection
--    File_Name  - Of the trace file
--
-- This procedure enables tracing into the speciied file.
--
-- Exceptions :
--
--    Data_Error - Database or file error
--
   procedure Enable_Tracing
             (  Connection : in out ODBC_Connection;
                File_Name  : String
             );
   procedure Enable_Tracing
             (  Connection : in out ODBC_Connection;
                File_Name  : Wide_String
             );
--
-- End_Transaction -- Commit a transaction
--
--    Connection - The data base connection object
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure End_Transaction (Connection : in out ODBC_Connection);
--
-- Finalize -- Destructor
--
--    Connection - The data base connection object
--
-- The destructor drops the connection to the data base.
--
   procedure Finalize (Connection : in out ODBC_Connection);
--
-- Get_Access_Mode -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Connection mode (SQL_MODE_READ_WRITE, SQL_MODE_READ_ONLY)
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Access_Mode
            (  Connection : ODBC_Connection
            )  return SQL_MODE;
--
-- Get_Accessible_Procedures -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if all procedures returned by SQLProcedures can be executed
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Accessible_Procedures
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Accessible_Tables -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if  the user is  guaranteed  SELECT  privileges to all tables
--    returned by SQLTables
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Accessible_Tables
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Active_Environments -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum  number  of active  environments  that  the driver can
--    support or zero when limit is unknown or not set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Active_Environments
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Aggregate_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating support for aggregation functions
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Aggregate_Functions
            (  Connection : ODBC_Connection
            )  return SQL_AF;
--
-- Get_Alter_Domain -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses in the ALTER DOMAIN statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Alter_Domain
            (  Connection : ODBC_Connection
            )  return SQL_AD;
--
-- Get_Alter_Table -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask  enumerating  the  clauses  in the  ALTER TABLE  statement
--    supported by the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Alter_Table
            (  Connection : ODBC_Connection
            )  return SQL_AT;
--
-- Get_Async_DBC_Enable -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if asynchronous execution of selected functions enabled
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Async_DBC_Enable
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Async_DBC_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the driver can execute connection functions asynchronously
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Async_DBC_Functions
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Async_Enable -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True  if  a function  called with  a statement  on  the  specified
--    connection is executed asynchronously
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Async_Enable
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Async_Mode -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The level of asynchronous support in the driver
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Async_Mode
            (  Connection : ODBC_Connection
            )  return SQL_AM;
--
-- Get_Auto_IPD -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if automatic population of the IPD after a call to SQLPrepare
--    is supported
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Auto_IPD
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Autocommit -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the driver uses auto-commit mode
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Autocommit
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Batch_Row_Count -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask that enumerates the behavior of the driver with respect to
--    the availability of row counts
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Batch_Row_Count
            (  Connection : ODBC_Connection
            )  return SQL_BRC;
--
-- Get_Batch_Support -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask that enumerating the driver's support for batches
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Batch_Support
            (  Connection : ODBC_Connection
            )  return SQL_BS;
--
-- Get_Bookmark_Persistence -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask that  enumerating  the operations  through which bookmarks
--    persist
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Bookmark_Persistence
            (  Connection : ODBC_Connection
            )  return SQL_BP;
--
-- Get_Catalog_Location -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The position of the catalog in a qualified table name
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Catalog_Location
            (  Connection : ODBC_Connection
            )  return SQL_CL;
--
-- Get_Catalog_Name -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the server supports catalog names
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Catalog_Name
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Catalog_Name_Separator -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Characters that the data source defines as the separator between a
--    catalog  name  and  the qualified  name element  that  follows  or
--    precedes it
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Catalog_Name_Separator
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Catalog_Term -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character  string  with  the  data source  vendor's  name  for a
--    catalog; for example,  "database" or "directory".  This string can
--    be in upper, lower, or mixed case
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Catalog_Term
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Catalog_Usage -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the statements in which catalogs can be used
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Catalog_Usage
            (  Connection : ODBC_Connection
            )  return SQL_CU;
--
-- Get_Class_Origin -- Get diag record field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    A string  that  indicates the  document  that  defines  the  class
--    portion of the SQLSTATE value in this record
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Class_Origin
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_Connection_Name -- Get diag record field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    The name of the connection that the diagnostic record relates to
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Connection_Name
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_Collation_Seq -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The name of the collation sequence
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Collation_Seq
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Column_Alias -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the data source supports column aliases
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Column_Alias
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Column_Number -- Get diag record field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    The value that represents  the column number in the result  set or
--    the parameter number in the set of parameters
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Column_Number
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_Concat_Null_Behavior -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Handling  the concatenation  of NULL valued  character  data  type
--    columns with non-NULL valued character data type columns
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Concat_Null_Behavior
            (  Connection : ODBC_Connection
            )  return SQL_CB;
--
-- Get_Connection_Dead -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the connection was lost
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Connection_Dead
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Connection_Timeout -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- This function returns a value corresponding to the number  of seconds
-- to  wait  for  any  request on  the  connection  to  complete  before
-- returning to the application.
--
-- Returns :
--
--    Timeout resolution is 1 second
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Connection_Timeout
            (  Connection : ODBC_Connection
            )  return Duration;
--
-- Get_Convert -- Get connection information
--
--    Connection - The ODBC connection object
--    Data_Type  - The type of the conversion argument
--
-- The result is the bitmask indicates  the conversions supported by the
-- data source with  the CONVERT scalar  function  for data of  the type
-- specified.  If the bitmask  equals zero,  the data  source  does  not
-- support  any conversions  from  data of  the  named  type,  including
-- conversion to the same data type.
--
-- Returns :
--
--    Conversions bitmask
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Convert
            (  Connection : ODBC_Connection;
               Data_Type  : SQL_DATA_TYPE
            )  return SQL_CVT;
--
-- Get_Convert_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Bitmask enumerating the scalar conversion functions supported
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Convert_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_CVT;
--
-- Get_Correlation_Name -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Indicates whether table correlation names are supported
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Correlation_Name
            (  Connection : ODBC_Connection
            )  return SQL_CN;
--
-- Get_Create_Assertion -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Bitmask enumerating the clauses in the CREATE ASSERTION statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Create_Assertion
            (  Connection : ODBC_Connection
            )  return SQL_CA;
--
-- Get_Create_Character_Set -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Bitmask  enumerating  the  clauses  in  the  CREATE CHARACTER SET
--     statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Create_Character_Set
            (  Connection : ODBC_Connection
            )  return SQL_CCS;
--
-- Get_Create_Collation -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Bitmask enumerating the clauses in the CREATE COLLATION statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Create_Collation
            (  Connection : ODBC_Connection
            )  return SQL_CCOL;
--
-- Get_Create_Domain -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Bitmask enumerating the clauses in the CREATE DOMAIN statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Create_Domain
            (  Connection : ODBC_Connection
            )  return SQL_CDO;
--
-- Get_Create_Schema -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Bitmask enumerating the clauses in the CREATE SCHEMA statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Create_Schema
            (  Connection : ODBC_Connection
            )  return SQL_CS;
--
-- Get_Create_Table -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Bitmask enumerating the clauses in the CREATE TABLE statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Create_Table
            (  Connection : ODBC_Connection
            )  return SQL_CT;
--
-- Get_Create_Translation -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Bitmask enumerating the clauses in the CREATE TABLE statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Create_Translation
            (  Connection : ODBC_Connection
            )  return SQL_CTR;
--
-- Get_Create_View -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--     Bitmask enumerating the clauses in the CREATE VIEW statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Create_View
            (  Connection : ODBC_Connection
            )  return SQL_CV;
--
-- Get_Current_Catalog -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The name of the catalog to be used by the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Current_Catalog
            (  Connection : ODBC_Connection
            )  return String;
   function Get_Current_Catalog
            (  Connection : ODBC_Connection
            )  return Wide_String;
--
-- Get_Cursor_Commit_Behavior -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    How a COMMIT operation affects cursors and prepared statements
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Cursor_Commit_Behavior
            (  Connection : ODBC_Connection
            )  return SQL_CCB;
--
-- Get_Cursor_Rollback_Behavior -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    How a ROLLBACK operation affects cursors and prepared statements
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Cursor_Rollback_Behavior
            (  Connection : ODBC_Connection
            )  return SQL_CCB;
--
-- Get_Cursor_Sensitivity -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Value that indicates the support for cursor sensitivity
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Cursor_Sensitivity
            (  Connection : ODBC_Connection
            )  return SQL_SENSITIVITY;
--
-- Get_Data_Source_Name -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The data source name that was used during connection
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Data_Source_Name
            (  Connection : ODBC_Connection
            )  return String;
   function Get_Data_Source_Name
            (  Connection : ODBC_Connection
            )  return Wide_String;
--
-- Get_Data_Source_Readonly -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the data source is set to READ ONLY mode
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Data_Source_Readonly
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Database_Name -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The name of the current database in use
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Database_Name
            (  Connection : ODBC_Connection
            )  return String;
   function Get_Database_Name
            (  Connection : ODBC_Connection
            )  return Wide_String;
--
-- Get_Datetime_Literals -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the SQL-92 datetime literals supported
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Datetime_Literals
            (  Connection : ODBC_Connection
            )  return SQL_DL;
--
-- Get_DBMS_Name -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The name of the DBMS product accessed by the driver
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_DBMS_Name
            (  Connection : ODBC_Connection
            )  return String;
   function Get_DBMS_Name
            (  Connection : ODBC_Connection
            )  return Wide_String;
--
-- Get_DBMS_Version -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The version of the DBMS product accessed by the driver
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_DBMS_Version
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_DDL_Index -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmap that indicates support for creation and dropping of indexes
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_DDL_Index
            (  Connection : ODBC_Connection
            )  return SQL_DI;
--
-- Get_Default_TXN_Isolation -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The default transaction isolation level supported by the driver or
--    data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Default_TXN_Isolation
            (  Connection : ODBC_Connection
            )  return SQL_TXN;
--
-- Get_Describe_Parameter -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if parameters can be described
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Describe_Parameter
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_DM_Version -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The driver manager version in the form ##.##.####.####
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_DM_Version
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Driver_Name -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The file name of the driver used to access the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Driver_Name
            (  Connection : ODBC_Connection
            )  return String;
   function Get_Driver_Name
            (  Connection : ODBC_Connection
            )  return Wide_String;
--
-- Get_Driver_ODBC_Ver -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The version of ODBC that the driver supports (##.##)
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Driver_ODBC_Ver
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Driver_Ver -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character string with  the version of the driver and optionally,
--    a description of the driver
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Driver_Ver
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Drop_Assertion -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses in the DROP ASSERTION statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Drop_Assertion
            (  Connection : ODBC_Connection
            )  return SQL_DA;
--
-- Get_Drop_Character_Set -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask bitmask  enumerating the clauses in the DROP CHARACTER SET
--    statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Drop_Character_Set
            (  Connection : ODBC_Connection
            )  return SQL_DCS;
--
-- Get_Drop_Collation -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses in the DROP COLLATION statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Drop_Collation
            (  Connection : ODBC_Connection
            )  return SQL_DC;
--
-- Get_Drop_Domain -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses in the DROP DOMAIN statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Drop_Domain
            (  Connection : ODBC_Connection
            )  return SQL_DD;
--
-- Get_Drop_Schema -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses in the DROP SCHEMA statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Drop_Schema
            (  Connection : ODBC_Connection
            )  return SQL_DS;
--
-- Get_Drop_Table -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses in the DROP SCHEMA statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Drop_Table
            (  Connection : ODBC_Connection
            )  return SQL_DT;
--
-- Get_Drop_Translation -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses in the DROP TRANSLATION statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Drop_Translation
            (  Connection : ODBC_Connection
            )  return SQL_DTR;
--
-- Get_Drop_View -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses in the DROP VIEW statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Drop_View
            (  Connection : ODBC_Connection
            )  return SQL_DV;
--
-- Get_Dynamic_Cursor_Attributes -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask that describes the attributes of a dynamic cursor
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Dynamic_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA1;
   function Get_Dynamic_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA2;
--
-- Get_Execution_Mode -- Change the execution mode
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Current mode
--
   function Get_Execution_Mode
            (  Connection : ODBC_Connection
            )  return Execution_Mode;
--
-- Get_Expressions_In_Orderby -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the data source supports expressions in the ORDER BY list
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Expressions_In_Orderby
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_File_Usage -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    How a single-tier driver directly treats files in a data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_File_Usage
            (  Connection : ODBC_Connection
            )  return SQL_FILE;
--
-- Get_Forward_Only_Cursor_Attributes -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask that describes the attributes of a forward-only cursor
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Forward_Only_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA1;
   function Get_Forward_Only_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA2;
--
-- Get_Functions -- Get ODBC function support by connection
--
--    Connection  - The ODBC connection object
--    Function_ID - The function ID to query
--
-- Returns :
--
--    True if driver supports the function
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Functions
            (  Connection  : ODBC_Connection;
               Function_ID : SQL_API
            )  return Boolean;
--
-- Get_Getdata_Extensions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating extensions to SQLGetData
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Getdata_Extensions
            (  Connection : ODBC_Connection
            )  return SQL_GD;
--
-- Get_Group_By -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The relationship  between the columns  in the GROUP BY clause  and
--    the nonaggregated columns in the select list
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Group_By
            (  Connection : ODBC_Connection
            )  return SQL_GR;
--
-- Get_Identifier_Case -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Identifier case
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Identifier_Case
            (  Connection : ODBC_Connection
            )  return SQL_IC;
--
-- Get_Identifier_Quote_Char -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The character  string that  is used  as  the  starting  and ending
--    delimiter of a quoted (delimited) identifier in SQL statements
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Identifier_Quote_Char
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Index_Keywords -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask that  enumerates  keywords in the  CREATE INDEX  statement
--    that are supported by the driver
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Index_Keywords
            (  Connection : ODBC_Connection
            )  return SQL_IK;
--
-- Get_Info_Schema_Views -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the views in the  INFORMATION_SCHEMA  that are
--    supported by the driver
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Info_Schema_Views
            (  Connection : ODBC_Connection
            )  return SQL_ISV;
--
-- Get_Insert_Statement -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask that indicates support for INSERT statements
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Insert_Statement
            (  Connection : ODBC_Connection
            )  return SQL_IS;
--
-- Get_Integrity -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True  if  the  data  source  supports  the  Integrity  Enhancement
--    Facility
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Integrity
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Keyset_Cursor_Attributes -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask that describes the attributes of a keyset cursor
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Keyset_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA1;
   function Get_Keyset_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA2;
--
-- Get_Keywords -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A comma-separated list of all data source-specific keywords
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Keywords
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Like_Escape_Clause -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Check if the data source supports an escape character for the percent
-- character (%) and  underscore  character (_) in a LIKE predicate  and
-- the driver supports  the ODBC  syntax  for defining  a LIKE predicate
-- escape character
--
-- Returns :
--
--    True if supported
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Like_Escape_Clause
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Login_Timeout -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Timeout resolution is 1 second
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Login_Timeout
            (  Connection : ODBC_Connection
            )  return Duration;
--
-- Get_Max_Async_Concurrent_Statements -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum number of active concurrent statements in asynchronous
--    mode
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Async_Concurrent_Statements
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Binary_Literal_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- This  function  returns  the  maximum  length  (number of hexadecimal
-- characters,  excluding  the  literal  prefix  and  suffix returned by
-- SQLGetTypeInfo) of a binary literal in an SQL statement. For example,
-- the binary literal 0xFFAA has a length of 4. If there is  no  maximum
-- length or the length is unknown, this value is set to zero
--
-- Returns :
--
--    The maximum length
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Binary_Literal_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Catalog_Name_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum length of a catalog name in the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Catalog_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Char_Literal_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum  length (number  of characters,  excluding the literal
--    prefix and suffix of a character literal in an SQL statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Char_Literal_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Column_Name_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum length of a column name in the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Column_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Columns_In_Group_By -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum number of columns allowed in a GROUP BY clause
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Columns_In_Group_By
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Columns_In_Index -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum number of columns allowed in an index
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Columns_In_Index
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Columns_In_Order_By -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum number of columns allowed in an ORDER BY clause
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Columns_In_Order_By
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Columns_In_Select -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum number of columns allowed in a select list
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Columns_In_Select
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Columns_In_Table -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum number of columns allowed in a table
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Columns_In_Table
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Concurrent_Activities -- Get connection information
--
--    Connection - The ODBC connection object
--
-- This  function  returns  the maximum number of active statements that
-- the  driver  can  support for a connection. A statement is defined as
-- active  if  it  has  results pending, with the term "results" meaning
-- rows from a SELECT operation or rows affected by an  INSERT,  UPDATE,
-- or DELETE operation (such as a row count), or if it is in a NEED_DATA
-- state. This value can reflect a  limitation  imposed  by  either  the
-- driver or the data source. If there is  no  specified  limit  or  the
-- limit is unknown, this value is set to zero.
--
-- Returns :
--
--    The maximum number of active statements
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Concurrent_Activities
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Cursor_Name_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum length of a cursor name in the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Cursor_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Driver_Connections -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum number of active connections supported
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Driver_Connections
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Identifier_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum size in characters for user-defined names
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Identifier_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Index_Size -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum number of bytes in the combined fields of an index
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Index_Size
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Procedure_Name_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum length of a procedure name in the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Procedure_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Row_Size -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum length of a single row in a table
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Row_Size
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Row_Size_Includes_Long -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if  the maximum row  size returned  for the  Get_Max_Row_Size
--    information  type includes the  length of  all SQL_LONGVARCHAR and
--    SQL_LONGVARBINARY columns in the row
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Row_Size_Includes_Long
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Max_Schema_Name_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum length of a schema name in the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Schema_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Statement_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum  length (number of characters,  including white space)
--    of an SQL statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Statement_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Table_Name_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum length of a table name in the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Table_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_Tables_In_Select -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum  number of  tables a llowed  in  the  FROM clause of a
--    SELECT statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_Tables_In_Select
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Max_User_Name_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The maximum length of a user name in the data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Max_User_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural;
--
-- Get_Message_Text -- Get diag record field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    An informational message on the error or warning
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Message_Text
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String;
   function Get_Message_Text
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return Wide_String;
--
-- Get_Metadata_ID -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the  string argument  of catalog functions  are treated as
--    identifiers.
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Metadata_ID
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Multiple_Result_Sets -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the driver supports multiple result sets
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Multiple_Result_Sets
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Multiple_Active_TXN -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the driver  supports more  than one active transaction  at
--    the same time
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Multiple_Active_TXN
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Native -- Get diag record field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    A driver/data  source-specific  native error code.  If there is no
--    native error code, the driver returns 0
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Native
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_Need_Long_Data_Len -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the data source needs the length of a long data value
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Need_Long_Data_Len
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Non_Nullable_Columns -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Whether the data source supports NOT NULL in columns
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Non_Nullable_Columns
            (  Connection : ODBC_Connection
            )  return SQL_NNC;
--
-- Get_Null_Collation -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A value that specifies where NULLs are sorted in a result set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Null_Collation
            (  Connection : ODBC_Connection
            )  return SQL_NC;
--
-- Get_Number -- Diag field
--
--    Connection    - The ODBC connection object
--    Record_Number - Diagnostic record number
--
-- Returns :
--
--    The number of status records available
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Number
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_Numeric_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating  the scalar numeric functions supported by the
--    driver and associated data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Numeric_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_NUM;
--
-- Get_ODBC_Interface_Conformance -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A value that indicates  the level of the ODBC 3.x  interface  that
--    the driver complies with
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_ODBC_Interface_Conformance
            (  Connection : ODBC_Connection
            )  return SQL_OIC;
--
-- Get_ODBC_Ver -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character  string with  the version of ODBC to  which the Driver
--    Manager conforms
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_ODBC_Ver
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_OJ_Capabilities -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the types of outer joins supported
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_OJ_Capabilities
            (  Connection : ODBC_Connection
            )  return SQL_OJ;
--
-- Get_Order_By_Columns_In_Select -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if the columns in  the ORDER BY  clause must be in the select
--    list
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Order_By_Columns_In_Select
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Packet_Size -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The network packet size in bytes
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Packet_Size
            (  Connection : ODBC_Connection
            )  return Positive;
--
-- Get_Param_Array_Row_Count -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A  value  enumerating  the  driver's   properties   regarding  the
--    availability of row counts in a parameterized execution
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Param_Array_Row_Count
            (  Connection : ODBC_Connection
            )  return SQL_PARC;
--
-- Get_Param_Array_Selects -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A   value   enumerating  the  driver's  properties  regarding  the
--    availability of result sets in a parameterized execution
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Param_Array_Selects
            (  Connection : ODBC_Connection
            )  return SQL_PAS;
--
-- Get_Procedure_Term -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character  string  with  the  data  source  vendor's  name for a
--    procedure
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Procedure_Term
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Procedures -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True  if the  data  source  supports  procedures  and  the  driver
--    supports the ODBC procedure invocation syntax
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Procedures
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Pos_Operations -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the support operations in SQLSetPos
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Pos_Operations
            (  Connection : ODBC_Connection
            )  return SQL_POS;
--
-- Get_Quoted_Identifier_Case -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The case used for quoted identifiers
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Quoted_Identifier_Case
            (  Connection : ODBC_Connection
            )  return SQL_IC;
--
-- Get_Returncode -- Diag field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    Return code returned by the function
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Returncode
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLRETURN;
--
-- Get_Row_Number -- Get diag record field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    The row number in the rowset,  or the parameter  number in the set
--    of parameters, with which the status record is associated
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Row_Number
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLLEN;
--
-- Get_Row_Updates -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if a keyset-driven  or mixed cursor maintains row versions or
--    values for all fetched rows
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Row_Updates
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Schema_Term -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character string with the data source vendor's name for a schema
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Schema_Term
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Schema_Usage -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the statements in which schemas can be used
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Schema_Usage
            (  Connection : ODBC_Connection
            )  return SQL_SU;
--
-- Get_Scroll_Options -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating scroll options
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Scroll_Options
            (  Connection : ODBC_Connection
            )  return SQL_SO;
--
-- Get_Search_Pattern_Escape -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character string specifying  what  the  driver  supports  as  an
--    escape  character  that  allows  the  use  of  the  pattern  match
--    metacharacters underscore  (_)  and  percent  sign  (%)  as  valid
--    characters in search patterns
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Search_Pattern_Escape
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Server_Name -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character  string  with the  actual  data source-specific server
--    name
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Server_Name
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Server_Name -- Get diag record field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    A string that indicates the server name that the diagnostic record
--    relates to
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Server_Name
            (  Connection    : ODBC_Connection;
               Record_Number : Positive
            )  return String;
   function Get_Server_Name
            (  Connection    : ODBC_Connection;
               Record_Number : Positive
            )  return Wide_String;
--
-- Get_Special_Characters -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character string that contains all special characters
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Special_Characters
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_SQLSTATE -- Get diag record field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    A five-character SQLSTATE diagnostic code. For more information
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_SQLSTATE
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_SQL_Conformance -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A value that indicates the level of SQL-92 supported by the driver
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL_Conformance
            (  Connection : ODBC_Connection
            )  return SQL_SC;
--
-- Get_SQL92_Datetime_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A value that indicates the level of SQL-92 supported by the driver
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Datetime_Functions
            (  Connection : ODBC_Connection
            )  return SQL_SDF;
--
-- Get_SQL92_Foreign_Key_Delete_Rule -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask  enumerating the rules  supported  for a foreign  key in a
--    DELETE statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Foreign_Key_Delete_Rule
            (  Connection : ODBC_Connection
            )  return SQL_SFKD;
--
-- Get_SQL92_Foreign_Key_Update_Rule -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask  enumerating  the rules supported  for a foreign key in an
--    UPDATE statement, as defined in SQL-92
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Foreign_Key_Update_Rule
            (  Connection : ODBC_Connection
            )  return SQL_SFKU;
--
-- Get_SQL92_Grant -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses supported in the GRANT statement
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Grant
            (  Connection : ODBC_Connection
            )  return SQL_SG;
--
-- Get_SQL92_Numeric_Value_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating  the numeric value  scalar functions  that are
--    supported by the driver and the associated data source, as defined
--    in SQL-92
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Numeric_Value_Functions
            (  Connection : ODBC_Connection
            )  return SQL_SNVF;
--
-- Get_SQL92_Predicates -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating  the numeric value  scalar functions  that are
--    supported by the driver and the associated data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Predicates
            (  Connection : ODBC_Connection
            )  return SQL_SP;
--
-- Get_SQL92_Relational_Join_Operations -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating  the relational  join operators supported in a
--    SELECT statement, as defined in SQL-92
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Relational_Join_Operations
            (  Connection : ODBC_Connection
            )  return SQL_SRJO;
--
-- Get_SQL92_Revoke -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the clauses supported in the REVOKE statement,
--    as defined in SQL-92
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Revoke
            (  Connection : ODBC_Connection
            )  return SQL_SR;
--
-- Get_SQL92_Row_Value_Constructor -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask  enumerating   the  row   value  constructor   expressions
--    supported in a SELECT statement, as defined in SQL-92
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Row_Value_Constructor
            (  Connection : ODBC_Connection
            )  return SQL_SRVC;
--
-- Get_SQL92_String_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the string scalar functions that are supported
--    by the driver and the associated data source, as defined in SQL-92
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_String_Functions
            (  Connection : ODBC_Connection
            )  return SQL_SSF;
--
-- Get_SQL92_Value_Expressions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the value expressions supported, as defined in
--    SQL-92
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_SQL92_Value_Expressions
            (  Connection : ODBC_Connection
            )  return SQL_SVE;
--
-- Get_Standard_CLI_Conformance -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask  enumerating the  CLI standard  or standards  to which the
--    driver conforms
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Standard_CLI_Conformance
            (  Connection : ODBC_Connection
            )  return SQL_SCC;
--
-- Get_Static_Cursor_Attributes -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask that describes the attributes of a static cursor supported
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Static_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA1;
   function Get_Static_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA2;
--
-- Get_Subqueries -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the predicates that support subqueries
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Subqueries
            (  Connection : ODBC_Connection
            )  return SQL_SQ;
--
-- Get_String_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating  the scalar string  functions supported by the
--    driver and associated data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_String_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_STR;
--
-- Get_Subclass_Origin -- Get diag record field
--
--    Connection    - The ODBC connection object
--    Record_Number - The record number
--
-- Returns :
--
--    Identifies  the defining  portion  of the  subclass portion of the
--    SQLSTATE code
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Subclass_Origin
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_System_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask  enumerating  the scalar system functions supported by the
--    driver and associated data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_System_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_SYS;
--
-- Get_Table_Term -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character string with the data source vendor's name for a table
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Table_Term
            (  Connection : ODBC_Connection
            )  return String;
--
-- Get_Timedate_Add_Intervals -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask  enumerating  the timestamp  intervals  supported  by  the
--    driver and  associated data  source for  the  TIMESTAMPADD  scalar
--    function
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Timedate_Add_Intervals
            (  Connection : ODBC_Connection
            )  return SQL_FN_TSI;
--
-- Get_Timedate_Diff_Intervals -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask  enumerating  the timestamp  intervals  supported  by  the
--    driver and  associated data  source for  the  TIMESTAMPDIFF scalar
--    function
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Timedate_Diff_Intervals
            (  Connection : ODBC_Connection
            )  return SQL_FN_TSI;
--
-- Get_Timedate_Functions -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating  the scalar  date and time functions supported
--    by the driver and associated data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Timedate_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_TSI;
--
-- Get_Trace -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    True if tracing is on
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Trace
            (  Connection : ODBC_Connection
            )  return Boolean;
--
-- Get_Tracefile -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Trace file or empty string
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Tracefile
            (  Connection : ODBC_Connection
            )  return String;
   function Get_Tracefile
            (  Connection : ODBC_Connection
            )  return Wide_String;
--
-- Get_Translate_Lib -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- This function returns the name of  a library containing the functions
-- SQLDriverToDataSource   and  SQLDataSourceToDriver  that  the  driver
-- accesses to perform tasks such as character set translation.
--
-- Returns :
--
--    The library name
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Translate_Lib
            (  Connection : ODBC_Connection
            )  return String;
   function Get_Translate_Lib
            (  Connection : ODBC_Connection
            )  return Wide_String;
--
-- Get_Translate_Option -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A 32-bit flag value that is passed to the translation DLL
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Translate_Option
            (  Connection : ODBC_Connection
            )  return SQLUINTEGER;
--
-- Get_TXN_Capable -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Describes the transaction support in the driver or data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_TXN_Capable
            (  Connection : ODBC_Connection
            )  return SQL_TC;
--
-- Get_TXN_Isolation -- Get connection attribute
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The bitmask that sets the transaction isolation level
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_TXN_Isolation
            (  Connection : ODBC_Connection
            )  return SQL_TXN;
--
-- Get_TXN_Isolation_Option -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating  the transaction  isolation  levels  available
--    from the driver or data source
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_TXN_Isolation_Option
            (  Connection : ODBC_Connection
            )  return SQL_TXN;
--
-- Get_Union -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    Bitmask enumerating the support for the UNION clause
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_Union
            (  Connection : ODBC_Connection
            )  return SQL_U;
--
-- Get_User_Name -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    The name used in a particular database
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_User_Name
            (  Connection : ODBC_Connection
            )  return String;
   function Get_User_Name
            (  Connection : ODBC_Connection
            )  return Wide_String;
--
-- Get_XOpen_CLI_Year -- Get connection information
--
--    Connection - The ODBC connection object
--
-- Returns :
--
--    A character string that indicates the year  of publication  of the
--    Open Group specification with which the version of the ODBC Driver
--    Manager fully complies
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   function Get_XOpen_CLI_Year
            (  Connection : ODBC_Connection
            )  return String;
   function Get_XOpen_CLI_Year
            (  Connection : ODBC_Connection
            )  return Wide_String;
--
-- Initialize -- Constructor
--
--    Connection - The data base connection object
--
-- The  derived  type  must  call  this   from  its   implementation  of
-- Initialize, when overridden.
--
   procedure Initialize (Connection : in out ODBC_Connection);
--
-- Release -- Unlock the data base
--
--    Connection - The data base connection object
--
-- The opposite to Seize.
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Release (Connection : in out ODBC_Connection);
--
-- RollBack -- Discard a transaction
--
--    Connection - The data base connection object
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
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
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Seize (Connection : in out ODBC_Connection);
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
-- Set_Access_Mode -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Access_Mode
             (  Connection : in out ODBC_Connection;
                Value      : SQL_MODE
             );
--
-- Set_Async_DBC_Enable -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Async_DBC_Enable
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             );
--
-- Set_Async_Enable -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Async_Enable
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             );
--
-- Set_Autocommit -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Set_Autocommit
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             );
--
-- Set_Connection_Timeout -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Connection_Timeout
             (  Connection : in out ODBC_Connection;
                Value      : Duration
             );
--
-- Set_Current_Catalog -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Current_Catalog
             (  Connection : in out ODBC_Connection;
                Value      : String
             );
   procedure Set_Current_Catalog
             (  Connection : in out ODBC_Connection;
                Value      : Wide_String
             );
--
-- Set_Execution_Mode -- Change the execution mode
--
--    Connection - The ODBC connection object
--    Mode       - The new access mode
--
   procedure Set_Execution_Mode
             (  Connection : in out ODBC_Connection;
                Mode       : Execution_Mode
             );
--
-- Set_Metadata_ID -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Set_Metadata_ID
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             );
--
-- Set_Login_Timeout -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Login_Timeout
             (  Connection : in out ODBC_Connection;
                Value      : Duration
             );
--
-- Set_Packet_Size -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Set_Packet_Size
             (  Connection : in out ODBC_Connection;
                Value      : Positive
             );
--
-- Set_Trace -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--
   procedure Set_Trace
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             );
--
-- Set_Tracefile -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Tracefile
             (  Connection : in out ODBC_Connection;
                Value      : String
             );
   procedure Set_Tracefile
             (  Connection : in out ODBC_Connection;
                Value      : Wide_String
             );
--
-- Set_Translate_Lib -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Translate_Lib
             (  Connection : in out ODBC_Connection;
                Value      : String
             );
   procedure Set_Translate_Lib
             (  Connection : in out ODBC_Connection;
                Value      : Wide_String
             );
--
-- Set_Translate_Option -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Translate_Option
             (  Connection : in out ODBC_Connection;
                Value      : SQLUINTEGER
             );
--
-- Set_TXN_Isolation -- Set connection attribute
--
--    Connection - The ODBC connection object
--    Value      - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_TXN_Isolation
             (  Connection : in out ODBC_Connection;
                Value      : SQL_TXN
             );
--
-- Table_Exists -- The command
--
--    Connection - To be used
--    Table_Name - The table name
--
-- Returns :
--
--    True if the table exists
--
   function Table_Exists
            (  Connection : access ODBC_Connection;
               Table_Name : String
            )  return Boolean;
   function Table_Exists
            (  Connection : access ODBC_Connection;
               Table_Name : Wide_String
            )  return Boolean;
------------------------------------------------------------------------
-- ODBC_Command -- The base type of an ODBC command
--
--    Connection - To use
--
   type ODBC_Command
        (  Connection : access ODBC_Connection'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
--
-- Bind_Null -- Bind NULL value of the parameter of a command
--
--    Command - To be prepared
--    Index   - The parameter number 1..
--
-- The  parameters  to  be  bound are specified as ? in the command text
-- (see Prepare). Each such parameter has to be bound to a variable. The
-- position  of  a  parameter  is  specified  by  its index, i.e. by the
-- position  of  ?  in  the  command  text.  The first parameter has the
-- position 1.
--
   procedure Bind_Null
             (  Command : in out ODBC_Command;
                Index   : Positive
             );
--
-- Bind_Parameter -- Of a command
--
--    Command     - To be prepared
--    Index       - The parameter number 1..
--    Parameter   - To be bound
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
                Parameter : access String_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_LONGVARCHAR
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access Wide_String_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_WLONGVARCHAR
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLBIGINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_BIGINT
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLUBIGINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_UBIGINT
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLINTEGER_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_INTEGER
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLUINTEGER_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_ULONG
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLSMALLINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_SMALLINT
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLUSMALLINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_USHORT
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLTINYINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_TINYINT
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLUTINYINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_UTINYINT
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLDOUBLE_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_DOUBLE
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLGUID_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_GUID
             );
   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQL_TIMESTAMP_STRUCT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_TIMESTAMP
             );
--
-- Bind_Result -- Of a command
--
--    Command   - To be prepared
--    Column    - The column number 1..
--    Parameter - To be bound
--    Data_Type - The data type description (defaulted)
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
                Parameter : access SQLBIGINT
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLUBIGINT
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLINTEGER
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLUINTEGER
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLSMALLINT
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLUSMALLINT
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLTINYINT
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLUTINYINT
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLDOUBLE
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLGUID
             );
   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQL_TIMESTAMP_STRUCT
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
-- Describe_Col -- Get attribute
--
--    Command - The command
--    Column  - The column number 1..
--
-- This  function  is called with  a command previously prepared with an
-- SQL statement.  The result  describes  the specified  column  of  the
-- command's result set.  The number  of columns  can  be obtained using
-- Num_Result_Cols.
--
-- Returns :
--
--    Column description
--
-- Exceptions :
--
--    Constraint_Error - Wrong column number
--    Data_Error       - A data base error
--    Use_Error        - Not supported
--
   function Describe_Col
            (  Command : ODBC_Command;
               Column  : Positive
            )  return Column_Description;
--
-- Describe_Param -- Delete a table
--
--    Command   - With a statement prepared in it
--    Parameter - The parameter number
--
-- Returns :
--
--    Description of a the parameter
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Driver does not support parameter description
--
   function Describe_Param
            (  Command   : ODBC_Command;
               Parameter : Positive
            )  return Param_Description;
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
--    Data_Error - A data base error
--    Use_Error  - Wrong access mode
--
-- Effects :
--
--    Command is modified
--
   procedure Drop
             (  Command    : in out ODBC_Command;
                Table_Name : String
             );
   procedure Drop
             (  Command    : in out ODBC_Command;
                Table_Name : Wide_String
             );
--
-- Execute -- The command
--
--    Command - To be executed
--  [ Text  ] - The SQL command
--
-- These  procedures  execute  a command.  When  the parameter  Text  is
-- omitted  the command  to be executed is  one  prepared  before  using
-- Prepare. Otherwise it is prepared from Text.
--
-- Exceptions :
--
--    Data_Error   - Data base error
--    End_Error    - Data needed
--    Status_Error - No table exists
--    Use_Error    - Wrong access mode
--
   procedure Execute
             (  Command : in out ODBC_Command
             );
   procedure Execute
             (  Command : in out ODBC_Command;
                Text    : String
             );
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
-- These  functions are equivalent  to Execute with Get_Row_Count called
-- after it. It can be used to execute  the  commands  such  as  DELETE,
-- INSERT and UPDATE.
--
-- Returns :
--
--    The number of affected rows, which can be 0
--
-- Exceptions :
--
--    Data_Error   - Data base error
--    End_Error    - Data needed
--    Status_Error - No table exists
--    Use_Error    - Wrong access mode
--
   function Execute
            (  Command : access ODBC_Command
            )  return Natural;
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
--    Command - Of which result has to be fetched
--
-- Exceptions :
--
--    Data_Error - Data base error
--    End_Error  - No more rows
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
-- Exceptions :
--
--    Data_Error - Data base error
--    End_Error  - No more rows
--
   function Fetch (Command : access ODBC_Command)
      return SQLRETURN;
--
-- Finalize -- Destructor
--
--    Command - To be finalized
--
-- The derived  type must call this if  overridden.  The ODBC  statement
-- handle is freed.
--
   procedure Finalize (Command : in out ODBC_Command);
--
-- Get_Async_Enable -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    True if function called with  the specified  statement is executed
--    asynchronously
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Async_Enable
            (  Command : ODBC_Command
            )  return Boolean;
--
-- Get_Class_Origin -- Get diag record field
--
--    Command       - The command
--    Record_Number - The record number
--
-- Returns :
--
--    A string  that  indicates the  document  that  defines  the  class
--    portion of the SQLSTATE value in this record
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Class_Origin
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_Column_Number -- Get diag record field
--
--    Command       - The command
--    Record_Number - The record number
--
-- Returns :
--
--    The value that  represents the column number  in the result set or
--    the parameter number in the set of parameters
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Column_Number
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_Concurrency -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    A bitmask that specifies the cursor concurrency support
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Not supported
--
   function Get_Concurrency
            (  Command : ODBC_Command
            )  return SQL_CONCUR;
--
-- Get_Cursor_Row_Count -- Diag field
--
--    Command       - The command
--    Record_Number - Diagnostic record number
--
-- The contents of this field are defined only for statement handles and
-- only after Execute has been called
--
-- Returns :
--
--    The count of rows in the cursor
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Cursor_Row_Count
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLLEN;
--
-- Get_Connection_Name -- Get diag record field
--
--    Command       - The command
--    Record_Number - The record number
--
-- Returns :
--
--    The name of the connection that the diagnostic record relates to
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   function Get_Connection_Name
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_Cursor_Scrollable -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    True if cursor is scrollable
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Cursor_Scrollable
            (  Command : ODBC_Command
            )  return Boolean;
--
-- Get_Cursor_Sensitivity -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    Whether cursors on the statement handle  make  visible the changes
--    made to a result set by another cursor
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Not supported
--
   function Get_Cursor_Sensitivity
            (  Command : ODBC_Command
            )  return SQL_SENSITIVITY;
--
-- Get_Cursor_Type -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    A value that specifies the cursor type
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Not supported
--
   function Get_Cursor_Type
            (  Command : ODBC_Command
            )  return SQL_CURSOR;
--
-- Get_Data -- Of a command
--
--    Command       - The command which result is requested
--    Column        - The result column number
--    Finish        - Close cursor before return
--  [ Null_As_Empty - Return NULL string values as empty strings
--    Block_Size ]  - Number of characters to read as one block
--
-- The data of the specified column are read  from  the  result  of  the
-- command  execution.  Note  that  some  ODBC  drivers  might   require
-- accessing the result columns  in  their  number  order.  When  cursor
-- remains unclosed the command processing should  be  either  continued
-- using new Fetch or  finished  calling  Close_Cursor  explicitly.  The
-- parameter Finish determines when Get_Data closes the cursor. On_Error
-- and On_No_Result differs in what happens on NULL value. On_Error does
-- not close the cursor on NULL.  Note  that  some  data  bases  do  not
-- support NULL values for some data  types,  these  could  for  example
-- return an empty string, and so the result  of  Get_Data  will  be  an
-- empty string instead of End_Error.  The parameter  Null_As_Empty when
-- True forces  NULL  string  values  returned  as  empty  strings.  The
-- parameter  Block_Size  specifies  the  size  in which string data are
-- fetched.  When the column value exceeds the size the value is fetched
-- in several in several requests and then concatenated.
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
            (  Command       : access ODBC_Command;
               Column        : Positive;
               Finish        : Cursor_Disposition;
               Null_As_Empty : Boolean  := True;
               Block_Size    : Positive := Default_Block_Size
            )  return String;
   function Get_Data
            (  Command       : access ODBC_Command;
               Column        : Positive;
               Finish        : Cursor_Disposition;
               Null_As_Empty : Boolean  := True;
               Block_Size    : Positive := Default_Block_Size
            )  return Wide_String;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return Time;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLBIGINT;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLUBIGINT;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLINTEGER;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLUINTEGER;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLSMALLINT;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLUSMALLINT;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLTINYINT;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLUTINYINT;
   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLDOUBLE;
--
-- Get_Data -- Of a command
--
--    Command       - The command which result is requested
--    Stream        - To write data into
--    Column        - The result column number
--    Finish        - Close cursor before return
--  [ Null_As_Empty - Return NULL string values as empty strings
--    Block_Size ]  - Number of characters to read as one block
--
-- This procedure  is similar to the Get_Data functions,  but stores the
-- column data (string) into a stream.
--
-- Exceptions :
--
--    Data_Error - A data base error
--    End_Error  - No result, the value requested is NULL (unset)
--    Stream errors if any
--
   procedure Get_Data
             (  Command       : in out ODBC_Command;
                Stream        : in out Root_Stream_Type'Class;
                Column        : Positive;
                Finish        : Cursor_Disposition;
                Null_As_Empty : Boolean  := True;
                Block_Size    : Stream_Element_Count :=
                                   Default_Block_Size
             );
--
-- Get_Data -- Of a command
--
--    Command       - The command which result is requested
--    Stream        - To write data into
--    Column        - The result column number
--    Finish        - Close cursor before return
--    Null_As_Empty - Return NULL string values as empty strings
--
-- This  procedure stores the column data (string)  into the Destination
-- string at the position specified by Pointer. Pointer is then advanced
-- to the position after the string.
--
-- Exceptions :
--
--    Data_Error   - A data base error
--    End_Error    - No result, the value requested is NULL (unset)
--    Layout_Error - No  room   for  data  or  else   Pointer   not   in
--                   Destination'First..Destination'Last + 1
--
   procedure Get_Data
             (  Command       : in out ODBC_Command;
                Destination   : in out String;
                Pointer       : in out Integer;
                Column        : Positive;
                Finish        : Cursor_Disposition;
                Null_As_Empty : Boolean := True
             );
--
-- Get_Dynamic_Function -- Diag field
--
--    Command       - The command
--    Record_Number - Diagnostic record number
--
-- The contents of this field are defined only for statement handles and
-- only after Execute has been called
--
-- Returns :
--
--    A string  that describes  the SQL  statement  that the  underlying
--    function executed
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Dynamic_Function
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String;
   function Get_Dynamic_Function
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return Wide_String;
--
-- Get_Dynamic_Function_Code -- Diag field
--
--    Command       - The command
--    Record_Number - Diagnostic record number
--
-- Returns :
--
--    A numeric code that describes the SQL statement that was executed
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Dynamic_Function_Code
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_Enable_Auto_IPD -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    True if automatic population of the IPD is performed
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - The feature is not supported
--
   function Get_Enable_Auto_IPD
            (  Command : ODBC_Command
            )  return Boolean;
--
-- Get_Keyset_Size -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    The number of rows in the keyset for a keyset-driven cursor
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - The feature is not supported
--
   function Get_Keyset_Size
            (  Command : ODBC_Command
            )  return SQLULEN;
--
-- Get_Max_Length -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    The  maximum  amount  of data  that  the  driver  returns  from  a
--    character or binary column
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - The feature is not supported
--
   function Get_Max_Length
            (  Command : ODBC_Command
            )  return SQLULEN;
--
-- Get_Max_Rows -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    The maximum number of  rows to  return to  the  application  for a
--    SELECT statement. 0 indicates all rows
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - The feature is not supported
--
   function Get_Max_Rows
            (  Command : ODBC_Command
            )  return SQLULEN;
--
-- Get_Metadata_ID -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    True if the string  argument  of catalog functions  are treated as
--    identifiers
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Not supported
--
   function Get_Metadata_ID
            (  Command : ODBC_Command
            )  return Boolean;
--
-- Get_Message_Text -- Get diag record field
--
--    Command       - The command
--    Record_Number - The record number
--
-- Returns :
--
--    An informational message on the error or warning
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - Not supported
--
   function Get_Message_Text
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String;
   function Get_Message_Text
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return Wide_String;
--
-- Get_Native -- Get diag record field
--
--    Command       - The command
--    Record_Number - The record number
--
-- Returns :
--
--    An informational message on the error or warning
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - Not supported
--
   function Get_Native
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_Noscan -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    True if the string  argument  of catalog functions  are treated as
--    identifiers
--
-- Exceptions :
--
--    Data_Error - A data base error
--
    function Get_Noscan
             (  Command : ODBC_Command
             )  return Boolean;
--
-- Get_Number -- Diag field
--
--    Command       - The command which result will be the table list
--    Record_Number - Diagnostic record number
--
-- Returns :
--
--    The number of status records available
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Number
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLINTEGER;
--
-- Get_Query_Timeout -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    Time to wait before returning to the application
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Not supported
--
    function Get_Query_Timeout
             (  Command : ODBC_Command
             )  return Duration;
--
-- Get_Retrieve_Data -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    True if fetching  retrieves  data after it positions the cursor to
--    the specified location
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Not supported
--
    function Get_Retrieve_Data
             (  Command : ODBC_Command
             )  return Boolean;
--
-- Get_Returncode -- Diag field
--
--    Command       - The command
--    Record_Number - Diagnostic record number
--
-- Returns :
--
--    Return code returned by the function
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Returncode
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLRETURN;
--
-- Get_Row_Count -- Diag field
--
--    Command       - The command
--    Record_Number - Diagnostic record number
--
-- The contents of this field are defined  only after  Execute  has been
-- called
--
-- Returns :
--
--    The count of rows in the cursor
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Get_Row_Count
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLLEN;
--
-- Get_Row_Array_Size -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    The number of rows returned by each fetching
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Not supported
--
    function Get_Row_Array_Size
             (  Command : ODBC_Command
             )  return SQLULEN;
--
-- Get_Row_Number -- Get attribute
--
--    Command       - The command
--    Record_Number - Diagnostic record number
--
-- Returns :
--
--    The number of the current row in the entire result set
--
-- Exceptions :
--
--    Data_Error - A data base error, e.g. no result set available
--    Use_Error  - Not supported
--
    function Get_Row_Number
             (  Command : ODBC_Command
             )  return SQLULEN;
--
-- Get_Server_Name -- Get diag record field
--
--    Command       - The command
--    Record_Number - The record number
--
-- Returns :
--
--    A string  that indicates  the  server  name  that  the  diagnostic
--    record relates to
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - Not supported
--
   function Get_Server_Name
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String;
   function Get_Server_Name
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return Wide_String;
--
-- Get_SQLSTATE -- Get diag record field
--
--    Command       - The command
--    Record_Number - The record number
--
-- Returns :
--
--    A five-character SQLSTATE diagnostic code. For more information
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - Not supported
--
   function Get_SQLSTATE
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_Subclass_Origin -- Get diag record field
--
--    Command       - The command
--    Record_Number - The record number
--
-- Returns :
--
--    Identifies  the defining  portion  of  the subclass portion of the
--    SQLSTATE code
--
-- Exceptions :
--
--    End_Error  - No record
--    Data_Error - ODBC driver error
--    Use_Error  - Not supported
--
   function Get_Subclass_Origin
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String;
--
-- Get_Tables -- Enumerates tables
--
--    Command    - The command
--    Table_Type - The table type to list
--
-- Fetch  and  Get_Data  can  be  used to retrieve the table names after
-- successful completion of this procedure.
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   procedure Get_Tables
             (  Command    : in out ODBC_Command;
                Table_Type : String := "TABLE"
             );
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
-- Initialize -- Constructor
--
--    Command - To be initialized
--
-- A statement handle is allocated as a result of execution
--
   procedure Initialize (Command : in out ODBC_Command);
--
-- Num_Params -- Of a prepared statement
--
--    Command - A prepared command
--
-- Returns :
--
--    Number of parameters of a prepared statement
--
   function Num_Params (Command : ODBC_Command) return Natural;
--
-- Num_Result_Cols -- Of a prepared statement
--
--    Command - The command
--    Column  - The column number 1..
--
-- This  function  is called with  a command previously prepared with an
-- SQL statement to get the number of columns in the result set.
--
-- Returns :
--
--    Columns number
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Not supported
--
   function Num_Result_Cols (Command : ODBC_Command) return Natural;
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
--
-- Row_Count -- Get attribute
--
--    Command - The command
--
-- Returns :
--
--    The number of affected rows
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Row_Count
            (  Command : ODBC_Command
            )  return Natural;
--
-- Set_Async_Enable -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Async_Enable
             (  Command : in out ODBC_Command;
                Value   : Boolean
             );
--
-- Set_Concurrency -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Concurrency
             (  Command : in out ODBC_Command;
                Value   : SQL_CONCUR
             );
--
-- Set_Cursor_Scrollable -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Cursor_Scrollable
             (  Command : in out ODBC_Command;
                Value   : Boolean
             );
--
-- Set_Cursor_Sensitivity -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Cursor_Sensitivity
             (  Command : in out ODBC_Command;
                Value   : SQL_SENSITIVITY
             );
--
-- Set_Cursor_Type -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Cursor_Type
             (  Command : in out ODBC_Command;
                Value   : SQL_CURSOR
             );
--
-- Set_Enable_Auto_IPD -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Enable_Auto_IPD
             (  Command : in out ODBC_Command;
                Value   : Boolean
             );
--
-- Set_Keyset_Size -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Keyset_Size
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             );
--
-- Set_Max_Length -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Max_Length
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             );
--
-- Set_Max_Rows -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Max_Rows
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             );
--
-- Set_Metadata_ID -- Set statement attribute
--
--    Command - The ODBC command
--     Value  - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Metadata_ID
             (  Command : in out ODBC_Command;
                Value   : Boolean
             );
--
-- Set_Noscan -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Noscan
             (  Command : in out ODBC_Command;
                Value   : Boolean
             );
--
-- Set_Pos -- Set cursor in the result row set
--
--    Command   - The ODBC command
--    Row       - The row
--    Operation - To perform
--    Locking   - Row locking
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - Operation is not supported
--
   procedure Set_Pos
             (  Command   : in out ODBC_Command;
                Row       : Positive;
                Operation : SQL_OPERATION;
                Locking   : SQL_LOCKTYPE
             );
--
-- Set_Query_Timeout -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Query_Timeout
             (  Command : in out ODBC_Command;
                Value   : Duration
             );
--
-- Set_Retrieve_Data -- Set statement attribute
--
--    Command - The ODBC command
--    Value  - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Retrieve_Data
             (  Command : in out ODBC_Command;
                Value   : Boolean
             );
--
-- Set_Row_Array_Size -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error
--    Use_Error  - The attribute is not supported
--
   procedure Set_Row_Array_Size
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             );
--
-- Set_Row_Number -- Set statement attribute
--
--    Command - The ODBC command
--    Value   - The attribute value to set
--
-- Exceptions :
--
--    Data_Error - ODBC driver error, e.g. no result set
--    Use_Error  - The attribute is not supported
--
   procedure Set_Row_Number
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             );
--
-- Table_Exists -- The command
--
--    Command    - To be used
--    Table_Name - The table name
--
-- Returns :
--
--    True if the table exists
--
   function Table_Exists
            (  Command    : access ODBC_Command;
               Table_Name : String
            )  return Boolean;
   function Table_Exists
            (  Command    : access ODBC_Command;
               Table_Name : Wide_String
            )  return Boolean;

private
   pragma Inline (Bind_Parameter);
   pragma Inline (Bind_Result);
   pragma Inline (Close_Cursor);
   pragma Inline (Execute);
   pragma Inline (Execute);
   pragma Inline (Fetch);
   pragma Inline (Prepare);
--
-- Filter_Ptr -- Exception state filter
--
--    State - To filter
--
-- This function is called when an operation fails.  The execution state
-- is the parameter State. When the function return True,  Data_Error is
-- propagated.  When it returns False, the exception is suppressed.  The
-- function may also  raise an exception which will propagate out of the
-- operation.
--
-- Returns :
--
--    True if the state should raise exception
--
   type Filter_Ptr is access function (State : SQLSTATE) return Boolean;
   function No_Filter (State : SQLSTATE) return Boolean;
   function Filter_All (State : SQLSTATE) return Boolean;
   function Filter_HYC00_HY114 (State : SQLSTATE) return Boolean;
   function Filter_HY004 (State : SQLSTATE) return Boolean;
   function Filter_IM001 (State : SQLSTATE) return Boolean;
   function Filter_07009 (State : SQLSTATE) return Boolean;
   function Filter_42S02 (State : SQLSTATE) return Boolean;

   type ODBC_Environment is
      new Ada.Finalization.Limited_Controlled with
   record
      Handle : SQLHENV := SQLHENV (SQL_NULL_HANDLE);
   end record;
--
-- Get_Attribute -- Get attribute
--
--    Environment - The environment
--    Attribute   - The attribute
--
-- Returns :
--
--    The attribute value
--
   function Get_Attribute
            (  Environment : ODBC_Environment;
               Attribute   : SQL_ENV_ATTR
            )  return SQLUINTEGER;
--
-- Check -- Verify result of an operation on environmental
--
--    Environment - The environment
--    Result      - To check
--    Filter      - State exceptions filter
--
-- Exceptions :
--
--    Data_Error - Result does indicate successful completion
--
   procedure Check
             (  Environment : ODBC_Environment;
                Result      : SQLRETURN;
                Filter      : Filter_Ptr := No_Filter'Access
             );

   type ODBC_Connection
        (  Environment : access ODBC_Environment'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Handle : SQLHDBC := SQLHDBC (SQL_NULL_HANDLE);
      Mode   : Execution_Mode := Read_Write;
   end record;
--
-- Check -- Verify result of an operation on connection
--
--    Connection - The connection
--    Result     - To check
--    Filter     - State exceptions filter
--
-- Exceptions :
--
--    Data_Error - Result does indicate successful completion
--
   procedure Check
             (  Connection : ODBC_Connection;
                Result     : SQLRETURN;
                Filter     : Filter_Ptr := No_Filter'Access
             );
--
-- Get_Attribute -- Get attribute
--
--    Connection - The connection
--    Attribute  - The attribute
--    Filter     - Error filter
--
-- Returns :
--
--    The attribute value
--
-- Exceptions :
--
--    Data_Error - Database errors
--
   function Get_Attribute
            (  Connection : ODBC_Connection;
               Attribute  : SQL_ATTR;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLUINTEGER;
   function Get_Attribute
            (  Connection : ODBC_Connection;
               Attribute  : SQL_ATTR;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLULEN;
   function Get_Attribute
            (  Connection : ODBC_Connection;
               Attribute  : SQL_ATTR;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return String;
   function Get_Attribute
            (  Connection : ODBC_Connection;
               Attribute  : SQL_ATTR;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return Wide_String;
--
-- Get_Info -- Get string information
--
--    Connection - The connection
--    Info_Type  - The information type
--    Filter     - Error filter
--
-- Returns :
--
--    The information value
--
-- Exceptions :
--
--    Constraint_Error - Output string is too large
--    Data_Error       - Database errors
--
   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLUSMALLINT;
   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLUINTEGER;
   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLULEN;
   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return String;
   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return Wide_String;

   type ODBC_Command
        (  Connection : access ODBC_Connection'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Handle : SQLHSTMT := SQLHSTMT (SQL_NULL_HANDLE);
      Length : aliased SQLLEN;
   end record;
--
-- Check -- Verify returned result of a command
--
--    Command - The command
--    Result  - To check
--    Filter  - State exceptions filter
--
-- Exceptions :
--
--    Data_Error - Result does indicate successful completion
--
   procedure Check
             (  Command : ODBC_Command;
                Result  : SQLRETURN;
                Filter  : Filter_Ptr := No_Filter'Access
             );
   pragma Inline (Check);
--
-- Get_Attribute -- Get attribute
--
--    Command   - The command
--    Attribute - The attribute
--    Filter    - Error filter
--
-- Returns :
--
--    The attribute value
--
-- Exceptions :
--
--    Data_Error - Database errors
--
   function Get_Attribute
            (  Command   : ODBC_Command;
               Attribute : SQL_ATTR;
               Filter    : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLULEN;
--
-- Get_Field -- Verify returned result of a command
--
--    Handle_Type   - The type of the handle for which checking is done
--    Handle        - The handle
--    Diag_Field    - The field of the diagnostic
--    Record_Number - The record number
--    Filter        - State exceptions filter
--
-- Returns :
--
--    Value of the field
--
-- Exceptions :
--
--    Data_Error - Result does indicate successful completion
--
   function Get_Field
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               Diag_Field    : SQL_DIAG;
               Record_Number : Positive   := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return SQLINTEGER;
   function Get_Field
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               Diag_Field    : SQL_DIAG;
               Record_Number : Positive   := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return SQLLEN;
   function Get_Field
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               Diag_Field    : SQL_DIAG;
               Record_Number : Positive   := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return String;
   function Get_Field
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               Diag_Field    : SQL_DIAG;
               Record_Number : Positive   := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return Wide_String;
--
-- Get_Rec -- Get diagnostic record
--
--    Handle_Type   - The type of the handle for which checking is done
--    Handle        - The handle
--    State         - State
--    Error         - Native error code
--    Record_Number - The record number
--    Filter        - State exceptions filter
--
-- Returns :
--
--    String value of the field
--
-- Exceptions :
--
--    Data_Error - Result does indicate successful completion
--
   function Get_Rec
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               State         : access SQLSTATE;
               Error         : access SQLINTEGER;
               Record_Number : Positive := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return String;
   function Get_Rec
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               State         : access SQLSTATE;
               Error         : access SQLINTEGER;
               Record_Number : Positive := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return Wide_String;
--
-- Check -- Verify returned result of an operation
--
--    Result      - To check
--    Handle_Type - The type of the handle for which checking is done
--    Handle      - The handle
--    Filter      - State exceptions filter
--
-- Exceptions :
--
--    Data_Error - Result does indicate successful completion
--
   procedure Check
             (  Result      : SQLRETURN;
                Handle_Type : SQL_HANDLE;
                Handle      : SQLHANDLE;
                Filter      : Filter_Ptr
             );
--  --
--  -- Complete_Async -- Get status of a connection operation
--  --
--  --    Connection - The ODBC connection object
--  --
--  -- Returns :
--  --
--  --    True if asynchronous operation completed
--  --
--  -- Exceptions :
--  --
--  --    Data_Error - ODBC driver error
--  --
--     function Complete_Async
--              (  Connection : ODBC_Connection
--              )  return Boolean;
--  --
--  -- Complete_Async -- Get command completion status
--  --
--  --    Command - The command
--  --
--  -- Returns :
--  --
--  --    True if asynchronous operation completed
--  --
--  -- Exceptions :
--  --
--  --    Data_Error - ODBC driver error
--  --
--     function Complete_Async (Command : ODBC_Command) return Boolean;

end ODBC.API;
