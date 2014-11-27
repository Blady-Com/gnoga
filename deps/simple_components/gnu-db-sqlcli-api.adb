--                                                                    --
--  package GNU.DB.SQLCLI.API       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  14:24 11 Feb 2012  --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Unbounded;  use Ada.Strings.Wide_Unbounded;
with GNU.DB.SQLCLI.Info;          use GNU.DB.SQLCLI.Info;

with Ada.Unchecked_Conversion;

with GNU.DB.SQLCLI.Environment_Attribute;
use  GNU.DB.SQLCLI.Environment_Attribute;

with GNU.DB.SQLCLI.Connection_Attribute;
use  GNU.DB.SQLCLI.Connection_Attribute;

package body GNU.DB.SQLCLI.API is
   use Interfaces;

   Buffer_Size         : constant := 1024;
   Bytes_In_Wide_Char  : constant := 2;
   SQLGUID_Length      : constant := 16;
   SQLINTEGER_Length   : constant := SQLINTEGER'Size  / SQLCHAR'Size;
   SQLDOUBLE_Length    : constant := SQLDOUBLE'Size   / SQLCHAR'Size;
   SQLSMALLINT_Length  : constant := SQLSMALLINT'Size / SQLCHAR'Size;
   SQLBIGINT_Length    : constant := SQLBIGINT'Size   / SQLCHAR'Size;
   SQLTIMESTAMP_Length : SQLINTEGER :=
      SQL_TIMESTAMP_STRUCT'Size / SQLCHAR'Size;

   type PTR_SQLGUID   is access all SQLGUID;
   type PTR_SQLBIGINT is access all SQLBIGINT;
   type PTR_TIMESTAMP is access all SQL_TIMESTAMP_STRUCT;

   function "<" (Left, Right : SQLGUID) return Boolean is
   begin
      if Left.Data1 /= Right.Data1 then
         return Left.Data1 < Right.Data1;
      end if;
      if Left.Data2 /= Right.Data2 then
         return Left.Data2 < Right.Data2;
      end if;
      if Left.Data3 /= Right.Data3 then
         return Left.Data3 < Right.Data3;
      end if;
      for Index in SQLGUID_Octet'Range loop
         if Left.Data4 (Index) /= Right.Data4 (Index) then
            return Left.Data4 (Index) < Right.Data4 (Index);
         end if;
      end loop;
      return False;
   end "<";

   function "<=" (Left, Right : SQLGUID) return Boolean is
   begin
      if Left.Data1 /= Right.Data1 then
         return Left.Data1 < Right.Data1;
      end if;
      if Left.Data2 /= Right.Data2 then
         return Left.Data2 < Right.Data2;
      end if;
      if Left.Data3 /= Right.Data3 then
         return Left.Data3 < Right.Data3;
      end if;
      for Index in SQLGUID_Octet'Range loop
         if Left.Data4 (Index) /= Right.Data4 (Index) then
            return Left.Data4 (Index) < Right.Data4 (Index);
         end if;
      end loop;
      return True;
   end "<=";

   function ">" (Left, Right : SQLGUID) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">=" (Left, Right : SQLGUID) return Boolean is
   begin
      return Right <= Left;
   end ">=";

   function To_SQLPOINTER is
      new Ada.Unchecked_Conversion (PTR_SQLDOUBLE, SQLPOINTER);

   function To_SQLPOINTER is
      new Ada.Unchecked_Conversion (PTR_SQLBIGINT, SQLPOINTER);

   function To_SQLPOINTER is
      new Ada.Unchecked_Conversion (PTR_SQLGUID, SQLPOINTER);

   function To_SQLPOINTER is
      new Ada.Unchecked_Conversion (PTR_TIMESTAMP, SQLPOINTER);

   function SQLTables
            (  StatementHandle : SQLHSTMT;
               pCatalogName    : PTR_SQLTCHAR;
               NameLength1     : SQLSMALLINT;
               pSchemaName     : PTR_SQLTCHAR;
               NameLength2     : SQLSMALLINT;
               pTableName      : PTR_SQLTCHAR;
               NameLength3     : SQLSMALLINT;
               pTableType      : PTR_SQLTCHAR;
               NameLength4     : SQLSMALLINT
            )  return SQLRETURN;
   function SQLTables
            (  StatementHandle : SQLHSTMT;
               pCatalogName    : PTR_SQLCHAR;
               NameLength1     : SQLSMALLINT;
               pSchemaName     : PTR_SQLCHAR;
               NameLength2     : SQLSMALLINT;
               pTableName      : PTR_SQLCHAR;
               NameLength3     : SQLSMALLINT;
               pTableType      : PTR_SQLCHAR;
               NameLength4     : SQLSMALLINT
            )  return SQLRETURN;
   pragma Import (Stdcall, SQLTables, "SQLTables");

   procedure Initialize (Environment : in out ODBC_Environment) is
   begin
      SQLAllocHandle
      (  SQL_HANDLE_ENV,
         SQL_NULL_HANDLE,
         Environment.Handle
      );
      SQLSetEnvAttr
      (  Environment.Handle,
         Environment_Attribute_ODBC_Version'
         (  Attribute => SQL_ATTR_ODBC_VERSION,
            Value     => SQL_OV_ODBC3
      )  );
   end Initialize;

   procedure Finalize (Environment : in out ODBC_Environment) is
   begin
      SQLFreeHandle (SQL_HANDLE_ENV, Environment.Handle);
   exception
      when others =>
         null;
   end Finalize;

   procedure Initialize (Connection : in out ODBC_Connection) is
   begin
      SQLAllocHandle
      (  SQL_HANDLE_DBC,
         Connection.Environment.Handle,
         Connection.Handle
      );
   end Initialize;

   procedure Connect
             (  Connection  : in out ODBC_Connection;
                Server_Name : String;
                User_Name   : String;
                Password    : String;
                Auto_Commit : Boolean
             )  is
   begin
      SQLConnect
      (  ConnectionHandle => Connection.Handle,
         ServerName       => Server_Name,
         UserName         => User_Name,
         Authentication   => Password
      );
      begin
         if Auto_Commit then
            SQLSetConnectAttr
            (  Connection.Handle,
               Connection_Attribute_AutoCommit'
               (  Attribute => SQL_ATTR_AUTOCOMMIT,
                  Value     => SQL_AUTOCOMMIT_ON
            )  );
         else
            SQLSetConnectAttr
            (  Connection.Handle,
               Connection_Attribute_AutoCommit'
               (  Attribute => SQL_ATTR_AUTOCOMMIT,
                  Value     => SQL_AUTOCOMMIT_OFF
            )  );
         end if;
      exception
         when others =>
            null;
      end;
   end Connect;

   procedure Connect
             (  Connection  : in out ODBC_Connection;
                Server_Name : Wide_String;
                User_Name   : Wide_String;
                Password    : Wide_String;
                Auto_Commit : Boolean
             )  is
   begin
      SQLConnect
      (  ConnectionHandle => Connection.Handle,
         ServerName       => Server_Name,
         UserName         => User_Name,
         Authentication   => Password
      );
      if Auto_Commit then
         SQLSetConnectAttr
         (  Connection.Handle,
            Connection_Attribute_AutoCommit'
            (  Attribute => SQL_ATTR_AUTOCOMMIT,
               Value     => SQL_AUTOCOMMIT_ON
         )  );
      else
         SQLSetConnectAttr
         (  Connection.Handle,
            Connection_Attribute_AutoCommit'
            (  Attribute => SQL_ATTR_AUTOCOMMIT,
               Value     => SQL_AUTOCOMMIT_OFF
         )  );
      end if;
   end Connect;

   procedure Finalize (Connection : in out ODBC_Connection) is
   begin
      begin
         SQLDisconnect (Connection.Handle);
      exception
         when others => -- Ignore disconnection errors for
            null;       -- we might be not connected at all
      end;
      SQLFreeHandle (SQL_HANDLE_DBC, Connection.Handle);
   exception
      when others =>
         null;
   end Finalize;

   procedure Initialize (Command : in out ODBC_Command) is
   begin
      SQLAllocHandle
      (  SQL_HANDLE_STMT,
         Command.Connection.Handle,
         Command.Handle
      );
   end Initialize;

   procedure Close_Cursor (Command : in out ODBC_Command) is
   begin
      SQLCloseCursor (Command.Handle);
   exception
      when others => -- Ignore any cursor errors
         null;
   end Close_Cursor;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access String;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_LONGVARCHAR
             )  is
   begin
      Length.all := Parameter'Length;
      SQLBindParameter
      (  StatementHandle  => Command.Handle,
         ParameterNumber  => SQL_Parameter_Number (Index),
         InputOutputType  => SQL_PARAM_INPUT,
         ValueType        => SQL_C_CHAR,
         ParameterType    => Data_Type,
         ColumnSize       => SQLUINTEGER (Length.all),
         DecimalDigits    => 0,
         Value            => To_SQLPOINTER (Parameter),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access Wide_String;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_WLONGVARCHAR
             )  is
   begin
      Length.all := Parameter'Length * Bytes_In_Wide_Char;
      SQLBindParameter
      (  StatementHandle  => Command.Handle,
         ParameterNumber  => SQL_Parameter_Number (Index),
         InputOutputType  => SQL_PARAM_INPUT,
         ValueType        => SQL_C_WCHAR,
         ParameterType    => Data_Type,
         ColumnSize       => SQLUINTEGER (Length.all),
         DecimalDigits    => 0,
         Value            => To_SQLPOINTER (Parameter),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLSMALLINT;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_SMALLINT
             )  is
   begin
      Length.all := SQLSMALLINT_Length;
      SQLBindParameter
      (  StatementHandle  => Command.Handle,
         ParameterNumber  => SQL_Parameter_Number (Index),
         InputOutputType  => SQL_PARAM_INPUT,
         ValueType        => SQL_C_SSHORT,
         ParameterType    => Data_Type,
         ColumnSize       => 0,
         DecimalDigits    => 0,
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length,
         Value            => To_SQLPOINTER (PTR_SQLSMALLINT (Parameter))
      );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLINTEGER;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_INTEGER
             )  is
   begin
      Length.all := SQLINTEGER_Length;
      SQLBindParameter
      (  StatementHandle  => Command.Handle,
         ParameterNumber  => SQL_Parameter_Number (Index),
         InputOutputType  => SQL_PARAM_INPUT,
         ValueType        => SQL_C_SLONG,
         ParameterType    => Data_Type,
         ColumnSize       => 0,
         DecimalDigits    => 0,
         Value            => To_SQLPOINTER (PTR_SQLINTEGER (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLBIGINT;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_BIGINT
             )  is
   begin
      Length.all := SQLBIGINT_Length;
      SQLBindParameter
      (  StatementHandle  => Command.Handle,
         ParameterNumber  => SQL_Parameter_Number (Index),
         InputOutputType  => SQL_PARAM_INPUT,
         ValueType        => SQL_C_SBIGINT,
         ParameterType    => Data_Type,
         ColumnSize       => 0,
         DecimalDigits    => 0,
         Value            => To_SQLPOINTER (PTR_SQLBIGINT (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLDOUBLE;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_DOUBLE
             )  is
   begin
      Length.all := SQLDOUBLE_Length;
      SQLBindParameter
      (  StatementHandle  => Command.Handle,
         ParameterNumber  => SQL_Parameter_Number (Index),
         InputOutputType  => SQL_PARAM_INPUT,
         ValueType        => SQL_C_DOUBLE,
         ParameterType    => Data_Type,
         ColumnSize       => 0,
         DecimalDigits    => 0,
         Value            => To_SQLPOINTER (PTR_SQLDOUBLE (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLGUID;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_GUID
             )  is
   begin
      Length.all := SQLGUID_Length;
      SQLBindParameter
      (  StatementHandle  => Command.Handle,
         ParameterNumber  => SQL_Parameter_Number (Index),
         InputOutputType  => SQL_PARAM_INPUT,
         ValueType        => SQL_C_GUID,
         ParameterType    => Data_Type,
         ColumnSize       => 0,
         DecimalDigits    => 0,
         Value            => To_SQLPOINTER (PTR_SQLGUID (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQL_TIMESTAMP_STRUCT;
                Length    : access SQLINTEGER;
                Data_Type : SQL_DATA_TYPE := SQL_TIMESTAMP
             )  is
   begin
      Length.all := SQLTIMESTAMP_Length;
      SQLBindParameter
      (  StatementHandle  => Command.Handle,
         ParameterNumber  => SQL_Parameter_Number (Index),
         InputOutputType  => SQL_PARAM_INPUT,
         ValueType        => SQL_C_TYPE_TIMESTAMP,
         ParameterType    => Data_Type,
         ColumnSize       => 0,
         DecimalDigits    => 0,
         Value            => To_SQLPOINTER (PTR_TIMESTAMP (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Parameter;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLSMALLINT;
                Length    : access SQLINTEGER
             )  is
   begin
      Length.all := SQLSMALLINT_Length;
      SQLBindCol
      (  StatementHandle  => Command.Handle,
         ColumnNumber     => SQL_Column_Number (Column),
         TargetType       => SQL_C_SSHORT,
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length,
         TargetValuePtr   => To_SQLPOINTER (PTR_SQLSMALLINT (Parameter))
      );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLINTEGER;
                Length    : access SQLINTEGER
             )  is
   begin
      Length.all := SQLINTEGER_Length;
      SQLBindCol
      (  StatementHandle  => Command.Handle,
         ColumnNumber     => SQL_Column_Number (Column),
         TargetType       => SQL_C_SLONG,
         TargetValuePtr   => To_SQLPOINTER (PTR_SQLINTEGER (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLBIGINT;
                Length    : access SQLINTEGER
             )  is
   begin
      Length.all := SQLBIGINT_Length;
      SQLBindCol
      (  StatementHandle  => Command.Handle,
         ColumnNumber     => SQL_Column_Number (Column),
         TargetType       => SQL_C_SBIGINT,
         TargetValuePtr   => To_SQLPOINTER (PTR_SQLBIGINT (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLDOUBLE;
                Length    : access SQLINTEGER
             )  is
   begin
      Length.all := SQLDOUBLE_Length;
      SQLBindCol
      (  StatementHandle  => Command.Handle,
         ColumnNumber     => SQL_Column_Number (Column),
         TargetType       => SQL_C_DOUBLE,
         TargetValuePtr   => To_SQLPOINTER (PTR_SQLDOUBLE (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLGUID;
                Length    : access SQLINTEGER
             )  is
   begin
      Length.all := SQLGUID_Length;
      SQLBindCol
      (  StatementHandle  => Command.Handle,
         ColumnNumber     => SQL_Column_Number (Column),
         TargetType       => SQL_C_GUID,
         TargetValuePtr   => To_SQLPOINTER (PTR_SQLGUID (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQL_TIMESTAMP_STRUCT;
                Length    : access SQLINTEGER
             )  is
   begin
      Length.all := SQLTIMESTAMP_Length;
      SQLBindCol
      (  StatementHandle  => Command.Handle,
         ColumnNumber     => SQL_Column_Number (Column),
         TargetType       => SQL_C_TYPE_TIMESTAMP,
         TargetValuePtr   => To_SQLPOINTER (PTR_TIMESTAMP (Parameter)),
         BufferLength     => 0,
         StrLen_Or_IndPtr => Length
      );
   end Bind_Result;

   procedure Drop
             (  Command    : in out ODBC_Command'Class;
                Table_Name : String
             )  is
   begin
      if Table_Exists (Command.Connection, Table_Name) then
         Execute (Command, "DROP TABLE " & Table_Name);
      end if;
   exception
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Drop;

   procedure Drop
             (  Command    : in out ODBC_Command'Class;
                Table_Name : Wide_String
             )  is
   begin
      if Table_Exists (Command.Connection, Table_Name) then
         Execute (Command, "DROP TABLE " & Table_Name);
      end if;
   exception
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Drop;

   procedure EndTran (Connection : in out ODBC_Connection) is
   begin
      SQLEndTran
      (  SQL_HANDLE_DBC,
         Connection.Handle,
         SQL_COMMIT
      );
   end EndTran;

   procedure Execute (Command : in out ODBC_Command) is
   begin
      if Command.Connection.Mode = None then
         Raise_Exception
         (  Use_Error'Identity,
            "No access to execute"
         );
      end if;
      SQLExecute (Command.Handle);
   end Execute;

   procedure Execute (Command : in out ODBC_Command; Text : String) is
   begin
      if Command.Connection.Mode = None then
         Raise_Exception
         (  Use_Error'Identity,
            "No access to execute"
         );
      end if;
      SQLFreeStmt   (Command.Handle, SQL_UNBIND);
      SQLFreeStmt   (Command.Handle, SQL_RESET_PARAMS);
      SQLExecDirect (Command.Handle, Text);
   end Execute;

   procedure Execute
             (  Command : in out ODBC_Command;
                Text    : Wide_String
             )  is
   begin
      if Command.Connection.Mode = None then
         Raise_Exception
         (  Use_Error'Identity,
            "The connection is in no access mode"
         );
      end if;
      SQLFreeStmt (Command.Handle, SQL_UNBIND);
      SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS);
      SQLExecDirect (Command.Handle, Text);
   end Execute;

   function Execute (Command : access ODBC_Command) return Natural is
   begin
      if Command.Connection.Mode = None then
         Raise_Exception
         (  Use_Error'Identity,
            "No access to execute"
         );
      end if;
      SQLExecute (Command.Handle);
      return Natural (SQLRowCount (Command.Handle));
   exception
      when No_Data =>
         return 0;
   end Execute;

   function Execute (Command : access ODBC_Command; Text : String)
      return Natural is
   begin
      if Command.Connection.Mode = None then
         Raise_Exception
         (  Use_Error'Identity,
            "No access to execute"
         );
      end if;
      SQLFreeStmt (Command.Handle, SQL_UNBIND);
      SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS);
      SQLExecDirect (Command.Handle, Text);
      return Natural (SQLRowCount (Command.Handle));
   exception
      when No_Data =>
         return 0;
   end Execute;

   function Execute (Command : access ODBC_Command; Text : Wide_String)
      return Natural is
   begin
      if Command.Connection.Mode = None then
         Raise_Exception
         (  Use_Error'Identity,
            "No access to execute"
         );
      end if;
      SQLFreeStmt (Command.Handle, SQL_UNBIND);
      SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS);
      SQLExecDirect (Command.Handle, Text);
      return Natural (SQLRowCount (Command.Handle));
   exception
      when No_Data =>
         return 0;
   end Execute;

   procedure Fetch (Command : in out ODBC_Command) is
   begin
      SQLFetch (Command.Handle);
   end Fetch;

   function Fetch (Command : access ODBC_Command)
      return SQLRETURN is
   begin
      return SQLFetch (Command.Handle);
   end Fetch;

   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return String is
      Length : aliased SQLINTEGER;
      Block  : aliased String := (1..Buffer_Size => ' ');
      Buffer : Unbounded_String;
   begin
      loop
         case SQLGetData
              (  StatementHandle => Command.Handle,
                 ColumnNumber    => SQL_Column_Number (Column),
                 TargetType      => SQL_C_CHAR,
                 TargetValue     => To_SQLPOINTER (Block'Access),
                 BufferLength    => Block'Length,
                 StrLen_Or_Ind   => Length'Access
              )  is
            when SQL_NO_DATA =>
               if Length = SQL_NULL_DATA then
                  Raise_Exception
                  (  End_Error'Identity,
                     "No data set"
                  );
               end if;
               exit;
            when SQL_SUCCESS =>
               if Length = SQL_NULL_DATA then
                  Raise_Exception
                  (  End_Error'Identity,
                     "No data set"
                  );
               end if;
               Append (Buffer, Block (1..Integer (Length)));
               exit;
            when SQL_SUCCESS_WITH_INFO =>
               if Length < 0 then
                  Append (Buffer, Block);
               else
                  Append (Buffer, Block (1..Integer (Length)));
               end if;
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  Get_Message (Command.all)
               );
         end case;
      end loop;
      if Finish = Always then
         Close_Cursor (Command.all);
      end if;
      return To_String (Buffer);
   exception
      when End_Error =>
         if Finish in On_No_Result..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
      when others =>
         if Finish in On_Error..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
   end Get_Data;

   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return Time is
      Length : aliased SQLINTEGER := SQLTIMESTAMP_Length;
      Result : aliased SQL_TIMESTAMP_STRUCT;
   begin
      case SQLGetData
           (  StatementHandle => Command.Handle,
              ColumnNumber    => SQL_Column_Number (Column),
              TargetType    => SQL_C_TIMESTAMP,
              TargetValue   => To_SQLPOINTER (Result'Address),
              BufferLength  => SQLTIMESTAMP_Length,
              StrLen_Or_Ind => Length'Access
           )  is
         when SQL_NO_DATA =>
            Raise_Exception
            (  End_Error'Identity,
               "No data set"
            );
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return
               Time_Of
               (  Year    => Year_Number  (Result.Year),
                  Month   => Month_Number (Result.Month),
                  Day     => Day_Number   (Result.Day),
                  Seconds =>
                     (  Duration (Result.Minute * 60)
                     +  Duration (Result.Second)
                     +  Duration
                        (  Long_Float (Result.Fraction)
                        /  (  Long_Float (SQL_TIME_FRACTION'Last)
                           +  1.0
               )     )  )  );
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception
            (  Data_Error'Identity,
               "Data overrun"
            );
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               Get_Message (Command.all)
            );
      end case;
   exception
      when End_Error =>
         if Finish in On_No_Result..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
      when others =>
         if Finish in On_Error..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
   end Get_Data;

   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return Wide_String is
      Length : aliased SQLINTEGER;
      Block  : aliased Wide_String := (1..Buffer_Size => ' ');
      Buffer : Unbounded_Wide_String;
   begin
      loop
         case SQLGetData
              (  StatementHandle => Command.Handle,
                 ColumnNumber    => SQL_Column_Number (Column),
                 TargetType      => SQL_C_WCHAR,
                 TargetValue     => To_SQLPOINTER (Block'Access),
                 BufferLength    => Block'Length * Bytes_In_Wide_Char,
                 StrLen_Or_Ind   => Length'Access
              )  is
            when SQL_NO_DATA =>
               if Length = SQL_NULL_DATA then
                  Raise_Exception
                  (  End_Error'Identity,
                     "No data set"
                  );
               end if;
               exit;
            when SQL_SUCCESS =>
               if Length = SQL_NULL_DATA then
                  Raise_Exception
                  (  End_Error'Identity,
                     "No data set"
                  );
               end if;
               Append
               (  Buffer,
                  Block (1..Integer (Length / Bytes_In_Wide_Char))
               );
               exit;
            when SQL_SUCCESS_WITH_INFO =>
               if Length < 0 then
                  Append (Buffer, Block);
               else
                  Append
                  (  Buffer,
                     Block (1..Integer (Length / Bytes_In_Wide_Char))
                  );
               end if;
            when others =>
               Raise_Exception
               (  Data_Error'Identity,
                  Get_Message (Command.all)
               );
         end case;
      end loop;
      if Finish = Always then
         Close_Cursor (Command.all);
      end if;
      return To_Wide_String (Buffer);
   exception
      when End_Error =>
         if Finish in On_No_Result..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
      when others =>
         if Finish in On_Error..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
   end Get_Data;

   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLINTEGER is
      Length : aliased SQLINTEGER := SQLINTEGER_Length;
      Result : aliased SQLINTEGER;
   begin
      case SQLGetData
           (  StatementHandle => Command.Handle,
              ColumnNumber    => SQL_Column_Number (Column),
              TargetType      => SQL_C_SLONG,
              TargetValue     => To_SQLPOINTER (Result'Unchecked_Access),
              BufferLength    => SQLINTEGER_Length,
              StrLen_Or_Ind   => Length'Access
           )  is
         when SQL_NO_DATA =>
            Raise_Exception
            (  End_Error'Identity,
               "No data set"
            );
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception
            (  Data_Error'Identity,
               "Data overrun"
            );
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               Get_Message (Command.all)
            );
      end case;
   exception
      when End_Error =>
         if Finish in On_No_Result..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
      when others =>
         if Finish in On_Error..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
   end Get_Data;

   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLBIGINT is
      Length : aliased SQLINTEGER := SQLBIGINT_Length;
      Result : aliased SQLBIGINT;
   begin
      case SQLGetData
           (  StatementHandle => Command.Handle,
              ColumnNumber    => SQL_Column_Number (Column),
              TargetType      => SQL_C_SBIGINT,
              TargetValue     => To_SQLPOINTER (Result'Unchecked_Access),
              BufferLength    => SQLBIGINT_Length,
              StrLen_Or_Ind   => Length'Access
           )  is
         when SQL_NO_DATA =>
            Raise_Exception
            (  End_Error'Identity,
               "No data set"
            );
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception
            (  Data_Error'Identity,
               "Data overrun"
            );
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               Get_Message (Command.all)
            );
      end case;
   exception
      when End_Error =>
         if Finish in On_No_Result..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
      when others =>
         if Finish in On_Error..Always then
            Close_Cursor (Command.all);
         end if;
         raise;
   end Get_Data;

   function Get_Message (Command : ODBC_Command) return String is
      State : aliased SQLSTATE;
   begin
      return
         SQL_Error_Message
         (  HandleType => SQL_HANDLE_STMT,
            Handle     => Command.Handle,
            State      => State'Access
         );
   end Get_Message;

   procedure Get_Tables (Command : in out ODBC_Command) is
      Code : SQLRETURN;
   begin
      SQLFreeStmt (Command.Handle, SQL_UNBIND);
      SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS);
      Code :=
         SQLTables
         (  Command.Handle,
            PTR_SQLCHAR'(null), 0,
            null, 0,
            null, 0,
            null, 0
         );
      Check_SQL_Error
      (  RC            => Code,
         ProcedureName => "SQLTables",
         HandleType    => SQL_HANDLE_STMT,
         Handle        => Command.Handle
      );
   end Get_Tables;

   function Get_Type_Info
            (  Command   : access ODBC_Command;
               Data_Type : SQL_DATA_TYPE
            )  return Type_Info is
      Result : Type_Info;
      function To_SQL_DATA_TYPE is new
         Ada.Unchecked_Conversion (SQLSMALLINT, SQL_DATA_TYPE);
      function To_NULLABLE_INFO is new
         Ada.Unchecked_Conversion (SQLSMALLINT, SQL_NULLABLE_INFO);
--      function To_SEARCHABLE_ATTRIBUTE is new
--         Ada.Unchecked_Conversion (SQLSMALLINT, SEARCHABLE_ATTRIBUTE);
   begin
      SQLFreeStmt (Command.Handle, SQL_UNBIND);
      SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS);
      SQLGetTypeInfo (Command.Handle, Data_Type);
      Fetch (Command.all);
      -- Column 1
      Append (Result.Type_Name, Get_Data (Command, 1, On_Error));
      -- Column 2
      Result.Data_Type :=
         To_SQL_DATA_TYPE
         (  SQLSMALLINT
            (  SQLINTEGER'
               (  Get_Data (Command, 2, On_Error)
         )  )  );
      -- Column 3
      begin
         Result.Column_Size :=
            Integer (SQLINTEGER'(Get_Data (Command, 3, On_Error)));
      exception
         when End_Error =>
            Result.Column_Size := 0;
      end;
      -- Column 4
      begin
         Append
         (  Result.Literal_Prefix,
            Get_Data (Command, 4, On_Error)
         );
      exception
         when End_Error =>
            null;
      end;
      -- Column 5
      begin
         Append
         (  Result.Literal_Suffix,
            Get_Data (Command, 5, On_Error)
         );
      exception
         when End_Error =>
            null;
      end;
      -- Column 6
      begin
         Append
         (  Result.Create_Parameters,
            Get_Data (Command, 6, On_Error)
         );
      exception
         when End_Error =>
            null;
      end;
      -- Column 7
      begin
         Result.Nullable :=
            (  SQL_NULLABLE
            =  To_NULLABLE_INFO
               (  SQLSMALLINT
                  (  SQLINTEGER'
                     (  Get_Data (Command, 7, On_Error)
            )  )  )  );
      exception
         when End_Error =>
            Result.Nullable := False;
      end;
      -- Column 8
      Result.Case_Sensitive :=
         SQLINTEGER'(Get_Data (Command, 8, On_Error)) /= 0;
      -- Column 9
      Result.Searchable :=
--       To_SEARCHABLE_ATTRIBUTE
         (  SQLSMALLINT
            (  SQLINTEGER'
               (  Get_Data (Command, 9, On_Error)
         )  )  );
      -- Column 10
      begin
         Result.Unsigned_Attribute :=
            SQLINTEGER'(Get_Data (Command, 10, On_Error)) /= 0;
      exception
         when End_Error =>
            Result.Unsigned_Attribute := True;
      end;
      -- Column 11
      Result.Fixed_Prec_Scale :=
         SQLINTEGER'(Get_Data (Command, 11, On_Error)) /= 0;
      -- Column 12
      begin
         Result.Auto_Unique_Value :=
            SQLINTEGER'(Get_Data (Command, 12, On_Error)) /= 0;
      exception
         when End_Error =>
            Result.Auto_Unique_Value := False;
      end;
      -- Column 13
      begin
         Append
         (  Result.Local_Name,
            Get_Data (Command, 13, On_Error)
         );
      exception
         when End_Error =>
            null;
      end;
      Close_Cursor (Command.all);
      return Result;
   exception
      when No_Data | Constraint_Error =>
         Close_Cursor (Command.all);
         Raise_Exception
         (  Constraint_Error'Identity,
            "Not supported"
         );
      when others =>
         Close_Cursor (Command.all);
         Raise_Exception
         (  Data_Error'Identity,
            Get_Message (Command.all)
         );
   end Get_Type_Info;

   procedure Prepare
             (  Command : in out ODBC_Command;
                Request : String
             )  is
   begin
      SQLFreeStmt (Command.Handle, SQL_UNBIND);
      SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS);
      SQLPrepare  (Command.Handle, Request);
   end Prepare;

   procedure Prepare
             (  Command : in out ODBC_Command;
                Request : Wide_String
             )  is
   begin
      SQLFreeStmt (Command.Handle, SQL_UNBIND);
      SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS);
      SQLPrepare (Command.Handle, Request);
   end Prepare;

   procedure Finalize (Command : in out ODBC_Command) is
   begin
      SQLFreeHandle (SQL_HANDLE_STMT, Command.Handle);
   exception
      when others =>
         null;
   end Finalize;

   procedure Release (Connection : ODBC_Connection) is
   begin
      SQLSetConnectAttr
      (  Connection.Handle,
         Connection_Attribute_TXN_Isolation'
         (  SQL_ATTR_TXN_ISOLATION,
            SQL_TRANSACTION_READ_UNCOMMITTED
      )  );
   end Release;

   procedure RollBack (Connection  : in out ODBC_Connection) is
   begin
      SQLEndTran
      (  SQL_HANDLE_DBC,
         Connection.Handle,
         SQL_ROLLBACK
      );
   end RollBack;

   procedure Seize (Connection : ODBC_Connection) is
   begin
      SQLSetConnectAttr
      (  Connection.Handle,
         Connection_Attribute_TXN_Isolation'
         (  SQL_ATTR_TXN_ISOLATION,
            SQL_TRANSACTION_SERIALIZABLE
      )  );
   end Seize;

   procedure Set_Access_Mode
             (  Connection : in out ODBC_Connection;
                Mode       : Access_Mode
             )  is
   begin
      Connection.Mode := Mode;
   end Set_Access_Mode;

   function Serializable (Connection : ODBC_Connection)
      return Boolean is
   begin
      return
         TXN_Isolation_Info
         (  SQLGetInfo
            (  Connection.Handle,
               SQL_TXN_ISOLATION_OPTION
         )  ) .Value (SQL_TXN_SERIALIZABLE);
   end Serializable;

   function Table_Exists
            (  Connection : access ODBC_Connection;
               Name       : String
            )  return Boolean is
      Code    : SQLRETURN;
      Command : ODBC_Command (Connection);
   begin
      Code :=
         SQLTables
         (  Command.Handle,
            null, 0,
            null, 0,
            To_PTR_SQLCHAR (Name'Address), SQLSMALLINT (Name'Length),
            null, 0
         );
      Check_SQL_Error
      (  RC            => Code,
         ProcedureName => "SQLTables",
         HandleType    => SQL_HANDLE_STMT,
         Handle        => Command.Handle
      );
      SQLFetch (Command.Handle);
      return True;
   exception
      when No_Data | Table_Not_Found =>
         return False;
   end Table_Exists;

   function Table_Exists
            (  Connection : access ODBC_Connection;
               Name       : Wide_String
            )  return Boolean is
      Code    : SQLRETURN;
      Command : ODBC_Command (Connection);
   begin
      Code :=
         SQLTables
         (  Command.Handle,
            null, 0,
            null, 0,
            To_PTR_SQLTCHAR (Name'Address), SQLSMALLINT (Name'Length),
            null, 0
         );
      Check_SQL_Error
      (  RC            => Code,
         ProcedureName => "SQLTables",
         HandleType    => SQL_HANDLE_STMT,
         Handle        => Command.Handle
      );
      SQLFetch (Command.Handle);
      return True;
   exception
      when No_Data | Table_Not_Found =>
         return False;
   end Table_Exists;

begin
   Generate_Detailed_Exceptions := True;
end GNU.DB.SQLCLI.API;
