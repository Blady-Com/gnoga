--                                                                    --
--  package GNU.DB.SQLCLI.API       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with ODBC.Thin;                   use ODBC.Thin;
with Strings_Edit;                use Strings_Edit;
with Strings_Edit.Integers;       use Strings_Edit.Integers;
with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;

with Ada.Unchecked_Deallocation;
with System;

package body ODBC.API is
   Bytes_In_Wide_Char   : constant := 2;
   SQLGUID_Length       : constant := 16;
   SQLTINYINT_Length    : constant := SQLTINYINT'Size   / SQLCHAR'Size;
   SQLUTINYINT_Length   : constant := SQLUTINYINT'Size  / SQLCHAR'Size;
   SQLINTEGER_Length    : constant := SQLINTEGER'Size   / SQLCHAR'Size;
   SQLUINTEGER_Length   : constant := SQLUINTEGER'Size  / SQLCHAR'Size;
   SQLDOUBLE_Length     : constant := SQLDOUBLE'Size    / SQLCHAR'Size;
   SQLSMALLINT_Length   : constant := SQLSMALLINT'Size  / SQLCHAR'Size;
   SQLUSMALLINT_Length  : constant := SQLUSMALLINT'Size / SQLCHAR'Size;
   SQLBIGINT_Length     : constant := SQLBIGINT'Size    / SQLCHAR'Size;
   SQLUBIGINT_Length    : constant := SQLUBIGINT'Size   / SQLCHAR'Size;
   SQLLEN_Length        : constant := SQLLEN'Size       / SQLCHAR'Size;
   SQLULEN_Length       : constant := SQLULEN'Size      / SQLCHAR'Size;
   SQLTIMESTAMP_Length  : constant SQLLEN :=
      SQL_TIMESTAMP_STRUCT'Size / SQLCHAR'Size;

   function Dump (Value : char_array) return String is
      Pointer : Integer := 1;
      Result  : String (1..Value'Length * 3 - 1);
   begin
      for Index in Value'Range loop
         if Pointer > 1 then
            Put (Result, Pointer, ',');
         end if;
         Put
         (  Destination => Result,
            Pointer     => Pointer,
            Value       => char'Pos (Value (Index)),
            Base        => 16,
            Field       => 2,
            Fill        => '0',
            Justify     => Right
         );
      end loop;
      return Result (1..Pointer - 1);
   end Dump;

   function Dump (Value : SQLWCHAR_Array) return String is
      Pointer : Integer := 1;
      Result  : String (1..Value'Length * 5 - 1);
   begin
      for Index in Value'Range loop
         if Pointer > 1 then
            Put (Result, Pointer, ',');
         end if;
         Put
         (  Destination => Result,
            Pointer     => Pointer,
            Value       => SQLWCHAR'Pos (Value (Index)),
            Base        => 16,
            Field       => 4,
            Fill        => '0',
            Justify     => Right
         );
      end loop;
      return Result (1..Pointer - 1);
   end Dump;

   NULL_Length : aliased SQLLEN := SQL_NULL_DATA;

   procedure Bind_Null
             (  Command : in out ODBC_Command;
                Index   : Positive
             )  is
   begin
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_LONG,
            ParameterType     => SQL_INTEGER,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => NULL_Length'Access,
            ParameterValuePtr => SQLPOINTER (System.Null_Address)
      )  );
   end Bind_Null;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access String_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_LONGVARCHAR
             )  is
   begin
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_CHAR,
            ParameterType     => Data_Type,
            ColumnSize        => SQLULEN (Parameter.Length),
            DecimalDigits     => 0,
            BufferLength      => SQLLEN (Parameter.Length + 1),
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access Wide_String_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_WLONGVARCHAR
             )  is
   begin
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_WCHAR,
            ParameterType     => Data_Type,
            ColumnSize        => SQLULEN (Parameter.Length) *
                                 Bytes_In_Wide_Char,
            DecimalDigits     => 0,
            BufferLength      => SQLLEN (Parameter.Length + 1) *
                                 Bytes_In_Wide_Char,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLTINYINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_TINYINT
             )  is
   begin
      Parameter.Size := SQLTINYINT_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_STINYINT,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLUTINYINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_UTINYINT
             )  is
   begin
      Parameter.Size := SQLTINYINT_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_UTINYINT,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLSMALLINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_SMALLINT
             )  is
   begin
      Parameter.Size := SQLSMALLINT_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_SSHORT,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLUSMALLINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_USHORT
             )  is
   begin
      Parameter.Size := SQLUSMALLINT_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_USHORT,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLINTEGER_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_INTEGER
             )  is
   begin
      Parameter.Size := SQLINTEGER_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_SLONG,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLUINTEGER_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_ULONG
             )  is
   begin
      Parameter.Size := SQLUINTEGER_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_ULONG,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLBIGINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_BIGINT
             )  is
   begin
      Parameter.Size := SQLBIGINT_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_SBIGINT,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLUBIGINT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_UBIGINT
             )  is
   begin
      Parameter.Size := SQLUBIGINT_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_UBIGINT,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLDOUBLE_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_DOUBLE
             )  is
   begin
      Parameter.Size := SQLDOUBLE_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_DOUBLE,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQLGUID_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_GUID
             )  is
   begin
      Parameter.Size := SQLGUID_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_GUID,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Parameter
             (  Command   : in out ODBC_Command;
                Index     : Positive;
                Parameter : access SQL_TIMESTAMP_STRUCT_Parameter;
                Data_Type : SQL_DATA_TYPE := SQL_TIMESTAMP
             )  is
   begin
      Parameter.Size := SQLTIMESTAMP_Length;
      Check
      (  Command,
         SQLBindParameter
         (  StatementHandle   => Command.Handle,
            ParameterNumber   => SQLUSMALLINT (Index),
            InputOutputType   => SQL_PARAM_INPUT,
            ValueType         => SQL_C_TYPE_TIMESTAMP,
            ParameterType     => Data_Type,
            ColumnSize        => 0,
            DecimalDigits     => 0,
            BufferLength      => 0,
            StrLen_Or_IndPtr  => Parameter.Size'Unchecked_Access,
            ParameterValuePtr => SQLPOINTER (Parameter.Value'Address)
      )  );
   end Bind_Parameter;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLTINYINT
             )  is
   begin
      Command.Length := SQLTINYINT_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_STINYINT,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLUTINYINT
             )  is
   begin
      Command.Length := SQLUTINYINT_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_UTINYINT,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_Or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLSMALLINT
             )  is
   begin
      Command.Length := SQLSMALLINT_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_SSHORT,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLUSMALLINT
             )  is
   begin
      Command.Length := SQLUSMALLINT_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_USHORT,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_Or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLINTEGER
             )  is
   begin
      Command.Length := SQLINTEGER_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_SLONG,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_Or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLUINTEGER
             )  is
   begin
      Command.Length := SQLUINTEGER_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_ULONG,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_Or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLBIGINT
             )  is
   begin
      Command.Length := SQLBIGINT_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_SBIGINT,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_Or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLUBIGINT
             )  is
   begin
      Command.Length := SQLUBIGINT_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_UBIGINT,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_Or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLDOUBLE
             )  is
   begin
      Command.Length := SQLDOUBLE_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_DOUBLE,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_Or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQLGUID
             )  is
   begin
      Command.Length := SQLGUID_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_GUID,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_Or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Bind_Result
             (  Command   : in out ODBC_Command;
                Column    : Positive;
                Parameter : access SQL_TIMESTAMP_STRUCT
             )  is
   begin
      Command.Length := SQLTIMESTAMP_Length;
      Check
      (  Command,
         SQLBindCol
         (  StatementHandle => Command.Handle,
            ColumnNumber    => SQLUSMALLINT (Column),
            TargetType      => SQL_C_TYPE_TIMESTAMP,
            TargetValuePtr  => SQLPOINTER (Parameter.all'Address),
            BufferLength    => 0,
            StrLen_Or_Ind   => Command.Length'Unchecked_Access
      )  );
   end Bind_Result;

   procedure Check
             (  Environment : ODBC_Environment;
                Result      : SQLRETURN;
                Filter      : Filter_Ptr := No_Filter'Access
             )  is
   begin
      Check
      (  Result,
         SQL_HANDLE_ENV,
         SQLHANDLE (Environment.Handle),
         Filter
      );
   end Check;

   procedure Check
             (  Connection : ODBC_Connection;
                Result     : SQLRETURN;
                Filter     : Filter_Ptr := No_Filter'Access
             )  is
   begin
      Check
      (  Result,
         SQL_HANDLE_DBC,
         SQLHANDLE (Connection.Handle),
         Filter
      );
   end Check;

   procedure Check
             (  Command : ODBC_Command;
                Result  : SQLRETURN;
                Filter  : Filter_Ptr := No_Filter'Access
             )  is
   begin
      Check
      (  Result,
         SQL_HANDLE_STMT,
         SQLHANDLE (Command.Handle),
         Filter
      );
   end Check;

   procedure Check
             (  Result      : SQLRETURN;
                Handle_Type : SQL_HANDLE;
                Handle      : SQLHANDLE;
                Filter      : Filter_Ptr
             )  is
      Ignore : Boolean := False;

      function Get_Message return String is
         Error   : aliased SQLINTEGER;
         State   : aliased SQLSTATE := "00000" & nul;
         Message : constant String :=
                   Get_Rec
                   (  Handle_Type => Handle_Type,
                      Handle      => Handle,
                      State       => State'Access,
                      Error       => Error'Access,
                      Filter      => Filter_All'Access
                   );
      begin
         if Filter (State) then
            if Message'Length > 0 then
               return To_Ada (char_array (State)) & ": " & Message;
            else
               return To_Ada (char_array (State)) & ": ?";
            end if;
         else
            Ignore := True;
            return "";
         end if;
      end Get_Message;

      function Get_Server return String is
         Result : constant String :=
                  Get_Field
                  (  Handle_Type   => Handle_Type,
                     Handle        => Handle,
                     Diag_Field    => SQL_DIAG_SERVER_NAME,
                     Record_Number => 1,
                     Filter        => Filter_All'Access
                  );
      begin
         if Result'Length = 0 then
            return "";
         else
            return " [" & Result & ']';
         end if;
      end Get_Server;
   begin
      case Result is
         when SQL_SUCCESS | SQL_SUCCESS_WITH_INFO =>
            null;
         when SQL_ERROR =>
            declare
               Message : constant String := Get_Message;
            begin
               if not Ignore then
                  Raise_Exception
                  (  Data_Error'Identity,
                     Message & Get_Server
                  );
               end if;
            end;
         when SQL_STILL_EXECUTING =>
            Raise_Exception (Data_Error'Identity, "Still executing");
         when SQL_INVALID_HANDLE =>
            Raise_Exception (Data_Error'Identity, "Invalid handle");
         when SQL_NEED_DATA =>
            Raise_Exception (End_Error'Identity, "Need data");
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data");
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               "Unknown SQLRETURN value" & SQLRETURN'Image (Result)
            );
      end case;
   end Check;

   procedure Close_Cursor (Command : in out ODBC_Command) is
      Result : SQLRETURN;
   begin
      Result := SQLCloseCursor (Command.Handle);
   exception
      when others => -- Ignore any cursor errors
         null;
   end Close_Cursor;

--     function Complete_Async (Connection : ODBC_Connection)
--        return Boolean is
--        Code   : aliased RETCODE;
--        Result : SQLRETURN :=
--                 SQLCompleteAsync
--                 (  HandleType      => SQL_HANDLE_DBC,
--                    Handle          => SQLHANDLE (Connection.Handle),
--                    AsyncRetCodePtr => Code'Access
--                 );
--     begin
--        case Result is
--           when SQL_SUCCESS | SQL_SUCCESS_WITH_INFO =>
--              return True;
--           when SQL_STILL_EXECUTING =>
--              return False;
--           when others =>
--              Check (Connection, Result);
--              return False;
--        end case;
--     end Complete_Async;
--
--     function Complete_Async (Command : ODBC_Command) return Boolean is
--        Code   : aliased RETCODE;
--        Result : SQLRETURN :=
--                 SQLCompleteAsync
--                 (  HandleType      => SQL_HANDLE_STMT,
--                    Handle          => SQLHANDLE (Command.Handle),
--                    AsyncRetCodePtr => Code'Access
--                 );
--     begin
--        case Result is
--           when SQL_SUCCESS | SQL_SUCCESS_WITH_INFO =>
--              return True;
--           when SQL_STILL_EXECUTING =>
--              return False;
--           when others =>
--              Check (Command, Result);
--              return False;
--        end case;
--     end Complete_Async;

   procedure Connect
             (  Connection  : in out ODBC_Connection;
                Server_Name : String;
                User_Name   : String;
                Password    : String;
                Auto_Commit : Boolean
             )  is
   begin
      Check
      (  Connection,
         SQLConnect
         (  ConnectionHandle => Connection.Handle,
            ServerName       => To_C (Server_Name),
            NameLength1      => Server_Name'Length,
            UserName         => To_C (User_Name),
            NameLength2      => User_Name'Length,
            Authentication   => To_C (Password),
            NameLength3      => Password'Length
      )  );
      Set_Autocommit (Connection, Auto_Commit);
   end Connect;

   procedure Connect
             (  Connection  : in out ODBC_Connection;
                Server_Name : Wide_String;
                User_Name   : Wide_String;
                Password    : Wide_String;
                Auto_Commit : Boolean
             )  is
   begin
      Check
      (  Connection,
         SQLConnect
         (  ConnectionHandle => Connection.Handle,
            ServerName       => To_C (Server_Name),
            NameLength1      => Server_Name'Length,
            UserName         => To_C (User_Name),
            NameLength2      => User_Name'Length,
            Authentication   => To_C (Password),
            NameLength3      => Password'Length
      )  );
      Set_Autocommit (Connection, Auto_Commit);
   end Connect;

   function Describe_Col
             (  Command : ODBC_Command;
                Column  : Positive
             )  return Column_Description is
      Length         : aliased SQLSMALLINT := 0;
      Column_Size    : aliased SQLULEN     := 0;
      Decimal_Digits : aliased SQLSMALLINT := 0;
      Data_Type      : aliased SQL_DATA_TYPE;
      Nullable       : aliased SQL_NULLABLE_FIELD;
   begin
      Check
      (  Command,
         SQLDescribeCol
         (  StatementHandle  => Command.Handle,
            ColumnNumber     => SQLUSMALLINT (Column),
            NameLengthPtr    => Length'Access,
            DataTypePtr      => Data_Type'Access,
            ColumnSizePtr    => Column_Size'Access,
            DecimalDigitsPtr => Decimal_Digits'Access,
            NullablePtr      => Nullable'Access
        ),
        Filter_07009'Access
     );
     declare
        Buffer : char_array (0..size_t (Length));
     begin
        Check
        (  Command,
           SQLDescribeCol
           (  StatementHandle  => Command.Handle,
              ColumnNumber     => SQLUSMALLINT (Column),
              ColumnName       => Buffer (Buffer'First)'Access,
              BufferLength     => Buffer'Length,
              NameLengthPtr    => Length'Access,
              DataTypePtr      => Data_Type'Access,
              ColumnSizePtr    => Column_Size'Access,
              DecimalDigitsPtr => Decimal_Digits'Access,
              NullablePtr      => Nullable'Access
           ),
           Filter_07009'Access
        );
        declare
           Name : constant String := To_Ada (Buffer);
        begin
           return
           (  Name_Length    => Name'Length,
              Data_Type      => Data_Type,
              Column_Size    => Column_Size,
              Decimal_Digits => Natural (Decimal_Digits),
              Nullable       => Nullable,
              Column_Name    => Name
           );
        end;
     end;
   end Describe_Col;

   function Describe_Param
            (  Command   : ODBC_Command;
               Parameter : Positive
            )  return Param_Description is
      Data_Type      : aliased SQL_DATA_TYPE;
      Parameter_Size : aliased SQLULEN;
      Decimal_Digits : aliased SQLSMALLINT;
      Nullable       : aliased SQL_NULLABLE_FIELD;
   begin
      Check
      (  Command,
         SQLDescribeParam
         (  StatementHandle  => Command.Handle,
            ParameterNumber  => SQLUSMALLINT (Parameter),
            DataTypePtr      => Data_Type'Access,
            ParameterSizePtr => Parameter_Size'Access,
            DecimalDigitsPtr => Decimal_Digits'Access,
            NullablePtr      => Nullable'Access
         ),
         Filter_IM001'Access
      );
      return
      (  Data_Type      => Data_Type,
         Parameter_Size => Parameter_Size,
         Decimal_Digits => Natural (Decimal_Digits),
         Nullable       => Nullable
      );
   end Describe_Param;

   procedure Disable_Tracing (Connection : in out ODBC_Connection) is
   begin
      Set_Trace (Connection, False);
   end Disable_Tracing;

   procedure Drop
             (  Command    : in out ODBC_Command;
                Table_Name : String
             )  is
   begin
      if Table_Exists (Command'Unchecked_Access, Table_Name) then
         Execute (Command, "DROP TABLE " & Table_Name);
      end if;
   end Drop;

   procedure Drop
             (  Connection : in out ODBC_Connection;
                Table_Name : String
             )  is
      Command : ODBC_Command (Connection'Access);
   begin
      Drop (Command, Table_Name);
   end Drop;

   procedure Drop
             (  Command    : in out ODBC_Command;
                Table_Name : Wide_String
             )  is
   begin
      if Table_Exists (Command'Unchecked_Access, Table_Name) then
         Execute (Command, "DROP TABLE " & Table_Name);
      end if;
   end Drop;

   procedure Drop
             (  Connection : in out ODBC_Connection;
                Table_Name : Wide_String
             )  is
      Command : ODBC_Command (Connection'Access);
   begin
      Drop (Command, Table_Name);
   end Drop;

   procedure Enable_Tracing
             (  Connection : in out ODBC_Connection;
                File_Name  : String
             )  is
   begin
      Set_Trace (Connection, False);
      Set_Tracefile (Connection, File_Name);
      Set_Trace (Connection, True);
   end Enable_Tracing;

   procedure Enable_Tracing
             (  Connection : in out ODBC_Connection;
                File_Name  : Wide_String
             )  is
   begin
      Set_Trace (Connection, False);
      Set_Tracefile (Connection, File_Name);
      Set_Trace (Connection, True);
   end Enable_Tracing;

   procedure End_Transaction (Connection : in out ODBC_Connection) is
   begin
      Check
      (  Connection,
         SQLEndTran
         (  SQL_HANDLE_DBC,
            SQLHANDLE (Connection.Handle),
            SQL_COMMIT
      )  );
   end End_Transaction;

   procedure Execute (Command : in out ODBC_Command) is
   begin
      if Command.Connection.Mode = None then
         Raise_Exception
         (  Use_Error'Identity,
            "No access to execute"
         );
      end if;
      Check
      (  Command,
         SQLExecute (Command.Handle),
         Filter_42S02'Access
      );
   end Execute;

   procedure Execute (Command : in out ODBC_Command; Text : String) is
   begin
      if Command.Connection.Mode = None then
         Raise_Exception
         (  Use_Error'Identity,
            "No access to execute"
         );
      end if;
      Check (Command, SQLFreeStmt (Command.Handle, SQL_UNBIND));
      Check (Command, SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS));
      Check
      (  Command,
         SQLExecDirect (Command.Handle, To_C (Text), Text'Length),
         Filter_42S02'Access
      );
   end Execute;

   procedure Execute
             (  Command : in out ODBC_Command;
                Text    : Wide_String
             )  is
   begin
      if Command.Connection.Mode = None then
         Raise_Exception
         (  Use_Error'Identity,
            "No access to execute"
         );
      end if;
      Check (Command, SQLFreeStmt (Command.Handle, SQL_UNBIND));
      Check (Command, SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS));
      Check
      (  Command,
         SQLExecDirect (Command.Handle, To_C (Text), Text'Length),
         Filter_42S02'Access
      );
   end Execute;

   function Execute
            (  Command : access ODBC_Command
            )  return Natural is
   begin
      Execute (Command.all);
      return Row_Count (Command.all);
   end Execute;

   function Execute
            (  Command : access ODBC_Command;
               Text    : String
            )  return Natural is
   begin
      Execute (Command.all, Text);
      return Row_Count (Command.all);
   end Execute;

   function Execute
            (  Command : access ODBC_Command;
               Text    : Wide_String
            )  return Natural is
   begin
      Execute (Command.all, Text);
      return Row_Count (Command.all);
   end Execute;

   procedure Fetch (Command : in out ODBC_Command) is
   begin
      Check (Command, SQLFetch (Command.Handle));
   end Fetch;

   function Fetch (Command : access ODBC_Command)
      return SQLRETURN is
   begin
      return SQLFetch (Command.Handle);
   end Fetch;

   function Filter_All (State : SQLSTATE) return Boolean is
   begin
      return False;
   end Filter_All;

   function Filter_HYC00_HY114 (State : SQLSTATE) return Boolean is
   begin
      if State (1..5) = "HYC00" then
         Raise_Exception
         (  Use_Error'Identity,
            "Optional feature not implemented"
         );
      elsif State (1..5) = "HY114" then
         Raise_Exception
         (  Use_Error'Identity,
            "No connection level asynchronous function execution"
         );
      else
         return True;
      end if;
   end Filter_HYC00_HY114;

   function Filter_HY004 (State : SQLSTATE) return Boolean is
   begin
      if State (1..5) = "HY004" then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Driver does not support this data type"
         );
      else
         return True;
      end if;
   end Filter_HY004;

   function Filter_IM001 (State : SQLSTATE) return Boolean is
   begin
      if State (1..5) = "IM001" then
         Raise_Exception
         (  Use_Error'Identity,
            "Driver does not support this function"
         );
      else
         return True;
      end if;
   end Filter_IM001;

   function Filter_07009 (State : SQLSTATE) return Boolean is
   begin
      if State (1..5) = "07009" then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong column number"
         );
      else
         return True;
      end if;
   end Filter_07009;

   function Filter_42S02 (State : SQLSTATE) return Boolean is
   begin
      if State (1..5) = "42S02" then
         Raise_Exception
         (  Status_Error'Identity,
            "Table does not exist"
         );
      else
         return True;
      end if;
   end Filter_42S02;

   procedure Finalize (Environment : in out ODBC_Environment) is
   begin
      if SQLHANDLE (Environment.Handle) /= SQL_NULL_HANDLE then
         Check
         (  Environment,
            SQLFreeHandle
            (  SQL_HANDLE_ENV,
               SQLHANDLE (Environment.Handle)
            ),
            Filter_All'Access
         );
         Environment.Handle := SQLHENV (SQL_NULL_HANDLE);
      end if;
   exception
      when others =>
         Environment.Handle := SQLHENV (SQL_NULL_HANDLE);
   end Finalize;

   procedure Finalize (Connection : in out ODBC_Connection) is
      Result : SQLRETURN;
   begin
      if SQLHANDLE (Connection.Handle) /= SQL_NULL_HANDLE then
         Result := -- Rollback pending transaction before disconnect
            SQLEndTran
            (  SQL_HANDLE_DBC,
               SQLHANDLE (Connection.Handle),
               SQL_ROLLBACK
            );
         Result := SQLDisconnect (Connection.Handle);
         Result :=
            SQLFreeHandle
            (  SQL_HANDLE_DBC,
               SQLHANDLE (Connection.Handle)
            );
         Connection.Handle := SQLHDBC (SQL_NULL_HANDLE);
      end if;
   exception
      when others =>
         Connection.Handle := SQLHDBC (SQL_NULL_HANDLE);
   end Finalize;

   procedure Finalize (Command : in out ODBC_Command) is
   begin
      if SQLHANDLE (Command.Handle) /= SQL_NULL_HANDLE then
         Check
         (  Command,
            SQLFreeHandle
            (  SQL_HANDLE_STMT,
               SQLHANDLE (Command.Handle)
            ),
            Filter_All'Access
         );
         Command.Handle := SQLHSTMT (SQL_NULL_HANDLE);
      end if;
   exception
      when others =>
         Command.Handle := SQLHSTMT (SQL_NULL_HANDLE);
   end Finalize;

   function Get_Access_Mode
            (  Connection : ODBC_Connection
            )  return SQL_MODE is
   begin
      return
         SQL_MODE
         (  SQLUINTEGER'
            (  Get_Attribute (Connection, SQL_ATTR_ACCESS_MODE)
         )  );
   end Get_Access_Mode;

   function Get_Accessible_Procedures
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_ACCESSIBLE_PROCEDURES);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Accessible_Procedures;

   function Get_Accessible_Tables
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_ACCESSIBLE_TABLES);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Accessible_Tables;

   function Get_Active_Environments
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_ACTIVE_ENVIRONMENTS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Active_Environments;

   function Get_Aggregate_Functions
            (  Connection : ODBC_Connection
            )  return SQL_AF is
   begin
      return
         SQL_AF
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_AGGREGATE_FUNCTIONS)
         )  );
   end Get_Aggregate_Functions;

   function Get_Alter_Domain
            (  Connection : ODBC_Connection
            )  return SQL_AD is
   begin
      return
         SQL_AD
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_ALTER_DOMAIN)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Alter_Domain;

   function Get_Alter_Table
            (  Connection : ODBC_Connection
            )  return SQL_AT is
   begin
      return
         SQL_AT
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_ALTER_DOMAIN)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Alter_Table;

   function Get_Async_DBC_Enable
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
      (  SQL_ASYNC_ENABLE_ON
      =  Get_Attribute
         (  Connection,
            SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE,
            Filter_HYC00_HY114'Access
      )  );
   exception
      when End_Error => -- No data, assume false
         return False;
      when Use_Error =>
         return False;
   end Get_Async_DBC_Enable;

   function Get_Async_Enable
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
      (  SQL_ASYNC_ENABLE_ON
      =  SQLULEN'
         (  Get_Attribute
            (  Connection,
               SQL_ATTR_ASYNC_ENABLE,
               Filter_HYC00_HY114'Access
      )  )  );
   exception
      when End_Error => -- No data, assume false
         return False;
      when Use_Error =>
         return False;
   end Get_Async_Enable;

   function Get_Async_Enable
            (  Command : ODBC_Command
            )  return Boolean is
   begin
      return
      (  SQL_ASYNC_ENABLE_ON
      =  SQLULEN'
         (  Get_Attribute
            (  Command,
               SQL_ATTR_ASYNC_ENABLE,
               Filter_HYC00_HY114'Access
      )  )  );
   exception
      when End_Error => -- No data, assume false
         return False;
      when Use_Error =>
         return False;
   end Get_Async_Enable;

   function Get_Async_DBC_Functions
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
      (  SQL_ASYNC_DBC_CAPABLE
      =  SQLUINTEGER'
         (  Get_Info (Connection, SQL_ALTER_DOMAIN)
      )  );
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Async_DBC_Functions;

   function Get_Async_Mode
            (  Connection : ODBC_Connection
            )  return SQL_AM is
   begin
      return
         SQL_AM (SQLUINTEGER'(Get_Info (Connection, SQL_ASYNC_MODE)));
   exception
      when End_Error => -- No data, assume none
         return SQL_AM_NONE;
   end Get_Async_Mode;

   function Get_Attribute
            (  Connection : ODBC_Connection;
               Attribute  : SQL_ATTR;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return String is
      Length : aliased SQLINTEGER := 0;
   begin
      Check
      (  Connection,
         SQLGetConnectAttr
         (  ConnectionHandle => Connection.Handle,
            Attribute        => Attribute,
            StringLengthPtr  => Length'Access
         ),
         Filter
      );
      if Length <= 0 then
         return "";
      end if;
      declare
         Value : char_array (0..size_t (Length)) := (others => nul);
      begin
         Check
         (  Connection,
            SQLGetConnectAttr
            (  ConnectionHandle => Connection.Handle,
               Attribute        => Attribute,
               ValuePtr         => Value (0)'Access,
               BufferLength     => Value'Length,
               StringLengthPtr  => Length'Access
         )  );
         return To_Ada (Value);
      end;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            (  "The connection attribute"
            &  SQL_ATTR'Image (Attribute)
            &  " is not defined"
         )  );
   end Get_Attribute;

   function Get_Attribute
            (  Environment : ODBC_Environment;
               Attribute   : SQL_ENV_ATTR
            )  return SQLUINTEGER is
      Result : aliased SQLUINTEGER := 0;
      Length : aliased SQLINTEGER  := 0;
   begin
      Check
      (  Environment,
         SQLGetEnvAttr
         (  Environment.Handle,
            Attribute,
            SQLPOINTER (Result'Address),
            SQL_LEN_BINARY_ATTR (SQLUINTEGER_Length),
            Length'Access
      )  );
      return Result;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            (  "The environment attribute"
            &  SQL_ENV_ATTR'Image (Attribute)
            &  " is not defined"
         )  );
   end Get_Attribute;

   function Get_Attribute
            (  Connection : ODBC_Connection;
               Attribute  : SQL_ATTR;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLUINTEGER is
      Result : aliased SQLUINTEGER := 0;
      Length : aliased SQLINTEGER  := 0;
   begin
      Check
      (  Connection,
         SQLGetConnectAttr
         (  Connection.Handle,
            Attribute,
            SQLPOINTER (Result'Address),
            SQL_IS_UINTEGER,
            Length'Access
         ),
         Filter
      );
      return Result;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            (  "The connection attribute"
            &  SQL_ATTR'Image (Attribute)
            &  " is not defined"
         )  );
   end Get_Attribute;

   function Get_Attribute
            (  Connection : ODBC_Connection;
               Attribute  : SQL_ATTR;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLULEN is
      Result : aliased SQLULEN    := 0;
      Length : aliased SQLINTEGER := 0;
   begin
      Check
      (  Connection,
         SQLGetConnectAttr
         (  Connection.Handle,
            Attribute,
            SQLPOINTER (Result'Address),
            SQL_LEN_BINARY_ATTR (SQLULEN_Length),
            Length'Access
         ),
         Filter
      );
      return Result;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            (  "The connection attribute"
            &  SQL_ATTR'Image (Attribute)
            &  " is not defined"
         )  );
   end Get_Attribute;

   function Get_Attribute
            (  Command   : ODBC_Command;
               Attribute : SQL_ATTR;
               Filter    : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLULEN is
      Result : aliased SQLULEN    := 0;
      Length : aliased SQLINTEGER := 0;
   begin
      Check
      (  Command,
         SQLGetStmtAttr
         (  Command.Handle,
            Attribute,
            SQLPOINTER (Result'Address),
            SQL_LEN_BINARY_ATTR (SQLULEN_Length),
            Length'Access
         ),
         Filter
      );
      return Result;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            (  "The statement attribute"
            &  SQL_ATTR'Image (Attribute)
            &  " is not defined"
         )  );
   end Get_Attribute;

   function Get_Attribute
            (  Connection : ODBC_Connection;
               Attribute  : SQL_ATTR;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return Wide_String is
      Length : aliased SQLINTEGER := 0;
   begin
      Check
      (  Connection,
         SQLGetConnectAttr
         (  ConnectionHandle => Connection.Handle,
            Attribute        => Attribute,
            StringLengthPtr  => Length'Access
         ),
         Filter
      );
      if Length <= 0 then
         return "";
      end if;
      declare
         Value : SQLWCHAR_Array (0..size_t (Length)) :=
                    (others => SQLWCHAR'Val (0));
      begin
         Check
         (  Connection,
            SQLGetConnectAttr
            (  ConnectionHandle => Connection.Handle,
               Attribute        => Attribute,
               ValuePtr         => Value (0)'Access,
               BufferLength     => Value'Length * Bytes_In_Wide_Char,
               StringLengthPtr  => Length'Access
         )  );
         return To_Ada (Value);
      end;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            (  "The connection attribute"
            &  SQL_ATTR'Image (Attribute)
            &  " is not defined"
         )  );
   end Get_Attribute;

   function Get_Auto_IPD
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
      (  SQL_TRUE
      =  Get_Attribute
         (  Connection,
            SQL_ATTR_AUTO_IPD,
            Filter_HYC00_HY114'Access
      )  );
   exception
      when Use_Error =>
         return False;
   end Get_Auto_IPD;

   function Get_Autocommit
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
      (  SQL_AUTOCOMMIT_ON
      =  Get_Attribute (Connection, SQL_ATTR_AUTOCOMMIT)
      );
   end Get_Autocommit;

   function Get_Batch_Row_Count
            (  Connection : ODBC_Connection
            )  return SQL_BRC is
   begin
      return
         SQL_BRC
         (  SQLUINTEGER'(Get_Info (Connection, SQL_BATCH_ROW_COUNT))
         );
   end Get_Batch_Row_Count;

   function Get_Batch_Support
            (  Connection : ODBC_Connection
            )  return SQL_BS is
   begin
      return
         SQL_BS
         (  SQLUINTEGER'(Get_Info (Connection, SQL_BATCH_SUPPORT))
         );
   end Get_Batch_Support;

   function Get_Bookmark_Persistence
            (  Connection : ODBC_Connection
            )  return SQL_BP is
   begin
      return
         SQL_BP
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_BOOKMARK_PERSISTENCE)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Bookmark_Persistence;

   function Get_Catalog_Location
            (  Connection : ODBC_Connection
            )  return SQL_CL is
   begin
      return
         SQL_CL
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_CATALOG_LOCATION)
         )  );
   end Get_Catalog_Location;

   function Get_Catalog_Name
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_CATALOG_NAME);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Catalog_Name;

   function Get_Catalog_Name_Separator
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_CATALOG_NAME_SEPARATOR);
   end Get_Catalog_Name_Separator;

   function Get_Catalog_Term
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_CATALOG_TERM);
   exception
      when End_Error => -- No data, assume empty
         return "";
   end Get_Catalog_Term;

   function Get_Catalog_Usage
            (  Connection : ODBC_Connection
            )  return SQL_CU is
   begin
      return
         SQL_CU
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_CATALOG_USAGE)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Catalog_Usage;

   function Get_Class_Origin
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_CLASS_ORIGIN,
                Record_Number => Record_Number
             );
   end Get_Class_Origin;

   function Get_Class_Origin
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_CLASS_ORIGIN,
                Record_Number => Record_Number
             );
   end Get_Class_Origin;

   function Get_Class_Origin
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_CLASS_ORIGIN,
                Record_Number => Record_Number
             );
   end Get_Class_Origin;

   function Get_Collation_Seq
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_COLLATION_SEQ);
   end Get_Collation_Seq;

   function Get_Column_Alias
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_COLUMN_ALIAS);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Column_Alias;

   function Get_Column_Number
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_COLUMN_NUMBER,
                Record_Number => Record_Number
             );
   end Get_Column_Number;

   function Get_Column_Number
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_COLUMN_NUMBER,
                Record_Number => Record_Number
             );
   end Get_Column_Number;

   function Get_Column_Number
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_COLUMN_NUMBER,
                Record_Number => Record_Number
             );
   end Get_Column_Number;

   function Get_Concat_Null_Behavior
            (  Connection : ODBC_Connection
            )  return SQL_CB is
   begin
      return
         SQL_CB
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_CONCAT_NULL_BEHAVIOR)
         )  );
   end Get_Concat_Null_Behavior;

   function Get_Concurrency
            (  Command : ODBC_Command
            )  return SQL_CONCUR is
   begin
      return
         SQL_CONCUR
         (  SQLULEN'
            (  Get_Attribute
               (  Command,
                  SQL_ATTR_CONCURRENCY,
                  Filter_HYC00_HY114'Access
         )  )  );
   end Get_Concurrency;

   function Get_Connection_Dead
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
      (  SQL_CD_TRUE
      =  Get_Attribute (Connection, SQL_ATTR_CONNECTION_DEAD)
      );
   end Get_Connection_Dead;

   function Get_Connection_Name
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_CONNECTION_NAME,
                Record_Number => Record_Number
             );
   end Get_Connection_Name;

   function Get_Connection_Name
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_CONNECTION_NAME,
                Record_Number => Record_Number
             );
   end Get_Connection_Name;

   function Get_Connection_Name
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_CONNECTION_NAME,
                Record_Number => Record_Number
             );
   end Get_Connection_Name;

   function Get_Connection_Pooling
            (  Environment : ODBC_Environment
            )  return SQL_CP is
   begin
      return
         SQL_CP
         (  Get_Attribute (Environment, SQL_ATTR_CONNECTION_POOLING)
         );
   end Get_Connection_Pooling;

   function Get_Connection_Timeout
            (  Connection : ODBC_Connection
            )  return Duration is
   begin
      return
         Duration
         (  SQLUINTEGER'
            (  Get_Attribute (Connection, SQL_ATTR_CONNECTION_TIMEOUT)
         )  );
   end Get_Connection_Timeout;

   function Get_Convert
            (  Connection : ODBC_Connection;
               Data_Type  : SQL_DATA_TYPE
            )  return SQL_CVT is
      Info : SQL_INFO;
   begin
      case Data_Type is
         when SQL_BIGINT        => Info := SQL_CONVERT_BIGINT;
         when SQL_BINARY        => Info := SQL_CONVERT_BINARY;
         when SQL_BIT           => Info := SQL_CONVERT_BIT;
         when SQL_CHAR          => Info := SQL_CONVERT_CHAR;
         when SQL_DATE          => Info := SQL_CONVERT_DATE;
         when SQL_DECIMAL       => Info := SQL_CONVERT_DECIMAL;
         when SQL_DOUBLE        => Info := SQL_CONVERT_DOUBLE;
         when SQL_FLOAT         => Info := SQL_CONVERT_FLOAT;
         when SQL_INTEGER       => Info := SQL_CONVERT_INTEGER;
         when SQL_LONGVARCHAR   => Info := SQL_CONVERT_LONGVARCHAR;
         when SQL_NUMERIC       => Info := SQL_CONVERT_NUMERIC;
         when SQL_REAL          => Info := SQL_CONVERT_REAL;
         when SQL_SMALLINT      => Info := SQL_CONVERT_SMALLINT;
         when SQL_TIME          => Info := SQL_CONVERT_TIME;
         when SQL_TIMESTAMP     => Info := SQL_CONVERT_TIMESTAMP;
         when SQL_TINYINT       => Info := SQL_CONVERT_TINYINT;
         when SQL_VARBINARY     => Info := SQL_CONVERT_VARBINARY;
         when SQL_VARCHAR       => Info := SQL_CONVERT_VARCHAR;
         when SQL_LONGVARBINARY => Info := SQL_CONVERT_LONGVARBINARY;
         when others => return 0;
      end case;
      return SQL_CVT (SQLUINTEGER'(Get_Info (Connection, Info)));
   end Get_Convert;

   function Get_Convert_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_CVT is
   begin
      return
         SQL_FN_CVT
         (  SQLUINTEGER'(Get_Info (Connection, SQL_CONVERT_FUNCTIONS))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Convert_Functions;

   function Get_Correlation_Name
            (  Connection : ODBC_Connection
            )  return SQL_CN is
   begin
      return
         SQL_CN
         (  SQLUSMALLINT'(Get_Info (Connection, SQL_CORRELATION_NAME))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Correlation_Name;

   function Get_CP_Match
            (  Environment : ODBC_Environment
            )  return SQL_CP_MATCH is
   begin
      return
         SQL_CP_MATCH (Get_Attribute (Environment, SQL_ATTR_CP_MATCH));
   end Get_CP_Match;

   function Get_Create_Assertion
            (  Connection : ODBC_Connection
            )  return SQL_CA is
   begin
      return
         SQL_CA
         (  SQLUINTEGER'(Get_Info (Connection, SQL_CREATE_ASSERTION))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Create_Assertion;

   function Get_Create_Character_Set
            (  Connection : ODBC_Connection
            )  return SQL_CCS is
   begin
      return
         SQL_CCS
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_CREATE_CHARACTER_SET)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Create_Character_Set;

   function Get_Create_Collation
            (  Connection : ODBC_Connection
            )  return SQL_CCOL is
   begin
      return
         SQL_CCOL
         (  SQLUINTEGER'(Get_Info (Connection, SQL_CREATE_COLLATION))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Create_Collation;

   function Get_Create_Domain
            (  Connection : ODBC_Connection
            )  return SQL_CDO is
   begin
      return
         SQL_CDO
         (  SQLUINTEGER'(Get_Info (Connection, SQL_CREATE_DOMAIN))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Create_Domain;

   function Get_Create_Schema
            (  Connection : ODBC_Connection
            )  return SQL_CS is
   begin
      return
         SQL_CS
         (  SQLUINTEGER'(Get_Info (Connection, SQL_CREATE_SCHEMA))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Create_Schema;

   function Get_Create_Table
            (  Connection : ODBC_Connection
            )  return SQL_CT is
   begin
      return
         SQL_CT
         (  SQLUINTEGER'(Get_Info (Connection, SQL_CREATE_TABLE))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Create_Table;

   function Get_Create_Translation
            (  Connection : ODBC_Connection
            )  return SQL_CTR is
   begin
      return
         SQL_CTR
         (  SQLUINTEGER'(Get_Info (Connection, SQL_CREATE_TRANSLATION))
         );
   exception
      when End_Error => -- No data, assume -
         return 0;
   end Get_Create_Translation;

   function Get_Create_View
            (  Connection : ODBC_Connection
            )  return SQL_CV is
   begin
      return
         SQL_CV (SQLUINTEGER'(Get_Info (Connection, SQL_CREATE_VIEW)));
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Create_View;

   function Get_Current_Catalog
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Attribute (Connection, SQL_ATTR_CURRENT_CATALOG);
   exception
      when End_Error => -- No data, assume none
         return "";
   end Get_Current_Catalog;

   function Get_Current_Catalog
            (  Connection : ODBC_Connection
            )  return Wide_String is
   begin
      return Get_Attribute (Connection, SQL_ATTR_CURRENT_CATALOG);
   exception
      when End_Error => -- No data, assume none
         return "";
   end Get_Current_Catalog;

   function Get_Cursor_Commit_Behavior
            (  Connection : ODBC_Connection
            )  return SQL_CCB is
   begin
      return
         SQL_CCB
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_CURSOR_COMMIT_BEHAVIOR)
         )  );
   end Get_Cursor_Commit_Behavior;

   function Get_Cursor_Rollback_Behavior
            (  Connection : ODBC_Connection
            )  return SQL_CCB is
   begin
      return
         SQL_CCB
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_CURSOR_ROLLBACK_BEHAVIOR)
         )  );
   end Get_Cursor_Rollback_Behavior;

   function Get_Cursor_Row_Count
             (  Command       : ODBC_Command;
                Record_Number : Positive := 1
            )  return SQLLEN is
      Count  : aliased SQLLEN      := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Command,
         SQLGetDiagField
         (  HandleType      => SQL_HANDLE_STMT,
            Handle          => SQLHANDLE (Command.Handle),
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => SQL_DIAG_CURSOR_ROW_COUNT,
            DiagInfoPtr     => SQLPOINTER (Count'Address),
            BufferLength    => SQL_IS_UINTEGER,
            StringLengthPtr => Length'Access
      )  );
       return Count;
   end Get_Cursor_Row_Count;

   function Get_Cursor_Scrollable
            (  Command : ODBC_Command
            )  return Boolean is
   begin
      return
      (  SQL_SCROLLABLE
      =  SQLULEN'
         (  Get_Attribute
            (  Command,
               SQL_ATTR_CURSOR_SCROLLABLE,
               Filter_HYC00_HY114'Access
      )  )  );
   exception
      when End_Error => -- No data, assume false
         return False;
      when Use_Error =>
         return False;
   end Get_Cursor_Scrollable;

   function Get_Cursor_Sensitivity
            (  Connection : ODBC_Connection
            )  return SQL_SENSITIVITY is
   begin
      return
         SQL_SENSITIVITY
         (  SQLUINTEGER'(Get_Info (Connection, SQL_CURSOR_SENSITIVITY))
         );
   exception
      when End_Error => -- No data, assume unspecified
         return SQL_UNSPECIFIED;
   end Get_Cursor_Sensitivity;

   function Get_Cursor_Sensitivity
            (  Command : ODBC_Command
            )  return SQL_SENSITIVITY is
   begin
      return
         SQL_SENSITIVITY
         (  SQLULEN'
            (  Get_Attribute
               (  Command,
                  SQL_ATTR_CURSOR_SENSITIVITY,
                  Filter_HYC00_HY114'Access
         )  )  );
   exception
      when End_Error => -- No data, assume unspecified
         return SQL_UNSPECIFIED;
      when Use_Error =>
         return SQL_UNSPECIFIED;
   end Get_Cursor_Sensitivity;

   function Get_Cursor_Type
            (  Command : ODBC_Command
            )  return SQL_CURSOR is
   begin
      return
         SQL_CURSOR
         (  SQLULEN'
            (  Get_Attribute
               (  Command,
                  SQL_ATTR_CURSOR_TYPE,
                  Filter_HYC00_HY114'Access
         )  )  );
   end Get_Cursor_Type;

   pragma Assert (Stream_Element'Size = 8);

   procedure Get_Data
             (  Command       : in out ODBC_Command;
                Destination   : in out String;
                Pointer       : in out Integer;
                Column        : Positive;
                Finish        : Cursor_Disposition;
                Null_As_Empty : Boolean := True
             )  is
      Length : aliased SQLLEN := 0;
      Code   : SQLRETURN;
   begin
      if (  Pointer < Destination'First
         or else
            Pointer - 1 > Destination'Last
         )
      then
         Raise_Exception (Layout_Error'Identity, "Wrong pointer");
      end if;
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   =>
               SQLPOINTER (Destination (Pointer)'Address),
            BufferLength     => SQLLEN (Destination'Last - Pointer) + 1,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            if Length = SQL_NULL_DATA then
               if not Null_As_Empty then
                  Raise_Exception
                  (  End_Error'Identity,
                     "No data set"
                  );
               end if;
            end if;
         when SQL_SUCCESS =>
            if Length = SQL_NULL_DATA then
               if not Null_As_Empty then
                  Raise_Exception
                  (  End_Error'Identity,
                     "No data set"
                  );
               end if;
            end if;
            Pointer := Pointer + Natural (Length);
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception
            (  Layout_Error'Identity,
               "No room for data"
            );
         when others =>
            Check (Command, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
      end case;
      if Finish = Always then
         Close_Cursor (Command);
      end if;
   exception
      when End_Error =>
         if Finish in On_No_Result..Always then
            Close_Cursor (Command);
         end if;
         raise;
      when others =>
         if Finish in On_Error..Always then
            Close_Cursor (Command);
         end if;
         raise;
   end Get_Data;

   procedure Get_Data
             (  Command       : in out ODBC_Command;
                Stream        : in out Root_Stream_Type'Class;
                Column        : Positive;
                Finish        : Cursor_Disposition;
                Null_As_Empty : Boolean  := True;
                Block_Size    : Stream_Element_Count :=
                                   Default_Block_Size
             )  is
      Data   : aliased Stream_Element_Array (1..Block_Size + 1);
      Length : aliased SQLLEN;
      Code   : SQLRETURN;
   begin
      loop
         Length := 0;
         Code :=
            SQLGetData
            (  StatementHandle  => Command.Handle,
               Col_or_Param_Num => SQLUSMALLINT (Column),
               TargetType       => SQL_C_CHAR,
               TargetValuePtr   => SQLPOINTER (Data (1)'Address),
               BufferLength     => SQLLEN (Block_Size) + 1,
               StrLen_or_IndPtr => Length'Access
            );
         case Code is
            when SQL_NO_DATA =>
               if Length = SQL_NULL_DATA then
                  if not Null_As_Empty then
                     Raise_Exception
                     (  End_Error'Identity,
                        "No data set"
                     );
                  end if;
               end if;
               exit;
            when SQL_SUCCESS =>
               if Length = SQL_NULL_DATA then
                  if not Null_As_Empty then
                     Raise_Exception
                     (  End_Error'Identity,
                        "No data set"
                     );
                  end if;
               end if;
               Write (Stream, Data (1..Stream_Element_Offset (Length)));
               exit;
            when SQL_SUCCESS_WITH_INFO =>
               Write
               (  Stream,
                  Data (1..Stream_Element_Offset (Block_Size))
               );
            when others =>
               Check (Command, Code);
               Raise_Exception (Data_Error'Identity, "Internal error");
         end case;
      end loop;
      if Finish = Always then
         Close_Cursor (Command);
      end if;
   exception
      when End_Error =>
         if Finish in On_No_Result..Always then
            Close_Cursor (Command);
         end if;
         raise;
      when others =>
         if Finish in On_Error..Always then
            Close_Cursor (Command);
         end if;
         raise;
   end Get_Data;

   function Get_Data
            (  Command       : access ODBC_Command;
               Column        : Positive;
               Finish        : Cursor_Disposition;
               Null_As_Empty : Boolean  := True;
               Block_Size    : Positive := Default_Block_Size
            )  return String is
      type Block;
      type Block_Ptr is access all Block;
      type Block is record
         Data : String (1..Block_Size + 1);
         Next : Block_Ptr;
      end record;
      Buffer : aliased Block;
      procedure Free is
         procedure Free is
            new Ada.Unchecked_Deallocation (Block, Block_Ptr);
         This : Block_Ptr := Buffer.Next;
         Next : Block_Ptr;
      begin
         while This /= null loop
            Next := This.Next;
            Free (This);
            This := Next;
         end loop;
      end Free;
      Size   : Natural := 0;
      Length : aliased SQLLEN;
      This   : Block_Ptr := Buffer'Access;
      Code   : SQLRETURN;
   begin
      loop
         Length := 0;
         Code :=
            SQLGetData
            (  StatementHandle  => Command.Handle,
               Col_or_Param_Num => SQLUSMALLINT (Column),
               TargetType       => SQL_C_CHAR,
               TargetValuePtr   => SQLPOINTER (This.Data (1)'Address),
               BufferLength     => SQLLEN (Block_Size + 1),
               StrLen_or_IndPtr => Length'Access
            );
         case Code is
            when SQL_NO_DATA =>
               if Length = SQL_NULL_DATA then
                  if Null_As_Empty then
                     Size := 0;
                  else
                     Raise_Exception
                     (  End_Error'Identity,
                        "No data set"
                     );
                  end if;
               end if;
               exit;
            when SQL_SUCCESS =>
               if Length = SQL_NULL_DATA then
                  if Null_As_Empty then
                     Length := 0;
                  else
                     Raise_Exception
                     (  End_Error'Identity,
                        "No data set"
                     );
                  end if;
               end if;
               Size := Size + Natural (Length);
               exit;
            when SQL_SUCCESS_WITH_INFO =>
               Size := Size + Block_Size;
               This.Next := new Block;
               This := This.Next;
            when others =>
               Free;
               Check (Command.all, Code);
               Raise_Exception (Data_Error'Identity, "Internal error");
         end case;
      end loop;
      if Finish = Always then
         Close_Cursor (Command.all);
      end if;
      declare
         Result  : String (1..Size);
         Pointer : Integer := Result'First;
      begin
         This := Buffer'Access;
         while This.Next /= null loop
            Result (Pointer..Pointer + (Block_Size - 1)) :=
               This.Data (1..Block_Size);
            Pointer := Pointer + Block_Size;
            Size := Size - Block_Size;
            This := This.Next;
         end loop;
         Result (Pointer..Pointer + (Size - 1)) := This.Data (1..Size);
         Free;
         return Result;
      end;
   exception
      when End_Error =>
         if Finish in On_No_Result..Always then
            Close_Cursor (Command.all);
         end if;
         Free;
         raise;
      when others =>
         if Finish in On_Error..Always then
            Close_Cursor (Command.all);
         end if;
         Free;
         raise;
   end Get_Data;

   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return Time is
      Length : aliased SQLLEN := SQLTIMESTAMP_Length;
      Result : aliased SQL_TIMESTAMP_STRUCT;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_TIMESTAMP,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLTIMESTAMP_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return To_Time (Result);
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity, "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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
            (  Command       : access ODBC_Command;
               Column        : Positive;
               Finish        : Cursor_Disposition;
               Null_As_Empty : Boolean  := True;
               Block_Size    : Positive := Default_Block_Size
            )  return Wide_String is
      type Block;
      type Block_Ptr is access all Block;
      type Block is record
         Data : Wide_String (1..Block_Size + 1);
         Next : Block_Ptr;
      end record;
      Buffer : aliased Block;
      procedure Free is
         procedure Free is
            new Ada.Unchecked_Deallocation (Block, Block_Ptr);
         This : Block_Ptr := Buffer.Next;
         Next : Block_Ptr;
      begin
         while This /= null loop
            Next := This.Next;
            Free (This);
            This := Next;
         end loop;
      end Free;
      Size   : Natural := 0;
      Length : aliased SQLLEN;
      This   : Block_Ptr := Buffer'Access;
      Code   : SQLRETURN;
   begin
      loop
         Length := 0;
         Code :=
            SQLGetData
            (  StatementHandle  => Command.Handle,
               Col_or_Param_Num => SQLUSMALLINT (Column),
               TargetType       => SQL_C_WCHAR,
               TargetValuePtr   => SQLPOINTER (This.Data (1)'Address),
               BufferLength     => SQLLEN (Block_Size + 1) *
                                   Bytes_In_Wide_Char,
               StrLen_or_IndPtr => Length'Access
            );
         case Code is
            when SQL_NO_DATA =>
               if Length = SQL_NULL_DATA then
                  if Null_As_Empty then
                     Size := 0;
                  else
                     Raise_Exception
                     (  End_Error'Identity,
                        "No data set"
                     );
                  end if;
               end if;
               exit;
            when SQL_SUCCESS =>
               if Length = SQL_NULL_DATA then
                  if Null_As_Empty then
                     Length := 0;
                  else
                     Raise_Exception
                     (  End_Error'Identity,
                        "No data set"
                     );
                  end if;
               end if;
               Size := Size + Natural (Length);
               exit;
            when SQL_SUCCESS_WITH_INFO =>
               Size := Size + Block_Size;
               This.Next := new Block;
               This := This.Next;
            when others =>
               Free;
               Check (Command.all, Code);
               Raise_Exception (Data_Error'Identity, "Internal error");
         end case;
      end loop;
      if Finish = Always then
         Close_Cursor (Command.all);
      end if;
      declare
         Result  : Wide_String (1..Size);
         Pointer : Integer := Result'First;
      begin
         This := Buffer'Access;
         while This.Next /= null loop
            Result (Pointer..Pointer + (Block_Size - 1)) :=
               This.Data (1..Block_Size);
            Pointer := Pointer + Block_Size;
            Size := Size - Block_Size;
            This := This.Next;
         end loop;
         Result (Pointer..Pointer + (Size - 1)) := This.Data (1..Size);
         Free;
         return Result;
      end;
   exception
      when End_Error =>
         if Finish in On_No_Result..Always then
            Close_Cursor (Command.all);
         end if;
         Free;
         raise;
      when others =>
         if Finish in On_Error..Always then
            Close_Cursor (Command.all);
         end if;
         Free;
         raise;
   end Get_Data;

   function Get_Data
            (  Command : access ODBC_Command;
               Column  : Positive;
               Finish  : Cursor_Disposition
            )  return SQLSMALLINT is
      Length : aliased SQLLEN      := SQLSMALLINT_Length;
      Result : aliased SQLSMALLINT := 0;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_SHORT,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLSMALLINT_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity,  "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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
            )  return SQLUSMALLINT is
      Length : aliased SQLLEN       := SQLUSMALLINT_Length;
      Result : aliased SQLUSMALLINT := 0;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_USHORT,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLUSMALLINT_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity,  "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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
            )  return SQLINTEGER is
      Length : aliased SQLLEN     := SQLINTEGER_Length;
      Result : aliased SQLINTEGER := 0;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_SLONG,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLINTEGER_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity,  "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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
            )  return SQLUINTEGER is
      Length : aliased SQLLEN      := SQLUINTEGER_Length;
      Result : aliased SQLUINTEGER := 0;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_ULONG,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLUINTEGER_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity,  "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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
      Length : aliased SQLLEN    := SQLBIGINT_Length;
      Result : aliased SQLBIGINT := 0;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_SBIGINT,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLBIGINT_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity,  "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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
            )  return SQLUBIGINT is
      Length : aliased SQLLEN     := SQLUBIGINT_Length;
      Result : aliased SQLUBIGINT := 0;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_UBIGINT,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLUBIGINT_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity,  "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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
            )  return SQLTINYINT is
      Length : aliased SQLLEN     := SQLTINYINT_Length;
      Result : aliased SQLTINYINT := 0;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_TINYINT,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLTINYINT_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity,  "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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
            )  return SQLUTINYINT is
      Length : aliased SQLLEN      := SQLUTINYINT_Length;
      Result : aliased SQLUTINYINT := 0;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_UTINYINT,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLUTINYINT_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity,  "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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
            )  return SQLDOUBLE is
      Length : aliased SQLLEN    := SQLUTINYINT_Length;
      Result : aliased SQLDOUBLE := 0.0;
      Code   : SQLRETURN;
   begin
      Code :=
         SQLGetData
         (  StatementHandle  => Command.Handle,
            Col_or_Param_Num => SQLUSMALLINT (Column),
            TargetType       => SQL_C_DOUBLE,
            TargetValuePtr   => SQLPOINTER (Result'Address),
            BufferLength     => SQLDOUBLE_Length,
            StrLen_or_IndPtr => Length'Access
         );
      case Code is
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No data set");
         when SQL_SUCCESS =>
            if Finish = Always then
               Close_Cursor (Command.all);
            end if;
            return Result;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception (Data_Error'Identity,  "Data overrun");
         when others =>
            Check (Command.all, Code);
            Raise_Exception (Data_Error'Identity, "Internal error");
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

   function Get_Data_Source_Name
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_DATA_SOURCE_NAME);
   end Get_Data_Source_Name;

   function Get_Data_Source_Name
            (  Connection : ODBC_Connection
            )  return Wide_String is
   begin
      return Get_Info (Connection, SQL_DATA_SOURCE_NAME);
   end Get_Data_Source_Name;

   function Get_Data_Source_Readonly
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_DATA_SOURCE_READ_ONLY);
   end Get_Data_Source_Readonly;

   function Get_Database_Name
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_DATABASE_NAME);
   end Get_Database_Name;

   function Get_Database_Name
            (  Connection : ODBC_Connection
            )  return Wide_String is
   begin
      return Get_Info (Connection, SQL_DATABASE_NAME);
   end Get_Database_Name;

   function Get_Datetime_Literals
            (  Connection : ODBC_Connection
            )  return SQL_DL is
   begin
      return
         SQL_DL
         (  SQLUINTEGER'(Get_Info (Connection, SQL_DATABASE_NAME))
         );
   end Get_Datetime_Literals;

   function Get_DBMS_Name
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_DBMS_NAME);
   end Get_DBMS_Name;

   function Get_DBMS_Name
            (  Connection : ODBC_Connection
            )  return Wide_String is
   begin
      return Get_Info (Connection, SQL_DBMS_NAME);
   end Get_DBMS_Name;

   function Get_DBMS_Version
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_DBMS_VER);
   end Get_DBMS_Version;

   function Get_DDL_Index
            (  Connection : ODBC_Connection
            )  return SQL_DI is
   begin
      return
         SQL_DI (SQLUINTEGER'(Get_Info (Connection, SQL_DDL_INDEX)));
   end Get_DDL_Index;

   function Get_Default_TXN_Isolation
            (  Connection : ODBC_Connection
            )  return SQL_TXN is
   begin
      return
         SQL_TXN
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_DEFAULT_TXN_ISOLATION)
         )  );
   end Get_Default_TXN_Isolation;

   function Get_Describe_Parameter
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_DESCRIBE_PARAMETER);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Describe_Parameter;

   function Get_DM_Version
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_DM_VER);
   end Get_DM_Version;

   function Get_Driver_Name
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_DRIVER_NAME);
   end Get_Driver_Name;

   function Get_Driver_Name
            (  Connection : ODBC_Connection
            )  return Wide_String is
   begin
      return Get_Info (Connection, SQL_DRIVER_NAME);
   end Get_Driver_Name;

   function Get_Driver_ODBC_Ver
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_DRIVER_ODBC_VER);
   end Get_Driver_ODBC_Ver;

   function Get_Driver_Ver
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_DRIVER_VER);
   end Get_Driver_Ver;

   function Get_Drop_Assertion
            (  Connection : ODBC_Connection
            )  return SQL_DA is
   begin
      return
         SQL_DA
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_DROP_ASSERTION)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Drop_Assertion;

   function Get_Drop_Character_Set
            (  Connection : ODBC_Connection
            )  return SQL_DCS is
   begin
      return
         SQL_DCS
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_DROP_CHARACTER_SET)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Drop_Character_Set;

   function Get_Drop_Collation
            (  Connection : ODBC_Connection
            )  return SQL_DC is
   begin
      return
         SQL_DC
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_DROP_COLLATION)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Drop_Collation;

   function Get_Drop_Domain
            (  Connection : ODBC_Connection
            )  return SQL_DD is
   begin
      return
         SQL_DD (SQLUINTEGER'(Get_Info (Connection, SQL_DROP_DOMAIN)));
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Drop_Domain;

   function Get_Drop_Schema
            (  Connection : ODBC_Connection
            )  return SQL_DS is
   begin
      return
         SQL_DS (SQLUINTEGER'(Get_Info (Connection, SQL_DROP_SCHEMA)));
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Drop_Schema;

   function Get_Drop_Table
            (  Connection : ODBC_Connection
            )  return SQL_DT is
   begin
      return
         SQL_DT (SQLUINTEGER'(Get_Info (Connection, SQL_DROP_TABLE)));
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Drop_Table;

   function Get_Drop_Translation
            (  Connection : ODBC_Connection
            )  return SQL_DTR is
   begin
      return
         SQL_DTR
         (  SQLUINTEGER'(Get_Info (Connection, SQL_DROP_TRANSLATION))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Drop_Translation;

   function Get_Drop_View
            (  Connection : ODBC_Connection
            )  return SQL_DV is
   begin
      return
         SQL_DV
         (  SQLUINTEGER'(Get_Info (Connection, SQL_DROP_VIEW))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Drop_View;

   function Get_Dynamic_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA1 is
   begin
      return
         SQL_CA1
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_DYNAMIC_CURSOR_ATTRIBUTES1)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Dynamic_Cursor_Attributes;

   function Get_Dynamic_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA2 is
   begin
      return
         SQL_CA2
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_DYNAMIC_CURSOR_ATTRIBUTES2)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Dynamic_Cursor_Attributes;

   function Get_Dynamic_Function
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String is
   begin
      return
         Get_Field
         (  SQL_HANDLE_STMT,
            SQLHANDLE (Command.Handle),
            SQL_DIAG_DYNAMIC_FUNCTION,
            Record_Number
         );
   end Get_Dynamic_Function;

   function Get_Dynamic_Function
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return Wide_String is
   begin
      return
         Get_Field
         (  SQL_HANDLE_STMT,
            SQLHANDLE (Command.Handle),
            SQL_DIAG_DYNAMIC_FUNCTION,
            Record_Number
         );
   end Get_Dynamic_Function;

   function Get_Dynamic_Function_Code
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
      Result : aliased SQLINTEGER  := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Command,
         SQLGetDiagField
         (  HandleType      => SQL_HANDLE_STMT,
            Handle          => SQLHANDLE (Command.Handle),
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => SQL_DIAG_DYNAMIC_FUNCTION_CODE,
            DiagInfoPtr     => SQLPOINTER (Result'Address),
            BufferLength    => SQL_IS_INTEGER,
            StringLengthPtr => Length'Access
      )  );
      return Result;
   end Get_Dynamic_Function_Code;

   function Get_Enable_Auto_IPD
            (  Command : ODBC_Command
            )  return Boolean is
   begin
      return
      (  SQL_TRUE
      =  SQLULEN'
         (  Get_Attribute
            (  Command,
               SQL_ATTR_ENABLE_AUTO_IPD,
               Filter_HYC00_HY114'Access
      )  )  );
   exception
      when End_Error => -- No data, assume false
         return False;
      when Use_Error =>
         return False;
   end Get_Enable_Auto_IPD;

   function Get_Execution_Mode
            (  Connection : ODBC_Connection
            )  return Execution_Mode is
   begin
      return Connection.Mode;
   end Get_Execution_Mode;

   function Get_Expressions_In_Orderby
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_EXPRESSIONS_IN_ORDERBY);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Expressions_In_Orderby;

   function Get_File_Usage
            (  Connection : ODBC_Connection
            )  return SQL_FILE is
   begin
      return
         SQL_FILE
         (  SQLUSMALLINT'(Get_Info (Connection, SQL_FILE_USAGE))
         );
   end Get_File_Usage;

   function Get_Forward_Only_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA1 is
   begin
      return
         SQL_CA1
         (  SQLUINTEGER'
            (  Get_Info
               (  Connection,
                  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1
         )  )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Forward_Only_Cursor_Attributes;

   function Get_Forward_Only_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA2 is
   begin
      return
         SQL_CA2
         (  SQLUINTEGER'
            (  Get_Info
               (  Connection,
                  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2
         )  )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Forward_Only_Cursor_Attributes;

   function Get_Field
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               Diag_Field    : SQL_DIAG;
               Record_Number : Positive := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return SQLINTEGER is
      Result : aliased SQLINTEGER  := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  SQLGetDiagField
         (  HandleType      => Handle_Type,
            Handle          => Handle,
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => Diag_Field,
            DiagInfoPtr     => SQLPOINTER (Result'Address),
            BufferLength    => SQL_IS_INTEGER,
            StringLengthPtr => Length'Access
         ),
         Handle_Type,
         Handle,
         Filter
      );
      return Result;
   end Get_Field;

   function Get_Field
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               Diag_Field    : SQL_DIAG;
               Record_Number : Positive := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return SQLLEN is
      Result : aliased SQLLEN      := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  SQLGetDiagField
         (  HandleType      => Handle_Type,
            Handle          => Handle,
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => Diag_Field,
            DiagInfoPtr     => SQLPOINTER (Result'Address),
            BufferLength    => SQL_LEN_BINARY_ATTR (SQLLEN_Length),
            StringLengthPtr => Length'Access
         ),
         Handle_Type,
         Handle,
         Filter
      );
      return Result;
   end Get_Field;

   function Get_Field
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               Diag_Field    : SQL_DIAG;
               Record_Number : Positive := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return String is
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  SQLGetDiagField
         (  HandleType      => Handle_Type,
            Handle          => Handle,
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => Diag_Field,
            StringLengthPtr => Length'Access
         ),
         Handle_Type,
         Handle,
         Filter
      );
      if Length <= 0 then
         return "";
      end if;
      declare
         Result : char_array (0..size_t (Length)) := (others => nul);
      begin
         Check
         (  SQLGetDiagField
            (  HandleType      => Handle_Type,
               Handle          => Handle,
               RecNumber       => SQLSMALLINT (Record_Number),
               DiagIdentifier  => Diag_Field,
               DiagInfoPtr     => Result (0)'Access,
               BufferLength    => Result'Length,
               StringLengthPtr => Length'Access
            ),
            Handle_Type,
            Handle,
            Filter
         );
         return To_Ada (Result);
      end;
   end Get_Field;

   function Get_Field
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               Diag_Field    : SQL_DIAG;
               Record_Number : Positive := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return Wide_String is
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  SQLGetDiagField
         (  HandleType      => Handle_Type,
            Handle          => Handle,
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => Diag_Field,
            StringLengthPtr => Length'Access
         ),
         Handle_Type,
         Handle,
         Filter
      );
      if Length <= 0 then
         return "";
      end if;
      declare
         Result : SQLWCHAR_Array (0..size_t (Length)) :=
                     (others => SQLWCHAR'Val (0));
      begin
         Check
         (  SQLGetDiagField
            (  HandleType      => Handle_Type,
               Handle          => Handle,
               RecNumber       => SQLSMALLINT (Record_Number),
               DiagIdentifier  => Diag_Field,
               DiagInfoPtr     => Result (0)'Access,
               BufferLength    => Result'Length * 2,
               StringLengthPtr => Length'Access
            ),
            Handle_Type,
            Handle,
            Filter
         );
         return To_Ada (Result);
      end;
   end Get_Field;

   function Get_First_DSN
            (  Environment   : ODBC_Environment;
               DSN           : DSN_Type;
               Buffer_Length : Positive := Default_Block_Size
            )  return DSN_Description is
      Result             : SQLRETURN;
      Direction          : SQL_DIRECTION;
      Name_Length        : aliased SQLSMALLINT;
      Description_Length : aliased SQLSMALLINT;
      Name               : char_array (0..SQL_MAX_DSN_LENGTH);
      Description        : char_array (0..size_t (Buffer_Length));
   begin
      case DSN is
         when Any_DSN =>
            Direction := SQL_FETCH_FIRST;
         when User_DSN =>
            Direction := SQL_FETCH_FIRST_USER;
         when System_DSN =>
            Direction := SQL_FETCH_FIRST_System;
      end case;
      Result :=
         SQLDataSources
         (  EnvironmentHandle => Environment.Handle,
            Direction         => Direction,
            ServerName        => Name (0)'Access,
            BufferLength1     => Name'Length,
            NameLength1Ptr    => Name_Length'Access,
            Description       => Description (0)'Access,
            BufferLength2     => Description'Length,
            NameLength2Ptr    => Description_Length'Access
         );
      case Result is
         when SQL_SUCCESS =>
            declare
               DSN_Name        : constant String := To_Ada (Name);
               DSN_Description : constant String :=
                                    To_Ada (Description);
            begin
               return
               (  DSN_Name'Length,
                  DSN_Description'Length,
                  DSN_Name,
                  DSN_Description
               );
            end;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "Buffer for DSN description is too small"
            );
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No more DSN");
         when others =>
            Check (Environment, Result);
            return (0, 0, "", "");
      end case;
   end Get_First_DSN;

   function Get_Functions
            (  Connection  : ODBC_Connection;
               Function_ID : SQL_API
            )  return Boolean is
      Value : aliased SQLUSMALLINT;
   begin
      Check
      (  Connection,
         SQLGetFunctions
         (  ConnectionHandle => Connection.Handle,
            FunctionId       => Function_ID,
            SupportedPtr     => Value'Access
      )  );
      return SQL_TRUE = Value;
   end Get_Functions;

   function Get_Getdata_Extensions
            (  Connection : ODBC_Connection
            )  return SQL_GD is
   begin
      return
         SQL_GD
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_GETDATA_EXTENSIONS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Getdata_Extensions;

   function Get_Group_By
            (  Connection : ODBC_Connection
            )  return SQL_GR is
   begin
      return
         SQL_GR
         (  SQLUSMALLINT'(Get_Info (Connection, SQL_GROUP_BY))
         );
   exception
      when End_Error => -- No data, assume not supported
         return SQL_GB_NOT_SUPPORTED;
   end Get_Group_By;

   function Get_Identifier_Case
            (  Connection : ODBC_Connection
            )  return SQL_IC is
   begin
      return
         SQL_IC
         (  SQLUSMALLINT'(Get_Info (Connection, SQL_IDENTIFIER_CASE))
         );
   end Get_Identifier_Case;

   function Get_Identifier_Quote_Char
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_IDENTIFIER_QUOTE_CHAR);
   end Get_Identifier_Quote_Char;

   function Get_Index_Keywords
            (  Connection : ODBC_Connection
            )  return SQL_IK is
   begin
      return
         SQL_IK
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_INDEX_KEYWORDS)
         )  );
   exception
      when End_Error => -- No data, assume none
         return SQL_IK_NONE;
   end Get_Index_Keywords;

   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLUSMALLINT is
      Length : aliased SQLSMALLINT := 0;
      Data   : aliased SQLUSMALLINT;
   begin
      Check
      (  Connection,
         SQLGetInfo
         (  ConnectionHandle => Connection.Handle,
            InfoType         => Info_Type,
            InfoValuePtr     => SQLPOINTER (Data'Address),
            BufferLength     => SQLUSMALLINT_Length,
            StringLengthPtr  => Length'Access
         ),
         Filter
      );
      return Data;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            "No connection info type" & SQL_INFO'Image (Info_Type)
         );
   end Get_Info;

   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLUINTEGER is
      Length : aliased SQLSMALLINT := 0;
      Data   : aliased SQLUINTEGER;
   begin
      Check
      (  Connection,
         SQLGetInfo
         (  ConnectionHandle => Connection.Handle,
            InfoType         => Info_Type,
            InfoValuePtr     => SQLPOINTER (Data'Address),
            BufferLength     => SQLUINTEGER_Length,
            StringLengthPtr  => Length'Access
         ),
         Filter
      );
      return Data;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            "No connection info type" & SQL_INFO'Image (Info_Type)
         );
   end Get_Info;

   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return SQLULEN is
      Length : aliased SQLSMALLINT := 0;
      Data   : aliased SQLULEN;
   begin
      Check
      (  Connection,
         SQLGetInfo
         (  ConnectionHandle => Connection.Handle,
            InfoType         => Info_Type,
            InfoValuePtr     => SQLPOINTER (Data'Address),
            BufferLength     => SQLULEN_Length,
            StringLengthPtr  => Length'Access
         ),
         Filter
      );
      return Data;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            "No connection info type" & SQL_INFO'Image (Info_Type)
         );
   end Get_Info;

   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return String is
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Connection,
         SQLGetInfo
         (  ConnectionHandle => Connection.Handle,
            InfoType         => Info_Type,
            StringLengthPtr  => Length'Access
         ),
         Filter
      );
      if Length <= 0 then
         return "";
      end if;
      declare
         Text : char_array (0..size_t (Length)) := (others => nul);
      begin
         Check
         (  Connection,
            SQLGetInfo
            (  ConnectionHandle => Connection.Handle,
               InfoType         => Info_Type,
               InfoValuePtr     => Text (0)'Access,
               BufferLength     => Text'Length,
               StringLengthPtr  => Length'Access
            ),
            Filter
         );
         return To_Ada (Text);
      end;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            "No connection info type" & SQL_INFO'Image (Info_Type)
         );
   end Get_Info;

   function Get_Info
            (  Connection : ODBC_Connection;
               Info_Type  : SQL_INFO;
               Filter     : Filter_Ptr := Filter_HYC00_HY114'Access
            )  return Wide_String is
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Connection,
         SQLGetInfo
         (  ConnectionHandle => Connection.Handle,
            InfoType         => Info_Type,
            StringLengthPtr  => Length'Access
         ),
         Filter
      );
      if Length <= 0 then
         return "";
      end if;
      declare
         Text : SQLWCHAR_Array (0..size_t (Length)) :=
                   (others => SQLWCHAR'Val (0));
      begin
         Check
         (  Connection,
            SQLGetInfo
            (  ConnectionHandle => Connection.Handle,
               InfoType         => Info_Type,
               InfoValuePtr     => Text (0)'Access,
               BufferLength     => Text'Length * Bytes_In_Wide_Char,
               StringLengthPtr  => Length'Access
            ),
            Filter
         );
         return To_Ada (Text);
      end;
   exception
      when End_Error =>
         Raise_Exception
         (  End_Error'Identity,
            "No connection info type" & SQL_INFO'Image (Info_Type)
         );
   end Get_Info;

   function Get_Info_Schema_Views
            (  Connection : ODBC_Connection
            )  return SQL_ISV is
   begin
      return
         SQL_ISV
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_INFO_SCHEMA_VIEWS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Info_Schema_Views;

   function Get_Insert_Statement
            (  Connection : ODBC_Connection
            )  return SQL_IS is
   begin
      return
         SQL_IS
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_INSERT_STATEMENT)
         )  );
   end Get_Insert_Statement;

   function Get_Integrity
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_INTEGRITY);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Integrity;

   function Get_Keyset_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA1 is
   begin
      return
         SQL_CA1
         (  SQLUINTEGER'
            (  Get_Info
               (  Connection,
                  SQL_KEYSET_CURSOR_ATTRIBUTES1
         )  )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Keyset_Cursor_Attributes;

   function Get_Keyset_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA2 is
   begin
      return
         SQL_CA2
         (  SQLUINTEGER'
            (  Get_Info
               (  Connection,
                  SQL_KEYSET_CURSOR_ATTRIBUTES2
         )  )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Keyset_Cursor_Attributes;

   function Get_Keyset_Size
            (  Command : ODBC_Command
            )  return SQLULEN is
   begin
      return
         Get_Attribute
         (  Command,
            SQL_ATTR_KEYSET_SIZE,
            Filter_HYC00_HY114'Access
         );
   end Get_Keyset_Size;

   function Get_Keywords
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_KEYWORDS);
   end Get_Keywords;

   function Get_Like_Escape_Clause
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_LIKE_ESCAPE_CLAUSE);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Like_Escape_Clause;

   function Get_Login_Timeout
            (  Connection : ODBC_Connection
            )  return Duration is
      Result : aliased SQLUINTEGER := 0;
      Length : aliased SQLINTEGER  := 0;
   begin
      Check
      (  Connection,
         SQLGetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_LOGIN_TIMEOUT,
            SQLPOINTER (Result'Address),
            SQL_IS_UINTEGER,
            Length'Access
         ),
         Filter_HYC00_HY114'Access
      );
      return Duration (Result);
   end Get_Login_Timeout;

   function Get_Next_DSN
            (  Environment   : ODBC_Environment;
               Buffer_Length : Positive := Default_Block_Size
            )  return DSN_Description is
      Result             : SQLRETURN;
      Name_Length        : aliased SQLSMALLINT;
      Description_Length : aliased SQLSMALLINT;
      Name               : char_array (0..SQL_MAX_DSN_LENGTH);
      Description        : char_array (0..size_t (Buffer_Length));
   begin
      Result :=
         SQLDataSources
         (  EnvironmentHandle => Environment.Handle,
            Direction         => SQL_FETCH_NEXT,
            ServerName        => Name (0)'Access,
            BufferLength1     => Name'Length,
            NameLength1Ptr    => Name_Length'Access,
            Description       => Description (0)'Access,
            BufferLength2     => Description'Length,
            NameLength2Ptr    => Description_Length'Access
         );
      case Result is
         when SQL_SUCCESS =>
            declare
               DSN_Name        : constant String := To_Ada (Name);
               DSN_Description : constant String :=
                                    To_Ada (Description);
            begin
               return
               (  DSN_Name'Length,
                  DSN_Description'Length,
                  DSN_Name,
                  DSN_Description
               );
            end;
         when SQL_SUCCESS_WITH_INFO =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "Buffer for DSN description is too small"
            );
         when SQL_NO_DATA =>
            Raise_Exception (End_Error'Identity, "No more DSN");
         when others =>
            Check (Environment, Result);
            return (0, 0, "", "");
      end case;
   end Get_Next_DSN;

   function Get_Number
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
      Result : aliased SQLINTEGER  := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Environment,
         SQLGetDiagField
         (  HandleType      => SQL_HANDLE_ENV,
            Handle          => SQLHANDLE (Environment.Handle),
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => SQL_DIAG_NUMBER,
            DiagInfoPtr     => SQLPOINTER (Result'Address),
            BufferLength    => SQL_IS_INTEGER,
            StringLengthPtr => Length'Access
      )  );
      return Result;
   end Get_Number;

   function Get_Number
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
      Result : aliased SQLINTEGER  := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Connection,
         SQLGetDiagField
         (  HandleType      => SQL_HANDLE_DBC,
            Handle          => SQLHANDLE (Connection.Handle),
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => SQL_DIAG_NUMBER,
            DiagInfoPtr     => SQLPOINTER (Result'Address),
            BufferLength    => SQL_IS_INTEGER,
            StringLengthPtr => Length'Access
      )  );
      return Result;
   end Get_Number;

   function Get_Number
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
      Result : aliased SQLINTEGER  := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Command,
         SQLGetDiagField
         (  HandleType      => SQL_HANDLE_STMT,
            Handle          => SQLHANDLE (Command.Handle),
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => SQL_DIAG_NUMBER,
            DiagInfoPtr     => SQLPOINTER (Result'Address),
            BufferLength    => SQL_IS_INTEGER,
            StringLengthPtr => Length'Access
      )  );
      return Result;
   end Get_Number;

   function Get_Max_Async_Concurrent_Statements
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUINTEGER'
            (  Get_Info
               (  Connection,
                  SQL_MAX_ASYNC_CONCURRENT_STATEMENTS
         )  )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Async_Concurrent_Statements;

   function Get_Max_Binary_Literal_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_MAX_BINARY_LITERAL_LEN)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Binary_Literal_Len;

   function Get_Max_Catalog_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_CATALOG_NAME_LEN)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Catalog_Name_Len;

   function Get_Max_Char_Literal_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_MAX_CHAR_LITERAL_LEN)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Char_Literal_Len;

   function Get_Max_Column_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_COLUMN_NAME_LEN)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Column_Name_Len;

   function Get_Max_Columns_In_Group_By
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_COLUMNS_IN_GROUP_BY)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Columns_In_Group_By;

   function Get_Max_Columns_In_Index
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_COLUMNS_IN_INDEX)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Columns_In_Index;

   function Get_Max_Columns_In_Order_By
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_COLUMNS_IN_ORDER_BY)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Columns_In_Order_By;

   function Get_Max_Columns_In_Select
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_COLUMNS_IN_SELECT)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Columns_In_Select;

   function Get_Max_Columns_In_Table
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_COLUMNS_IN_TABLE)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Columns_In_Table;

   function Get_Max_Concurrent_Activities
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_CONCURRENT_ACTIVITIES)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Concurrent_Activities;

   function Get_Max_Cursor_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_CURSOR_NAME_LEN)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Cursor_Name_Len;

   function Get_Max_Driver_Connections
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_DRIVER_CONNECTIONS)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Driver_Connections;

   function Get_Max_Identifier_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_IDENTIFIER_LEN)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Identifier_Len;

   function Get_Max_Index_Size
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_MAX_INDEX_SIZE)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Index_Size;

   function Get_Max_Length
            (  Command : ODBC_Command
            )  return SQLULEN is
   begin
      return
         Get_Attribute
         (  Command,
            SQL_ATTR_MAX_LENGTH,
            Filter_HYC00_HY114'Access
         );
   end Get_Max_Length;

   function Get_Max_Procedure_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_PROCEDURE_NAME_LEN)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Procedure_Name_Len;

   function Get_Max_Rows
            (  Command : ODBC_Command
            )  return SQLULEN is
   begin
      return
         Get_Attribute
         (  Command,
            SQL_ATTR_MAX_ROWS,
            Filter_HYC00_HY114'Access
         );
   end Get_Max_Rows;

   function Get_Max_Row_Size
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUINTEGER'(Get_Info (Connection, SQL_MAX_ROW_SIZE))
         );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Row_Size;

   function Get_Max_Row_Size_Includes_Long
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
         "Y" = Get_Info (Connection, SQL_MAX_ROW_SIZE_INCLUDES_LONG);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Max_Row_Size_Includes_Long;

   function Get_Max_Schema_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_SCHEMA_NAME_LEN)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Schema_Name_Len;

   function Get_Max_Statement_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUINTEGER'(Get_Info (Connection, SQL_MAX_STATEMENT_LEN))
         );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Statement_Len;

   function Get_Max_Table_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_TABLE_NAME_LEN)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Table_Name_Len;

   function Get_Max_Tables_In_Select
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_MAX_TABLES_IN_SELECT)
         )  );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_Tables_In_Select;

   function Get_Max_User_Name_Len
            (  Connection : ODBC_Connection
            )  return Natural is
   begin
      return
         Natural
         (  SQLUSMALLINT'(Get_Info (Connection, SQL_MAX_USER_NAME_LEN))
         );
   exception
      when End_Error => -- No data, assume unknown
         return 0;
   end Get_Max_User_Name_Len;

   function Get_Message_Text
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_MESSAGE_TEXT,
                Record_Number => Record_Number
             );
   end Get_Message_Text;

   function Get_Message_Text
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return Wide_String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_MESSAGE_TEXT,
                Record_Number => Record_Number
             );
   end Get_Message_Text;

   function Get_Message_Text
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_MESSAGE_TEXT,
                Record_Number => Record_Number
             );
   end Get_Message_Text;

   function Get_Message_Text
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return Wide_String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_MESSAGE_TEXT,
                Record_Number => Record_Number
             );
   end Get_Message_Text;

   function Get_Message_Text
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_MESSAGE_TEXT,
                Record_Number => Record_Number
             );
   end Get_Message_Text;

   function Get_Message_Text
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return Wide_String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_MESSAGE_TEXT,
                Record_Number => Record_Number
             );
   end Get_Message_Text;

   function Get_Metadata_ID
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
      (  SQL_TRUE
      =  SQLUINTEGER'
         (  Get_Attribute (Connection, SQL_ATTR_METADATA_ID)
      )  );
   exception
      when End_Error => -- No data, assume false
         Raise_Exception (Use_Error'Identity, "Not supported");
   end Get_Metadata_ID;

   function Get_Metadata_ID
            (  Command : ODBC_Command
            )  return Boolean is
   begin
      return
      (  SQL_TRUE
      =  SQLULEN'
         (  Get_Attribute
            (  Command,
               SQL_ATTR_METADATA_ID,
               Filter_HYC00_HY114'Access
      )  )  );
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Metadata_ID;

   function Get_Multiple_Result_Sets
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
         "Y" = Get_Info (Connection, SQL_MULT_RESULT_SETS);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Multiple_Result_Sets;

   function Get_Multiple_Active_TXN
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
         "Y" = Get_Info (Connection, SQL_MULTIPLE_ACTIVE_TXN);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Multiple_Active_TXN;

   function Get_Native
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_NATIVE,
                Record_Number => Record_Number
             );
   end Get_Native;

   function Get_Native
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_NATIVE,
                Record_Number => Record_Number
             );
   end Get_Native;

   function Get_Native
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLINTEGER is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_NATIVE,
                Record_Number => Record_Number
             );
   end Get_Native;

   function Get_Need_Long_Data_Len
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
         "Y" = Get_Info (Connection, SQL_NEED_LONG_DATA_LEN);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Need_Long_Data_Len;

   function Get_Noscan
            (  Command : ODBC_Command
            )  return Boolean is
   begin
      return
      (  SQL_NOSCAN_ON
      =  SQLULEN'
         (  Get_Attribute
            (  Command,
               SQL_ATTR_NOSCAN,
               Filter_HYC00_HY114'Access
      )  )  );
   exception
      when End_Error => -- No data, assume false
         return False;
      when Use_Error =>
         return False;
   end Get_Noscan;

   function Get_Non_Nullable_Columns
            (  Connection : ODBC_Connection
            )  return SQL_NNC is
   begin
      return
         SQL_NNC
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_NON_NULLABLE_COLUMNS)
         )  );
   end Get_Non_Nullable_Columns;

   function Get_Null_Collation
            (  Connection : ODBC_Connection
            )  return SQL_NC is
   begin
      return
         SQL_NC
         (  SQLUSMALLINT'(Get_Info (Connection, SQL_NULL_COLLATION))
         );
   end Get_Null_Collation;

   function Get_Numeric_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_NUM is
   begin
      return
         SQL_FN_NUM
         (  SQLUINTEGER'(Get_Info (Connection, SQL_NUMERIC_FUNCTIONS))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Numeric_Functions;

   function Get_ODBC_Interface_Conformance
            (  Connection : ODBC_Connection
            )  return SQL_OIC is
   begin
      return
         SQL_OIC
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_ODBC_INTERFACE_CONFORMANCE)
         )  );
   exception
      when End_Error => -- No data, assume core
         return SQL_OIC_CORE;
   end Get_ODBC_Interface_Conformance;

   function Get_ODBC_Ver
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_ODBC_VER);
   end Get_ODBC_Ver;

   function Get_ODBC_Version
            (  Environment : ODBC_Environment
            )  return SQL_OV is
   begin
      return
         SQL_OV (Get_Attribute (Environment, SQL_ATTR_ODBC_VERSION));
   end Get_ODBC_Version;

   function Get_OJ_Capabilities
            (  Connection : ODBC_Connection
            )  return SQL_OJ is
   begin
      return
         SQL_OJ
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_OJ_CAPABILITIES)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_OJ_Capabilities;

   function Get_Order_By_Columns_In_Select
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return
         "Y" = Get_Info (Connection, SQL_ORDER_BY_COLUMNS_IN_SELECT);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Order_By_Columns_In_Select;

   function Get_Output_NTS
            (  Environment : ODBC_Environment
            )  return Boolean is
   begin
      return
         SQL_TRUE = Get_Attribute (Environment, SQL_ATTR_OUTPUT_NTS);
   end Get_Output_NTS;

   function Get_Packet_Size
            (  Connection : ODBC_Connection
            )  return Positive is
   begin
      return
         Positive
         (  SQLUINTEGER'
            (  Get_Attribute (Connection, SQL_ATTR_PACKET_SIZE)
         )  );
   end Get_Packet_Size;

   function Get_Param_Array_Row_Count
            (  Connection : ODBC_Connection
            )  return SQL_PARC is
   begin
      return
         SQL_PARC
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_PARAM_ARRAY_ROW_COUNTS)
         )  );
   end Get_Param_Array_Row_Count;

   function Get_Param_Array_Selects
            (  Connection : ODBC_Connection
            )  return SQL_PAS is
   begin
      return
         SQL_PAS
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_PARAM_ARRAY_SELECTS)
         )  );
   end Get_Param_Array_Selects;

   function Get_Procedure_Term
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_PROCEDURE_TERM);
   end Get_Procedure_Term;

   function Get_Procedures
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_PROCEDURES);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Procedures;

   function Get_Pos_Operations
            (  Connection : ODBC_Connection
            )  return SQL_POS is
   begin
      return
         SQL_POS
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_POS_OPERATIONS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Pos_Operations;

   function Get_Query_Timeout
            (  Command : ODBC_Command
            )  return Duration is
   begin
      return
         Duration
         (  SQLULEN'
            (  Get_Attribute
               (  Command,
                  SQL_ATTR_QUERY_TIMEOUT,
                  Filter_HYC00_HY114'Access
         )  )  );
   end Get_Query_Timeout;

   function Get_Quoted_Identifier_Case
            (  Connection : ODBC_Connection
            )  return SQL_IC is
   begin
      return
         SQL_IC
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_QUOTED_IDENTIFIER_CASE)
         )  );
   end Get_Quoted_Identifier_Case;

   function Get_Rec
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               State         : access SQLSTATE;
               Error         : access SQLINTEGER;
               Record_Number : Positive   := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return String is
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  SQLGetDiagRec
         (  HandleType     => Handle_Type,
            Handle         => Handle,
            RecNumber      => SQLSMALLINT (Record_Number),
            State          => State,
            NativeErrorPtr => Error,
            TextLengthPtr  => Length'Access
         ),
         Handle_Type,
         Handle,
         Filter
      );
      if Length <= 0 then
         return "";
      end if;
      declare
         Result : char_array (0..size_t (Length)) := (others => nul);
      begin
         Check
         (  SQLGetDiagRec
            (  HandleType     => Handle_Type,
               Handle         => Handle,
               RecNumber      => SQLSMALLINT (Record_Number),
               State          => State,
               NativeErrorPtr => Error,
               MessageText    => Result (0)'Access,
               BufferLength   => Result'Length,
               TextLengthPtr  => Length'Access
            ),
            Handle_Type,
            Handle,
            Filter
         );
         return To_Ada (Result);
      end;
   end Get_Rec;

   function Get_Rec
            (  Handle_Type   : SQL_HANDLE;
               Handle        : SQLHANDLE;
               State         : access SQLSTATE;
               Error         : access SQLINTEGER;
               Record_Number : Positive   := 1;
               Filter        : Filter_Ptr := No_Filter'Access
            )  return Wide_String is
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  SQLGetDiagRec
         (  HandleType     => Handle_Type,
            Handle         => Handle,
            RecNumber      => SQLSMALLINT (Record_Number),
            State          => State,
            NativeErrorPtr => Error,
            TextLengthPtr  => Length'Access
         ),
         Handle_Type,
         Handle,
         Filter
      );
      if Length <= 0 then
         return "";
      end if;
      declare
         State  : aliased SQLWSTATE;
         Result : SQLWCHAR_Array (0..size_t (Length)) :=
                     (others => SQLWCHAR'Val (0));
      begin
         Check
         (  SQLGetDiagRec
            (  HandleType     => Handle_Type,
               Handle         => Handle,
               RecNumber      => SQLSMALLINT (Record_Number),
               State          => State'Access,
               NativeErrorPtr => Error,
               MessageText    => Result (0)'Access,
               BufferLength   => Result'Length,
               TextLengthPtr  => Length'Access
            ),
            Handle_Type,
            Handle,
            Filter
         );
         return To_Ada (Result);
      end;
   end Get_Rec;

   function Get_Retrieve_Data
            (  Command : ODBC_Command
            )  return Boolean is
   begin
      return
      (  SQL_RD_ON
      =  SQLULEN'
         (  Get_Attribute
            (  Command,
               SQL_ATTR_RETRIEVE_DATA,
               Filter_HYC00_HY114'Access
      )  )  );
   end Get_Retrieve_Data;

   function Get_Returncode
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLRETURN is
      Result : aliased SQLRETURN   := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Environment,
         SQLGetDiagField
         (  HandleType      => SQL_HANDLE_ENV,
            Handle          => SQLHANDLE (Environment.Handle),
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => SQL_DIAG_RETURNCODE,
            DiagInfoPtr     => SQLPOINTER (Result'Address),
            BufferLength    => SQL_IS_INTEGER,
            StringLengthPtr => Length'Access
      )  );
      return Result;
   end Get_Returncode;

   function Get_Returncode
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLRETURN is
      Result : aliased SQLRETURN   := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Connection,
         SQLGetDiagField
         (  HandleType      => SQL_HANDLE_DBC,
            Handle          => SQLHANDLE (Connection.Handle),
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => SQL_DIAG_RETURNCODE,
            DiagInfoPtr     => SQLPOINTER (Result'Address),
            BufferLength    => SQL_IS_INTEGER,
            StringLengthPtr => Length'Access
      )  );
      return Result;
   end Get_Returncode;

   function Get_Returncode
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLRETURN is
      Result : aliased SQLRETURN   := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Command,
         SQLGetDiagField
         (  HandleType      => SQL_HANDLE_STMT,
            Handle          => SQLHANDLE (Command.Handle),
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => SQL_DIAG_RETURNCODE,
            DiagInfoPtr     => SQLPOINTER (Result'Address),
            BufferLength    => SQL_IS_INTEGER,
            StringLengthPtr => Length'Access
      )  );
      return Result;
   end Get_Returncode;

   function Get_Row_Array_Size
            (  Command : ODBC_Command
            )  return SQLULEN is
   begin
      return
         Get_Attribute
         (  Command,
            SQL_ATTR_ROW_ARRAY_SIZE,
            Filter_HYC00_HY114'Access
         );
   end Get_Row_Array_Size;

   function Get_Row_Count
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return SQLLEN is
      Count  : aliased SQLLEN      := 0;
      Length : aliased SQLSMALLINT := 0;
   begin
      Check
      (  Command,
         SQLGetDiagField
         (  HandleType      => SQL_HANDLE_STMT,
            Handle          => SQLHANDLE (Command.Handle),
            RecNumber       => SQLSMALLINT (Record_Number),
            DiagIdentifier  => SQL_DIAG_ROW_COUNT,
            DiagInfoPtr     => SQLPOINTER (Count'Address),
            BufferLength    => SQL_IS_UINTEGER,
            StringLengthPtr => Length'Access
      )  );
      return Count;
   end Get_Row_Count;

   function Get_Row_Updates
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return "Y" = Get_Info (Connection, SQL_ROW_UPDATES);
   exception
      when End_Error => -- No data, assume false
         return False;
   end Get_Row_Updates;

   function Get_Row_Number
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return SQLLEN is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_ROW_NUMBER,
                Record_Number => Record_Number
             );
   end Get_Row_Number;

   function Get_Row_Number
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return SQLLEN is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_ROW_NUMBER,
                Record_Number => Record_Number
             );
   end Get_Row_Number;

   function Get_Row_Number
            (  Command : ODBC_Command
            )  return SQLULEN is
   begin
      return
         Get_Attribute
         (  Command,
            SQL_ATTR_ROW_NUMBER,
            Filter_HYC00_HY114'Access
         );
   end Get_Row_Number;

   function Get_Schema_Term
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_SCHEMA_TERM);
   end Get_Schema_Term;

   function Get_Schema_Usage
            (  Connection : ODBC_Connection
            )  return SQL_SU is
   begin
      return
         SQL_SU
         (  SQLUINTEGER'(Get_Info (Connection, SQL_SCHEMA_USAGE))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Schema_Usage;

   function Get_Scroll_Options
            (  Connection : ODBC_Connection
            )  return SQL_SO is
   begin
      return
         SQL_SO
         (  SQLUINTEGER'(Get_Info (Connection, SQL_SCROLL_OPTIONS))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Scroll_Options;

   function Get_Search_Pattern_Escape
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_SEARCH_PATTERN_ESCAPE);
   end Get_Search_Pattern_Escape;

   function Get_Server_Name
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_SERVER_NAME,
                Record_Number => Record_Number
             );
   end Get_Server_Name;

   function Get_Server_Name
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return Wide_String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_SERVER_NAME,
                Record_Number => Record_Number
             );
   end Get_Server_Name;

   function Get_Server_Name
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_SERVER_NAME);
   end Get_Server_Name;

   function Get_Server_Name
            (  Connection    : ODBC_Connection;
               Record_Number : Positive
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_SERVER_NAME,
                Record_Number => Record_Number
             );
   end Get_Server_Name;

   function Get_Server_Name
            (  Connection    : ODBC_Connection;
               Record_Number : Positive
            )  return Wide_String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_SERVER_NAME,
                Record_Number => Record_Number
             );
   end Get_Server_Name;

   function Get_Server_Name
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_SERVER_NAME,
                Record_Number => Record_Number
             );
   end Get_Server_Name;

   function Get_Server_Name
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return Wide_String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_SERVER_NAME,
                Record_Number => Record_Number
             );
   end Get_Server_Name;

   function Get_Special_Characters
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_SPECIAL_CHARACTERS);
   end Get_Special_Characters;

   function Get_SQLSTATE
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_SQLSTATE,
                Record_Number => Record_Number
             );
   end Get_SQLSTATE;

   function Get_SQLSTATE
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_SQLSTATE,
                Record_Number => Record_Number
             );
   end Get_SQLSTATE;

   function Get_SQLSTATE
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_SQLSTATE,
                Record_Number => Record_Number
             );
   end Get_SQLSTATE;

   function Get_SQL_Conformance
            (  Connection : ODBC_Connection
            )  return SQL_SC is
   begin
      return
         SQL_SC
         (  SQLUINTEGER'(Get_Info (Connection, SQL_SQL_CONFORMANCE))
         );
   exception
      when End_Error => -- No data, assume entry
         return SQL_SC_SQL92_ENTRY;
   end Get_SQL_Conformance;

   function Get_SQL92_Datetime_Functions
            (  Connection : ODBC_Connection
            )  return SQL_SDF is
   begin
      return
         SQL_SDF
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_SQL92_DATETIME_FUNCTIONS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Datetime_Functions;

   function Get_SQL92_Foreign_Key_Delete_Rule
            (  Connection : ODBC_Connection
            )  return SQL_SFKD is
   begin
      return
         SQL_SFKD
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_SQL92_FOREIGN_KEY_DELETE_RULE)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Foreign_Key_Delete_Rule;

   function Get_SQL92_Foreign_Key_Update_Rule
            (  Connection : ODBC_Connection
            )  return SQL_SFKU is
   begin
      return
         SQL_SFKU
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_SQL92_FOREIGN_KEY_UPDATE_RULE)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Foreign_Key_Update_Rule;

   function Get_SQL92_Grant
            (  Connection : ODBC_Connection
            )  return SQL_SG is
   begin
      return
         SQL_SG
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_SQL92_GRANT)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Grant;

   function Get_SQL92_Numeric_Value_Functions
            (  Connection : ODBC_Connection
            )  return SQL_SNVF is
   begin
      return
         SQL_SNVF
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_SQL92_NUMERIC_VALUE_FUNCTIONS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Numeric_Value_Functions;

   function Get_SQL92_Predicates
            (  Connection : ODBC_Connection
            )  return SQL_SP is
   begin
      return
         SQL_SP
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_SQL92_PREDICATES)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Predicates;

   function Get_SQL92_Relational_Join_Operations
            (  Connection : ODBC_Connection
            )  return SQL_SRJO is
   begin
      return
         SQL_SRJO
         (  SQLUINTEGER'
            (  Get_Info
               (  Connection,
                  SQL_SQL92_RELATIONAL_JOIN_OPERATORS
         )  )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Relational_Join_Operations;

   function Get_SQL92_Revoke
            (  Connection : ODBC_Connection
            )  return SQL_SR is
   begin
      return
         SQL_SR (SQLUINTEGER'(Get_Info (Connection, SQL_SQL92_REVOKE)));
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Revoke;

   function Get_SQL92_Row_Value_Constructor
            (  Connection : ODBC_Connection
            )  return SQL_SRVC is
   begin
      return
         SQL_SRVC
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_SQL92_ROW_VALUE_CONSTRUCTOR)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Row_Value_Constructor;

   function Get_SQL92_String_Functions
            (  Connection : ODBC_Connection
            )  return SQL_SSF is
   begin
      return
         SQL_SSF
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_SQL92_STRING_FUNCTIONS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_String_Functions;

   function Get_SQL92_Value_Expressions
            (  Connection : ODBC_Connection
            )  return SQL_SVE is
   begin
      return
         SQL_SVE
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_SQL92_VALUE_EXPRESSIONS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_SQL92_Value_Expressions;

   function Get_Standard_CLI_Conformance
            (  Connection : ODBC_Connection
            )  return SQL_SCC is
   begin
      return
         SQL_SCC
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_STANDARD_CLI_CONFORMANCE)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Standard_CLI_Conformance;

   function Get_Static_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA1 is
   begin
      return
         SQL_CA1
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_STATIC_CURSOR_ATTRIBUTES1)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Static_Cursor_Attributes;

   function Get_Static_Cursor_Attributes
            (  Connection : ODBC_Connection
            )  return SQL_CA2 is
   begin
      return
         SQL_CA2
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_STATIC_CURSOR_ATTRIBUTES2)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Static_Cursor_Attributes;

   function Get_String_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_STR is
   begin
      return
         SQL_FN_STR
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_STRING_FUNCTIONS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_String_Functions;

   function Get_Subclass_Origin
            (  Environment   : ODBC_Environment;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_ENV,
                Handle        => SQLHANDLE (Environment.Handle),
                Diag_Field    => SQL_DIAG_SUBCLASS_ORIGIN,
                Record_Number => Record_Number
             );
   end Get_Subclass_Origin;

   function Get_Subclass_Origin
            (  Connection    : ODBC_Connection;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_DBC,
                Handle        => SQLHANDLE (Connection.Handle),
                Diag_Field    => SQL_DIAG_SUBCLASS_ORIGIN,
                Record_Number => Record_Number
             );
   end Get_Subclass_Origin;

   function Get_Subclass_Origin
            (  Command       : ODBC_Command;
               Record_Number : Positive := 1
            )  return String is
   begin
      return Get_Field
             (  Handle_Type   => SQL_HANDLE_STMT,
                Handle        => SQLHANDLE (Command.Handle),
                Diag_Field    => SQL_DIAG_SUBCLASS_ORIGIN,
                Record_Number => Record_Number
             );
   end Get_Subclass_Origin;

   function Get_Subqueries
            (  Connection : ODBC_Connection
            )  return SQL_SQ is
   begin
      return
         SQL_SQ (SQLUINTEGER'(Get_Info (Connection, SQL_SUBQUERIES)));
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Subqueries;

   function Get_System_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_SYS is
   begin
      return
         SQL_FN_SYS
         (  SQLUINTEGER'(Get_Info (Connection, SQL_SYSTEM_FUNCTIONS))
         );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_System_Functions;

   function Get_Table_Term
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_TABLE_TERM);
   end Get_Table_Term;

   procedure Get_Tables
             (  Command    : in out ODBC_Command;
                Table_Type : String := "TABLE"
             )  is
      Table : char_array := To_C (Table_Type);
   begin
      Check (Command, SQLFreeStmt (Command.Handle, SQL_UNBIND));
      Check (Command, SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS));
      Check
      (  Command,
         SQLTables
         (  StatementHandle => Command.Handle,
            TableType       => Table (Table'First)'Unchecked_Access,
            NameLength4     => Table_Type'Length
      )  );
   end Get_Tables;

   function Get_Timedate_Add_Intervals
            (  Connection : ODBC_Connection
            )  return SQL_FN_TSI is
   begin
      return
         SQL_FN_TSI
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_TIMEDATE_ADD_INTERVALS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Timedate_Add_Intervals;

   function Get_Timedate_Diff_Intervals
            (  Connection : ODBC_Connection
            )  return SQL_FN_TSI is
   begin
      return
         SQL_FN_TSI
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_TIMEDATE_DIFF_INTERVALS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Timedate_Diff_Intervals;

   function Get_Timedate_Functions
            (  Connection : ODBC_Connection
            )  return SQL_FN_TSI is
   begin
      return
         SQL_FN_TSI
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_TIMEDATE_FUNCTIONS)
         )  );
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Timedate_Functions;

   function Get_Trace
            (  Connection : ODBC_Connection
            )  return Boolean is
   begin
      return SQL_TRUE = Get_Attribute (Connection, SQL_ATTR_TRACE);
   end Get_Trace;

   function Get_Tracefile
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Attribute (Connection, SQL_ATTR_TRACEFILE);
   end Get_Tracefile;

   function Get_Tracefile
            (  Connection : ODBC_Connection
            )  return Wide_String is
   begin
      return Get_Attribute (Connection, SQL_ATTR_TRACEFILE);
   end Get_Tracefile;

   function Get_Translate_Lib
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Attribute (Connection, SQL_ATTR_TRANSLATE_LIB);
   end Get_Translate_Lib;

   function Get_Translate_Lib
            (  Connection : ODBC_Connection
            )  return Wide_String is
   begin
      return Get_Attribute (Connection, SQL_ATTR_TRANSLATE_LIB);
   end Get_Translate_Lib;

   function Get_Translate_Option
            (  Connection : ODBC_Connection
            )  return SQLUINTEGER is
   begin
      return Get_Attribute (Connection, SQL_ATTR_TRANSLATE_OPTION);
   end Get_Translate_Option;

   function Get_TXN_Capable
            (  Connection : ODBC_Connection
            )  return SQL_TC is
   begin
      return
         SQL_TC
         (  SQLUSMALLINT'
            (  Get_Info (Connection, SQL_TXN_CAPABLE)
         )  );
   exception
      when End_Error => -- No data, assume none
         return SQL_TC_NONE;
   end Get_TXN_Capable;

   function Get_TXN_Isolation
            (  Connection : ODBC_Connection
            )  return SQL_TXN is
   begin
      return
         SQL_TXN
         (  SQLUINTEGER'
            (  Get_Attribute (Connection, SQL_ATTR_TXN_ISOLATION)
         )  );
   end Get_TXN_Isolation;

   function Get_TXN_Isolation_Option
            (  Connection : ODBC_Connection
            )  return SQL_TXN is
   begin
      return
         SQL_TXN
         (  SQLUINTEGER'
            (  Get_Info (Connection, SQL_TXN_CAPABLE)
         )  );
   end Get_TXN_Isolation_Option;

   function Get_Type_Info
            (  Command   : access ODBC_Command;
               Data_Type : SQL_DATA_TYPE
            )  return Type_Info is
       Data_Type_Value    : SQL_DATA_TYPE;
       Column_Size        : SQLINTEGER;
       Nullable           : Boolean;
       Case_Sensitive     : Boolean;
       Searchable         : SQL_COLUMN_SEARCHABLE;
       Unsigned_Attribute : Boolean;
       Fixed_Prec_Scale   : Boolean;
       Auto_Unique_Value  : Boolean;
   begin
      Check (Command.all, SQLFreeStmt (Command.Handle, SQL_UNBIND));
      Check
      (  Command.all,
         SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS)
      );
      Check
      (  Command.all,
         SQLGetTypeInfo (Command.Handle, Data_Type),
         Filter_HY004'Access
      );
      Fetch (Command.all);
   -- Column 1
      declare
         Type_Name : constant String := Get_Data (Command, 1, On_Error);
      begin
   -- Column 2
         begin
            Data_Type_Value :=
               SQL_DATA_TYPE'Val
               (  SQLSMALLINT'(Get_Data (Command, 2, On_Error))
               );
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  "Wrong data type (column 2)"
               );
         end;
   -- Column 3
         begin
            Column_Size := Get_Data (Command, 3, On_Error);
         exception
            when End_Error =>
               Column_Size := 0;
         end;
   -- Column 4
         declare
            Literal_Prefix : constant String :=
                             Get_Data (Command, 4, On_Error);
         begin
   -- Column 5
            declare
               Literal_Suffix : constant String :=
                                Get_Data (Command, 5, On_Error);
            begin
   -- Column 6
               declare
                  Create_Parameters : constant String :=
                                      Get_Data (Command, 6, On_Error);
               begin
   -- Column 7
                  begin
                     Nullable :=
                        (  SQL_NULLABLE
                        =  SQL_NULLABLE_FIELD'Val
                           (  SQLSMALLINT'
                              (  Get_Data (Command, 7, On_Error)
                        )  )  );
                  exception
                     when Constraint_Error | End_Error =>
                        Nullable := False;
                  end;
   -- Column 8
                  Case_Sensitive :=
                     SQLSMALLINT'(Get_Data (Command, 8, On_Error)) /= 0;
   -- Column 9
                  declare
                     Value : SQLSMALLINT;
                  begin
                     Value := Get_Data (Command, 9, On_Error);
                     Searchable := SQL_COLUMN_SEARCHABLE (Value);
                  exception
                     when Constraint_Error =>
                        Raise_Exception
                        (  Data_Error'Identity,
                           (  "Wrong searchable value"
                           &  SQLSMALLINT'Image (Value)
                           &  " (column 9)"
                        )  );
                  end;
   -- Column 10
                  begin
                     Unsigned_Attribute :=
                        (  SQLSMALLINT'
                           (  Get_Data (Command, 10, On_Error)
                           )
                        /= 0
                        );
                  exception
                     when End_Error =>
                        Unsigned_Attribute := True;
                  end;
   -- Column 11
                  Fixed_Prec_Scale :=
                     SQLINTEGER'(Get_Data (Command, 11, On_Error)) /= 0;
   -- Column 12
                  begin
                     Auto_Unique_Value :=
                        (  SQLSMALLINT'
                           (  Get_Data (Command, 12, On_Error)
                           )
                        /= 0
                        );
                  exception
                     when End_Error =>
                        Auto_Unique_Value := False;
                  end;
   -- Column 13
                  declare
                     Local_Name : constant String :=
                                  Get_Data (Command, 13, On_Error);
                  begin
                     Close_Cursor (Command.all);
                     return
                     (  Type_Name             => Type_Name,
                        Data_Type             => Data_Type_Value,
                        Column_Size           => Column_Size,
                        Literal_Prefix        => Literal_Prefix,
                        Literal_Suffix        => Literal_Suffix,
                        Create_Parameters     => Create_Parameters,
                        Nullable              => Nullable,
                        Case_Sensitive        => Case_Sensitive,
                        Searchable            => Searchable,
                        Unsigned_Attribute    => Unsigned_Attribute,
                        Fixed_Prec_Scale      => Fixed_Prec_Scale,
                        Auto_Unique_Value     => Auto_Unique_Value,
                        Local_Name            => Local_Name,
                        Type_Name_Length      => Type_Name'Length,
                        Local_Name_Length     => Local_Name'Length,
                        Literal_Prefix_Length => Literal_Prefix'Length,
                        Literal_Suffix_Length => Literal_Suffix'Length,
                        Create_Parameters_Length =>
                                               Create_Parameters'Length
                     );
                  end;
               end;
            end;
         end;
      end;
   exception
      when End_Error =>
         Close_Cursor (Command.all);
         Raise_Exception
         (  Constraint_Error'Identity,
            "Type not supported"
         );
      when others =>
         Close_Cursor (Command.all);
         raise;
   end Get_Type_Info;

   function Get_Union
            (  Connection : ODBC_Connection
            )  return SQL_U is
   begin
      return SQL_U (SQLUINTEGER'(Get_Info (Connection, SQL_UNION)));
   exception
      when End_Error => -- No data, assume 0
         return 0;
   end Get_Union;

   function Get_User_Name
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_USER_NAME);
   end Get_User_Name;

   function Get_User_Name
            (  Connection : ODBC_Connection
            )  return Wide_String is
   begin
      return Get_Info (Connection, SQL_USER_NAME);
   end Get_User_Name;

   function Get_XOpen_CLI_Year
            (  Connection : ODBC_Connection
            )  return String is
   begin
      return Get_Info (Connection, SQL_XOPEN_CLI_YEAR);
   end Get_XOpen_CLI_Year;

   function Get_XOpen_CLI_Year
            (  Connection : ODBC_Connection
            )  return Wide_String is
   begin
      return Get_Info (Connection, SQL_XOPEN_CLI_YEAR);
   end Get_XOpen_CLI_Year;

   procedure Initialize (Environment : in out ODBC_Environment) is
      Handle : aliased SQLHANDLE;
   begin
      Check
      (  Environment,
         SQLAllocHandle
         (  SQL_HANDLE_ENV,
            SQL_NULL_HANDLE,
            Handle'Access
      )  );
      Environment.Handle := SQLHENV (Handle);
      Set_ODBC_Version (Environment, Used_ODBC_Version);
   end Initialize;

   procedure Initialize (Connection : in out ODBC_Connection) is
      Handle : aliased SQLHANDLE;
   begin
      Check
      (  Connection,
         SQLAllocHandle
         (  SQL_HANDLE_DBC,
            SQLHANDLE (Connection.Environment.Handle),
            Handle'Access
      )  );
      Connection.Handle := SQLHDBC (Handle);
   end Initialize;

   procedure Initialize (Command : in out ODBC_Command) is
      Handle : aliased SQLHANDLE;
   begin
      Check
      (  Command,
         SQLAllocHandle
         (  SQL_HANDLE_STMT,
            SQLHANDLE (Command.Connection.Handle),
            Handle'Access
      )  );
      Command.Handle := SQLHSTMT (Handle);
   end Initialize;

   function No_Filter (State : SQLSTATE) return Boolean is
   begin
      return True;
   end No_Filter;

   function Num_Params (Command : ODBC_Command) return Natural is
      Count : aliased SQLSMALLINT;
   begin
      Check
      (  Command,
         SQLNumParams
         (  StatementHandle   => Command.Handle,
            ParameterCountPtr => Count'Access
      )  );
      return Natural (Count);
   end Num_Params;

   function Num_Result_Cols (Command : ODBC_Command) return Natural is
      Count : aliased SQLSMALLINT;
   begin
      Check
      (  Command,
         SQLNumResultCols
         (  StatementHandle => Command.Handle,
            ColumnCountPtr  => Count'Access
      )  );
      return Natural (Count);
   end Num_Result_Cols;

   procedure Prepare
             (  Command : in out ODBC_Command;
                Request : String
             )  is
   begin
      Check (Command, SQLFreeStmt (Command.Handle, SQL_UNBIND));
      Check (Command, SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS));
      Check
      (  Command,
         SQLPrepare
         (  StatementHandle => Command.Handle,
            StatementText   => To_C (Request),
            TextLength      => Request'Length
      )  );
   end Prepare;

   procedure Prepare
             (  Command : in out ODBC_Command;
                Request : Wide_String
             )  is
      Data : constant SQLWCHAR_Array := To_C (Request);
   begin
      Check (Command, SQLFreeStmt (Command.Handle, SQL_UNBIND));
      Check (Command, SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS));
      Check
      (  Command,
         SQLPrepare
         (  StatementHandle => Command.Handle,
            StatementText   => Data,
            TextLength      => Request'Length
      )  );
   end Prepare;

   procedure Release (Connection : in out ODBC_Connection) is
   begin
      Set_TXN_Isolation (Connection, SQL_TRANSACTION_READ_UNCOMMITTED);
   end Release;

   procedure RollBack (Connection  : in out ODBC_Connection) is
   begin
      Check
      (  Connection,
         SQLEndTran
         (  SQL_HANDLE_DBC,
            SQLHANDLE (Connection.Handle),
            SQL_ROLLBACK
      )  );
   end RollBack;

   function Row_Count
            (  Command : ODBC_Command
            )  return Natural is
      Count : aliased SQLLEN := 0;
   begin
      Check (Command, SQLRowCount (Command.Handle, Count'Access));
      if Count <= 0 then
         return 0;
      else
         return Natural (Count);
      end if;
   end Row_Count;

   procedure Seize (Connection : in out ODBC_Connection) is
   begin
      Set_TXN_Isolation (Connection, SQL_TRANSACTION_SERIALIZABLE);
   end Seize;

   function Serializable (Connection : ODBC_Connection)
      return Boolean is
   begin
      return
      (  0
      /= (  SQL_TXN_SERIALIZABLE
         and
            Get_TXN_Isolation_Option (Connection)
      )  );
   end Serializable;

   procedure Set
             (  Parameter : in out String_Parameter;
                Value     : String
             )  is
      Pointer : size_t := 0;
   begin
      if Value'Length > Parameter.Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Value is too large"
         );
      end if;
      for Index in Value'Range loop
         Parameter.Value (Pointer) := To_C (Value (Index));
         Pointer := Pointer + 1;
      end loop;
      Parameter.Value (Pointer) := char'Val (0);
      Parameter.Size := SQLLEN (Pointer);
   end Set;

   procedure Set
             (  Parameter : in out Wide_String_Parameter;
                Value     : Wide_String
             )  is
      Pointer : size_t := 0;
   begin
      if Value'Length > Parameter.Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Value is too large"
         );
      end if;
      for Index in Value'Range loop
         Parameter.Value (Pointer) := Value (Index);
         Pointer := Pointer + 1;
      end loop;
      Parameter.Value (Pointer) := SQLWCHAR'Val (0);
      Parameter.Size := SQLLEN (Pointer) * Bytes_In_Wide_Char;
   end Set;

   procedure Set_Access_Mode
             (  Connection : in out ODBC_Connection;
                Value      : SQL_MODE
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_ACCESS_MODE,
            SQLUINTEGER (Value)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Access_Mode;

   procedure Set_Async_DBC_Enable
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             )  is
      Data : aliased SQLUINTEGER;
   begin
      case Value is
         when True  => Data := SQL_ASYNC_ENABLE_ON;
         when False => Data := SQL_ASYNC_ENABLE_OFF;
      end case;
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE,
            Data
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Async_DBC_Enable;

   procedure Set_Async_Enable
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             )  is
      Data : aliased SQLULEN;
   begin
      case Value is
         when True  => Data := SQL_ASYNC_ENABLE_ON;
         when False => Data := SQL_ASYNC_ENABLE_OFF;
      end case;
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_ASYNC_ENABLE,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Async_Enable;

   procedure Set_Async_Enable
             (  Command : in out ODBC_Command;
                Value   : Boolean
             )  is
      Data : aliased SQLULEN;
   begin
      case Value is
         when True  => Data := SQL_ASYNC_ENABLE_ON;
         when False => Data := SQL_ASYNC_ENABLE_OFF;
      end case;
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_ASYNC_ENABLE,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Async_Enable;

   procedure Set_Autocommit
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             )  is
      Data : aliased SQLUINTEGER;
   begin
      case Value is
         when True  => Data := SQL_AUTOCOMMIT_ON;
         when False => Data := SQL_AUTOCOMMIT_OFF;
      end case;
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_AUTOCOMMIT,
            Data
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Autocommit;

   procedure Set_Concurrency
             (  Command : in out ODBC_Command;
                Value   : SQL_CONCUR
             )  is
      Data : constant SQLULEN := SQLULEN (Value);
   begin
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_CONCURRENCY,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Concurrency;

   procedure Set_Connection_Pooling
             (  Environment : in out ODBC_Environment;
                Value       : SQL_CP
             )  is
   begin
      Check
      (  Environment,
         SQLSetEnvAttr
         (  Environment.Handle,
            SQL_ATTR_CONNECTION_POOLING,
            SQLUINTEGER (Value)
      )  );
   end Set_Connection_Pooling;

   procedure Set_Connection_Timeout
             (  Connection : in out ODBC_Connection;
                Value      : Duration
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_CONNECTION_TIMEOUT,
            SQLUINTEGER (Value)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Connection_Timeout;

   procedure Set_CP_Match
             (  Environment : in out ODBC_Environment;
                Value       : SQL_CP_MATCH
             )  is
   begin
      Check
      (  Environment,
         SQLSetEnvAttr
         (  Environment.Handle,
            SQL_ATTR_CP_MATCH,
            SQLUINTEGER (Value)
      )  );
   end Set_CP_Match;

   procedure Set_Current_Catalog
             (  Connection : in out ODBC_Connection;
                Value      : String
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_CURRENT_CATALOG,
            To_C (Value),
            Value'Length
      )  );
   end Set_Current_Catalog;

   procedure Set_Current_Catalog
             (  Connection : in out ODBC_Connection;
                Value      : Wide_String
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_CURRENT_CATALOG,
            To_C (Value),
            Value'Length * Bytes_In_Wide_Char
      )  );
   end Set_Current_Catalog;

   procedure Set_Cursor_Scrollable
             (  Command : in out ODBC_Command;
                Value   : Boolean
             )  is
      Data : aliased SQLULEN;
   begin
      case Value is
         when True  => Data := SQL_SCROLLABLE;
         when False => Data := SQL_NONSCROLLABLE;
      end case;
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_CURSOR_SCROLLABLE,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Cursor_Scrollable;

   procedure Set_Cursor_Sensitivity
             (  Command : in out ODBC_Command;
                Value   : SQL_SENSITIVITY
             )  is
      Data : constant SQLULEN := SQLULEN (Value);
   begin
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_CURSOR_SENSITIVITY,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Cursor_Sensitivity;

   procedure Set_Cursor_Type
             (  Command : in out ODBC_Command;
                Value   : SQL_CURSOR
             )  is
      Data : constant SQLULEN := SQLULEN (Value);
   begin
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_CURSOR_TYPE,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Cursor_Type;

   procedure Set_Enable_Auto_IPD
             (  Command : in out ODBC_Command;
                Value   : Boolean
             )  is
      Data : aliased SQLULEN;
   begin
      case Value is
         when True  => Data := SQL_TRUE;
         when False => Data := SQL_FALSE;
      end case;
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_ENABLE_AUTO_IPD,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Enable_Auto_IPD;

   procedure Set_Execution_Mode
             (  Connection : in out ODBC_Connection;
                Mode       : Execution_Mode
             )  is
   begin
      Connection.Mode := Mode;
   end Set_Execution_Mode;

   procedure Set_Keyset_Size
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             )  is
   begin
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_KEYSET_SIZE,
            Value,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Keyset_Size;

   procedure Set_Login_Timeout
             (  Connection : in out ODBC_Connection;
                Value      : Duration
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_LOGIN_TIMEOUT,
            SQLUINTEGER (Value)
      )  );
   end Set_Login_Timeout;

   procedure Set_Max_Length
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             )  is
   begin
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_MAX_LENGTH,
            Value,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Max_Length;

   procedure Set_Max_Rows
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             )  is
   begin
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_MAX_ROWS,
            Value,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Max_Rows;

   procedure Set_Metadata_ID
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             )  is
      Data : aliased SQLUINTEGER;
   begin
      case Value is
         when True  => Data := SQL_TRUE;
         when False => Data := SQL_FALSE;
      end case;
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_METADATA_ID,
            Data
      )  );
   end Set_Metadata_ID;

   procedure Set_Metadata_ID
             (  Command : in out ODBC_Command;
                Value   : Boolean
             )  is
      Data : aliased SQLULEN;
   begin
      case Value is
         when True  => Data := SQL_TRUE;
         when False => Data := SQL_FALSE;
      end case;
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_METADATA_ID,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Metadata_ID;

   procedure Set_Noscan
             (  Command : in out ODBC_Command;
                Value   : Boolean
             )  is
      Data : aliased SQLULEN;
   begin
      case Value is
         when True  => Data := SQL_NOSCAN_ON;
         when False => Data := SQL_NOSCAN_OFF;
      end case;
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_NOSCAN,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Noscan;

   procedure Set_ODBC_Version
             (  Environment : in out ODBC_Environment;
                Value       : SQL_OV
             )  is
   begin
      Check
      (  Environment,
         SQLSetEnvAttr
         (  Environment.Handle,
            SQL_ATTR_ODBC_VERSION,
            SQLINTEGER (Value)
      )  );
   end Set_ODBC_Version;

   procedure Set_Output_NTS
             (  Environment : in out ODBC_Environment;
                Value       : Boolean
             )  is
      Data : SQLINTEGER;
   begin
      case Value is
         when True  => Data := SQL_TRUE;
         when False => Data := SQL_FALSE;
      end case;
      Check
      (  Environment,
         SQLSetEnvAttr
         (  Environment.Handle,
            SQL_ATTR_OUTPUT_NTS,
            Data
      )  );
   end Set_Output_NTS;

   procedure Set_Packet_Size
             (  Connection : in out ODBC_Connection;
                Value      : Positive
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_PACKET_SIZE,
            SQLUINTEGER (Value)
      )  );
   end Set_Packet_Size;

   procedure Set_Pos
             (  Command   : in out ODBC_Command;
                Row       : Positive;
                Operation : SQL_OPERATION;
                Locking   : SQL_LOCKTYPE
             )  is
   begin
      Check
      (  Command,
         SQLSetPos
         (  StatementHandle => Command.Handle,
            RowNumber       => SQLSETPOSIROW (Row),
            Operation       => Operation,
            LockType        => Locking
      )  );
   end Set_Pos;

   procedure Set_Query_Timeout
             (  Command : in out ODBC_Command;
                Value   : Duration
             )  is
      Data : constant SQLULEN := SQLULEN (Value);
   begin
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_QUERY_TIMEOUT,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Query_Timeout;

   procedure Set_Retrieve_Data
             (  Command : in out ODBC_Command;
                Value   : Boolean
             )  is
      Data : aliased SQLULEN;
   begin
      case Value is
         when True  => Data := SQL_RD_ON;
         when False => Data := SQL_RD_OFF;
      end case;
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_RETRIEVE_DATA,
            Data,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Retrieve_Data;

   procedure Set_Row_Array_Size
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             )  is
   begin
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_ROW_ARRAY_SIZE,
            Value,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Row_Array_Size;

   procedure Set_Row_Number
             (  Command : in out ODBC_Command;
                Value   : SQLULEN
             )  is
   begin
      Check
      (  Command,
         SQLSetStmtAttr
         (  Command.Handle,
            SQL_ATTR_ROW_NUMBER,
            Value,
            SQL_LEN_BINARY_ATTR (SQLULEN_Length)
         ),
         Filter_HYC00_HY114'Access
      );
   end Set_Row_Number;

   procedure Set_Trace
             (  Connection : in out ODBC_Connection;
                Value      : Boolean
             )  is
      Data : aliased SQLUINTEGER;
   begin
      case Value is
         when True  => Data := SQL_TRUE;
         when False => Data := SQL_FALSE;
      end case;
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_TRACE,
            Data
      )  );
   end Set_Trace;

   procedure Set_Tracefile
             (  Connection : in out ODBC_Connection;
                Value      : String
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_TRACEFILE,
            To_C (Value),
            Value'Length
      )  );
   end Set_Tracefile;

   procedure Set_Tracefile
             (  Connection : in out ODBC_Connection;
                Value      : Wide_String
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_TRACEFILE,
            To_C (Value),
            Value'Length * Bytes_In_Wide_Char
      )  );
   end Set_Tracefile;

   procedure Set_Translate_Lib
             (  Connection : in out ODBC_Connection;
                Value      : String
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_TRANSLATE_LIB,
            To_C (Value),
            Value'Length
      )  );
   end Set_Translate_Lib;

   procedure Set_Translate_Lib
             (  Connection : in out ODBC_Connection;
                Value      : Wide_String
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_TRANSLATE_LIB,
            To_C (Value),
            Value'Length
      )  );
   end Set_Translate_Lib;

   procedure Set_Translate_Option
             (  Connection : in out ODBC_Connection;
                Value      : SQLUINTEGER
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_TRANSLATE_OPTION,
            Value
      )  );
   end Set_Translate_Option;

   procedure Set_TXN_Isolation
             (  Connection : in out ODBC_Connection;
                Value      : SQL_TXN
             )  is
   begin
      Check
      (  Connection,
         SQLSetConnectAttr
         (  Connection.Handle,
            SQL_ATTR_TXN_ISOLATION,
            SQLUINTEGER (Value)
      )  );
   end Set_TXN_Isolation;

   function Table_Exists
            (  Command    : access ODBC_Command;
               Table_Name : String
            )  return Boolean is
      Data : char_array := To_C (Table_Name);
   begin
      Check (Command.all, SQLFreeStmt (Command.Handle, SQL_UNBIND));
      Check
      (  Command.all,
         SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS)
      );
      Check
      (  Command.all,
         SQLTables
         (  StatementHandle => Command.Handle,
            TableName       => Data (Data'First)'Unchecked_Access,
            NameLength3     => SQLSMALLINT (Table_Name'Length)
      )  );
      Check (Command.all, SQLFetch (Command.Handle));
      Close_Cursor (Command.all);
      return True;
   exception
      when Error : End_Error =>
         Close_Cursor (Command.all);
         return False;
      when others =>
         Close_Cursor (Command.all);
         raise;
   end Table_Exists;

   function Table_Exists
            (  Connection : access ODBC_Connection;
               Table_Name : String
            )  return Boolean is
      Command : aliased ODBC_Command (Connection);
   begin
      return Table_Exists (Command'Access, Table_Name);
   end Table_Exists;

   function Table_Exists
            (  Command    : access ODBC_Command;
               Table_Name : Wide_String
            )  return Boolean is
      Data : SQLWCHAR_Array := To_C (Table_Name);
   begin
      Check (Command.all, SQLFreeStmt (Command.Handle, SQL_UNBIND));
      Check
      (  Command.all,
         SQLFreeStmt (Command.Handle, SQL_RESET_PARAMS)
      );
      Check
      (  Command.all,
         SQLTables
         (  StatementHandle => Command.Handle,
            TableName       => Data (Data'First)'Unchecked_Access,
            NameLength3     => SQLSMALLINT (Table_Name'Length)
      )  );
      Check (Command.all, SQLFetch (Command.Handle));
      Close_Cursor (Command.all);
      return True;
   exception
      when End_Error =>
         Close_Cursor (Command.all);
         return False;
      when others =>
         Close_Cursor (Command.all);
         raise;
   end Table_Exists;

   function Table_Exists
            (  Connection : access ODBC_Connection;
               Table_Name : Wide_String
            )  return Boolean is
      Command : aliased ODBC_Command (Connection);
   begin
      return Table_Exists (Command'Access, Table_Name);
   end Table_Exists;

--  --
--  -- Get_Driver_HDESC -- Get connection information
--  --
--  --    Connection - The ODBC connection object
--  --
--  -- Returns :
--  --
--  --   The Driver Manager's descriptor handle
--  --
--  -- Exceptions :
--  --
--  --    Data_Error - ODBC driver error
--  --
--     function Get_Driver_HDESC
--              (  Connection : ODBC_Connection
--              )  return SQLULEN is
--     begin
--        return Get_Info (Connection, SQL_DRIVER_HDESC);
--     end Get_Driver_HDESC;
--  --
--  -- Get_Driver_HSTMT -- Get connection information
--  --
--  --    Connection - The ODBC connection object
--  --
--  -- Returns :
--  --
--  --   The Driver Manager statement handle
--  --
--  -- Exceptions :
--  --
--  --    Data_Error - ODBC driver error
--  --
--     function Get_Driver_HSTMT
--              (  Connection : ODBC_Connection
--              )  return SQLULEN is
--     begin
--        return Get_Info (Connection, SQL_DRIVER_HSTMT);
--     end Get_Driver_HSTMT;

end ODBC.API;
