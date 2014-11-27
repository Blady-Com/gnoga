--                                                                    --
--  package ODBC.Thin               Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2012       --
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
--  This is thin bindings to ODBC, which closely follows C API.  As such
--  it should  be somewhat  inconvenient  to use.  Where possible, plain
--  string  types like char_array  and SQLWCHAR_Array are  used  instead of
--  access SQLCHAR (SQLCHAR *). This interface is independent on the OS,
--  the ODBC implementation and machine architecture. It uses a machine/
--  ODBC provider-dependent  package  ODBC.Driver_Dependent.  When  GNAT
--  projects are used, the scenario variables select the correct version
--  of the latter.
--
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with ODBC.SQLTypes;         use ODBC.SQLTypes;

with ODBC.Driver_Dependent;

package ODBC.Thin is

   function SQL_LEN_BINARY_ATTR (Length : Positive) return SQLINTEGER;
   function SQL_LEN_BINARY_ATTR (Length : Positive) return SQLSMALLINT;

   function To_Ada (Value : SQLWCHAR_Array) return Wide_String;
   function To_C (Value : Wide_String) return SQLWCHAR_Array;

   function SQLAllocHandle
            (  HandleType      : SQL_HANDLE;
               InputHandle     : SQLHANDLE;
               OutputHandlePtr : access SQLHANDLE
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLAllocHandle;

   function SQLBindCol
            (  StatementHandle : SQLHSTMT;
               ColumnNumber    : SQLUSMALLINT;
               TargetType      : SQL_C_DATA_TYPE;
               TargetValuePtr  : SQLPOINTER;
               BufferLength    : SQLLEN;
               StrLen_or_Ind   : access SQLLEN
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLBindCol;

   function SQLBindParameter
            (  StatementHandle   : SQLHSTMT;
               ParameterNumber   : SQLUSMALLINT;
               InputOutputType   : SQL_PARAM;
               ValueType         : SQL_C_DATA_TYPE;
               ParameterType     : SQL_DATA_TYPE;
               ColumnSize        : SQLULEN;
               DecimalDigits     : SQLSMALLINT;
               ParameterValuePtr : SQLPOINTER;
               BufferLength      : SQLLEN;
               StrLen_or_IndPtr  : access SQLLEN
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLBindParameter;

   function SQLBrowseConnect
            (  ConnectionHandle    : SQLHDBC;
               InConnectionString  : char_array;
               StringLength1       : SQLSMALLINT;
               OutConnectionString : access char;
               BufferLength        : SQLSMALLINT;
               StringLength2Ptr    : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLBrowseConnect;

   function SQLBrowseConnect
            (  ConnectionHandle    : SQLHDBC;
               InConnectionString  : char_array;
               StringLength1       : SQLSMALLINT;
               OutConnectionString : access SQLWCHAR;
               BufferLength        : SQLSMALLINT;
               StringLength2Ptr    : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLBrowseConnectW;

   function SQLBulkOperations
            (  StatementHandle : SQLHSTMT;
               Operation       : SQLUSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLBulkOperations;

   function SQLCancel
            (  StatementHandle : SQLHSTMT
            )  return SQLRETURN renames
                      ODBC.Driver_Dependent.SQLCancel;

   function SQLCancelHandle
            (  HandleType : SQL_HANDLE;
               Handle     : SQLHANDLE
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLCancelHandle;

   function SQLCloseCursor
            (  StatementHandle : SQLHSTMT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLCloseCursor;

   function SQLColAttribute
            (  StatementHandle       : SQLHSTMT;
               ColumnNumber          : SQLUSMALLINT;
               FieldIdentifier       : SQL_DESC;
               CharacterAttributePtr : SQLPOINTER;
               BufferLength          : SQLSMALLINT;
               StringLengthPtr       : access SQLSMALLINT;
               NumericAttributePtr   : access SQLLEN
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLColAttribute;

   function SQLColAttribute
            (  StatementHandle       : SQLHSTMT;
               ColumnNumber          : SQLUSMALLINT;
               FieldIdentifier       : SQL_DESC;
               CharacterAttributePtr : access SQLWCHAR;
               BufferLength          : SQLSMALLINT;
               StringLengthPtr       : access SQLSMALLINT;
               NumericAttributePtr   : access SQLLEN
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLColAttributeW;

   function SQLColumnPrivileges
            (  StatementHandle : SQLHSTMT;
               CatalogName     : char_Ptr    := null;
               NameLength1     : SQLSMALLINT := 0;
               SchemaName      : char_Ptr    := null;
               NameLength2     : SQLSMALLINT:= 0;
               TableName       : char_Ptr    := null;
               NameLength3     : SQLSMALLINT := 0;
               ColumnName      : char_Ptr    := null;
               NameLength4     : SQLSMALLINT:= 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLColumnPrivileges;

   function SQLColumnPrivilegesW
            (  StatementHandle : SQLHSTMT;
               CatalogName     : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               SchemaName      : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               TableName       : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0;
               ColumnName      : SQLWCHAR_Ptr := null;
               NameLength4     : SQLSMALLINT  := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLColumnPrivilegesW;

   function SQLColumns
            (  StatementHandle : SQLHSTMT;
               CatalogName     : char_Ptr    := null;
               NameLength1     : SQLSMALLINT := 0;
               SchemaName      : char_Ptr    := null;
               NameLength2     : SQLSMALLINT := 0;
               TableName       : char_Ptr    := null;
               NameLength3     : SQLSMALLINT := 0;
               ColumnName      : char_Ptr    := null;
               NameLength4     : SQLSMALLINT := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLColumns;

   function SQLColumns
            (  StatementHandle : SQLHSTMT;
               CatalogName     : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               SchemaName      : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               TableName       : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0;
               ColumnName      : SQLWCHAR_Ptr := null;
               NameLength4     : SQLSMALLINT  := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLColumnsW;

--     function SQLCompleteAsync
--              (  HandleType      : SQL_HANDLE;
--                 Handle          : SQLHANDLE;
--                 AsyncRetCodePtr : access RETCODE
--              )  return SQLRETURN
--                    renames ODBC.Driver_Dependent.SQLCompleteAsync;

   function SQLConnect
            (  ConnectionHandle : SQLHDBC;
               ServerName       : char_array;
               NameLength1      : SQLSMALLINT;
               UserName         : char_array;
               NameLength2      : SQLSMALLINT;
               Authentication   : char_array;
               NameLength3      : SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLConnect;

   function SQLConnect
            (  ConnectionHandle : SQLHDBC;
               ServerName       : SQLWCHAR_Array;
               NameLength1      : SQLSMALLINT;
               UserName         : SQLWCHAR_Array;
               NameLength2      : SQLSMALLINT;
               Authentication   : SQLWCHAR_Array;
               NameLength3      : SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLConnectW;

   function SQLCopyDesc
            (  SourceDescHandle : SQLHDESC;
               TargetDescHandle : SQLHDESC
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLCopyDesc;

   function SQLDataSources
            (  EnvironmentHandle : SQLHENV;
               Direction         : SQL_DIRECTION;
               ServerName        : access char;
               BufferLength1     : SQLSMALLINT;
               NameLength1Ptr    : access SQLSMALLINT;
               Description       : access char;
               BufferLength2     : SQLSMALLINT;
               NameLength2Ptr    : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLDataSources;

   function SQLDataSources
            (  EnvironmentHandle : SQLHENV;
               Direction         : SQL_DIRECTION;
               ServerName        : access SQLWCHAR;
               BufferLength1     : SQLSMALLINT;
               NameLength1Ptr    : access SQLSMALLINT;
               Description       : access SQLWCHAR;
               BufferLength2     : SQLSMALLINT;
               NameLength2Ptr    : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLDataSourcesW;

   function SQLDescribeCol
            (  StatementHandle  : SQLHSTMT;
               ColumnNumber     : SQLUSMALLINT;
               ColumnName       : access char;
               BufferLength     : SQLSMALLINT;
               NameLengthPtr    : access SQLSMALLINT;
               DataTypePtr      : access SQL_DATA_TYPE;
               ColumnSizePtr    : access SQLULEN;
               DecimalDigitsPtr : access SQLSMALLINT;
               NullablePtr      : access SQL_NULLABLE_FIELD
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLDescribeCol;

   function SQLDescribeCol
            (  StatementHandle  : SQLHSTMT;
               ColumnNumber     : SQLUSMALLINT;
               ColumnName       : chars_ptr   := Null_Ptr;
               BufferLength     : SQLSMALLINT := 0;
               NameLengthPtr    : access SQLSMALLINT;
               DataTypePtr      : access SQL_DATA_TYPE;
               ColumnSizePtr    : access SQLULEN;
               DecimalDigitsPtr : access SQLSMALLINT;
               NullablePtr      : access SQL_NULLABLE_FIELD
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLDescribeCol;

   function SQLDescribeParam
            (  StatementHandle  : SQLHSTMT;
               ParameterNumber  : SQLUSMALLINT;
               DataTypePtr      : access SQL_DATA_TYPE;
               ParameterSizePtr : access SQLULEN;
               DecimalDigitsPtr : access SQLSMALLINT;
               NullablePtr      : access SQL_NULLABLE_FIELD
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLDescribeParam;

   function SQLDisconnect
            (  ConnectionHandle : SQLHDBC
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLDisconnect;

   function SQLDriverConnect
            (  ConnectionHandle    : SQLHDBC;
               WindowHandle        : SQLHWND;
               InConnectionString  : char_array;
               StringLength1       : SQLSMALLINT;
               OutConnectionString : char_array;
               BufferLength        : SQLSMALLINT;
               StringLength2Ptr    : access SQLSMALLINT;
               DriverCompletion    : SQLUSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLDriverConnect;

   function SQLDrivers
            (  EnvironmentHandle    : SQLHENV;
               Direction            : SQL_DRIVER_DIRECTION;
               DriverDescription    : access SQLCHAR;
               BufferLength1        : SQLSMALLINT;
               DescriptionLengthPtr : access SQLSMALLINT;
               DriverAttributes     : access SQLCHAR;
               BufferLength2        : SQLSMALLINT;
               AttributesLengthPtr  : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLDrivers;

   function SQLEndTran
            (  HandleType     : SQL_HANDLE;
               Handle         : SQLHANDLE;
               CompletionType : SQL_COMPLETION
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLEndTran;

   function SQLExecDirect
            (  StatementHandle : SQLHSTMT;
               StatementText   : char_array;
               TextLength      : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLExecDirect;

   function SQLExecDirect
            (  StatementHandle : SQLHSTMT;
               StatementText   : SQLWCHAR_Array;
               TextLength      : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLExecDirectW;

   function SQLExecute
            (  StatementHandle : SQLHSTMT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLExecute;

   function SQLExtendedFetch
            (  StatementHandle  : SQLHSTMT;
               FetchOrientation : SQL_DIRECTION;
               FetchOffset      : SQLLEN;
               RowCountPtr      : access SQLULEN;
               RowStatusArray   : access SQLUSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLExtendedFetch;

   function SQLFetch
            (  StatementHandle : SQLHSTMT
            )  return SQLRETURN renames ODBC.Driver_Dependent.SQLFetch;

   function SQLFetchScroll
            (  StatementHandle  : SQLHSTMT;
               FetchOrientation : SQL_DIRECTION;
               FetchOffset      : SQLLEN
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLFetchScroll;

   function SQLForeignKeys
            (  StatementHandle : SQLHSTMT;
               PKCatalogName   : char_Ptr    := null;
               NameLength1     : SQLSMALLINT := 0;
               PKSchemaName    : char_Ptr    := null;
               NameLength2     : SQLSMALLINT := 0;
               PKTableName     : char_Ptr    := null;
               NameLength3     : SQLSMALLINT := 0;
               FKCatalogName   : char_Ptr    := null;
               NameLength4     : SQLSMALLINT := 0;
               FKSchemaName    : char_Ptr    := null;
               NameLength5     : SQLSMALLINT := 0;
               FKTableName     : char_Ptr    := null;
               NameLength6     : SQLSMALLINT := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLForeignKeys;

   function SQLForeignKeysW
            (  StatementHandle : SQLHSTMT;
               PKCatalogName   : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               PKSchemaName    : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               PKTableName     : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0;
               FKCatalogName   : SQLWCHAR_Ptr := null;
               NameLength4     : SQLSMALLINT  := 0;
               FKSchemaName    : SQLWCHAR_Ptr := null;
               NameLength5     : SQLSMALLINT  := 0;
               FKTableName     : SQLWCHAR_Ptr := null;
               NameLength6     : SQLSMALLINT  := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLForeignKeysW;

   function SQLFreeHandle
            (  HandleType : SQL_HANDLE;
               Handle     : SQLHANDLE
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLFreeHandle;

   function SQLFreeStmt
            (  StatementHandle : SQLHSTMT;
               Option          : SQL_OPTION
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLFreeStmt;

   function SQLGetConnectAttr
            (  ConnectionHandle : SQLHDBC;
               Attribute        : SQL_ATTR;
               ValuePtr         : chars_ptr  := null_ptr;
               BufferLength     : SQLINTEGER := 0;
               StringLengthPtr  : access SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetConnectAttr;

   function SQLGetConnectAttr
            (  ConnectionHandle : SQLHDBC;
               Attribute        : SQL_ATTR;
               ValuePtr         : SQLPOINTER;
               BufferLength     : SQLINTEGER;
               StringLengthPtr  : access SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetConnectAttr;

   function SQLGetConnectAttr
            (  ConnectionHandle : SQLHDBC;
               Attribute        : SQL_ATTR;
               ValuePtr         : access char;
               BufferLength     : SQLINTEGER;
               StringLengthPtr  : access SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetConnectAttr;

   function SQLGetConnectAttr
            (  ConnectionHandle : SQLHDBC;
               Attribute        : SQL_ATTR;
               ValuePtr         : access SQLWCHAR;
               BufferLength     : SQLINTEGER;
               StringLengthPtr  : access SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetConnectAttrW;

   function SQLGetCursorName
            (  StatementHandle : SQLHSTMT;
               CursorName      : char_array;
               BufferLength    : SQLSMALLINT;
               NameLengthPtr   : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetCursorName;

   function SQLGetCursorName
            (  StatementHandle : SQLHSTMT;
               CursorName      : SQLWCHAR_Array;
               BufferLength    : SQLSMALLINT;
               NameLengthPtr   : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetCursorNameW;

   function SQLGetData
            (  StatementHandle  : SQLHSTMT;
               Col_or_Param_Num : SQLUSMALLINT;
               TargetType       : SQL_C_DATA_TYPE;
               TargetValuePtr   : SQLPOINTER;
               BufferLength     : SQLLEN;
               StrLen_or_IndPtr : access SQLLEN
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetData;

   function SQLGetDescField
            (  DescriptorHandle : SQLHDESC;
               RecNumber        : SQLSMALLINT;
               FieldIdentifier  : SQL_DESC;
               ValuePtr         : SQLPOINTER;
               BufferLength     : SQLINTEGER;
               StringLengthPtr  : access SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDescField;

   function SQLGetDescRec
            (  DescriptorHandle : SQLHDESC;
               RecNumber        : SQLSMALLINT;
               Name             : char_array;
               BufferLength     : SQLSMALLINT;
               StringLengthPtr  : access SQLSMALLINT;
               TypePtr          : access SQL_DATA_TYPE;
               SubTypePtr       : access SQL_DATETIME_SUBCODE;
               LengthPtr        : access SQLLEN;
               PrecisionPtr     : access SQLSMALLINT;
               ScalePtr         : access SQLSMALLINT;
               NullablePtr      : access SQL_NULLABLE_FIELD
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDescRec;

   function SQLGetDescRec
            (  DescriptorHandle : SQLHDESC;
               RecNumber        : SQLSMALLINT;
               Name             : SQLWCHAR_Array;
               BufferLength     : SQLSMALLINT;
               StringLengthPtr  : access SQLSMALLINT;
               TypePtr          : access SQL_DATA_TYPE;
               SubTypePtr       : access SQL_DATETIME_SUBCODE;
               LengthPtr        : access SQLLEN;
               PrecisionPtr     : access SQLSMALLINT;
               ScalePtr         : access SQLSMALLINT;
               NullablePtr      : access SQL_NULLABLE_FIELD
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDescRecW;

   function SQLGetDiagField -- Asking field size
            (  HandleType      : SQL_HANDLE;
               Handle          : SQLHANDLE;
               RecNumber       : SQLSMALLINT;
               DiagIdentifier  : SQL_DIAG;
               DiagInfoPtr     : chars_ptr   := null_ptr;
               BufferLength    : SQLSMALLINT := 0;
               StringLengthPtr : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDiagField;

   function SQLGetDiagField
            (  HandleType      : SQL_HANDLE;
               Handle          : SQLHANDLE;
               RecNumber       : SQLSMALLINT;
               DiagIdentifier  : SQL_DIAG;
               DiagInfoPtr     : access char;
               BufferLength    : SQLSMALLINT;
               StringLengthPtr : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDiagField;

   function SQLGetDiagField
            (  HandleType      : SQL_HANDLE;
               Handle          : SQLHANDLE;
               RecNumber       : SQLSMALLINT;
               DiagIdentifier  : SQL_DIAG;
               DiagInfoPtr     : access SQLWCHAR;
               BufferLength    : SQLSMALLINT;
               StringLengthPtr : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDiagFieldW;

   function SQLGetDiagField
            (  HandleType      : SQL_HANDLE;
               Handle          : SQLHANDLE;
               RecNumber       : SQLSMALLINT;
               DiagIdentifier  : SQL_DIAG;
               DiagInfoPtr     : SQLPOINTER;
               BufferLength    : SQLSMALLINT;
               StringLengthPtr : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDiagField;

   function SQLGetDiagRec -- Asking record size
            (  HandleType     : SQL_HANDLE;
               Handle         : SQLHANDLE;
               RecNumber      : SQLSMALLINT;
               State          : access SQLSTATE;
               NativeErrorPtr : access SQLINTEGER;
               MessageText    : chars_ptr   := null_ptr;
               BufferLength   : SQLSMALLINT := 0;
               TextLengthPtr  : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDiagRec;

   function SQLGetDiagRec
            (  HandleType     : SQL_HANDLE;
               Handle         : SQLHANDLE;
               RecNumber      : SQLSMALLINT;
               State          : access SQLSTATE;
               NativeErrorPtr : access SQLINTEGER;
               MessageText    : access char;
               BufferLength   : SQLSMALLINT;
               TextLengthPtr  : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDiagRec;

   function SQLGetDiagRec
            (  HandleType     : SQL_HANDLE;
               Handle         : SQLHANDLE;
               RecNumber      : SQLSMALLINT;
               State          : access SQLWSTATE;
               NativeErrorPtr : access SQLINTEGER;
               MessageText    : access SQLWCHAR;
               BufferLength   : SQLSMALLINT;
               TextLengthPtr  : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetDiagRecW;

   function SQLGetEnvAttr
            (  EnvironmentHandle : SQLHENV;
               Attribute         : SQL_ENV_ATTR;
               ValuePtr          : SQLPOINTER;
               BufferLength      : SQLINTEGER;
               StringLengthPtr   : access SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetEnvAttr;

   function SQLGetFunctions
            (  ConnectionHandle : SQLHDBC;
               FunctionId       : SQL_API;
               SupportedPtr     : access SQLUSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetFunctions;

   function SQLGetInfo
            (  ConnectionHandle : SQLHDBC;
               InfoType         : SQL_INFO;
               InfoValuePtr     : SQLPOINTER;
               BufferLength     : SQLSMALLINT;
               StringLengthPtr  : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetInfo;

   function SQLGetInfo
            (  ConnectionHandle : SQLHDBC;
               InfoType         : SQL_INFO;
               InfoValuePtr     : access char;
               BufferLength     : SQLSMALLINT;
               StringLengthPtr  : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetInfo;

   function SQLGetInfo
            (  ConnectionHandle : SQLHDBC;
               InfoType         : SQL_INFO;
               InfoValuePtr     : access SQLWCHAR;
               BufferLength     : SQLSMALLINT;
               StringLengthPtr  : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetInfoW;

   function SQLGetInfo
            (  ConnectionHandle : SQLHDBC;
               InfoType         : SQL_INFO;
               InfoValuePtr     : chars_ptr   := null_ptr;
               BufferLength     : SQLSMALLINT := 0;
               StringLengthPtr  : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetInfo;

   function SQLGetStmtAttr
             (  StatementHandle : SQLHSTMT;
                Attribute       : SQL_ATTR;
                ValuePtr        : SQLPOINTER;
                BufferLength    : SQLINTEGER;
                StringLengthPtr : access SQLINTEGER
             )  return SQLRETURN
                   renames ODBC.Driver_Dependent.SQLGetStmtAttr;

   function SQLGetTypeInfo
            (  StatementHandle : SQLHSTMT;
               DataType        : SQL_DATA_TYPE
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLGetTypeInfo;

   function SQLMoreResults
            (  StatementHandle : SQLHSTMT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLMoreResults;

   function SQLNativeSql
            (  ConnectionHandle : SQLHDBC;
               InStatementText  : char_array;
               TextLength1      : SQLINTEGER;
               OutStatementText : access char;
               BufferLength     : SQLINTEGER;
               TextLength2Ptr   : access SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLNativeSql;

   function SQLNativeSql
            (  ConnectionHandle : SQLHDBC;
               InStatementText  : SQLWCHAR_Array;
               TextLength1      : SQLINTEGER;
               OutStatementText : access SQLWCHAR;
               BufferLength     : SQLINTEGER;
               TextLength2Ptr   : access SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLNativeSqlW;

   function SQLNumParams
            (  StatementHandle   : SQLHSTMT;
               ParameterCountPtr : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLNumParams;

   function SQLNumResultCols
            (  StatementHandle : SQLHSTMT;
               ColumnCountPtr  : access SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLNumResultCols;

   function SQLParamData
            (  StatementHandle : SQLHSTMT;
               ValuePtrPtr     : access SQLPOINTER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLParamData;

   function SQLPrepare
            (  StatementHandle : SQLHSTMT;
               StatementText   : char_array;
               TextLength      : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLPrepare;

   function SQLPrepare
            (  StatementHandle : SQLHSTMT;
               StatementText   : SQLWCHAR_Array;
               TextLength      : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLPrepareW;

   function SQLPrimaryKeys
            (  StatementHandle : SQLHSTMT;
               CatalogName     : char_Ptr    := null;
               NameLength1     : SQLSMALLINT := 0;
               SchemaName      : char_Ptr    := null;
               NameLength2     : SQLSMALLINT := 0;
               TableName       : char_Ptr    := null;
               NameLength3     : SQLSMALLINT := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLPrimaryKeys;

   function SQLPrimaryKeys
            (  StatementHandle : SQLHSTMT;
               CatalogName     : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               SchemaName      : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               TableName       : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLPrimaryKeysW;

   function SQLProcedureColumns
            (  StatementHandle : SQLHSTMT;
               CatalogName     : char_Ptr    := null;
               NameLength1     : SQLSMALLINT := 0;
               SchemaName      : char_Ptr    := null;
               NameLength2     : SQLSMALLINT := 0;
               ProcName        : char_Ptr    := null;
               NameLength3     : SQLSMALLINT := 0;
               ColumnName      : char_Ptr    := null;
               NameLength4     : SQLSMALLINT := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLProcedureColumns;

   function SQLProcedureColumns
            (  StatementHandle : SQLHSTMT;
               CatalogName     : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               SchemaName      : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               ProcName        : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0;
               ColumnName      : SQLWCHAR_Ptr := null;
               NameLength4     : SQLSMALLINT  := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLProcedureColumnsW;

   function SQLProcedures
            (  StatementHandle : SQLHSTMT;
               CatalogName     : char_Ptr    := null;
               NameLength1     : SQLSMALLINT := 0;
               SchemaName      : char_Ptr    := null;
               NameLength2     : SQLSMALLINT := 0;
               ProcName        : char_Ptr    := null;
               NameLength3     : SQLSMALLINT := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLProcedures;

   function SQLProcedures
            (  StatementHandle : SQLHSTMT;
               CatalogName     : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               SchemaName      : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               ProcName        : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLProceduresW;

   function SQLPutData
            (  StatementHandle : SQLHSTMT;
               DataPtr         : SQLPOINTER;
               StrLen_or_Ind   : SQLLEN
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLPutData;

   function SQLRowCount
            (  StatementHandle : SQLHSTMT;
               RowCountPtr     : access SQLLEN
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLRowCount;

   function SQLSetConnectAttr
            (  ConnectionHandle : SQLHDBC;
               Attribute        : SQL_ATTR;
               ValuePtr         : SQLUINTEGER;
               StringLength     : SQLINTEGER := SQL_IS_UINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetConnectAttr;

   function SQLSetConnectAttr
            (  ConnectionHandle : SQLHDBC;
               Attribute        : SQL_ATTR;
               ValuePtr         : SQLULEN;
               StringLength     : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetConnectAttr;

   function SQLSetConnectAttr
            (  ConnectionHandle : SQLHDBC;
               Attribute        : SQL_ATTR;
               ValuePtr         : char_array;
               StringLength     : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetConnectAttr;

   function SQLSetConnectAttr
            (  ConnectionHandle : SQLHDBC;
               Attribute        : SQL_ATTR;
               ValuePtr         : SQLWCHAR_Array;
               StringLength     : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetConnectAttrW;

   function SQLSetCursorName
            (  StatementHandle : SQLHSTMT;
               CursorName      : access SQLCHAR;
               NameLength      : SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetCursorName;

   function SQLSetDescField
            (  DescriptorHandle : SQLHDESC;
               RecNumber        : SQLSMALLINT;
               FieldIdentifier  : SQL_DESC;
               ValuePtr         : SQLPOINTER;
               BufferLength     : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetDescField;

   function SQLSetDescRec
            (  DescriptorHandle : SQLHDESC;
               RecNumber        : SQLSMALLINT;
               SQLType          : SQL_DATA_TYPE;
               SQLSubType       : SQL_DATETIME_SUBCODE;
               Length           : SQLLEN;
               Precision        : SQLSMALLINT;
               Scale            : SQLSMALLINT;
               DataPtr          : SQLPOINTER;
               StringLengthPtr  : access SQLLEN;
               IndicatorPtr     : access SQLLEN
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetDescRec;

   function SQLSetEnvAttr
            (  EnvironmentHandle : SQLHENV;
               Attribute         : SQL_ENV_ATTR;
               Value             : SQLINTEGER;
               StringLength      : SQLINTEGER := 0 -- Never used
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetEnvAttr;

   function SQLSetEnvAttr
            (  EnvironmentHandle : SQLHENV;
               Attribute         : SQL_ENV_ATTR;
               Value             : SQLUINTEGER;
               StringLength      : SQLINTEGER := 0 -- Never used
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetEnvAttr;

   function SQLSetEnvAttr
            (  EnvironmentHandle : SQLHENV;
               Attribute         : SQL_ENV_ATTR;
               ValuePtr          : char_array;
               StringLength      : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetEnvAttr;

   function SQLSetPos
            (  StatementHandle : SQLHSTMT;
               RowNumber       : SQLSETPOSIROW;
               Operation       : SQL_OPERATION;
               LockType        : SQL_LOCKTYPE
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetPos;

   function SQLSetStmtAttr
            (  StatementHandle : SQLHSTMT;
               Attribute       : SQL_ATTR;
               ValuePtr        : SQLULEN;
               StringLength    : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetStmtAttr;

   function SQLSetStmtAttr
            (  StatementHandle : SQLHSTMT;
               Attribute       : SQL_ATTR;
               ValuePtr        : SQLPOINTER;
               StringLength    : SQLINTEGER
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSetStmtAttr;

   function SQLSpecialColumns
            (  StatementHandle : SQLHSTMT;
               IdentifierType  : SQL_IDENTIFIER;
               CatalogName     : char_Ptr    := null;
               NameLength1     : SQLSMALLINT;
               SchemaName      : char_Ptr    := null;
               NameLength2     : SQLSMALLINT;
               TableName       : char_Ptr    := null;
               NameLength3     : SQLSMALLINT;
               Scope           : SQLSMALLINT;
               Nullable        : SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSpecialColumns;

   function SQLSpecialColumns
            (  StatementHandle : SQLHSTMT;
               IdentifierType  : SQL_IDENTIFIER;
               CatalogName     : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               SchemaName      : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               TableName       : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0;
               Scope           : SQLSMALLINT;
               Nullable        : SQLSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLSpecialColumnsW;

   function SQLStatistics
            (  StatementHandle : SQLHSTMT;
               CatalogName     : char_Ptr    := null;
               NameLength1     : SQLSMALLINT := 0;
               SchemaName      : char_Ptr    := null;
               NameLength2     : SQLSMALLINT := 0;
               TableName       : char_Ptr    := null;
               NameLength3     : SQLSMALLINT := 0;
               Unique          : SQLUSMALLINT;
               Reserved        : SQLUSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLStatistics;

   function SQLStatistics
            (  StatementHandle : SQLHSTMT;
               CatalogName     : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               SchemaName      : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               TableName       : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0;
               Unique          : SQLUSMALLINT;
               Reserved        : SQLUSMALLINT
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLStatisticsW;

   function SQLTables
            (  StatementHandle : SQLHSTMT;
               CatalogName     : char_Ptr    := null;
               NameLength1     : SQLSMALLINT := 0;
               SchemaName      : char_Ptr    := null;
               NameLength2     : SQLSMALLINT := 0;
               TableName       : char_Ptr    := null;
               NameLength3     : SQLSMALLINT := 0;
               TableType       : char_Ptr    := null;
               NameLength4     : SQLSMALLINT := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLTables;

   function SQLTables
            (  StatementHandle : SQLHSTMT;
               CatalogName     : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               SchemaName      : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               TableName       : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0;
               TableType       : SQLWCHAR_Ptr := null;
               NameLength4     : SQLSMALLINT  := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLTablesW;

   function SQLTablePrivileges
            (  StatementHandle : SQLHSTMT;
               CatalogName     : char_Ptr    := null;
               NameLength1     : SQLSMALLINT := 0;
               SchemaName      : char_Ptr    := null;
               NameLength2     : SQLSMALLINT := 0;
               TableName       : char_Ptr    := null;
               NameLength3     : SQLSMALLINT := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLTablePrivileges;

   function SQLTablePrivileges
            (  StatementHandle : SQLHSTMT;
               CatalogName     : SQLWCHAR_Ptr := null;
               NameLength1     : SQLSMALLINT  := 0;
               SchemaName      : SQLWCHAR_Ptr := null;
               NameLength2     : SQLSMALLINT  := 0;
               TableName       : SQLWCHAR_Ptr := null;
               NameLength3     : SQLSMALLINT  := 0
            )  return SQLRETURN
                  renames ODBC.Driver_Dependent.SQLTablePrivilegesW;

end ODBC.Thin;
