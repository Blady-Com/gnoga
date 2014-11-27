--                                                                    --
--  package ODBC.SQLTypes           Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2012       --
--                                                                    --
--                                Last revision :  11:46 20 Oct 2012  --
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
with Ada.Calendar;  use Ada.Calendar;

with Interfaces.C;
with ODBC.Architecture_Dependent;
with System;

pragma Elaborate_All (ODBC.Architecture_Dependent);

package ODBC.SQLTypes is
--
-- SQL data types
--
   type SQLINTEGER   is new Interfaces.Integer_32;
   type SQLUINTEGER  is new Interfaces.Unsigned_32;
   type SQLSMALLINT  is new Interfaces.Integer_16;
   type SQLUSMALLINT is new Interfaces.Unsigned_16;
   type SQLBIGINT    is new Interfaces.Integer_64;
   type SQLUBIGINT   is new Interfaces.Unsigned_64;
   type SQLTINYINT   is new Interfaces.Integer_8;
   type SQLUTINYINT  is new Interfaces.Unsigned_8;
   type SQLDOUBLE    is new Interfaces.C.Double;
   type SQLREAL      is new Interfaces.C.C_Float;
   type SQLCHAR      is new Interfaces.C.unsigned_char;
   type SQLSCHAR     is new Interfaces.C.signed_char;
   subtype SQLWCHAR  is Wide_Character;

   type char_Ptr is access all Interfaces.C.char;
   pragma Convention (C, char_Ptr);

   type SQLWCHAR_Ptr is access all SQLWCHAR;
   pragma Convention (C, SQLWCHAR_Ptr);

   type SQLULEN is new ODBC.Architecture_Dependent.SQLULEN;
   type SQLLEN  is new ODBC.Architecture_Dependent.SQLLEN;

   type SQLROWSETSIZE is new SQLULEN;
   type SQLTRANSID    is new SQLULEN;

   type SQLROWOFFSET  is new SQLLEN;

   type SQLSETPOSIROW is new ODBC.Architecture_Dependent.SQLSETPOSIROW;

   type SQLSTATE  is array (1..6) of aliased Interfaces.C.char;
   pragma Convention (C, SQLSTATE);
   type SQLWSTATE is array (1..6) of aliased SQLWCHAR;
   pragma Convention (C, SQLWSTATE);

   type SQLWCHAR_Array is
      array (Interfaces.C.size_t range <>) of aliased SQLWCHAR;
   pragma Convention (C, SQLWCHAR_Array);

   type SQL_DATA_TYPE is new SQLSMALLINT;
   SQL_UTINYINT                  : constant SQL_DATA_TYPE := -28;
   SQL_UBIGINT                   : constant SQL_DATA_TYPE := -27;
   SQL_STINYINT                  : constant SQL_DATA_TYPE := -26;
   SQL_SBIGINT                   : constant SQL_DATA_TYPE := -25;
   SQL_ULONG                     : constant SQL_DATA_TYPE := -18;
   SQL_USHORT                    : constant SQL_DATA_TYPE := -17;
   SQL_SLONG                     : constant SQL_DATA_TYPE := -16;
   SQL_SSHORT                    : constant SQL_DATA_TYPE := -15;
   SQL_GUID                      : constant SQL_DATA_TYPE := -11;
   SQL_WLONGVARCHAR              : constant SQL_DATA_TYPE := -10;
   SQL_WVARCHAR                  : constant SQL_DATA_TYPE := -9;
   SQL_WCHAR                     : constant SQL_DATA_TYPE := -8;
   SQL_BIT                       : constant SQL_DATA_TYPE := -7;
   SQL_TINYINT                   : constant SQL_DATA_TYPE := -6;
   SQL_BIGINT                    : constant SQL_DATA_TYPE := -5;
   SQL_LONGVARBINARY             : constant SQL_DATA_TYPE := -4;
   SQL_VARBINARY                 : constant SQL_DATA_TYPE := -3;
   SQL_BINARY                    : constant SQL_DATA_TYPE := -2;
   SQL_LONGVARCHAR               : constant SQL_DATA_TYPE := -1;
   SQL_UNKNOWN_TYPE              : constant SQL_DATA_TYPE := 0;
   SQL_CHAR                      : constant SQL_DATA_TYPE := 1;
   SQL_NUMERIC                   : constant SQL_DATA_TYPE := 2;
   SQL_DECIMAL                   : constant SQL_DATA_TYPE := 3;
   SQL_INTEGER                   : constant SQL_DATA_TYPE := 4;
   SQL_LONG                      : constant SQL_DATA_TYPE := 4;
   SQL_SMALLINT                  : constant SQL_DATA_TYPE := 5;
   SQL_SHORT                     : constant SQL_DATA_TYPE := 5;
   SQL_FLOAT                     : constant SQL_DATA_TYPE := 6;
   SQL_REAL                      : constant SQL_DATA_TYPE := 7;
   SQL_DOUBLE                    : constant SQL_DATA_TYPE := 8;
   SQL_DATETIME                  : constant SQL_DATA_TYPE := 9;
   SQL_DATE                      : constant SQL_DATA_TYPE := 9;
   SQL_TIME                      : constant SQL_DATA_TYPE := 10;
   SQL_TIMESTAMP                 : constant SQL_DATA_TYPE := 11;
   SQL_VARCHAR                   : constant SQL_DATA_TYPE := 12;
   SQL_TYPE_DATE                 : constant SQL_DATA_TYPE := 91;
   SQL_TYPE_TIME                 : constant SQL_DATA_TYPE := 92;
   SQL_TYPE_TIMESTAMP            : constant SQL_DATA_TYPE := 93;
   SQL_DEFAULT                   : constant SQL_DATA_TYPE := 99;
   SQL_INTERVAL_YEAR             : constant SQL_DATA_TYPE := 101;
   SQL_INTERVAL_MONTH            : constant SQL_DATA_TYPE := 102;
   SQL_INTERVAL_DAY              : constant SQL_DATA_TYPE := 103;
   SQL_INTERVAL_HOUR             : constant SQL_DATA_TYPE := 104;
   SQL_INTERVAL_MINUTE           : constant SQL_DATA_TYPE := 105;
   SQL_INTERVAL_SECOND           : constant SQL_DATA_TYPE := 106;
   SQL_INTERVAL_YEAR_TO_MONTH    : constant SQL_DATA_TYPE := 107;
   SQL_INTERVAL_DAY_TO_HOUR      : constant SQL_DATA_TYPE := 108;
   SQL_INTERVAL_DAY_TO_MINUTE    : constant SQL_DATA_TYPE := 109;
   SQL_INTERVAL_DAY_TO_SECOND    : constant SQL_DATA_TYPE := 110;
   SQL_INTERVAL_HOUR_TO_MINUTE   : constant SQL_DATA_TYPE := 111;
   SQL_INTERVAL_HOUR_TO_SECOND   : constant SQL_DATA_TYPE := 112;
   SQL_INTERVAL_MINUTE_TO_SECOND : constant SQL_DATA_TYPE := 113;

   type SQL_C_DATA_TYPE is new SQLSMALLINT;
   SQL_C_UTINYINT                  : constant SQL_C_DATA_TYPE := -28;
   SQL_C_UBIGINT                   : constant SQL_C_DATA_TYPE := -27;
   SQL_C_STINYINT                  : constant SQL_C_DATA_TYPE := -26;
   SQL_C_SBIGINT                   : constant SQL_C_DATA_TYPE := -25;
   SQL_C_ULONG                     : constant SQL_C_DATA_TYPE := -18;
   SQL_C_USHORT                    : constant SQL_C_DATA_TYPE := -17;
   SQL_C_SLONG                     : constant SQL_C_DATA_TYPE := -16;
   SQL_C_SSHORT                    : constant SQL_C_DATA_TYPE := -15;
   SQL_C_GUID                      : constant SQL_C_DATA_TYPE := -11;
   SQL_C_WCHAR                     : constant SQL_C_DATA_TYPE := -8;
   SQL_C_BIT                       : constant SQL_C_DATA_TYPE := -7;
   SQL_C_TINYINT                   : constant SQL_C_DATA_TYPE := -6;
   SQL_C_BINARY                    : constant SQL_C_DATA_TYPE := -2;
   SQL_C_CHAR                      : constant SQL_C_DATA_TYPE := 1;
   SQL_C_NUMERIC                   : constant SQL_C_DATA_TYPE := 2;
   SQL_C_LONG                      : constant SQL_C_DATA_TYPE := 4;
   SQL_C_SHORT                     : constant SQL_C_DATA_TYPE := 5;
   SQL_C_FLOAT                     : constant SQL_C_DATA_TYPE := 7;
   SQL_C_DOUBLE                    : constant SQL_C_DATA_TYPE := 8;
   SQL_C_DATE                      : constant SQL_C_DATA_TYPE := 9;
   SQL_C_TIME                      : constant SQL_C_DATA_TYPE := 10;
   SQL_C_TIMESTAMP                 : constant SQL_C_DATA_TYPE := 11;
   SQL_C_TYPE_DATE                 : constant SQL_C_DATA_TYPE := 91;
   SQL_C_TYPE_TIME                 : constant SQL_C_DATA_TYPE := 92;
   SQL_C_TYPE_TIMESTAMP            : constant SQL_C_DATA_TYPE := 93;
   SQL_C_DEFAULT                   : constant SQL_C_DATA_TYPE := 99;
   SQL_C_INTERVAL_YEAR             : constant SQL_C_DATA_TYPE := 101;
   SQL_C_INTERVAL_MONTH            : constant SQL_C_DATA_TYPE := 102;
   SQL_C_INTERVAL_DAY              : constant SQL_C_DATA_TYPE := 103;
   SQL_C_INTERVAL_HOUR             : constant SQL_C_DATA_TYPE := 104;
   SQL_C_INTERVAL_MINUTE           : constant SQL_C_DATA_TYPE := 105;
   SQL_C_INTERVAL_SECOND           : constant SQL_C_DATA_TYPE := 106;
   SQL_C_INTERVAL_YEAR_TO_MONTH    : constant SQL_C_DATA_TYPE := 107;
   SQL_C_INTERVAL_DAY_TO_HOUR      : constant SQL_C_DATA_TYPE := 108;
   SQL_C_INTERVAL_DAY_TO_MINUTE    : constant SQL_C_DATA_TYPE := 109;
   SQL_C_INTERVAL_DAY_TO_SECOND    : constant SQL_C_DATA_TYPE := 110;
   SQL_C_INTERVAL_HOUR_TO_MINUTE   : constant SQL_C_DATA_TYPE := 111;
   SQL_C_INTERVAL_HOUR_TO_SECOND   : constant SQL_C_DATA_TYPE := 112;
   SQL_C_INTERVAL_MINUTE_TO_SECOND : constant SQL_C_DATA_TYPE := 113;
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

   type SQL_TIME_FRACTION is new SQLUINTEGER range 0 .. 999_999_999;

   type SQL_TIMESTAMP_STRUCT is record
      Year     : SQLSMALLINT;
      Month    : SQLUSMALLINT;
      Day      : SQLUSMALLINT;
      Hour     : SQLUSMALLINT;
      Minute   : SQLUSMALLINT;
      Second   : SQLUSMALLINT;
      Fraction : SQL_TIME_FRACTION;
   end record;
   pragma Convention (C, SQL_TIMESTAMP_STRUCT);
--
-- From_Time -- Conversion from and to Ada.Calendar.Time
-- To_Time
--
   function To_Time (Value : SQL_TIMESTAMP_STRUCT) return Time;
   function From_Time (Value : Time) return SQL_TIMESTAMP_STRUCT;

   type SQL_COMPLETION is new SQLSMALLINT;
   SQL_COMMIT   : constant SQL_COMPLETION := 0;
   SQL_ROLLBACK : constant SQL_COMPLETION := 1;

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

   type SQLRETURN is new SQLSMALLINT;
   type RETCODE   is new SQLSMALLINT;

   SQL_INVALID_HANDLE    : constant SQLRETURN := -2;
   SQL_ERROR             : constant SQLRETURN := -1;
   SQL_SUCCESS           : constant SQLRETURN := 0;
   SQL_SUCCESS_WITH_INFO : constant SQLRETURN := 1;
   SQL_STILL_EXECUTING   : constant SQLRETURN := 2;
   SQL_NEED_DATA         : constant SQLRETURN := 99;
   SQL_NO_DATA           : constant SQLRETURN := 100;
   SQL_NO_DATA_FOUND     : constant SQLRETURN := SQL_NO_DATA;

   SQL_DATA_AT_EXEC      : constant := -2;
   SQL_NULL_DATA         : constant := -1;

   type SQLPOINTER is new System.Address;
   type SQLHANDLE  is new SQLPOINTER;
   type SQLHENV    is new SQLHANDLE;
   type SQLHDBC    is new SQLHANDLE;
   type SQLHDESC   is new SQLHANDLE;
   type SQLHSTMT   is new SQLHANDLE;
   type SQLHWND    is new SQLHANDLE;

   SQL_NULL_HANDLE : constant SQLHANDLE :=
      SQLHANDLE (System.Null_Address);

   type SQL_HANDLE is new SQLSMALLINT;
   SQL_HANDLE_ENV  : constant SQL_HANDLE := 1;
   SQL_HANDLE_DBC  : constant SQL_HANDLE := 2;
   SQL_HANDLE_STMT : constant SQL_HANDLE := 3;
   SQL_HANDLE_DESC : constant SQL_HANDLE := 4;

   type SQL_OPTION is new SQLUSMALLINT;
   SQL_CLOSE        : constant SQL_OPTION := 0;
   SQL_DROP         : constant SQL_OPTION := 1;
   SQL_UNBIND       : constant SQL_OPTION := 2;
   SQL_RESET_PARAMS : constant SQL_OPTION := 3;

   SQL_LEN_BINARY_ATTR_OFFSET : constant := -100;

   type SQL_API is new SQLSMALLINT;
   SQL_API_SQLALLOCCONNECT      : constant SQL_API := 1;
   SQL_API_SQLALLOCENV          : constant SQL_API := 2;
   SQL_API_SQLALLOCSTMT         : constant SQL_API := 3;
   SQL_API_SQLBINDCOL           : constant SQL_API := 4;
   SQL_API_SQLCANCEL            : constant SQL_API := 5;
   SQL_API_SQLCOLATTRIBUTE      : constant SQL_API := 6;
   SQL_API_SQLCONNECT           : constant SQL_API := 7;
   SQL_API_SQLDESCRIBECOL       : constant SQL_API := 8;
   SQL_API_SQLDISCONNECT        : constant SQL_API := 9;
   SQL_API_SQLERROR             : constant SQL_API := 10;
   SQL_API_SQLEXECDIRECT        : constant SQL_API := 11;
   SQL_API_SQLEXECUTE           : constant SQL_API := 12;
   SQL_API_SQLFETCH             : constant SQL_API := 13;
   SQL_API_SQLFREECONNECT       : constant SQL_API := 14;
   SQL_API_SQLFREEENV           : constant SQL_API := 15;
   SQL_API_SQLFREESTMT          : constant SQL_API := 16;
   SQL_API_SQLGETCURSORNAME     : constant SQL_API := 17;
   SQL_API_SQLNUMRESULTCOLS     : constant SQL_API := 18;
   SQL_API_SQLPREPARE           : constant SQL_API := 19;
   SQL_API_SQLROWCOUNT          : constant SQL_API := 20;
   SQL_API_SQLSETCURSORNAME     : constant SQL_API := 21;
   SQL_API_SQLSETPARAM          : constant SQL_API := 22;
   SQL_API_SQLTRANSACT          : constant SQL_API := 23;
   SQL_API_SQLBULKOPERATIONS    : constant SQL_API := 24;
   SQL_API_SQLCOLUMNS           : constant SQL_API := 40;
   SQL_API_SQLDRIVERCONNECT     : constant SQL_API := 41;
   SQL_API_SQLGETCONNECTOPTION  : constant SQL_API := 42;
   SQL_API_SQLGETDATA           : constant SQL_API := 43;
   SQL_API_SQLGETFUNCTIONS      : constant SQL_API := 44;
   SQL_API_SQLGETINFO           : constant SQL_API := 45;
   SQL_API_SQLGETSTMTOPTION     : constant SQL_API := 46;
   SQL_API_SQLGETTYPEINFO       : constant SQL_API := 47;
   SQL_API_SQLPARAMDATA         : constant SQL_API := 48;
   SQL_API_SQLPUTDATA           : constant SQL_API := 49;
   SQL_API_SQLSETCONNECTOPTION  : constant SQL_API := 50;
   SQL_API_SQLSETSTMTOPTION     : constant SQL_API := 51;
   SQL_API_SQLSPECIALCOLUMNS    : constant SQL_API := 52;
   SQL_API_SQLSTATISTICS        : constant SQL_API := 53;
   SQL_API_SQLTABLES            : constant SQL_API := 54;
   SQL_API_SQLBROWSECONNECT     : constant SQL_API := 55;
   SQL_API_SQLCOLUMNPRIVILEGES  : constant SQL_API := 56;
   SQL_API_SQLDATASOURCES       : constant SQL_API := 57;
   SQL_API_SQLDESCRIBEPARAM     : constant SQL_API := 58;
   SQL_API_SQLEXTENDEDFETCH     : constant SQL_API := 59;
   SQL_API_SQLFOREIGNKEYS       : constant SQL_API := 60;
   SQL_API_SQLMORERESULTS       : constant SQL_API := 61;
   SQL_API_SQLNATIVESQL         : constant SQL_API := 62;
   SQL_API_SQLNUMPARAMS         : constant SQL_API := 63;
   SQL_API_SQLPARAMOPTIONS      : constant SQL_API := 64;
   SQL_API_SQLPRIMARYKEYS       : constant SQL_API := 65;
   SQL_API_SQLPROCEDURECOLUMNS  : constant SQL_API := 66;
   SQL_API_SQLPROCEDURES        : constant SQL_API := 67;
   SQL_API_SQLSETPOS            : constant SQL_API := 68;
   SQL_API_SQLSETSCROLLOPTIONS  : constant SQL_API := 69;
   SQL_API_SQLTABLEPRIVILEGES   : constant SQL_API := 70;
   SQL_API_SQLDRIVERS           : constant SQL_API := 71;
   SQL_API_SQLBINDPARAMETER     : constant SQL_API := 72;
   SQL_API_SQLALLOCHANDLESTD    : constant SQL_API := 73;
   SQL_API_SQLALLOCHANDLE       : constant SQL_API := 1001;
   SQL_API_SQLBINDPARAM         : constant SQL_API := 1002;
   SQL_API_SQLCLOSECURSOR       : constant SQL_API := 1003;
   SQL_API_SQLCOPYDESC          : constant SQL_API := 1004;
   SQL_API_SQLENDTRAN           : constant SQL_API := 1005;
   SQL_API_SQLFREEHANDLE        : constant SQL_API := 1006;
   SQL_API_SQLGETCONNECTATTR    : constant SQL_API := 1007;
   SQL_API_SQLGETDESCFIELD      : constant SQL_API := 1008;
   SQL_API_SQLGETDESCREC        : constant SQL_API := 1009;
   SQL_API_SQLGETDIAGFIELD      : constant SQL_API := 1010;
   SQL_API_SQLGETDIAGREC        : constant SQL_API := 1011;
   SQL_API_SQLGETENVATTR        : constant SQL_API := 1012;
   SQL_API_SQLGETSTMTATTR       : constant SQL_API := 1014;
   SQL_API_SQLSETCONNECTATTR    : constant SQL_API := 1016;
   SQL_API_SQLSETDESCFIELD      : constant SQL_API := 1017;
   SQL_API_SQLSETDESCREC        : constant SQL_API := 1018;
   SQL_API_SQLSETENVATTR        : constant SQL_API := 1019;
   SQL_API_SQLSETSTMTATTR       : constant SQL_API := 1020;
   SQL_API_SQLFETCHSCROLL       : constant SQL_API := 1021;

   type SQL_ATTR is new SQLINTEGER;
   SQL_ATTR_CURSOR_SENSITIVITY         : constant SQL_ATTR := -2;
   SQL_ATTR_CURSOR_SCROLLABLE          : constant SQL_ATTR := -1;
   SQL_ATTR_QUERY_TIMEOUT              : constant SQL_ATTR := 0;
   SQL_ATTR_MAX_ROWS                   : constant SQL_ATTR := 1;
   SQL_ATTR_NOSCAN                     : constant SQL_ATTR := 2;
   SQL_ATTR_MAX_LENGTH                 : constant SQL_ATTR := 3;
   SQL_ATTR_ASYNC_ENABLE               : constant SQL_ATTR := 4;
   SQL_ATTR_ROW_BIND_TYPE              : constant SQL_ATTR := 5;
   SQL_ATTR_CURSOR_TYPE                : constant SQL_ATTR := 6;
   SQL_ATTR_CONCURRENCY                : constant SQL_ATTR := 7;
   SQL_ATTR_KEYSET_SIZE                : constant SQL_ATTR := 8;
   SQL_ATTR_SIMULATE_CURSOR            : constant SQL_ATTR := 10;
   SQL_ATTR_RETRIEVE_DATA              : constant SQL_ATTR := 11;
   SQL_ATTR_USE_BOOKMARKS              : constant SQL_ATTR := 12;
   SQL_ATTR_ROW_NUMBER                 : constant SQL_ATTR := 14;
   SQL_ATTR_ENABLE_AUTO_IPD            : constant SQL_ATTR := 15;
   SQL_ATTR_FETCH_BOOKMARK_PTR         : constant SQL_ATTR := 16;
   SQL_ATTR_PARAM_BIND_OFFSET_PTR      : constant SQL_ATTR := 17;
   SQL_ATTR_PARAM_BIND_TYPE            : constant SQL_ATTR := 18;
   SQL_ATTR_PARAM_OPERATION_PTR        : constant SQL_ATTR := 19;
   SQL_ATTR_PARAM_STATUS_PTR           : constant SQL_ATTR := 20;
   SQL_ATTR_PARAMS_PROCESSED_PTR       : constant SQL_ATTR := 21;
   SQL_ATTR_PARAMSET_SIZE              : constant SQL_ATTR := 22;
   SQL_ATTR_ROW_ARRAY_SIZE             : constant SQL_ATTR := 27;
   SQL_ATTR_ROW_BIND_OFFSET_PTR        : constant SQL_ATTR := 23;
   SQL_ATTR_ROW_OPERATION_PTR          : constant SQL_ATTR := 24;
   SQL_ATTR_ROW_STATUS_PTR             : constant SQL_ATTR := 25;
   SQL_ATTR_ROWS_FETCHED_PTR           : constant SQL_ATTR := 26;
   SQL_ATTR_ACCESS_MODE                : constant SQL_ATTR := 101;
   SQL_ATTR_AUTOCOMMIT                 : constant SQL_ATTR := 102;
   SQL_ATTR_LOGIN_TIMEOUT              : constant SQL_ATTR := 103;
   SQL_ATTR_TRACE                      : constant SQL_ATTR := 104;
   SQL_ATTR_TRACEFILE                  : constant SQL_ATTR := 105;
   SQL_ATTR_TRANSLATE_LIB              : constant SQL_ATTR := 106;
   SQL_ATTR_TRANSLATE_OPTION           : constant SQL_ATTR := 107;
   SQL_ATTR_TXN_ISOLATION              : constant SQL_ATTR := 108;
   SQL_ATTR_CURRENT_CATALOG            : constant SQL_ATTR := 109;
   SQL_ATTR_ODBC_CURSORS               : constant SQL_ATTR := 110;
   SQL_ATTR_QUIET_MODE                 : constant SQL_ATTR := 111;
   SQL_ATTR_PACKET_SIZE                : constant SQL_ATTR := 112;
   SQL_ATTR_CONNECTION_TIMEOUT         : constant SQL_ATTR := 113;
   SQL_ATTR_DISCONNECT_BEHAVIOR        : constant SQL_ATTR := 114;
   SQL_ATTR_ANSI_APP                   : constant SQL_ATTR := 115;
   SQL_ATTR_ASYNC_DBC_FUNCTIONS_ENABLE : constant SQL_ATTR := 117;
   SQL_ATTR_ENLIST_IN_DTC              : constant SQL_ATTR := 1207;
   SQL_ATTR_ENLIST_IN_XA               : constant SQL_ATTR := 1208;
   SQL_ATTR_CONNECTION_DEAD            : constant SQL_ATTR := 1209;
   SQL_ATTR_AUTO_IPD                   : constant SQL_ATTR := 10001;
   SQL_ATTR_METADATA_ID                : constant SQL_ATTR := 10014;
   SQL_ATTR_APP_ROW_DESC               : constant SQL_ATTR := 10010;
   SQL_ATTR_APP_PARAM_DESC             : constant SQL_ATTR := 10011;
   SQL_ATTR_IMP_ROW_DESC               : constant SQL_ATTR := 10012;
   SQL_ATTR_IMP_PARAM_DESC             : constant SQL_ATTR := 10013;

-- SQL_ATTR_ASYNC_EVENT
-- SQL_ATTR_ASYNC_PCALLBACK
-- SQL_ATTR_ASYNC_PCONTEXT
-- SQL_ATTR_INFO_TOKEN
-- SQL_ATTR_READONLY          0
-- SQL_ATTR_WRITE             1
-- SQL_ATTR_READWRITE_UNKNOWN 2

   type SQL_ENV_ATTR is new SQLINTEGER;
   SQL_ATTR_ODBC_VERSION       : constant SQL_ENV_ATTR := 200;
   SQL_ATTR_CONNECTION_POOLING : constant SQL_ENV_ATTR := 201;
   SQL_ATTR_CP_MATCH           : constant SQL_ENV_ATTR := 202;
   SQL_ATTR_OUTPUT_NTS         : constant SQL_ENV_ATTR := 10001;

   type SQL_DESC is new SQLUSMALLINT;
   SQL_DESC_CONCISE_TYPE                : constant SQL_DESC := 2;
   SQL_DESC_DISPLAY_SIZE                : constant SQL_DESC := 6;
   SQL_DESC_UNSIGNED                    : constant SQL_DESC := 8;
   SQL_DESC_FIXED_PREC_SCALE            : constant SQL_DESC := 9;
   SQL_DESC_UPDATABLE                   : constant SQL_DESC := 10;
   SQL_DESC_AUTO_UNIQUE_VALUE           : constant SQL_DESC := 11;
   SQL_DESC_CASE_SENSITIVE              : constant SQL_DESC := 12;
   SQL_DESC_SEARCHABLE                  : constant SQL_DESC := 13;
   SQL_DESC_TYPE_NAME                   : constant SQL_DESC := 14;
   SQL_DESC_TABLE_NAME                  : constant SQL_DESC := 15;
   SQL_DESC_SCHEMA_NAME                 : constant SQL_DESC := 16;
   SQL_DESC_CATALOG_NAME                : constant SQL_DESC := 17;
   SQL_DESC_LABEL                       : constant SQL_DESC := 18;
   SQL_DESC_ARRAY_SIZE                  : constant SQL_DESC := 20;
   SQL_DESC_ARRAY_STATUS_PTR            : constant SQL_DESC := 21;
   SQL_DESC_BASE_COLUMN_NAME            : constant SQL_DESC := 22;
   SQL_DESC_BASE_TABLE_NAME             : constant SQL_DESC := 23;
   SQL_DESC_BIND_OFFSET_PTR             : constant SQL_DESC := 24;
   SQL_DESC_BIND_TYPE                   : constant SQL_DESC := 25;
   SQL_DESC_DATETIME_INTERVAL_PRECISION : constant SQL_DESC := 26;
   SQL_DESC_LITERAL_PREFIX              : constant SQL_DESC := 27;
   SQL_DESC_LITERAL_SUFFIX              : constant SQL_DESC := 28;
   SQL_DESC_LOCAL_TYPE_NAME             : constant SQL_DESC := 29;
   SQL_DESC_MAXIMUM_SCALE               : constant SQL_DESC := 30;
   SQL_DESC_MINIMUM_SCALE               : constant SQL_DESC := 31;
   SQL_DESC_NUM_PREC_RADIX              : constant SQL_DESC := 32;
   SQL_DESC_PARAMETER_TYPE              : constant SQL_DESC := 33;
   SQL_DESC_ROWS_PROCESSED_PTR          : constant SQL_DESC := 34;
   SQL_DESC_ROWVER                      : constant SQL_DESC := 35;
   SQL_DESC_COUNT                       : constant SQL_DESC := 1001;
   SQL_DESC_TYPE                        : constant SQL_DESC := 1002;
   SQL_DESC_LENGTH                      : constant SQL_DESC := 1003;
   SQL_DESC_OCTET_LENGTH_PTR            : constant SQL_DESC := 1004;
   SQL_DESC_PRECISION                   : constant SQL_DESC := 1005;
   SQL_DESC_SCALE                       : constant SQL_DESC := 1006;
   SQL_DESC_DATETIME_INTERVAL_CODE      : constant SQL_DESC := 1007;
   SQL_DESC_NULLABLE                    : constant SQL_DESC := 1008;
   SQL_DESC_INDICATOR_PTR               : constant SQL_DESC := 1009;
   SQL_DESC_DATA_PTR                    : constant SQL_DESC := 1010;
   SQL_DESC_NAME                        : constant SQL_DESC := 1011;
   SQL_DESC_UNNAMED                     : constant SQL_DESC := 1012;
   SQL_DESC_OCTET_LENGTH                : constant SQL_DESC := 1013;
   SQL_DESC_ALLOC_TYPE                  : constant SQL_DESC := 1099;

   type SQL_DATETIME_SUBCODE is new SQLSMALLINT range 1..13;
   SQL_CODE_YEAR             : constant SQL_DATETIME_SUBCODE := 1;
   SQL_CODE_MONTH            : constant SQL_DATETIME_SUBCODE := 2;
   SQL_CODE_DAY              : constant SQL_DATETIME_SUBCODE := 3;
   SQL_CODE_HOUR             : constant SQL_DATETIME_SUBCODE := 4;
   SQL_CODE_MINUTE           : constant SQL_DATETIME_SUBCODE := 5;
   SQL_CODE_SECOND           : constant SQL_DATETIME_SUBCODE := 6;
   SQL_CODE_YEAR_TO_MONTH    : constant SQL_DATETIME_SUBCODE := 7;
   SQL_CODE_DAY_TO_HOUR      : constant SQL_DATETIME_SUBCODE := 8;
   SQL_CODE_DAY_TO_MINUTE    : constant SQL_DATETIME_SUBCODE := 9;
   SQL_CODE_DAY_TO_SECOND    : constant SQL_DATETIME_SUBCODE := 10;
   SQL_CODE_HOUR_TO_MINUTE   : constant SQL_DATETIME_SUBCODE := 11;
   SQL_CODE_HOUR_TO_SECOND   : constant SQL_DATETIME_SUBCODE := 12;
   SQL_CODE_MINUTE_TO_SECOND : constant SQL_DATETIME_SUBCODE := 13;

   type SQL_DIAG is new SQLSMALLINT;
   SQL_DIAG_CURSOR_ROW_COUNT      : constant SQL_DIAG := -1249;
   SQL_DIAG_ROW_NUMBER            : constant SQL_DIAG := -1248;
   SQL_DIAG_COLUMN_NUMBER         : constant SQL_DIAG := -1247;
   SQL_DIAG_RETURNCODE            : constant SQL_DIAG := 1;
   SQL_DIAG_NUMBER                : constant SQL_DIAG := 2;
   SQL_DIAG_ROW_COUNT             : constant SQL_DIAG := 3;
   SQL_DIAG_SQLSTATE              : constant SQL_DIAG := 4;
   SQL_DIAG_NATIVE                : constant SQL_DIAG := 5;
   SQL_DIAG_MESSAGE_TEXT          : constant SQL_DIAG := 6;
   SQL_DIAG_DYNAMIC_FUNCTION      : constant SQL_DIAG := 7;
   SQL_DIAG_CLASS_ORIGIN          : constant SQL_DIAG := 8;
   SQL_DIAG_SUBCLASS_ORIGIN       : constant SQL_DIAG := 9;
   SQL_DIAG_CONNECTION_NAME       : constant SQL_DIAG := 10;
   SQL_DIAG_SERVER_NAME           : constant SQL_DIAG := 11;
   SQL_DIAG_DYNAMIC_FUNCTION_CODE : constant SQL_DIAG := 12;

   type SQL_INFO is new SQLUSMALLINT;
   SQL_MAXIMUM_DRIVER_CONNECTIONS      : constant SQL_INFO := 0;
   SQL_MAX_DRIVER_CONNECTIONS          : constant SQL_INFO := 0;
   SQL_MAXIMUM_CONCURRENT_ACTIVITIES   : constant SQL_INFO := 1;
   SQL_MAX_CONCURRENT_ACTIVITIES       : constant SQL_INFO := 1;
   SQL_DATA_SOURCE_NAME                : constant SQL_INFO := 2;
   SQL_DRIVER_HDBC                     : constant SQL_INFO := 3;
   SQL_DRIVER_HENV                     : constant SQL_INFO := 4;
   SQL_DRIVER_HSTMT                    : constant SQL_INFO := 5;
   SQL_DRIVER_NAME                     : constant SQL_INFO := 6;
   SQL_DRIVER_VER                      : constant SQL_INFO := 7;
   SQL_FETCH_DIRECTION                 : constant SQL_INFO := 8;
   SQL_ODBC_API_CONFORMANCE            : constant SQL_INFO := 9;
   SQL_ODBC_VER                        : constant SQL_INFO := 10;
   SQL_ROW_UPDATES                     : constant SQL_INFO := 11;
   SQL_ODBC_SAG_CLI_CONFORMANCE        : constant SQL_INFO := 12;
   SQL_SERVER_NAME                     : constant SQL_INFO := 13;
   SQL_SEARCH_PATTERN_ESCAPE           : constant SQL_INFO := 14;
   SQL_ODBC_SQL_CONFORMANCE            : constant SQL_INFO := 15;
   SQL_DATABASE_NAME                   : constant SQL_INFO := 16;
   SQL_DBMS_NAME                       : constant SQL_INFO := 17;
   SQL_DBMS_VER                        : constant SQL_INFO := 18;
   SQL_ACCESSIBLE_TABLES               : constant SQL_INFO := 19;
   SQL_ACCESSIBLE_PROCEDURES           : constant SQL_INFO := 20;
   SQL_PROCEDURES                      : constant SQL_INFO := 21;
   SQL_CONCAT_NULL_BEHAVIOR            : constant SQL_INFO := 22;
   SQL_CURSOR_COMMIT_BEHAVIOR          : constant SQL_INFO := 23;
   SQL_CURSOR_ROLLBACK_BEHAVIOR        : constant SQL_INFO := 24;
   SQL_DATA_SOURCE_READ_ONLY           : constant SQL_INFO := 25;
   SQL_DEFAULT_TXN_ISOLATION           : constant SQL_INFO := 26;
   SQL_DEFAULT_TRANSACTION_ISOLATION   : constant SQL_INFO := 26;
   SQL_EXPRESSIONS_IN_ORDERBY          : constant SQL_INFO := 27;
   SQL_IDENTIFIER_CASE                 : constant SQL_INFO := 28;
   SQL_IDENTIFIER_QUOTE_CHAR           : constant SQL_INFO := 29;
   SQL_MAXIMUM_COLUMN_NAME_LENGTH      : constant SQL_INFO := 30;
   SQL_MAX_COLUMN_NAME_LEN             : constant SQL_INFO := 30;
   SQL_MAXIMUM_CURSOR_NAME_LENGTH      : constant SQL_INFO := 31;
   SQL_MAX_CURSOR_NAME_LEN             : constant SQL_INFO := 31;
   SQL_MAXIMUM_SCHEMA_NAME_LENGTH      : constant SQL_INFO := 32;
   SQL_MAX_SCHEMA_NAME_LEN             : constant SQL_INFO := 32;
   SQL_MAXIMUM_PROCEDURE_NAME_LENGTH   : constant SQL_INFO := 33;
   SQL_MAX_PROCEDURE_NAME_LEN          : constant SQL_INFO := 33;
   SQL_MAXIMUM_CATALOG_NAME_LENGTH     : constant SQL_INFO := 34;
   SQL_MAX_CATALOG_NAME_LEN            : constant SQL_INFO := 34;
   SQL_MAXIMUM_TABLE_NAME_LENGTH       : constant SQL_INFO := 35;
   SQL_MAX_TABLE_NAME_LENGTH           : constant SQL_INFO := 35;
   SQL_MAX_TABLE_NAME_LEN              : constant SQL_INFO := 35;
   SQL_MULTIPLE_RESULT_SETS            : constant SQL_INFO := 36;
   SQL_MULT_RESULT_SETS                : constant SQL_INFO := 36;
   SQL_MULTIPLE_ACTIVE_TRANSACTIONS    : constant SQL_INFO := 37;
   SQL_MULTIPLE_ACTIVE_TXN             : constant SQL_INFO := 37;
   SQL_OUTER_JOINS                     : constant SQL_INFO := 38;
   SQL_SCHEMA_TERM                     : constant SQL_INFO := 39;
   SQL_OWNER_TERM                      : constant SQL_INFO := 39;
   SQL_PROCEDURE_TERM                  : constant SQL_INFO := 40;
   SQL_QUALIFIER_NAME_SEPARATOR        : constant SQL_INFO := 41;
   SQL_CATALOG_NAME_SEPARATOR          : constant SQL_INFO := 41;
   SQL_QUALIFIER_TERM                  : constant SQL_INFO := 42;
   SQL_CATALOG_TERM                    : constant SQL_INFO := 42;
   SQL_SCROLL_CONCURRENCY              : constant SQL_INFO := 43;
   SQL_SCROLL_OPTIONS                  : constant SQL_INFO := 44;
   SQL_TABLE_TERM                      : constant SQL_INFO := 45;
   SQL_TRANSACTION_CAPABLE             : constant SQL_INFO := 46;
   SQL_TXN_CAPABLE                     : constant SQL_INFO := 46;
   SQL_USER_NAME                       : constant SQL_INFO := 47;
   SQL_CONVERT_FUNCTIONS               : constant SQL_INFO := 48;
   SQL_NUMERIC_FUNCTIONS               : constant SQL_INFO := 49;
   SQL_STRING_FUNCTIONS                : constant SQL_INFO := 50;
   SQL_SYSTEM_FUNCTIONS                : constant SQL_INFO := 51;
   SQL_TIMEDATE_FUNCTIONS              : constant SQL_INFO := 52;
   SQL_CONVERT_BIGINT                  : constant SQL_INFO := 53;
   SQL_CONVERT_BINARY                  : constant SQL_INFO := 54;
   SQL_CONVERT_BIT                     : constant SQL_INFO := 55;
   SQL_CONVERT_CHAR                    : constant SQL_INFO := 56;
   SQL_CONVERT_DATE                    : constant SQL_INFO := 57;
   SQL_CONVERT_DECIMAL                 : constant SQL_INFO := 58;
   SQL_CONVERT_DOUBLE                  : constant SQL_INFO := 59;
   SQL_CONVERT_FLOAT                   : constant SQL_INFO := 60;
   SQL_CONVERT_INTEGER                 : constant SQL_INFO := 61;
   SQL_CONVERT_LONGVARCHAR             : constant SQL_INFO := 62;
   SQL_CONVERT_NUMERIC                 : constant SQL_INFO := 63;
   SQL_CONVERT_REAL                    : constant SQL_INFO := 64;
   SQL_CONVERT_SMALLINT                : constant SQL_INFO := 65;
   SQL_CONVERT_TIME                    : constant SQL_INFO := 66;
   SQL_CONVERT_TIMESTAMP               : constant SQL_INFO := 67;
   SQL_CONVERT_TINYINT                 : constant SQL_INFO := 68;
   SQL_CONVERT_VARBINARY               : constant SQL_INFO := 69;
   SQL_CONVERT_VARCHAR                 : constant SQL_INFO := 70;
   SQL_CONVERT_LONGVARBINARY           : constant SQL_INFO := 71;
   SQL_TRANSACTION_ISOLATION_OPTION    : constant SQL_INFO := 72;
   SQL_TXN_ISOLATION_OPTION            : constant SQL_INFO := 72;
   SQL_INTEGRITY                       : constant SQL_INFO := 73;
   SQL_CORRELATION_NAME                : constant SQL_INFO := 74;
   SQL_NON_NULLABLE_COLUMNS            : constant SQL_INFO := 75;
   SQL_DRIVER_HLIB                     : constant SQL_INFO := 76;
   SQL_DRIVER_ODBC_VER                 : constant SQL_INFO := 77;
   SQL_LOCK_TYPES                      : constant SQL_INFO := 78;
   SQL_POS_OPERATIONS                  : constant SQL_INFO := 79;
   SQL_POSITIONED_STATEMENTS           : constant SQL_INFO := 80;
   SQL_GETDATA_EXTENSIONS              : constant SQL_INFO := 81;
   SQL_BOOKMARK_PERSISTENCE            : constant SQL_INFO := 82;
   SQL_STATIC_SENSITIVITY              : constant SQL_INFO := 83;
   SQL_FILE_USAGE                      : constant SQL_INFO := 84;
   SQL_NULL_COLLATION                  : constant SQL_INFO := 85;
   SQL_ALTER_TABLE                     : constant SQL_INFO := 86;
   SQL_COLUMN_ALIAS                    : constant SQL_INFO := 87;
   SQL_GROUP_BY                        : constant SQL_INFO := 88;
   SQL_KEYWORDS                        : constant SQL_INFO := 89;
   SQL_ORDER_BY_COLUMNS_IN_SELECT      : constant SQL_INFO := 90;
   SQL_SCHEMA_USAGE                    : constant SQL_INFO := 91;
   SQL_OWNER_USAGE                     : constant SQL_INFO := 91;
   SQL_QUALIFIER_USAGE                 : constant SQL_INFO := 92;
   SQL_CATALOG_USAGE                   : constant SQL_INFO := 92;
   SQL_QUOTED_IDENTIFIER_CASE          : constant SQL_INFO := 93;
   SQL_SPECIAL_CHARACTERS              : constant SQL_INFO := 94;
   SQL_SUBQUERIES                      : constant SQL_INFO := 95;
   SQL_UNION_STATEMENT                 : constant SQL_INFO := 96;
   SQL_UNION                           : constant SQL_INFO := 96;
   SQL_MAXIMUM_COLUMNS_IN_GROUP_BY     : constant SQL_INFO := 97;
   SQL_MAX_COLUMNS_IN_GROUP_BY         : constant SQL_INFO := 97;
   SQL_MAXIMUM_COLUMNS_IN_INDEX        : constant SQL_INFO := 98;
   SQL_MAX_COLUMNS_IN_INDEX            : constant SQL_INFO := 98;
   SQL_MAXIMUM_COLUMNS_IN_ORDER_BY     : constant SQL_INFO := 99;
   SQL_MAX_COLUMNS_IN_ORDER_BY         : constant SQL_INFO := 99;
   SQL_MAXIMUM_COLUMNS_IN_SELECT       : constant SQL_INFO := 100;
   SQL_MAX_COLUMNS_IN_SELECT           : constant SQL_INFO := 100;
   SQL_MAXIMUM_COLUMNS_IN_TABLE        : constant SQL_INFO := 101;
   SQL_MAX_COLUMNS_IN_TABLE            : constant SQL_INFO := 101;
   SQL_MAXIMUM_INDEX_SIZE              : constant SQL_INFO := 102;
   SQL_MAX_INDEX_SIZE                  : constant SQL_INFO := 102;
   SQL_MAXIMUM_ROW_SIZE_INCLUDES_LONG  : constant SQL_INFO := 103;
   SQL_MAX_ROW_SIZE_INCLUDES_LONG      : constant SQL_INFO := 103;
   SQL_MAXIMUM_ROW_SIZE                : constant SQL_INFO := 104;
   SQL_MAX_ROW_SIZE                    : constant SQL_INFO := 104;
   SQL_MAXIMUM_STATEMENT_LENGTH        : constant SQL_INFO := 105;
   SQL_MAX_STATEMENT_LEN               : constant SQL_INFO := 105;
   SQL_MAXIMUM_TABLES_IN_SELECT        : constant SQL_INFO := 106;
   SQL_MAX_TABLES_IN_SELECT            : constant SQL_INFO := 106;
   SQL_MAXIMUM_USER_NAME_LENGTH        : constant SQL_INFO := 107;
   SQL_MAX_USER_NAME_LEN               : constant SQL_INFO := 107;
   SQL_MAXIMUM_CHAR_LITERAL_LENGTH     : constant SQL_INFO := 108;
   SQL_MAX_CHAR_LITERAL_LEN            : constant SQL_INFO := 108;
   SQL_TIMEDATE_ADD_INTERVALS          : constant SQL_INFO := 109;
   SQL_TIMEDATE_DIFF_INTERVALS         : constant SQL_INFO := 110;
   SQL_NEED_LONG_DATA_LEN              : constant SQL_INFO := 111;
   SQL_MAX_BINARY_LITERAL_LEN          : constant SQL_INFO := 112;
   SQL_LIKE_ESCAPE_CLAUSE              : constant SQL_INFO := 113;
   SQL_QUALIFIER_LOCATION              : constant SQL_INFO := 114;
   SQL_CATALOG_LOCATION                : constant SQL_INFO := 114;
   SQL_OUTER_JOIN_CAPABILITIES         : constant SQL_INFO := 115;
   SQL_OJ_CAPABILITIES                 : constant SQL_INFO := 115;
   SQL_ACTIVE_ENVIRONMENTS             : constant SQL_INFO := 116;
   SQL_ALTER_DOMAIN                    : constant SQL_INFO := 117;
   SQL_SQL_CONFORMANCE                 : constant SQL_INFO := 118;
   SQL_DATETIME_LITERALS               : constant SQL_INFO := 119;
   SQL_BATCH_ROW_COUNT                 : constant SQL_INFO := 120;
   SQL_BATCH_SUPPORT                   : constant SQL_INFO := 121;
   SQL_CONVERT_WCHAR                   : constant SQL_INFO := 122;
   SQL_CONVERT_INTERVAL_DAY_TIME       : constant SQL_INFO := 123;
   SQL_CONVERT_INTERVAL_YEAR_MONTH     : constant SQL_INFO := 124;
   SQL_CONVERT_WLONGVARCHAR            : constant SQL_INFO := 125;
   SQL_CONVERT_WVARCHAR                : constant SQL_INFO := 126;
   SQL_CREATE_ASSERTION                : constant SQL_INFO := 127;
   SQL_CREATE_CHARACTER_SET            : constant SQL_INFO := 128;
   SQL_CREATE_COLLATION                : constant SQL_INFO := 129;
   SQL_CREATE_DOMAIN                   : constant SQL_INFO := 130;
   SQL_CREATE_SCHEMA                   : constant SQL_INFO := 131;
   SQL_CREATE_TABLE                    : constant SQL_INFO := 132;
   SQL_CREATE_TRANSLATION              : constant SQL_INFO := 133;
   SQL_CREATE_VIEW                     : constant SQL_INFO := 134;
   SQL_DRIVER_HDESC                    : constant SQL_INFO := 135;
   SQL_DROP_ASSERTION                  : constant SQL_INFO := 136;
   SQL_DROP_CHARACTER_SET              : constant SQL_INFO := 137;
   SQL_DROP_COLLATION                  : constant SQL_INFO := 138;
   SQL_DROP_DOMAIN                     : constant SQL_INFO := 139;
   SQL_DROP_SCHEMA                     : constant SQL_INFO := 140;
   SQL_DROP_TABLE                      : constant SQL_INFO := 141;
   SQL_DROP_TRANSLATION                : constant SQL_INFO := 142;
   SQL_DROP_VIEW                       : constant SQL_INFO := 143;
   SQL_DYNAMIC_CURSOR_ATTRIBUTES1      : constant SQL_INFO := 144;
   SQL_DYNAMIC_CURSOR_ATTRIBUTES2      : constant SQL_INFO := 145;
   SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 : constant SQL_INFO := 146;
   SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 : constant SQL_INFO := 147;
   SQL_INDEX_KEYWORDS                  : constant SQL_INFO := 148;
   SQL_INFO_SCHEMA_VIEWS               : constant SQL_INFO := 149;
   SQL_KEYSET_CURSOR_ATTRIBUTES1       : constant SQL_INFO := 150;
   SQL_KEYSET_CURSOR_ATTRIBUTES2       : constant SQL_INFO := 151;
   SQL_ODBC_INTERFACE_CONFORMANCE      : constant SQL_INFO := 152;
   SQL_PARAM_ARRAY_ROW_COUNTS          : constant SQL_INFO := 153;
   SQL_PARAM_ARRAY_SELECTS             : constant SQL_INFO := 154;
   SQL_SQL92_DATETIME_FUNCTIONS        : constant SQL_INFO := 155;
   SQL_SQL92_FOREIGN_KEY_DELETE_RULE   : constant SQL_INFO := 156;
   SQL_SQL92_FOREIGN_KEY_UPDATE_RULE   : constant SQL_INFO := 157;
   SQL_SQL92_GRANT                     : constant SQL_INFO := 158;
   SQL_SQL92_NUMERIC_VALUE_FUNCTIONS   : constant SQL_INFO := 159;
   SQL_SQL92_PREDICATES                : constant SQL_INFO := 160;
   SQL_SQL92_RELATIONAL_JOIN_OPERATORS : constant SQL_INFO := 161;
   SQL_SQL92_REVOKE                    : constant SQL_INFO := 162;
   SQL_SQL92_ROW_VALUE_CONSTRUCTOR     : constant SQL_INFO := 163;
   SQL_SQL92_STRING_FUNCTIONS          : constant SQL_INFO := 164;
   SQL_SQL92_VALUE_EXPRESSIONS         : constant SQL_INFO := 165;
   SQL_STANDARD_CLI_CONFORMANCE        : constant SQL_INFO := 166;
   SQL_STATIC_CURSOR_ATTRIBUTES1       : constant SQL_INFO := 167;
   SQL_STATIC_CURSOR_ATTRIBUTES2       : constant SQL_INFO := 168;
   SQL_AGGREGATE_FUNCTIONS             : constant SQL_INFO := 169;
   SQL_DDL_INDEX                       : constant SQL_INFO := 170;
   SQL_DM_VER                          : constant SQL_INFO := 171;
   SQL_INSERT_STATEMENT                : constant SQL_INFO := 172;
   SQL_XOPEN_CLI_YEAR                  : constant SQL_INFO := 10000;
   SQL_CURSOR_SENSITIVITY              : constant SQL_INFO := 10001;
   SQL_DESCRIBE_PARAMETER              : constant SQL_INFO := 10002;
   SQL_CATALOG_NAME                    : constant SQL_INFO := 10003;
   SQL_COLLATION_SEQ                   : constant SQL_INFO := 10004;
   SQL_MAXIMUM_IDENTIFIER_LENGTH       : constant SQL_INFO := 10005;
   SQL_MAX_IDENTIFIER_LEN              : constant SQL_INFO := 10005;
   SQL_ASYNC_MODE                      : constant SQL_INFO := 10021;
   SQL_MAX_ASYNC_CONCURRENT_STATEMENTS : constant SQL_INFO := 10022;

   type SQL_INDEX is new SQLSMALLINT range 0..3;
   SQL_TABLE_STAT      : constant SQL_INDEX := 0;
   SQL_INDEX_CLUSTERED : constant SQL_INDEX := 1;
   SQL_INDEX_HASHED    : constant SQL_INDEX := 2;
   SQL_INDEX_OTHER     : constant SQL_INDEX := 3;

   type SQL_OPERATION is new SQLUSMALLINT range 0..3;
   SQL_POSITION : constant SQL_OPERATION := 0;
   SQL_REFRESH  : constant SQL_OPERATION := 1;
   SQL_UPDATE   : constant SQL_OPERATION := 2;
   SQL_DELETE   : constant SQL_OPERATION := 3;

   type SQL_LOCKTYPE is new SQLUSMALLINT range 0..2;
   SQL_LOCK_NO_CHANGE : constant SQL_LOCKTYPE := 0;
   SQL_LOCK_EXCLUSIVE : constant SQL_LOCKTYPE := 1;
   SQL_LOCK_UNLOCK    : constant SQL_LOCKTYPE := 2;

   type SQL_IDENTIFIER is new SQLSMALLINT range 1..2;
   SQL_BEST_ROWID : constant SQL_IDENTIFIER := 1;
   SQL_ROWVER     : constant SQL_IDENTIFIER := 2;

   type SQL_DIRECTION is new SQLUSMALLINT;
   SQL_FETCH_NEXT         : constant SQL_DIRECTION := 1;
   SQL_FETCH_FIRST        : constant SQL_DIRECTION := 2;
   SQL_FETCH_LAST         : constant SQL_DIRECTION := 3;
   SQL_FETCH_PRIOR        : constant SQL_DIRECTION := 4;
   SQL_FETCH_ABSOLUTE     : constant SQL_DIRECTION := 5;
   SQL_FETCH_RELATIVE     : constant SQL_DIRECTION := 6;
   SQL_FETCH_RESUME       : constant SQL_DIRECTION := 7;
   SQL_FETCH_BOOKMARK     : constant SQL_DIRECTION := 8;
   SQL_FETCH_FIRST_USER   : constant SQL_DIRECTION := 31;
   SQL_FETCH_FIRST_SYSTEM : constant SQL_DIRECTION := 32;

   subtype SQL_DRIVER_DIRECTION is SQL_DIRECTION
      range SQL_FETCH_NEXT .. SQL_FETCH_FIRST;

   type SQL_NULLABLE_FIELD is new SQLSMALLINT range 0..2;
   SQL_NO_NULLS         : constant SQL_NULLABLE_FIELD := 0;
   SQL_NULLABLE         : constant SQL_NULLABLE_FIELD := 1;
   SQL_NULLABLE_UNKNOWN : constant SQL_NULLABLE_FIELD := 2;

   SQL_FALSE : constant := 0;
   SQL_TRUE  : constant := 1;

   SQL_MAX_DSN_LENGTH       : constant := 32;

   SQL_ASYNC_ENABLE_ON      : constant := 1;
   SQL_ASYNC_ENABLE_OFF     : constant := 0;
   SQL_ASYNC_ENABLE_DEFAULT : constant := SQL_ASYNC_ENABLE_OFF;

   SQL_AUTOCOMMIT_OFF     : constant := 0;
   SQL_AUTOCOMMIT_ON      : constant := 1;
   SQL_AUTOCOMMIT_DEFAULT : constant := SQL_AUTOCOMMIT_ON;

   SQL_CD_TRUE  : constant := 1;
   SQL_CD_FALSE : constant := 0;

   type SQL_MODE is new SQLUINTEGER range 0..1;
   SQL_MODE_READ_WRITE : constant SQL_MODE := 0;
   SQL_MODE_READ_ONLY  : constant SQL_MODE := 1;
   SQL_MODE_DEFAULT    : constant SQL_MODE := SQL_MODE_READ_WRITE;

   type SQL_PRED_SEARCHABLE is new SQLSMALLINT range 0..2;
   SQL_PRED_NONE  : constant SQL_PRED_SEARCHABLE := 0;
   SQL_PRED_CHAR  : constant SQL_PRED_SEARCHABLE := 1;
   SQL_PRED_BASIC : constant SQL_PRED_SEARCHABLE := 2;

   type SQL_COLUMN_SEARCHABLE is new SQLSMALLINT range 0..3;
   SQL_UNSEARCHABLE    : constant SQL_COLUMN_SEARCHABLE := 0;
   SQL_LIKE_ONLY       : constant SQL_COLUMN_SEARCHABLE := 1;
   SQL_ALL_EXCEPT_LIKE : constant SQL_COLUMN_SEARCHABLE := 2;
   SQL_SEARCHABLE      : constant SQL_COLUMN_SEARCHABLE := 3;

   type SQL_AD is new SQLUINTEGER;
   SQL_AD_CONSTRAINT_NAME_DEFINITION        : constant SQL_AD :=16#001#;
   SQL_AD_ADD_DOMAIN_CONSTRAINT             : constant SQL_AD :=16#002#;
   SQL_AD_DROP_DOMAIN_CONSTRAINT            : constant SQL_AD :=16#004#;
   SQL_AD_ADD_DOMAIN_DEFAULT                : constant SQL_AD :=16#008#;
   SQL_AD_DROP_DOMAIN_DEFAULT               : constant SQL_AD :=16#010#;
   SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED : constant SQL_AD :=16#020#;
   SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE: constant SQL_AD :=16#040#;
   SQL_AD_ADD_CONSTRAINT_DEFERRABLE         : constant SQL_AD :=16#080#;
   SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE     : constant SQL_AD :=16#100#;

   type SQL_AF is new SQLUINTEGER;
   SQL_AF_AVG      : constant SQL_AF := 16#01#;
   SQL_AF_COUNT    : constant SQL_AF := 16#02#;
   SQL_AF_MAX      : constant SQL_AF := 16#04#;
   SQL_AF_MIN      : constant SQL_AF := 16#08#;
   SQL_AF_SUM      : constant SQL_AF := 16#10#;
   SQL_AF_DISTINCT : constant SQL_AF := 16#20#;
   SQL_AF_ALL      : constant SQL_AF := 16#40#;

   type SQL_AT is new SQLUINTEGER;
   SQL_AT_ADD_COLUMN                     : constant SQL_AT := 16#00001#;
   SQL_AT_DROP_COLUMN                    : constant SQL_AT := 16#00002#;
   SQL_AT_ADD_CONSTRAINT                 : constant SQL_AT := 16#00008#;
   SQL_AT_ADD_COLUMN_SINGLE              : constant SQL_AT := 16#00020#;
   SQL_AT_ADD_COLUMN_DEFAULT             : constant SQL_AT := 16#00040#;
   SQL_AT_ADD_COLUMN_COLLATION           : constant SQL_AT := 16#00080#;
   SQL_AT_SET_COLUMN_DEFAULT             : constant SQL_AT := 16#00100#;
   SQL_AT_DROP_COLUMN_DEFAULT            : constant SQL_AT := 16#00200#;
   SQL_AT_DROP_COLUMN_CASCADE            : constant SQL_AT := 16#00400#;
   SQL_AT_DROP_COLUMN_RESTRICT           : constant SQL_AT := 16#00800#;
   SQL_AT_ADD_TABLE_CONSTRAINT           : constant SQL_AT := 16#01000#;
   SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE  : constant SQL_AT := 16#02000#;
   SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT : constant SQL_AT := 16#04000#;
   SQL_AT_CONSTRAINT_NAME_DEFINITION     : constant SQL_AT := 16#08000#;
   SQL_AT_CONSTRAINT_INITIALLY_DEFERRED  : constant SQL_AT := 16#10000#;
   SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE : constant SQL_AT := 16#20000#;
   SQL_AT_CONSTRAINT_DEFERRABLE          : constant SQL_AT := 16#40000#;
   SQL_AT_CONSTRAINT_NON_DEFERRABLE      : constant SQL_AT := 16#80000#;

   SQL_ASYNC_NOT_CAPABLE : constant := 0;
   SQL_ASYNC_DBC_CAPABLE : constant := 1;

   type SQL_AM is new SQLUINTEGER range 0..2;
   SQL_AM_NONE       : constant SQL_AM := 0;
   SQL_AM_CONNECTION : constant SQL_AM := 1;
   SQL_AM_STATEMENT  : constant SQL_AM := 2;

   type SQL_BRC is new SQLUINTEGER;
   SQL_BRC_PROCEDURES : constant SQL_BRC := 16#0000001#;
   SQL_BRC_EXPLICIT   : constant SQL_BRC := 16#0000002#;
   SQL_BRC_ROLLED_UP  : constant SQL_BRC := 16#0000004#;

   type SQL_BS is new SQLUINTEGER;
   SQL_BS_SELECT_EXPLICIT    : constant SQL_BS := 16#00000001#;
   SQL_BS_ROW_COUNT_EXPLICIT : constant SQL_BS := 16#00000002#;
   SQL_BS_SELECT_PROC        : constant SQL_BS := 16#00000004#;
   SQL_BS_ROW_COUNT_PROC     : constant SQL_BS := 16#00000008#;

   type SQL_BP is new SQLUINTEGER;
   SQL_BP_CLOSE       : constant SQL_BP := 16#00000001#;
   SQL_BP_DELETE      : constant SQL_BP := 16#00000002#;
   SQL_BP_DROP        : constant SQL_BP := 16#00000004#;
   SQL_BP_TRANSACTION : constant SQL_BP := 16#00000008#;
   SQL_BP_UPDATE      : constant SQL_BP := 16#00000010#;
   SQL_BP_OTHER_HSTMT : constant SQL_BP := 16#00000020#;
   SQL_BP_SCROLL      : constant SQL_BP := 16#00000040#;

   type SQL_SENSITIVITY is new SQLUINTEGER range 0..2;
   SQL_UNSPECIFIED : constant SQL_SENSITIVITY := 0;
   SQL_INSENSITIVE : constant SQL_SENSITIVITY := 1;
   SQL_SENSITIVE   : constant SQL_SENSITIVITY := 2;

   type SQL_CA1 is new SQLUINTEGER;
   SQL_CA1_NEXT                    : constant SQL_CA1 := 16#00001#;
   SQL_CA1_ABSOLUTE                : constant SQL_CA1 := 16#00002#;
   SQL_CA1_RELATIVE                : constant SQL_CA1 := 16#00004#;
   SQL_CA1_BOOKMARK                : constant SQL_CA1 := 16#00008#;
   SQL_CA1_LOCK_NO_CHANGE          : constant SQL_CA1 := 16#00040#;
   SQL_CA1_LOCK_EXCLUSIVE          : constant SQL_CA1 := 16#00080#;
   SQL_CA1_LOCK_UNLOCK             : constant SQL_CA1 := 16#00100#;
   SQL_CA1_POS_POSITION            : constant SQL_CA1 := 16#00200#;
   SQL_CA1_POS_UPDATE              : constant SQL_CA1 := 16#00400#;
   SQL_CA1_POS_DELETE              : constant SQL_CA1 := 16#00800#;
   SQL_CA1_POS_REFRESH             : constant SQL_CA1 := 16#01000#;
   SQL_CA1_POSITIONED_UPDATE       : constant SQL_CA1 := 16#02000#;
   SQL_CA1_POSITIONED_DELETE       : constant SQL_CA1 := 16#04000#;
   SQL_CA1_SELECT_FOR_UPDATE       : constant SQL_CA1 := 16#08000#;
   SQL_CA1_BULK_ADD                : constant SQL_CA1 := 16#10000#;
   SQL_CA1_BULK_UPDATE_BY_BOOKMARK : constant SQL_CA1 := 16#20000#;
   SQL_CA1_BULK_DELETE_BY_BOOKMARK : constant SQL_CA1 := 16#40000#;
   SQL_CA1_BULK_FETCH_BY_BOOKMARK  : constant SQL_CA1 := 16#80000#;

   type SQL_CA2 is new SQLUINTEGER;
   SQL_CA2_READ_ONLY_CONCURRENCY  : constant SQL_CA1 := 16#00001#;
   SQL_CA2_LOCK_CONCURRENCY       : constant SQL_CA1 := 16#00002#;
   SQL_CA2_OPT_ROWVER_CONCURRENCY : constant SQL_CA1 := 16#00004#;
   SQL_CA2_OPT_VALUES_CONCURRENCY : constant SQL_CA1 := 16#00008#;
   SQL_CA2_SENSITIVITY_ADDITIONS  : constant SQL_CA1 := 16#00010#;
   SQL_CA2_SENSITIVITY_DELETIONS  : constant SQL_CA1 := 16#00020#;
   SQL_CA2_SENSITIVITY_UPDATES    : constant SQL_CA1 := 16#00040#;
   SQL_CA2_MAX_ROWS_SELECT        : constant SQL_CA1 := 16#00080#;
   SQL_CA2_MAX_ROWS_INSERT        : constant SQL_CA1 := 16#00100#;
   SQL_CA2_MAX_ROWS_DELETE        : constant SQL_CA1 := 16#00200#;
   SQL_CA2_MAX_ROWS_UPDATE        : constant SQL_CA1 := 16#00400#;
   SQL_CA2_MAX_ROWS_CATALOG       : constant SQL_CA1 := 16#00800#;
   SQL_CA2_CRC_EXACT              : constant SQL_CA1 := 16#01000#;
   SQL_CA2_CRC_APPROXIMATE        : constant SQL_CA1 := 16#02000#;
   SQL_CA2_SIMULATE_NON_UNIQUE    : constant SQL_CA1 := 16#04000#;
   SQL_CA2_SIMULATE_TRY_UNIQUE    : constant SQL_CA1 := 16#08000#;
   SQL_CA2_SIMULATE_UNIQUE        : constant SQL_CA1 := 16#10000#;
   SQL_CA2_MAX_ROWS_AFFECTS_ALL   : constant SQL_CA1 :=
                                    (  SQL_CA2_MAX_ROWS_SELECT
                                    or SQL_CA2_MAX_ROWS_INSERT
                                    or SQL_CA2_MAX_ROWS_DELETE
                                    or SQL_CA2_MAX_ROWS_UPDATE
                                    or SQL_CA2_MAX_ROWS_CATALOG
                                    );
   type SQL_CA is new SQLUINTEGER;
   SQL_CA_CREATE_ASSERTION               : constant SQL_CA := 16#0001#;
   SQL_CA_CONSTRAINT_INITIALLY_DEFERRED  : constant SQL_CA := 16#0010#;
   SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE : constant SQL_CA := 16#0020#;
   SQL_CA_CONSTRAINT_DEFERRABLE          : constant SQL_CA := 16#0040#;
   SQL_CA_CONSTRAINT_NON_DEFERRABLE      : constant SQL_CA := 16#0080#;

   type SQL_CB is new SQLUSMALLINT range 0..1;
   SQL_CB_NULL     : constant SQL_CB := 0;
   SQL_CB_NON_NULL : constant SQL_CB := 1;

   type SQL_CCB is new SQLUSMALLINT range 0..2;
   SQL_CB_DELETE   : constant SQL_CCB := 0;
   SQL_CB_CLOSE    : constant SQL_CCB := 1;
   SQL_CB_PRESERVE : constant SQL_CCB := 2;

   type SQL_CCOL is new SQLUINTEGER;
   SQL_CCOL_CREATE_COLLATION : constant SQL_CCOL := 16#0001#;

   type SQL_CCS is new SQLUINTEGER;
   SQL_CCS_CREATE_CHARACTER_SET : constant SQL_CCS := 16#0001#;
   SQL_CCS_COLLATE_CLAUSE       : constant SQL_CCS := 16#0002#;
   SQL_CCS_LIMITED_COLLATION    : constant SQL_CCS := 16#0004#;

   type SQL_CDO is new SQLUINTEGER;
   SQL_CDO_CREATE_DOMAIN                  : constant SQL_CDO := 16#001#;
   SQL_CDO_DEFAULT                        : constant SQL_CDO := 16#002#;
   SQL_CDO_CONSTRAINT                     : constant SQL_CDO := 16#004#;
   SQL_CDO_COLLATION                      : constant SQL_CDO := 16#008#;
   SQL_CDO_CONSTRAINT_NAME_DEFINITION     : constant SQL_CDO := 16#010#;
   SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED  : constant SQL_CDO := 16#020#;
   SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE : constant SQL_CDO := 16#040#;
   SQL_CDO_CONSTRAINT_DEFERRABLE          : constant SQL_CDO := 16#080#;
   SQL_CDO_CONSTRAINT_NON_DEFERRABLE      : constant SQL_CDO := 16#100#;

   type SQL_CL is new SQLUSMALLINT range 1..2;
   SQL_CL_START : constant SQL_CL := 1;
   SQL_CL_END   : constant SQL_CL := 2;

   type SQL_CN is new SQLUSMALLINT range 0..2;
   SQL_CN_NONE      : constant SQL_CN := 0;
   SQL_CN_DIFFERENT : constant SQL_CN := 1;
   SQL_CN_ANY       : constant SQL_CN := 2;

   type SQL_CONCUR is new SQLULEN range 1..4;
   SQL_CONCUR_READ_ONLY : constant SQL_CONCUR := 1;
   SQL_CONCUR_LOCK      : constant SQL_CONCUR := 2;
   SQL_CONCUR_ROWVER    : constant SQL_CONCUR := 3;
   SQL_CONCUR_VALUES    : constant SQL_CONCUR := 4;
   SQL_CONCUR_DEFAULT   : constant SQL_CONCUR := SQL_CONCUR_READ_ONLY;

   type SQL_CP is new SQLUINTEGER range 0..2;
   SQL_CP_OFF            : constant SQL_CP := 0;
   SQL_CP_ONE_PER_DRIVER : constant SQL_CP := 1;
   SQL_CP_ONE_PER_HENV   : constant SQL_CP := 2;
   SQL_CP_DEFAULT        : constant SQL_CP := 0;

   type SQL_CP_MATCH is new SQLUINTEGER range 0..1;
   SQL_CP_STRICT_MATCH  : constant SQL_CP_MATCH := 0;
   SQL_CP_RELAXED_MATCH : constant SQL_CP_MATCH := 1;
   SQL_CP_MATCH_DEFAULT : constant SQL_CP_MATCH := SQL_CP_STRICT_MATCH;

   type SQL_CS is new SQLUINTEGER;
   SQL_CS_CREATE_SCHEMA         : constant SQL_CS := 16#0001#;
   SQL_CS_AUTHORIZATION         : constant SQL_CS := 16#0002#;
   SQL_CS_DEFAULT_CHARACTER_SET : constant SQL_CS := 16#0004#;

   type SQL_CT is new SQLUINTEGER;
   SQL_CT_CREATE_TABLE                   : constant SQL_CT := 16#0001#;
   SQL_CT_COMMIT_PRESERVE                : constant SQL_CT := 16#0002#;
   SQL_CT_COMMIT_DELETE                  : constant SQL_CT := 16#0004#;
   SQL_CT_GLOBAL_TEMPORARY               : constant SQL_CT := 16#0008#;
   SQL_CT_LOCAL_TEMPORARY                : constant SQL_CT := 16#0010#;
   SQL_CT_CONSTRAINT_INITIALLY_DEFERRED  : constant SQL_CT := 16#0020#;
   SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE : constant SQL_CT := 16#0040#;
   SQL_CT_CONSTRAINT_DEFERRABLE          : constant SQL_CT := 16#0080#;
   SQL_CT_CONSTRAINT_NON_DEFERRABLE      : constant SQL_CT := 16#0100#;
   SQL_CT_COLUMN_CONSTRAINT              : constant SQL_CT := 16#0200#;
   SQL_CT_COLUMN_DEFAULT                 : constant SQL_CT := 16#0400#;
   SQL_CT_COLUMN_COLLATION               : constant SQL_CT := 16#0800#;
   SQL_CT_TABLE_CONSTRAINT               : constant SQL_CT := 16#1000#;
   SQL_CT_CONSTRAINT_NAME_DEFINITION     : constant SQL_CT := 16#2000#;

   type SQL_CTR is new SQLUINTEGER;
   SQL_CTR_CREATE_TRANSLATION : constant SQL_CTR := 16#0001#;

   type SQL_CU is new SQLUINTEGER;
   SQL_CU_DML_STATEMENTS       : constant SQL_CU := 16#00000001#;
   SQL_CU_PROCEDURE_INVOCATION : constant SQL_CU := 16#00000002#;
   SQL_CU_TABLE_DEFINITION     : constant SQL_CU := 16#00000004#;
   SQL_CU_INDEX_DEFINITION     : constant SQL_CU := 16#00000008#;
   SQL_CU_PRIVILEGE_DEFINITION : constant SQL_CU := 16#00000010#;

   SQL_NONSCROLLABLE : constant := 0;
   SQL_SCROLLABLE    : constant := 1;

   type SQL_CURSOR is new SQLULEN range 0..3;
   SQL_CURSOR_FORWARD_ONLY  : constant SQL_CURSOR := 0;
   SQL_CURSOR_KEYSET_DRIVEN : constant SQL_CURSOR := 1;
   SQL_CURSOR_DYNAMIC       : constant SQL_CURSOR := 2;
   SQL_CURSOR_STATIC        : constant SQL_CURSOR := 3;
   SQL_CURSOR_TYPE_DEFAULT  : constant SQL_CURSOR :=
                                       SQL_CURSOR_FORWARD_ONLY;

   type SQL_CV is new SQLUINTEGER;
   SQL_CV_CREATE_VIEW  : constant SQL_CV := 16#0001#;
   SQL_CV_CHECK_OPTION : constant SQL_CV := 16#0002#;
   SQL_CV_CASCADED     : constant SQL_CV := 16#0004#;
   SQL_CV_LOCAL        : constant SQL_CV := 16#0008#;

   type SQL_CVT is new SQLUINTEGER;
   SQL_CVT_CHAR                : constant SQL_CVT := 16#00000001#;
   SQL_CVT_NUMERIC             : constant SQL_CVT := 16#00000002#;
   SQL_CVT_DECIMAL             : constant SQL_CVT := 16#00000004#;
   SQL_CVT_INTEGER             : constant SQL_CVT := 16#00000008#;
   SQL_CVT_SMALLINT            : constant SQL_CVT := 16#00000010#;
   SQL_CVT_FLOAT               : constant SQL_CVT := 16#00000020#;
   SQL_CVT_REAL                : constant SQL_CVT := 16#00000040#;
   SQL_CVT_DOUBLE              : constant SQL_CVT := 16#00000080#;
   SQL_CVT_VARCHAR             : constant SQL_CVT := 16#00000100#;
   SQL_CVT_LONGVARCHAR         : constant SQL_CVT := 16#00000200#;
   SQL_CVT_BINARY              : constant SQL_CVT := 16#00000400#;
   SQL_CVT_VARBINARY           : constant SQL_CVT := 16#00000800#;
   SQL_CVT_BIT                 : constant SQL_CVT := 16#00001000#;
   SQL_CVT_TINYINT             : constant SQL_CVT := 16#00002000#;
   SQL_CVT_BIGINT              : constant SQL_CVT := 16#00004000#;
   SQL_CVT_DATE                : constant SQL_CVT := 16#00008000#;
   SQL_CVT_TIME                : constant SQL_CVT := 16#00010000#;
   SQL_CVT_TIMESTAMP           : constant SQL_CVT := 16#00020000#;
   SQL_CVT_LONGVARBINARY       : constant SQL_CVT := 16#00040000#;
   SQL_CVT_INTERVAL_YEAR_MONTH : constant SQL_CVT := 16#00080000#;
   SQL_CVT_INTERVAL_DAY_TIME   : constant SQL_CVT := 16#00100000#;
   SQL_CVT_WCHAR               : constant SQL_CVT := 16#00200000#;
   SQL_CVT_WLONGVARCHAR        : constant SQL_CVT := 16#00400000#;
   SQL_CVT_WVARCHAR            : constant SQL_CVT := 16#00800000#;
   SQL_CVT_GUID                : constant SQL_CVT := 16#01000000#;

   type SQL_DA is new SQLUINTEGER;
   SQL_DA_DROP_ASSERTION : constant SQL_DA := 1;

   type SQL_DC is new SQLUINTEGER;
   SQL_DC_DROP_COLLATION : constant SQL_DC := 1;

   type SQL_DCS is new SQLUINTEGER;
   SQL_DCS_DROP_CHARACTER_SET : constant SQL_DCS := 1;

   type SQL_DD is new SQLUINTEGER;
   SQL_DD_DROP_DOMAIN : constant SQL_DC := 16#1#;
   SQL_DD_RESTRICT    : constant SQL_DC := 16#2#;
   SQL_DD_CASCADE     : constant SQL_DC := 16#4#;

   type SQL_DI is new SQLUINTEGER;
   SQL_DI_CREATE_INDEX : constant SQL_DI := 1;
   SQL_DI_DROP_INDEX   : constant SQL_DI := 2;

   type SQL_DL is new SQLUINTEGER;
   SQL_DL_SQL92_DATE                      : constant SQL_DL := 16#0001#;
   SQL_DL_SQL92_TIME                      : constant SQL_DL := 16#0002#;
   SQL_DL_SQL92_TIMESTAMP                 : constant SQL_DL := 16#0004#;
   SQL_DL_SQL92_INTERVAL_YEAR             : constant SQL_DL := 16#0008#;
   SQL_DL_SQL92_INTERVAL_MONTH            : constant SQL_DL := 16#0010#;
   SQL_DL_SQL92_INTERVAL_DAY              : constant SQL_DL := 16#0020#;
   SQL_DL_SQL92_INTERVAL_HOUR             : constant SQL_DL := 16#0040#;
   SQL_DL_SQL92_INTERVAL_MINUTE           : constant SQL_DL := 16#0080#;
   SQL_DL_SQL92_INTERVAL_SECOND           : constant SQL_DL := 16#0100#;
   SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH    : constant SQL_DL := 16#0200#;
   SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR      : constant SQL_DL := 16#0400#;
   SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE    : constant SQL_DL := 16#0800#;
   SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND    : constant SQL_DL := 16#1000#;
   SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE   : constant SQL_DL := 16#2000#;
   SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND   : constant SQL_DL := 16#4000#;
   SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND : constant SQL_DL := 16#8000#;

   type SQL_DS is new SQLUINTEGER;
   SQL_DS_DROP_SCHEMA : constant SQL_DC := 16#1#;
   SQL_DS_RESTRICT    : constant SQL_DC := 16#2#;
   SQL_DS_CASCADE     : constant SQL_DC := 16#4#;

   type SQL_DT is new SQLUINTEGER;
   SQL_DT_DROP_TABLE : constant SQL_DT := 16#1#;
   SQL_DT_RESTRICT   : constant SQL_DT := 16#2#;
   SQL_DT_CASCADE    : constant SQL_DT := 16#4#;

   type SQL_DTR is new SQLUINTEGER;
   SQL_DTR_DROP_TRANSLATION : constant SQL_DTR := 1;

   type SQL_DV is new SQLUINTEGER;
   SQL_DV_DROP_VIEW : constant SQL_DV := 16#1#;
   SQL_DV_RESTRICT  : constant SQL_DV := 16#2#;
   SQL_DV_CASCADE   : constant SQL_DV := 16#4#;

   type SQL_FILE is new SQLUSMALLINT range 0..2;
   SQL_FILE_NOT_SUPPORTED : constant SQL_FILE := 0;
   SQL_FILE_TABLE         : constant SQL_FILE := 1;
   SQL_FILE_QUALIFIER     : constant SQL_FILE := 2;
   SQL_FILE_CATALOG       : constant SQL_FILE := SQL_FILE_QUALIFIER;

   type SQL_FN_CVT is new SQLUINTEGER;
   SQL_FN_CVT_CONVERT : constant SQL_FN_CVT := 1;
   SQL_FN_CVT_CAST    : constant SQL_FN_CVT := 2;

   type SQL_FN_NUM is new SQLUINTEGER;
   SQL_FN_NUM_ABS      : constant SQL_FN_NUM := 16#000001#;
   SQL_FN_NUM_ACOS     : constant SQL_FN_NUM := 16#000002#;
   SQL_FN_NUM_ASIN     : constant SQL_FN_NUM := 16#000004#;
   SQL_FN_NUM_ATAN     : constant SQL_FN_NUM := 16#000008#;
   SQL_FN_NUM_ATAN2    : constant SQL_FN_NUM := 16#000010#;
   SQL_FN_NUM_CEILING  : constant SQL_FN_NUM := 16#000020#;
   SQL_FN_NUM_COS      : constant SQL_FN_NUM := 16#000040#;
   SQL_FN_NUM_COT      : constant SQL_FN_NUM := 16#000080#;
   SQL_FN_NUM_EXP      : constant SQL_FN_NUM := 16#000100#;
   SQL_FN_NUM_FLOOR    : constant SQL_FN_NUM := 16#000200#;
   SQL_FN_NUM_LOG      : constant SQL_FN_NUM := 16#000400#;
   SQL_FN_NUM_MOD      : constant SQL_FN_NUM := 16#000800#;
   SQL_FN_NUM_SIGN     : constant SQL_FN_NUM := 16#001000#;
   SQL_FN_NUM_SIN      : constant SQL_FN_NUM := 16#002000#;
   SQL_FN_NUM_SQRT     : constant SQL_FN_NUM := 16#004000#;
   SQL_FN_NUM_TAN      : constant SQL_FN_NUM := 16#008000#;
   SQL_FN_NUM_PI       : constant SQL_FN_NUM := 16#010000#;
   SQL_FN_NUM_RAND     : constant SQL_FN_NUM := 16#020000#;
   SQL_FN_NUM_DEGREES  : constant SQL_FN_NUM := 16#040000#;
   SQL_FN_NUM_LOG10    : constant SQL_FN_NUM := 16#080000#;
   SQL_FN_NUM_POWER    : constant SQL_FN_NUM := 16#100000#;
   SQL_FN_NUM_RADIANS  : constant SQL_FN_NUM := 16#200000#;
   SQL_FN_NUM_ROUND    : constant SQL_FN_NUM := 16#400000#;
   SQL_FN_NUM_TRUNCATE : constant SQL_FN_NUM := 16#800000#;

   type SQL_FN_STR is new SQLUINTEGER;
   SQL_FN_STR_CONCAT           : constant SQL_FN_STR := 16#000001#;
   SQL_FN_STR_INSERT           : constant SQL_FN_STR := 16#000002#;
   SQL_FN_STR_LEFT             : constant SQL_FN_STR := 16#000004#;
   SQL_FN_STR_LTRIM            : constant SQL_FN_STR := 16#000008#;
   SQL_FN_STR_LENGTH           : constant SQL_FN_STR := 16#000010#;
   SQL_FN_STR_LOCATE           : constant SQL_FN_STR := 16#000020#;
   SQL_FN_STR_LCASE            : constant SQL_FN_STR := 16#000040#;
   SQL_FN_STR_REPEAT           : constant SQL_FN_STR := 16#000080#;
   SQL_FN_STR_REPLACE          : constant SQL_FN_STR := 16#000100#;
   SQL_FN_STR_RIGHT            : constant SQL_FN_STR := 16#000200#;
   SQL_FN_STR_RTRIM            : constant SQL_FN_STR := 16#000400#;
   SQL_FN_STR_SUBSTRING        : constant SQL_FN_STR := 16#000800#;
   SQL_FN_STR_UCASE            : constant SQL_FN_STR := 16#001000#;
   SQL_FN_STR_ASCII            : constant SQL_FN_STR := 16#002000#;
   SQL_FN_STR_CHAR             : constant SQL_FN_STR := 16#004000#;
   SQL_FN_STR_DIFFERENCE       : constant SQL_FN_STR := 16#008000#;
   SQL_FN_STR_LOCATE_2         : constant SQL_FN_STR := 16#010000#;
   SQL_FN_STR_SOUNDEX          : constant SQL_FN_STR := 16#020000#;
   SQL_FN_STR_SPACE            : constant SQL_FN_STR := 16#040000#;
   SQL_FN_STR_BIT_LENGTH       : constant SQL_FN_STR := 16#080000#;
   SQL_FN_STR_CHAR_LENGTH      : constant SQL_FN_STR := 16#100000#;
   SQL_FN_STR_CHARACTER_LENGTH : constant SQL_FN_STR := 16#200000#;
   SQL_FN_STR_OCTET_LENGTH     : constant SQL_FN_STR := 16#400000#;
   SQL_FN_STR_POSITION         : constant SQL_FN_STR := 16#800000#;

   type SQL_FN_SYS is new SQLUINTEGER;
   SQL_FN_SYS_USERNAME : constant SQL_FN_STR := 16#0001#;
   SQL_FN_SYS_DBNAME   : constant SQL_FN_STR := 16#0002#;
   SQL_FN_SYS_IFNULL   : constant SQL_FN_STR := 16#0004#;

   type SQL_FN_TSI is new SQLUINTEGER;
   SQL_FN_TSI_FRAC_SECOND : constant SQL_FN_TSI := 16#0001#;
   SQL_FN_TSI_SECOND      : constant SQL_FN_TSI := 16#0002#;
   SQL_FN_TSI_MINUTE      : constant SQL_FN_TSI := 16#0004#;
   SQL_FN_TSI_HOUR        : constant SQL_FN_TSI := 16#0008#;
   SQL_FN_TSI_DAY         : constant SQL_FN_TSI := 16#0010#;
   SQL_FN_TSI_WEEK        : constant SQL_FN_TSI := 16#0020#;
   SQL_FN_TSI_MONTH       : constant SQL_FN_TSI := 16#0040#;
   SQL_FN_TSI_QUARTER     : constant SQL_FN_TSI := 16#0080#;
   SQL_FN_TSI_YEAR        : constant SQL_FN_TSI := 16#0100#;

   type SQL_GR is new SQLUSMALLINT range 0..4;
   SQL_GB_NOT_SUPPORTED            : constant SQL_GR := 0;
   SQL_GB_GROUP_BY_EQUALS_SELECT   : constant SQL_GR := 1;
   SQL_GB_GROUP_BY_CONTAINS_SELECT : constant SQL_GR := 2;
   SQL_GB_NO_RELATION              : constant SQL_GR := 3;
   SQL_GB_COLLATE                  : constant SQL_GR := 4;

   type SQL_GD is new SQLUINTEGER;
   SQL_GD_ANY_COLUMN     : constant SQL_GD := 16#00001#;
   SQL_GD_ANY_ORDER      : constant SQL_GD := 16#00002#;
   SQL_GD_BLOCK          : constant SQL_GD := 16#00004#;
   SQL_GD_BOUND          : constant SQL_GD := 16#00008#;
   SQL_GD_OUTPUT_PARAMS  : constant SQL_GD := 16#00010#;

   type SQL_IC is new SQLUSMALLINT range 1..4;
   SQL_IC_UPPER     : constant SQL_IC := 1;
   SQL_IC_LOWER     : constant SQL_IC := 2;
   SQL_IC_SENSITIVE : constant SQL_IC := 3;
   SQL_IC_MIXED     : constant SQL_IC := 4;

   type SQL_IK is new SQLUINTEGER;
   SQL_IK_NONE : constant SQL_IK := 0;
   SQL_IK_ASC  : constant SQL_IK := 1;
   SQL_IK_DESC : constant SQL_IK := 2;
   SQL_IK_ALL  : constant SQL_IK := SQL_IK_ASC or SQL_IK_DESC;

   type SQL_IS is new SQLUINTEGER;
   SQL_IS_INSERT_LITERALS : constant SQL_IS := 1;
   SQL_IS_INSERT_SEARCHED : constant SQL_IS := 2;
   SQL_IS_SELECT_INTO     : constant SQL_IS := 4;

   SQL_IS_POINTER   : constant := -4;
   SQL_IS_UINTEGER  : constant := -5;
   SQL_IS_INTEGER   : constant := -6;
   SQL_IS_USMALLINT : constant := -7;
   SQL_IS_SMALLINT  : constant := -8;

   type SQL_ISV is new SQLUINTEGER;
   SQL_ISV_ASSERTIONS              : constant SQL_ISV := 16#000001#;
   SQL_ISV_CHARACTER_SETS          : constant SQL_ISV := 16#000002#;
   SQL_ISV_CHECK_CONSTRAINTS       : constant SQL_ISV := 16#000004#;
   SQL_ISV_COLLATIONS              : constant SQL_ISV := 16#000008#;
   SQL_ISV_COLUMN_DOMAIN_USAGE     : constant SQL_ISV := 16#000010#;
   SQL_ISV_COLUMN_PRIVILEGES       : constant SQL_ISV := 16#000020#;
   SQL_ISV_COLUMNS                 : constant SQL_ISV := 16#000040#;
   SQL_ISV_CONSTRAINT_COLUMN_USAGE : constant SQL_ISV := 16#000080#;
   SQL_ISV_CONSTRAINT_TABLE_USAGE  : constant SQL_ISV := 16#000100#;
   SQL_ISV_DOMAIN_CONSTRAINTS      : constant SQL_ISV := 16#000200#;
   SQL_ISV_DOMAINS                 : constant SQL_ISV := 16#000400#;
   SQL_ISV_KEY_COLUMN_USAGE        : constant SQL_ISV := 16#000800#;
   SQL_ISV_REFERENTIAL_CONSTRAINTS : constant SQL_ISV := 16#001000#;
   SQL_ISV_SCHEMATA                : constant SQL_ISV := 16#002000#;
   SQL_ISV_SQL_LANGUAGES           : constant SQL_ISV := 16#004000#;
   SQL_ISV_TABLE_CONSTRAINTS       : constant SQL_ISV := 16#008000#;
   SQL_ISV_TABLE_PRIVILEGES        : constant SQL_ISV := 16#010000#;
   SQL_ISV_TABLES                  : constant SQL_ISV := 16#020000#;
   SQL_ISV_TRANSLATIONS            : constant SQL_ISV := 16#040000#;
   SQL_ISV_USAGE_PRIVILEGES        : constant SQL_ISV := 16#080000#;
   SQL_ISV_VIEW_COLUMN_USAGE       : constant SQL_ISV := 16#100000#;
   SQL_ISV_VIEW_TABLE_USAGE        : constant SQL_ISV := 16#200000#;
   SQL_ISV_VIEWS                   : constant SQL_ISV := 16#400000#;

   type SQL_NC is new SQLUSMALLINT;
   SQL_NC_HIGH  : constant SQL_NC := 0;
   SQL_NC_LOW   : constant SQL_NC := 1;
   SQL_NC_START : constant SQL_NC := 2;
   SQL_NC_END   : constant SQL_NC := 4;

   type SQL_NNC is new SQLUSMALLINT range 0..1;
   SQL_NNC_NULL     : constant SQL_NNC := 0;
   SQL_NNC_NON_NULL : constant SQL_NNC := 1;

   SQL_NOSCAN_OFF     : constant := 0;
   SQL_NOSCAN_ON      : constant := 1;
   SQL_NOSCAN_DEFAULT : constant := SQL_NOSCAN_OFF;

   type SQL_OIC is new SQLUINTEGER range 1..3;
   SQL_OIC_CORE   : constant SQL_OIC := 1;
   SQL_OIC_LEVEL1 : constant SQL_OIC := 2;
   SQL_OIC_LEVEL2 : constant SQL_OIC := 3;

   type SQL_OJ is new SQLUINTEGER;
   SQL_OJ_LEFT               : constant SQL_OJ := 16#0001#;
   SQL_OJ_RIGHT              : constant SQL_OJ := 16#0002#;
   SQL_OJ_FULL               : constant SQL_OJ := 16#0004#;
   SQL_OJ_NESTED             : constant SQL_OJ := 16#0008#;
   SQL_OJ_NOT_ORDERED        : constant SQL_OJ := 16#0010#;
   SQL_OJ_INNER              : constant SQL_OJ := 16#0020#;
   SQL_OJ_ALL_COMPARISON_OPS : constant SQL_OJ := 16#0040#;

   type SQL_OV is new SQLUINTEGER;
   SQL_OV_ODBC2    : constant SQL_OV := 2;
   SQL_OV_ODBC3    : constant SQL_OV := 3;
   SQL_OV_ODBC3_80 : constant SQL_OV := 380;

   type SQL_PARAM is new SQLSMALLINT;
   SQL_PARAM_TYPE_UNKNOWN        : constant SQL_PARAM := 0;
   SQL_PARAM_INPUT               : constant SQL_PARAM := 1;
   SQL_PARAM_INPUT_OUTPUT        : constant SQL_PARAM := 2;
   SQL_RESULT_COL                : constant SQL_PARAM := 3;
   SQL_PARAM_OUTPUT              : constant SQL_PARAM := 4;
   SQL_RETURN_VALUE              : constant SQL_PARAM := 5;
   SQL_PARAM_INPUT_OUTPUT_STREAM : constant SQL_PARAM := 8;
   SQL_PARAM_OUTPUT_STREAM       : constant SQL_PARAM := 16;

   type SQL_PARC is new SQLUINTEGER range 1..2;
   SQL_PARC_BATCH    : constant SQL_PARC := 1;
   SQL_PARC_NO_BATCH : constant SQL_PARC := 2;

   type SQL_PAS is new SQLUINTEGER range 1..3;
   SQL_PAS_BATCH     : constant SQL_PAS := 1;
   SQL_PAS_NO_BATCH  : constant SQL_PAS := 2;
   SQL_PAS_NO_SELECT : constant SQL_PAS := 3;

   type SQL_POS is new SQLUINTEGER;
   SQL_POS_POSITION : constant SQL_POS := 16#0001#;
   SQL_POS_REFRESH  : constant SQL_POS := 16#0002#;
   SQL_POS_UPDATE   : constant SQL_POS := 16#0004#;
   SQL_POS_DELETE   : constant SQL_POS := 16#0008#;
   SQL_POS_ADD      : constant SQL_POS := 16#0010#;

   SQL_RD_OFF     : constant := 0;
   SQL_RD_ON      : constant := 1;
   SQL_RD_DEFAULT : constant := SQL_RD_ON;

   type SQL_SU is new SQLUINTEGER;
   SQL_SU_DML_STATEMENTS       : constant SQL_POS := 16#0001#;
   SQL_SU_PROCEDURE_INVOCATION : constant SQL_POS := 16#0002#;
   SQL_SU_TABLE_DEFINITION     : constant SQL_POS := 16#0004#;
   SQL_SU_INDEX_DEFINITION     : constant SQL_POS := 16#0008#;
   SQL_SU_PRIVILEGE_DEFINITION : constant SQL_POS := 16#0010#;

   type SQL_SO is new SQLUINTEGER;
   SQL_SO_FORWARD_ONLY  : constant SQL_POS := 16#0001#;
   SQL_SO_KEYSET_DRIVEN : constant SQL_POS := 16#0002#;
   SQL_SO_DYNAMIC       : constant SQL_POS := 16#0004#;
   SQL_SO_MIXED         : constant SQL_POS := 16#0008#;
   SQL_SO_STATIC        : constant SQL_POS := 16#0010#;

   type SQL_SC is new SQLUINTEGER;
   SQL_SC_SQL92_ENTRY            : constant SQL_SC := 16#0001#;
   SQL_SC_FIPS127_2_TRANSITIONAL : constant SQL_SC := 16#0002#;
   SQL_SC_SQL92_INTERMEDIATE     : constant SQL_SC := 16#0004#;
   SQL_SC_SQL92_FULL             : constant SQL_SC := 16#0008#;

   type SQL_SDF is new SQLUINTEGER;
   SQL_SDF_CURRENT_DATE      : constant SQL_SDF := 16#0001#;
   SQL_SDF_CURRENT_TIME      : constant SQL_SDF := 16#0002#;
   SQL_SDF_CURRENT_TIMESTAMP : constant SQL_SDF := 16#0004#;

   type SQL_SFKD is new SQLUINTEGER;
   SQL_SFKD_CASCADE     : constant SQL_SFKD := 16#0001#;
   SQL_SFKD_NO_ACTION   : constant SQL_SFKD := 16#0002#;
   SQL_SFKD_SET_DEFAULT : constant SQL_SFKD := 16#0004#;
   SQL_SFKD_SET_NULL    : constant SQL_SFKD := 16#0008#;

   type SQL_SFKU is new SQLUINTEGER;
   SQL_SFKU_CASCADE     : constant SQL_SFKU := 16#0001#;
   SQL_SFKU_NO_ACTION   : constant SQL_SFKU := 16#0002#;
   SQL_SFKU_SET_DEFAULT : constant SQL_SFKU := 16#0004#;
   SQL_SFKU_SET_NULL    : constant SQL_SFKU := 16#0008#;

   type SQL_SG is new SQLUINTEGER;
   SQL_SG_USAGE_ON_DOMAIN        : constant SQL_SG := 16#0001#;
   SQL_SG_USAGE_ON_CHARACTER_SET : constant SQL_SG := 16#0002#;
   SQL_SG_USAGE_ON_COLLATION     : constant SQL_SG := 16#0004#;
   SQL_SG_USAGE_ON_TRANSLATION   : constant SQL_SG := 16#0008#;
   SQL_SG_WITH_GRANT_OPTION      : constant SQL_SG := 16#0010#;
   SQL_SG_DELETE_TABLE           : constant SQL_SG := 16#0020#;
   SQL_SG_INSERT_TABLE           : constant SQL_SG := 16#0040#;
   SQL_SG_INSERT_COLUMN          : constant SQL_SG := 16#0080#;
   SQL_SG_REFERENCES_TABLE       : constant SQL_SG := 16#0100#;
   SQL_SG_REFERENCES_COLUMN      : constant SQL_SG := 16#0200#;
   SQL_SG_SELECT_TABLE           : constant SQL_SG := 16#0400#;
   SQL_SG_UPDATE_TABLE           : constant SQL_SG := 16#0800#;
   SQL_SG_UPDATE_COLUMN          : constant SQL_SG := 16#1000#;

   type SQL_SNVF is new SQLUINTEGER;
   SQL_SNVF_BIT_LENGTH       : constant SQL_SNVF := 16#0001#;
   SQL_SNVF_CHAR_LENGTH      : constant SQL_SNVF := 16#0002#;
   SQL_SNVF_CHARACTER_LENGTH : constant SQL_SNVF := 16#0004#;
   SQL_SNVF_EXTRACT          : constant SQL_SNVF := 16#0008#;
   SQL_SNVF_OCTET_LENGTH     : constant SQL_SNVF := 16#0010#;
   SQL_SNVF_POSITION         : constant SQL_SNVF := 16#0020#;

   type SQL_SP is new SQLUINTEGER;
   SQL_SP_EXISTS                : constant SQL_SP := 16#0001#;
   SQL_SP_ISNOTNULL             : constant SQL_SP := 16#0002#;
   SQL_SP_ISNULL                : constant SQL_SP := 16#0004#;
   SQL_SP_MATCH_FULL            : constant SQL_SP := 16#0008#;
   SQL_SP_MATCH_PARTIAL         : constant SQL_SP := 16#0010#;
   SQL_SP_MATCH_UNIQUE_FULL     : constant SQL_SP := 16#0020#;
   SQL_SP_MATCH_UNIQUE_PARTIAL  : constant SQL_SP := 16#0040#;
   SQL_SP_OVERLAPS              : constant SQL_SP := 16#0080#;
   SQL_SP_UNIQUE                : constant SQL_SP := 16#0100#;
   SQL_SP_LIKE                  : constant SQL_SP := 16#0200#;
   SQL_SP_IN                    : constant SQL_SP := 16#0400#;
   SQL_SP_BETWEEN               : constant SQL_SP := 16#0800#;
   SQL_SP_COMPARISON            : constant SQL_SP := 16#1000#;
   SQL_SP_QUANTIFIED_COMPARISON : constant SQL_SP := 16#2000#;

   type SQL_SRJO is new SQLUINTEGER;
   SQL_SRJO_CORRESPONDING_CLAUSE : constant SQL_SRJO := 16#0001#;
   SQL_SRJO_CROSS_JOIN           : constant SQL_SRJO := 16#0002#;
   SQL_SRJO_EXCEPT_JOIN          : constant SQL_SRJO := 16#0004#;
   SQL_SRJO_FULL_OUTER_JOIN      : constant SQL_SRJO := 16#0008#;
   SQL_SRJO_INNER_JOIN           : constant SQL_SRJO := 16#0010#;
   SQL_SRJO_INTERSECT_JOIN       : constant SQL_SRJO := 16#0020#;
   SQL_SRJO_LEFT_OUTER_JOIN      : constant SQL_SRJO := 16#0040#;
   SQL_SRJO_NATURAL_JOIN         : constant SQL_SRJO := 16#0080#;
   SQL_SRJO_RIGHT_OUTER_JOIN     : constant SQL_SRJO := 16#0100#;
   SQL_SRJO_UNION_JOIN           : constant SQL_SRJO := 16#0200#;

   type SQL_SR is new SQLUINTEGER;
   SQL_SR_USAGE_ON_DOMAIN        : constant SQL_SR := 16#0001#;
   SQL_SR_USAGE_ON_CHARACTER_SET : constant SQL_SR := 16#0002#;
   SQL_SR_USAGE_ON_COLLATION     : constant SQL_SR := 16#0004#;
   SQL_SR_USAGE_ON_TRANSLATION   : constant SQL_SR := 16#0008#;
   SQL_SR_GRANT_OPTION_FOR       : constant SQL_SR := 16#0010#;
   SQL_SR_CASCADE                : constant SQL_SR := 16#0020#;
   SQL_SR_RESTRICT               : constant SQL_SR := 16#0040#;
   SQL_SR_DELETE_TABLE           : constant SQL_SR := 16#0080#;
   SQL_SR_INSERT_TABLE           : constant SQL_SR := 16#0100#;
   SQL_SR_INSERT_COLUMN          : constant SQL_SR := 16#0200#;
   SQL_SR_REFERENCES_TABLE       : constant SQL_SR := 16#0400#;
   SQL_SR_REFERENCES_COLUMN      : constant SQL_SR := 16#0800#;
   SQL_SR_SELECT_TABLE           : constant SQL_SR := 16#1000#;
   SQL_SR_UPDATE_TABLE           : constant SQL_SR := 16#2000#;
   SQL_SR_UPDATE_COLUMN          : constant SQL_SR := 16#4000#;

   type SQL_SRVC is new SQLUINTEGER;
   SQL_SRVC_VALUE_EXPRESSION : constant SQL_SRVC := 16#0001#;
   SQL_SRVC_NULL             : constant SQL_SRVC := 16#0002#;
   SQL_SRVC_DEFAULT          : constant SQL_SRVC := 16#0004#;
   SQL_SRVC_ROW_SUBQUERY     : constant SQL_SRVC := 16#0008#;

   type SQL_SSF is new SQLUINTEGER;
   SQL_SSF_CONVERT       : constant SQL_SSF := 16#0001#;
   SQL_SSF_LOWER         : constant SQL_SSF := 16#0002#;
   SQL_SSF_UPPER         : constant SQL_SSF := 16#0004#;
   SQL_SSF_SUBSTRING     : constant SQL_SSF := 16#0008#;
   SQL_SSF_TRANSLATE     : constant SQL_SSF := 16#0010#;
   SQL_SSF_TRIM_BOTH     : constant SQL_SSF := 16#0020#;
   SQL_SSF_TRIM_LEADING  : constant SQL_SSF := 16#0040#;
   SQL_SSF_TRIM_TRAILING : constant SQL_SSF := 16#0080#;

   type SQL_SVE is new SQLUINTEGER;
   SQL_SVE_CASE     : constant SQL_SVE := 16#0001#;
   SQL_SVE_CAST     : constant SQL_SVE := 16#0002#;
   SQL_SVE_COALESCE : constant SQL_SVE := 16#0004#;
   SQL_SVE_NULLIF   : constant SQL_SVE := 16#0008#;

   type SQL_SCC is new SQLUINTEGER;
   SQL_SCC_XOPEN_CLI_VERSION1 : constant SQL_SCC := 16#0001#;
   SQL_SCC_ISO92_CLI          : constant SQL_SCC := 16#0002#;

   type SQL_SQ is new SQLUINTEGER;
   SQL_SQ_COMPARISON            : constant SQL_SQ := 16#0001#;
   SQL_SQ_EXISTS                : constant SQL_SQ := 16#0002#;
   SQL_SQ_IN                    : constant SQL_SQ := 16#0004#;
   SQL_SQ_QUANTIFIED            : constant SQL_SQ := 16#0008#;
   SQL_SQ_CORRELATED_SUBQUERIES : constant SQL_SQ := 16#0010#;

   type SQL_TC is new SQLUSMALLINT range 0..4;
   SQL_TC_NONE       : constant SQL_TC := 0;
   SQL_TC_DML        : constant SQL_TC := 1;
   SQL_TC_ALL        : constant SQL_TC := 2;
   SQL_TC_DDL_COMMIT : constant SQL_TC := 3;
   SQL_TC_DDL_IGNORE : constant SQL_TC := 4;

   type SQL_TXN is new SQLUINTEGER;
   SQL_TXN_READ_UNCOMMITTED         : constant SQL_TXN := 16#00000001#;
   SQL_TRANSACTION_READ_UNCOMMITTED : constant SQL_TXN :=
                                               SQL_TXN_READ_UNCOMMITTED;
   SQL_TXN_READ_COMMITTED           : constant SQL_TXN := 16#00000002#;
   SQL_TRANSACTION_READ_COMMITTED   : constant SQL_TXN :=
                                               SQL_TXN_READ_COMMITTED;
   SQL_TXN_REPEATABLE_READ          : constant SQL_TXN := 16#00000004#;
   SQL_TRANSACTION_REPEATABLE_READ  : constant SQL_TXN :=
                                               SQL_TXN_REPEATABLE_READ;
   SQL_TXN_SERIALIZABLE             : constant SQL_TXN := 16#00000008#;
   SQL_TRANSACTION_SERIALIZABLE     : constant SQL_TXN :=
                                               SQL_TXN_SERIALIZABLE;

   type SQL_U is new SQLUINTEGER;
   SQL_U_UNION     : constant SQL_U := 16#1#;
   SQL_U_UNION_ALL : constant SQL_U := 16#2#;

private
   pragma Inline ("<", "<=", ">=", ">");

end ODBC.SQLTypes;
