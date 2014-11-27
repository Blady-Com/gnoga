--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_ODBC_Bindings                          Luebeck            --
--  Implementation                                 Autumn, 2012       --
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

with Ada.Calendar;                use Ada.Calendar;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;                 use Ada.Text_IO;
with ODBC.API;                    use ODBC.API;
with ODBC.Bound_Parameters;       use ODBC.Bound_Parameters;
with ODBC.SQLTypes;               use ODBC.SQLTypes;
with  Strings_Edit.Streams;       use  Strings_Edit.Streams;
with Strings_Edit.UTF8.Handling;  use Strings_Edit.UTF8.Handling;

procedure Test_ODBC_Bindings is
   generic
      type Enumeration is (<>);
      List : String;
   function Enumeration_Image (Value : Enumeration)
      return String;

   function Enumeration_Image (Value : Enumeration)
      return String is
      Start : Integer := List'First;
      Stop  : Integer := Start;
   begin
      for Position in Enumeration'Range loop
         while Stop <= List'Last and then List (Stop) /= ' ' loop
            Stop := Stop + 1;
         end loop;
         if Position = Value then
            return List (Start..Stop - 1);
         end if;
         exit when Stop > List'Last;
         Stop  := Stop + 1;
         Start := Stop;
      end loop;
      return Enumeration'Image (Value);
   end Enumeration_Image;

   generic
      type Set is mod <>;
      List : String;
   function Set_Image (Value : Set) return String;

   function Set_Image (Value : Set)
      return String is
      Result : Unbounded_String;
      Start  : Integer := List'First;
      Stop   : Integer := Start;
   begin
      for Position in Natural'Range loop
         while Stop <= List'Last and then List (Stop) /= ' ' loop
            Stop := Stop + 1;
         end loop;
         exit when Stop - Start <= 1;
         if 0 /= (Value and Set'Val (2**Position)) then
            if Length (Result) > 0 then
               Append (Result, ", ");
            end if;
            Append (Result, List (Start..Stop - 1));
         end if;
         Stop  := Stop + 1;
         Start := Stop;
      end loop;
      return To_String (Result) & " =" & Set'Image (Value);
   end Set_Image;

   Server_Name : Unbounded_String := To_Unbounded_String ("test_odbc");
   User_Name   : Unbounded_String := To_Unbounded_String ("test");
   Password    : Unbounded_String := To_Unbounded_String ("test");
   Trace_File  : Unbounded_String;

   procedure Query is
      Text : String (1..512);
      procedure Get (Prompt : String; Name : in out Unbounded_String) is
         Last : Integer;
      begin
         Put ("   " & Prompt & " [" & To_String (Name) & "]: ");
         Get_Line (Text, Last);
         if Last >= 1 then
            Name := To_Unbounded_String (Text (1..Last));
         end if;
      end Get;
   begin
      Get ("The server name (DSN)",      Server_Name);
      Get ("The user name",              User_Name);
      Get ("The password",               Password);
      Get ("Trace file (empty if none)", Trace_File);
   end Query;

   function Image is
      new Enumeration_Image
          (  SQL_CP, "OFF ONE_PER_DRIVER ONE_PER_HENV"
          );
   function Image is
      new Enumeration_Image
          (  SQL_CP_MATCH, "STRICT_MATCH RELAXED_MATCH"
          );
   function Image is
      new Enumeration_Image
          (  SQL_MODE, "READ_WRITE READ_ONLY"
          );
   function Image is
      new Set_Image
          (  SQL_AF, "AVG COUNT MAX MIN SUM DISTINCT ALL"
          );
   function Image is
      new Set_Image
          (  SQL_AD,
             (  "CONSTRAINT_NAME_DEFINITION "
             &  "ADD_DOMAIN_CONSTRAINT "
             &  "DROP_DOMAIN_CONSTRAINT "
             &  "ADD_DOMAIN_DEFAULT "
             &  "DROP_DOMAIN_DEFAULT "
             &  "ADD_CONSTRAINT_INITIALLY_DEFERRED "
             &  "ADD_CONSTRAINT_INITIALLY_IMMEDIATE "
             &  "ADD_CONSTRAINT_DEFERRABLE "
             &  "ADD_CONSTRAINT_NON_DEFERRABLE"
          )  );
   function Image is
      new Set_Image
          (  SQL_AT,
             (  "ADD_COLUMN "
             &  "DROP_COLUMN "
             &  "ADD_CONSTRAINT "
             &  "ADD_COLUMN_SINGLE "
             &  "ADD_COLUMN_DEFAULT "
             &  "ADD_COLUMN_COLLATION "
             &  "SET_COLUMN_DEFAULT "
             &  "DROP_COLUMN_DEFAULT "
             &  "DROP_COLUMN_CASCADE "
             &  "DROP_COLUMN_RESTRICT "
             &  "ADD_TABLE_CONSTRAINT "
             &  "DROP_TABLE_CONSTRAINT_CASCADE "
             &  "DROP_TABLE_CONSTRAINT_RESTRICT "
             &  "CONSTRAINT_NAME_DEFINITION "
             &  "CONSTRAINT_INITIALLY_DEFERRED "
             &  "CONSTRAINT_INITIALLY_IMMEDIATE "
             &  "CONSTRAINT_DEFERRABLE "
             &  "CONSTRAINT_NON_DEFERRABLE"
          )  );
   function Image is
      new Enumeration_Image
          (  SQL_AM, "NONE CONNECTION STATEMENT"
          );
   function Image is
      new Set_Image
          (  SQL_BRC, "PROCEDURES EXPLICIT ROLLED_UP"
          );
   function Image is
      new Set_Image
          (  SQL_BS,
             (  "SELECT_EXPLICIT "
             &  "ROW_COUNT_EXPLICIT "
             &  "SELECT_PROC "
             &  "ROW_COUNT_PROC"
          )  );
   function Image is
      new Set_Image
          (  SQL_BP,
             "CLOSE DELETE DROP TRANSACTION UPDATE OTHER_HSTMT SCROLL"
          );
   function Image is new Set_Image (SQL_CL, "START END");
   function Image is
      new Set_Image
          (  SQL_CU,
             (  "DML_STATEMENTS "
             &  "PROCEDURE_INVOCATION "
             &  "TABLE_DEFINITION "
             &  "INDEX_DEFINITION "
             &  "PRIVILEGE_DEFINITION"
          )  );
   function Image is new Enumeration_Image (SQL_CB, "NULL NON_NULL");
   function Image is
      new Set_Image
          (  SQL_CVT,
             (  "CHAR "
             &  "NUMERIC "
             &  "DECIMAL "
             &  "INTEGER "
             &  "SMALLINT "
             &  "FLOAT "
             &  "REAL "
             &  "DOUBLE "
             &  "VARCHAR "
             &  "LONGVARCHAR "
             &  "BINARY "
             &  "VARBINARY "
             &  "BIT "
             &  "TINYINT "
             &  "BIGINT "
             &  "DATE "
             &  "TIME "
             &  "TIMESTAMP "
             &  "LONGVARBINARY "
             &  "INTERVAL_YEAR_MONTH "
             &  "INTERVAL_DAY_TIME "
             &  "WCHAR "
             &  "WLONGVARCHAR "
             &  "WVARCHAR "
             &  "GUID"
          )  );
   function Image is
      new Set_Image (SQL_FN_CVT, "CONVERT CAST");
   function Image is
      new Enumeration_Image (SQL_CN, "NONE DIFFERENT ANY");
   function Image is
      new Set_Image
          (  SQL_CA,
             (  "CREATE_ASSERTION "
             &  "2 4 8 "
             &  "CONSTRAINT_INITIALLY_DEFERRED "
             &  "CONSTRAINT_INITIALLY_IMMEDIATE "
             &  "CONSTRAINT_DEFERRABLE "
             &  "CONSTRAINT_NON_DEFERRABLE"
          )  );
   function Image is
      new Set_Image
          (  SQL_CCS,
             (  "CREATE_CHARACTER_SET "
             &  "COLLATE_CLAUSE "
             &  "LIMITED_COLLATION"
          )  );
   function Image is new Set_Image (SQL_CCOL, "CREATE_COLLATION");
   function Image is
      new Set_Image
          (  SQL_CDO,
             (  "CREATE_DOMAIN "
             &  "DEFAULT "
             &  "CONSTRAINT "
             &  "COLLATION "
             &  "CONSTRAINT_NAME_DEFINITION "
             &  "CONSTRAINT_INITIALLY_DEFERRED "
             &  "CONSTRAINT_INITIALLY_IMMEDIATE "
             &  "CONSTRAINT_DEFERRABLE "
             &  "CONSTRAINT_NON_DEFERRABLE"
          )  );
   function Image is
      new Set_Image
          (  SQL_CS,
             (  "CREATE_SCHEMA "
             &  "AUTHORIZATION "
             &  "DEFAULT_CHARACTER_SET"
          )  );
   function Image is
      new Set_Image
          (  SQL_CT,
             (  "CREATE_TABLE "
             &  "COMMIT_PRESERVE "
             &  "COMMIT_DELETE "
             &  "GLOBAL_TEMPORARY "
             &  "LOCAL_TEMPORARY "
             &  "CONSTRAINT_INITIALLY_DEFERRED "
             &  "CONSTRAINT_INITIALLY_IMMEDIATE "
             &  "CONSTRAINT_DEFERRABLE "
             &  "CONSTRAINT_NON_DEFERRABLE "
             &  "COLUMN_CONSTRAINT "
             &  "COLUMN_DEFAULT "
             &  "COLUMN_COLLATION "
             &  "TABLE_CONSTRAINT "
             &  "CONSTRAINT_NAME_DEFINITION"
          )  );
   function Image is new Set_Image (SQL_CTR, "CREATE_TRANSLATION");
   function Image is
      new Set_Image
          (  SQL_CV,
             "CREATE_VIEW CHECK_OPTION CASCADED LOCAL"
          );
   function Image is
      new Enumeration_Image
          (  SQL_CCB, "DELETE CLOSE PRESERVE"
          );
   function Image is
      new Enumeration_Image
          (  SQL_SENSITIVITY, "UNSPECIFIED INSENSITIVE SENSITIVE"
          );
   function Image is
      new Set_Image
          (  SQL_DL,
             (  "SQL92_DATE "
             &  "SQL92_TIME "
             &  "SQL92_TIMESTAMP "
             &  "SQL92_INTERVAL_YEAR "
             &  "SQL92_INTERVAL_MONTH "
             &  "SQL92_INTERVAL_DAY "
             &  "SQL92_INTERVAL_HOUR "
             &  "SQL92_INTERVAL_MINUTE "
             &  "SQL92_INTERVAL_SECOND "
             &  "SQL92_INTERVAL_YEAR_TO_MONTH "
             &  "SQL92_INTERVAL_DAY_TO_HOUR "
             &  "SQL92_INTERVAL_DAY_TO_MINUTE "
             &  "SQL92_INTERVAL_DAY_TO_SECOND "
             &  "SQL92_INTERVAL_HOUR_TO_MINUTE "
             &  "SQL92_INTERVAL_HOUR_TO_SECOND "
             &  "SQL92_INTERVAL_MINUTE_TO_SECOND"
          )  );
   function Image is
      new Set_Image (SQL_DI, "CREATE_INDEX DROP_INDEX");
   function Image is
      new Set_Image
          (  SQL_TXN,
             (  "READ_UNCOMMITTED "
             &  "READ_COMMITTED "
             &  "REPEATABLE_READ "
             &  "SERIALIZABLE"
          )  );
   function Image is new Set_Image (SQL_DA, "DROP_ASSERTION");
   function Image is new Set_Image (SQL_DC, "DROP_COLLATION");
   function Image is new Set_Image (SQL_DCS, "DROP_CHARACTER_SET");
   function Image is
      new Set_Image (SQL_DD, "DROP_DOMAIN RESTRICT CASCADE");
   function Image is
      new Set_Image (SQL_DS, "DROP_SCHEMA RESTRICT CASCADE");
   function Image is
      new Set_Image (SQL_DT, "DROP_TABLE RESTRICT CASCADE");
   function Image is new Set_Image (SQL_DTR, "DROP_TRANSLATION");
   function Image is
      new Set_Image (SQL_DV, "DROP_VIEW RESTRICT CASCADE");
   function Image is
      new Set_Image
          (  SQL_CA1,
             (  "NEXT "
             &  "ABSOLUTE "
             &  "RELATIVE "
             &  "BOOKMARK "
             &  "LOCK_NO_CHANGE "
             &  "LOCK_EXCLUSIVE "
             &  "LOCK_UNLOCK "
             &  "POS_POSITION "
             &  "POS_UPDATE "
             &  "POS_DELETE "
             &  "POS_REFRESH "
             &  "POSITIONED_UPDATE "
             &  "POSITIONED_DELETE "
             &  "SELECT_FOR_UPDATE "
             &  "BULK_ADD "
             &  "BULK_UPDATE_BY_BOOKMARK "
             &  "BULK_DELETE_BY_BOOKMARK "
             &  "BULK_FETCH_BY_BOOKMARK"
          )  );
   function Image is
      new Set_Image
          (  SQL_CA2,
             (  "READ_ONLY_CONCURRENCY "
             &  "LOCK_CONCURRENCY "
             &  "OPT_ROWVER_CONCURRENCY "
             &  "OPT_VALUES_CONCURRENCY "
             &  "SENSITIVITY_ADDITIONS "
             &  "SENSITIVITY_DELETIONS "
             &  "SENSITIVITY_UPDATES "
             &  "MAX_ROWS_SELECT "
             &  "MAX_ROWS_INSERT "
             &  "MAX_ROWS_DELETE "
             &  "MAX_ROWS_UPDATE "
             &  "MAX_ROWS_CATALOG "
             &  "CRC_EXACT "
             &  "CRC_APPROXIMATE "
             &  "SIMULATE_NON_UNIQUE "
             &  "SIMULATE_TRY_UNIQUE "
             &  "SIMULATE_UNIQUE"
          )  );
   function Image is
      new Enumeration_Image
          (  SQL_FILE, "NOT_SUPPORTED TABLE QUALIFIER CATALOG"
          );
   function Image is
      new Set_Image
          (  SQL_GD,
             (  "ANY_COLUMN "
             &  "ANY_ORDER "
             &  "BLOCK "
             &  "BOUND "
             &  "OUTPUT_PARAMS"
          )  );
   function Image is
      new Enumeration_Image
          (  SQL_GR,
             (  "NOT_SUPPORTED "
             &  "GROUP_BY_EQUALS_SELECT "
             &  "GROUP_BY_CONTAINS_SELECT "
             &  "NO_RELATION "
             &  "COLLATE"
          )  );
   function Image is
      new Enumeration_Image
          (  SQL_IC, "UPPER LOWER SENSITIVE MIXED"
          );
   function Image is new Set_Image (SQL_IK, "ASC DESC");
   function Image is
      new Set_Image
          (  SQL_IS,
             "INSERT_LITERALS INSERT_SEARCHED SELECT_INTO"
          );
   function Image is
      new Set_Image
          (  SQL_ISV,
             (  "ASSERTIONS "
             &  "CHARACTER_SETS "
             &  "CHECK_CONSTRAINTS "
             &  "COLLATIONS "
             &  "COLUMN_DOMAIN_USAGE "
             &  "COLUMN_PRIVILEGES "
             &  "COLUMNS "
             &  "CONSTRAINT_COLUMN_USAGE "
             &  "CONSTRAINT_TABLE_USAGE "
             &  "DOMAIN_CONSTRAINTS "
             &  "DOMAINS "
             &  "KEY_COLUMN_USAGE "
             &  "REFERENTIAL_CONSTRAINTS "
             &  "SCHEMATA "
             &  "SQL_LANGUAGES "
             &  "TABLE_CONSTRAINTS "
             &  "TABLE_PRIVILEGES "
             &  "TABLES "
             &  "TRANSLATIONS "
             &  "USAGE_PRIVILEGES "
             &  "VIEW_COLUMN_USAGE "
             &  "VIEW_TABLE_USAGE "
             &  "VIEWS"
          )  );
   function Image is
      new Enumeration_Image (SQL_NNC, "NULL NON_NULL");
   function Image is
      new Enumeration_Image
          (  SQL_NC,
             "HIGH LOW ? START END"
          );
   function Image is
      new Set_Image
          (  SQL_FN_NUM,
             (  "ABS ACOS ASIN ATAN ATAN2 CEILING COS COT EXP FLOOR "
             &  "LOG MOD SIGN SIN SQRT TAN PI RAND DEGREES LOG10 "
             &  "POWER RADIANS ROUND TRUNCATE"
          )  );
   function Image is
      new Enumeration_Image (SQL_OIC, "CORE LEVEL1 LEVEL2");
   function Image is
      new Set_Image
          (  SQL_OJ,
             (  "LEFT RIGHT FULL NESTED NOT_ORDERED INNER "
             &  "ALL_COMPARISON_OPS"
          )  );
   function Image is
      new Enumeration_Image (SQL_PARC, "BATCH NO_BATCH");
   function Image is
      new Enumeration_Image (SQL_PAS, "BATCH NO_BATCH NO_SELECT");
   function Image is
      new Set_Image (SQL_POS, "POSITION REFRESH UPDATE DELETE ADD");
   function Image is
      new Set_Image
          (  SQL_SU,
             (  "DML_STATEMENTS "
             &  "PROCEDURE_INVOCATION "
             &  "TABLE_DEFINITION "
             &  "INDEX_DEFINITION "
             &  "PRIVILEGE_DEFINITION"
          )  );
   function Image is
      new Set_Image
          (  SQL_SO,
             (  "FORWARD_ONLY "
             &  "KEYSET_DRIVEN "
             &  "DYNAMIC "
             &  "MIXED "
             &  "STATIC"
          )  );
   function Image is
      new Set_Image
          (  SQL_SC,
             (  "SQL92_ENTRY "
             &  "FIPS127_2_TRANSITIONAL "
             &  "SQL92_INTERMEDIATE "
             &  "SQL92_FULL"
          )  );
   function Image is
      new Set_Image
          (  SQL_SDF,
             "CURRENT_DATE CURRENT_TIME CURRENT_TIMESTAMP"
          );
   function Image is
      new Set_Image
          (  SQL_SFKD,
             "CASCADE NO_ACTION SET_DEFAULT SET_NULL"
          );
   function Image is
      new Set_Image
          (  SQL_SFKU,
             "CASCADE NO_ACTION SET_DEFAULT SET_NULL"
          );
   function Image is
      new Set_Image
          (  SQL_SG,
             (  "USAGE_ON_DOMAIN "
             &  "USAGE_ON_CHARACTER_SET "
             &  "USAGE_ON_COLLATION "
             &  "USAGE_ON_TRANSLATION "
             &  "WITH_GRANT_OPTION "
             &  "DELETE_TABLE "
             &  "INSERT_TABLE "
             &  "INSERT_COLUMN "
             &  "REFERENCES_TABLE "
             &  "REFERENCES_COLUMN "
             &  "SELECT_TABLE "
             &  "UPDATE_TABLE "
             &  "UPDATE_COLUMN"
          )  );
   function Image is
      new Set_Image
          (  SQL_SNVF,
             (  "BIT_LENGTH "
             &  "CHAR_LENGTH "
             &  "CHARACTER_LENGTH "
             &  "EXTRACT "
             &  "OCTET_LENGTH "
             &  "POSITION"
          )  );
   function Image is
      new Set_Image
          (  SQL_SP,
             (  "EXISTS "
             &  "ISNOTNULL "
             &  "ISNULL "
             &  "MATCH_FULL "
             &  "MATCH_PARTIAL "
             &  "MATCH_UNIQUE_FULL "
             &  "MATCH_UNIQUE_PARTIAL "
             &  "OVERLAPS "
             &  "UNIQUE "
             &  "LIKE "
             &  "IN "
             &  "BETWEEN "
             &  "COMPARISON "
             &  "QUANTIFIED_COMPARISON"
          )  );
   function Image is
      new Set_Image
          (  SQL_SRJO,
             (  "CORRESPONDING_CLAUSE "
             &  "CROSS_JOIN "
             &  "EXCEPT_JOIN "
             &  "FULL_OUTER_JOIN "
             &  "INNER_JOIN "
             &  "INTERSECT_JOIN "
             &  "LEFT_OUTER_JOIN "
             &  "NATURAL_JOIN "
             &  "RIGHT_OUTER_JOIN "
             &  "UNION_JOIN"
          )  );
   function Image is
      new Set_Image
          (  SQL_SR,
             (  "USAGE_ON_DOMAIN "
             &  "USAGE_ON_CHARACTER_SET "
             &  "USAGE_ON_COLLATION "
             &  "USAGE_ON_TRANSLATION "
             &  "GRANT_OPTION_FOR "
             &  "CASCADE "
             &  "RESTRICT "
             &  "DELETE_TABLE "
             &  "INSERT_TABLE "
             &  "INSERT_COLUMN "
             &  "REFERENCES_TABLE "
             &  "REFERENCES_COLUMN "
             &  "SELECT_TABLE "
             &  "UPDATE_TABLE "
             &  "UPDATE_COLUMN"
          )  );
   function Image is
      new Set_Image
          (  SQL_SRVC,
             (  "VALUE_EXPRESSION "
             &  "NULL "
             &  "DEFAULT "
             &  "ROW_SUBQUERY"
          )  );
   function Image is
      new Set_Image
          (  SQL_SSF,
             (  "CONVERT "
             &  "LOWER "
             &  "UPPER "
             &  "SUBSTRING "
             &  "TRANSLATE "
             &  "TRIM_BOTH "
             &  "TRIM_LEADING "
             &  "TRIM_LEADING"
          )  );
   function Image is
      new Set_Image (SQL_SVE, "CASE CAST COALESCE NULLIF");
   function Image is
      new Set_Image (SQL_SCC, "XOPEN_CLI_VERSION1 ISO92_CLI");
   function Image is
      new Set_Image
          (  SQL_SQ,
             "COMPARISON EXISTS IN QUANTIFIED CORRELATED_SUBQUERIES"
          );
   function Image is
      new Set_Image
          (  SQL_FN_STR,
             (  "CONCAT INSERT LEFT LTRIM LENGTH LOCATE LCASE REPEAT "
             &  "REPLACE RIGHT RTRIM SUBSTRING UCASE ASCII CHAR "
             &  "DIFFERENCE LOCATE_2 SOUNDEX SPACE BIT_LENGTH "
             &  "CHAR_LENGTH CHARACTER_LENGTH OCTET_LENGTH POSITION"
          )  );
   function Image is
      new Set_Image (SQL_FN_SYS, "USERNAME DBNAME IFNULL");
   function Image is
      new Set_Image
          (  SQL_FN_TSI,
             (  "FRAC_SECOND SECOND MINUTE HOUR DAY WEEK MONTH "
             &  "QUARTER YEAR"
          )  );
   function Image is
      new Enumeration_Image
          (  SQL_TC,
             "NONE DML ALL DDL_COMMIT DDL_IGNORE"
          );
   function Image is new Set_Image (SQL_U, "UNION UNION_ALL");
   function Image is
      new Enumeration_Image
          (  SQL_CONCUR, "READ_ONLY LOCK ROWVER VALUES"
          );
   function Image is
      new Enumeration_Image
          (  SQL_CURSOR, "FORWARD_ONLY KEYSET_DRIVEN DYNAMIC STATIC"
          );
   function Image is
      new Enumeration_Image
          (  SQL_COLUMN_SEARCHABLE,
             "UNSEARCHABLE LIKE_ONLY ALL_EXCEPT_LIKE SEARCHABLE"
          );
   function Image is
      new Enumeration_Image (SQL_PRED_SEARCHABLE, "NONE CHAR BASIC");
   function Image is
      new Enumeration_Image
          (  SQL_NULLABLE_FIELD,
             "NO_NULLS NULLABLE NULLABLE_UNKNOWN"
          );

   subtype Valid_Data_Type is SQL_DATA_TYPE range -28..113;
   function Image is
      new Enumeration_Image
          (  Valid_Data_Type,
             (  "UTINYINT UBIGINT STINYINT SBIGINT 24 23 22 21 20 19 "
             &  "ULONG USHORT SLONG SSHORT 14 13 12 GUID WLONGVARCHAR "
             &  "WVARCHAR WCHAR BIT TINYINT BIGINT LONGVARBINARY "
             &  "VARBINARY BINARY LONGVARCHAR UNKNOWN_TYPE CHAR "
             &  "NUMERIC DECIMAL INTEGER/LONG SMALLINT/SHORT FLOAT "
             &  "REAL DOUBLE DATETIME/DATE TIME TIMESTAMP VARCHAR "
             &  "13 14 15 16 17 18 19 20 "
             &  "21 22 23 24 25 26 27 28 29 30 "
             &  "31 32 33 34 35 36 37 38 39 40 "
             &  "41 42 43 44 45 46 47 48 49 50 "
             &  "51 52 53 54 55 56 57 58 59 60 "
             &  "61 62 63 64 65 66 67 68 69 70 "
             &  "71 72 73 74 75 76 77 78 79 80 "
             &  "81 82 83 84 85 86 87 88 89 90 "
             &  "TYPE_DATE TYPE_TIME TYPE_TIMESTAMP 94 95 96 97 98 "
             &  "DEFAULT 100 INTERVAL_YEAR INTERVAL_MONTH "
             &  "INTERVAL_DAY INTERVAL_HOUR INTERVAL_MINUTE "
             &  "INTERVAL_SECOND INTERVAL_YEAR_TO_MONTH "
             &  "INTERVAL_DAY_TO_HOUR INTERVAL_DAY_TO_MINUTE "
             &  "INTERVAL_DAY_TO_SECOND INTERVAL_HOUR_TO_MINUTE "
             &  "INTERVAL_HOUR_TO_SECOND INTERVAL_MINUTE_TO_SECOND"
          )  );

   Environment : aliased ODBC_Environment;

begin
   declare
      X : SQL_TIMESTAMP_STRUCT := From_Time (Clock);
   begin
      if X /= From_Time (To_Time (X)) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Time conversion error"
         );
      end if;
   end;
   -- Querying environment attributes
   Put_Line ("------------ environment attributes:");
   Put_Line
   (  "Connection pooling: "
   &  Image (Get_Connection_Pooling (Environment))
   );
   Put_Line
   (  "Connection pooling match: "
   &  Image (Get_CP_Match (Environment))
   );
   Put_Line
   (  "ODBC Version:"
   &  SQL_OV'Image (Get_ODBC_Version (Environment))
   );
   Put_Line
   (  "Output null-terminated strings: "
   &  Boolean'Image (Get_Output_NTS (Environment))
   );
   Put_Line ("------------ environment diag record fields:");
   begin
      Put_Line
      (  "Class origin field: " & Get_Class_Origin (Environment, 1)
      );
   exception
      when End_Error =>
         Put_Line ("*** Class origin field: No data");
   end;
   begin
      Put_Line
      (  "Column number:"
      &  SQLINTEGER'Image (Get_Column_Number (Environment, 1))
      );
   exception
      when End_Error =>
         Put_Line ("*** Column number: No data");
   end;
   begin
      Put_Line
      (  "Connection name: " & Get_Connection_Name (Environment, 1)
      );
   exception
      when End_Error =>
         Put_Line ("*** Connection name: No data");
   end;
   begin
      Put_Line
      (  "Message text [ASCII]: " & Get_Message_Text (Environment, 1)
      );
      Put_Line
      (  "           [Unicode]: "
      &  To_UTF8 (Wide_String'(Get_Message_Text (Environment, 1)))
      );
   exception
      when End_Error =>
         Put_Line ("*** Message text: No data");
   end;
   begin
      Put_Line
      (  "Native:" & SQLINTEGER'Image (Get_Native (Environment, 1))
      );
   exception
      when End_Error =>
         Put_Line ("*** Native: No data");
   end;
   begin
      Put_Line
      (  "Number:"
      &  SQLINTEGER'Image (Get_Number (Environment, 1))
      );
   exception
      when End_Error =>
         Put_Line ("*** Number: No data");
   end;
   begin
      Put_Line
      (  "Returncode:"
      &  SQLRETURN'Image (Get_Returncode (Environment, 1))
      );
   exception
      when End_Error =>
         Put_Line ("*** Returncode: No data");
   end;
   begin
      Put_Line
      (  "Row number:"
      &  SQLLEN'Image (Get_Row_Number (Environment, 1))
      );
   exception
      when End_Error =>
         Put_Line ("*** Row number: No data");
   end;
   begin
      Put_Line
      (  "Server name [ASCII]:" & Get_Server_Name (Environment, 1)
      );
      Put_Line
      (  "          [Unicode]:"
      &  To_UTF8 (Wide_String'(Get_Server_Name (Environment, 1)))
      );
   exception
      when End_Error =>
         Put_Line ("*** Server name: No data");
   end;
   begin
      Put_Line ("SQLSTATE:" & Get_SQLSTATE (Environment, 1));
   exception
      when End_Error =>
         Put_Line ("*** SQLSTATE: No data");
   end;
   begin
      Put_Line
      (  "Subclass oigin:"
      &  Get_Subclass_Origin (Environment, 1)
      );
   exception
      when End_Error =>
         Put_Line ("*** Subclass oigin: No data");
   end;
   declare
      Connection : aliased ODBC_Connection (Environment'Access);
   begin
      Put_Line
      (  "Ready to connect to a DB. Make sure you have a datasource "
      &  "configured."
      );
      Put_Line
      (  "- For Windows it is under Control Panel->"
      &  "Administrative Tools->Data sources (ODBC)"
      );
      Put_Line
      (  "- For Linux it is usually the /etc/odbc.ini file "
      &  "to edit"
      );
      Put_Line
      (  "The datasource must be available for full access. "
      &  "The values in []"
      );
      Put_Line
      (  "are defaults used when the corresponding input is empty"
      );
      Query;
      Connect
      (  Connection  => Connection,
         Server_Name => To_String (Server_Name),
         User_Name   => To_String (User_Name),
         Password    => To_String (Password),
         Auto_Commit => True
      );
      Put_Line ("------------ connection attributes:");
      Put_Line
      (  "Access mode: " & Image (Get_Access_Mode (Connection))
      );
      Set_Access_Mode (Connection, SQL_MODE_READ_WRITE);
      begin
         Put_Line
         (  "Async DBC enable: "
         &  Boolean'Image (Get_Async_DBC_Enable (Connection))
         );
      exception
         when Error : Use_Error =>
            Put_Line ("*** Async DBC enabled: not supported");
         when Error : Data_Error =>
            Put_Line
            (  "*** Async DBC enabled: Driver manager error: "
            &  Exception_Message (Error)
            );
      end;
      begin
         Put_Line
         (  "Async enable: "
         &  Boolean'Image (Get_Async_Enable (Connection))
         );
      exception
         when Error : Use_Error =>
            Put_Line ("*** Async enable: not supported");
         when Error : Data_Error =>
            Put_Line
            (  "*** Async enable: Driver manager error: "
            &  Exception_Message (Error)
            );
      end;
      Put_Line
      (  "IPD automatic population: "
      &  Boolean'Image (Get_Auto_IPD (Connection))
      );
      Put_Line
      (  "Autocommit: "
      &  Boolean'Image (Get_Autocommit (Connection))
      );
      Set_Autocommit (Connection, Get_Autocommit (Connection));
      begin
         Put_Line
         (  "Connection dead: "
         &  Boolean'Image (Get_Connection_Dead (Connection))
         );
      exception
         when Error : Use_Error =>
            Put_Line ("*** Connection dead: not supported");
         when Error : Data_Error =>
            Put_Line
            (  "*** Connection dead: Driver manager error: "
            &  Exception_Message (Error)
            );
      end;
      begin
         Put_Line
         (  "Connection timeout: "
         &  Duration'Image (Get_Connection_Timeout (Connection))
         );
      exception
         when Error : Use_Error =>
            Put_Line ("*** Connection timeout: not supported");
         when Error : Data_Error =>
            Put_Line
            (  "*** Connection timeout: Driver manager error: "
            &  Exception_Message (Error)
            );
      end;
      Put_Line
      (  "Current catalog [ASCII]: "
      &  Get_Current_Catalog (Connection)
      );
      Put_Line
      (  "              [Unicode]: "
      &  To_UTF8 (Wide_String'(Get_Current_Catalog (Connection)))
      );
      begin
         Put_Line
         (  "Login timeout: "
         &  Duration'Image (Get_Login_Timeout (Connection))
         );
      exception
         when Error : Use_Error =>
            Put_Line
            (  "*** Login timeout: Error: "
            &  Exception_Message (Error)
            );
      end;
      begin
         Put_Line
         (  "Metadata ID: "
         &  Boolean'Image (Get_Metadata_ID (Connection))
         );
      exception
         when Error : Use_Error =>
            Put_Line
            (  "*** Metadata ID: Error: "
            &  Exception_Message (Error)
            );
         when Error : Data_Error =>
            Put_Line
            (  "*** Metadata ID: Driver manager error: "
            &  Exception_Message (Error)
            );
      end;
      begin
         Put_Line
         (  "Packet size:"
         &  Integer'Image (Get_Packet_Size (Connection))
         );
      exception
         when Error : Use_Error =>
            Put_Line
            (  "*** Packet size: "
            &  Exception_Message (Error)
            );
         when Error : Data_Error =>
            Put_Line
            (  "*** Packet size: "
            &  Exception_Message (Error)
            );
      end;
      Put_Line ("Trace: " & Boolean'Image (Get_Trace (Connection)));
      Put_Line
      (  "Tracefile [ASCII]: "
      &  Get_Tracefile (Connection)
      );
      Put_Line
      (  "        [Unicode]: "
      &  To_UTF8 (Wide_String'(Get_Tracefile (Connection)))
      );
      begin
         Put_Line
         (  "Translate lib [ASCII]: "
         &  Get_Translate_Lib (Connection)
         );
         Put_Line
         (  "            [Unicode]: "
         &  To_UTF8 (Wide_String'(Get_Translate_Lib (Connection)))
         );
      exception
         when Error : others =>
            Put_Line
            (  "*** Translate lib: "
            &  Exception_Message (Error)
            );
      end;
      begin
         Put_Line
         (  "Translate option:"
         &  SQLUINTEGER'Image (Get_Translate_Option (Connection))
         );
      exception
         when Error : Use_Error =>
            Put_Line
            (  "*** Translate option: "
            &  Exception_Message (Error)
            );
         when Error : Data_Error =>
            Put_Line
            (  "*** Translate option: "
            &  Exception_Message (Error)
            );
      end;
      Put_Line
      (  "TXN isolation: "
      &  Image (Get_TXN_Isolation (Connection))
      );
      Put_Line ("------------ connection information:");
      Put_Line
      (  "Accessible procedures: "
      &  Boolean'Image (Get_Accessible_Procedures (Connection))
      );
      Put_Line
      (  "Accessible tables: "
      &  Boolean'Image (Get_Accessible_Tables (Connection))
      );
      Put_Line
      (  "Active environments: "
      &  Integer'Image (Get_Active_Environments (Connection))
      );
      Put_Line
      (  "Aggregate functions: "
      &  Image (Get_Aggregate_Functions (Connection))
      );
      Put_Line
      (  "ALTER DOMAIN: "
      &  Image (Get_Alter_Domain (Connection))
      );
      Put_Line
      (  "ALTER TABLE: "
      &  Image (Get_Alter_Table (Connection))
      );
--        if Get_Async_DBC_Enable (Connection) then
--           Set_Async_DBC_Enable
--           (  Connection,
--              Get_Async_DBC_Enable (Connection)
--           );
--        end if;
      Put_Line
      (  "Async DBC functions: "
      &  Boolean'Image (Get_Async_DBC_Functions (Connection))
      );
      Put_Line
      (  "Async mode: "
      &  Image (Get_Async_Mode (Connection))
      );
      Put_Line
      (  "Batch row count: "
      &  Image (Get_Batch_Row_Count (Connection))
      );
      Put_Line
      (  "Batch support: "
      &  Image (Get_Batch_Support (Connection))
      );
      Put_Line
      (  "Bookmark persistence: "
      &  Image (Get_Bookmark_Persistence (Connection))
      );
      Put_Line
      (  "Catalog location: "
      &  Image (Get_Catalog_Location (Connection))
      );
      Put_Line
      (  "Catalog name (support): "
      &  Boolean'Image (Get_Catalog_Name (Connection))
      );
      Put_Line
      (  "Catalog name separator: "
      &  Get_Catalog_Name_Separator (Connection)
      );
      Put_Line ("Catalog term: " & Get_Catalog_Term (Connection));
      Put_Line
      (  "Catalog usage: "
      &  Image (Get_Catalog_Usage (Connection))
      );
      Put_Line ("Collation seq: " & Get_Collation_Seq (Connection));
      Put_Line
      (  "Column alias: "
      &  Boolean'Image (Get_Column_Alias (Connection))
      );
      Put_Line
      (  "Concat Null Behavior: "
      &  Image (Get_Concat_Null_Behavior (Connection))
      );
      declare
         procedure Query_Type
                   (  Data_Type : SQL_DATA_TYPE;
                      Name      : String
                   )  is
         begin
            Put_Line
            (  "Conversions for "
            &  Name
            &  ": "
            &  Image (Get_Convert (Connection, Data_Type))
            );
         end;
      begin
         Query_Type (SQL_UTINYINT,        "SQL_UTINYINT");
         Query_Type (SQL_UBIGINT,         "SQL_UBIGINT");
         Query_Type (SQL_STINYINT,        "SQL_STINYINT");
         Query_Type (SQL_SBIGINT,         "SQL_SBIGINT");
         Query_Type (SQL_ULONG,           "SQL_ULONG");
         Query_Type (SQL_USHORT,          "SQL_USHORT");
         Query_Type (SQL_SLONG,           "SQL_SLONG");
         Query_Type (SQL_SSHORT,          "SQL_SSHORT");
         Query_Type (SQL_GUID,            "SQL_GUID");
         Query_Type (SQL_WLONGVARCHAR,    "SQL_WLONGVARCHAR");
         Query_Type (SQL_WVARCHAR,        "SQL_WVARCHAR");
         Query_Type (SQL_WCHAR,           "SQL_WCHAR");
         Query_Type (SQL_BIT,             "SQL_BIT");
         Query_Type (SQL_TINYINT,         "SQL_TINYINT");
         Query_Type (SQL_BIGINT,          "SQL_BIGINT");
         Query_Type (SQL_LONGVARBINARY,   "SQL_LONGVARBINARY");
         Query_Type (SQL_VARBINARY,       "SQL_VARBINARY");
         Query_Type (SQL_BINARY,          "SQL_BINARY");
         Query_Type (SQL_LONGVARCHAR,     "SQL_LONGVARCHAR");
         Query_Type (SQL_UNKNOWN_TYPE,    "SQL_UNKNOWN_TYPE");
         Query_Type (SQL_CHAR,            "SQL_CHAR");
         Query_Type (SQL_NUMERIC,         "SQL_NUMERIC");
         Query_Type (SQL_DECIMAL,         "SQL_DECIMAL");
         Query_Type (SQL_INTEGER,         "SQL_INTEGER");
         Query_Type (SQL_LONG,            "SQL_LONG");
         Query_Type (SQL_SMALLINT,        "SQL_SMALLINT");
         Query_Type (SQL_SHORT,           "SQL_SHORT");
         Query_Type (SQL_FLOAT,           "SQL_FLOAT");
         Query_Type (SQL_REAL,            "SQL_REAL");
         Query_Type (SQL_DOUBLE,          "SQL_DOUBLE");
         Query_Type (SQL_DATETIME,        "SQL_DATETIME");
         Query_Type (SQL_DATE,            "SQL_DATE");
         Query_Type (SQL_TIME,            "SQL_TIME");
         Query_Type (SQL_TIMESTAMP,       "SQL_TIMESTAMP");
         Query_Type (SQL_VARCHAR,         "SQL_VARCHAR");
         Query_Type (SQL_TYPE_DATE,       "SQL_TYPE_DATE");
         Query_Type (SQL_TYPE_TIME,       "SQL_TYPE_TIME");
         Query_Type (SQL_TYPE_TIMESTAMP,  "SQL_TYPE_TIMESTAMP");
         Query_Type (SQL_DEFAULT,         "SQL_DEFAULT");
         Query_Type (SQL_INTERVAL_YEAR,   "SQL_INTERVAL_YEAR");
         Query_Type (SQL_INTERVAL_MONTH,  "SQL_INTERVAL_MONTH");
         Query_Type (SQL_INTERVAL_DAY,    "SQL_INTERVAL_DAY");
         Query_Type (SQL_INTERVAL_HOUR,   "SQL_INTERVAL_HOUR");
         Query_Type (SQL_INTERVAL_MINUTE, "SQL_INTERVAL_MINUTE");
         Query_Type (SQL_INTERVAL_SECOND, "SQL_INTERVAL_SECOND");
         Query_Type
         (  SQL_INTERVAL_YEAR_TO_MONTH,
            "SQL_INTERVAL_YEAR_TO_MONTH"
         );
         Query_Type
         (  SQL_INTERVAL_DAY_TO_HOUR,
            "SQL_INTERVAL_DAY_TO_HOUR"
         );
         Query_Type
         (  SQL_INTERVAL_DAY_TO_MINUTE,
            "SQL_INTERVAL_DAY_TO_MINUTE"
         );
         Query_Type
         (  SQL_INTERVAL_DAY_TO_SECOND,
            "SQL_INTERVAL_DAY_TO_SECOND"
         );
         Query_Type
         (  SQL_INTERVAL_HOUR_TO_MINUTE,
            "SQL_INTERVAL_HOUR_TO_MINUTE"
         );
         Query_Type
         (  SQL_INTERVAL_HOUR_TO_SECOND,
            "SQL_INTERVAL_HOUR_TO_SECOND"
         );
         Query_Type
         (  SQL_INTERVAL_MINUTE_TO_SECOND,
            "SQL_INTERVAL_MINUTE_TO_SECOND");
      end;
      Put_Line
      (  "Convert functions: "
      &  Image (Get_Convert_Functions (Connection))
      );
      Put_Line
      (  "Correlation name: "
      &  Image (Get_Correlation_Name (Connection))
      );
      Put_Line
      (  "Create assertion: "
      &  Image (Get_Create_Assertion (Connection))
      );
      Put_Line
      (  "Create character set: "
      &  Image (Get_Create_Character_Set (Connection))
      );
      Put_Line
      (  "Create collation: "
      &  Image (Get_Create_Collation (Connection))
      );
      Put_Line
      (  "Create domain: " & Image (Get_Create_Domain (Connection))
      );
      Put_Line
      (  "Create schema: " & Image (Get_Create_Schema (Connection))
      );
      Put_Line
      (  "Create table: " & Image (Get_Create_Table (Connection))
      );
      Put_Line
      (  "Create translation: "
      &  Image (Get_Create_Translation (Connection))
      );
      Put_Line ("Create view: " & Image (Get_Create_View (Connection)));
      Put_Line
      (  "Cursor commit behavior: "
      &  Image (Get_Cursor_Commit_Behavior (Connection))
      );
      Put_Line
      (  "Cursor rollback behavior: "
      &  Image (Get_Cursor_Rollback_Behavior (Connection))
      );
      begin
         Put_Line
         (  "Cursor sensitivity: "
         &  Image (Get_Cursor_Sensitivity (Connection))
         );
      exception
         when Use_Error =>
            Put_Line ("*** Cursor sensitivity: Not supported");
      end;
      Put_Line
      (  "Data source name [ASCII]: "
      &  Get_Data_Source_Name (Connection)
      );
      Put_Line
      (  "               [Unicode]: "
      &  To_UTF8 (Wide_String'(Get_Data_Source_Name (Connection)))
      );
      Put_Line
      (  "Data source readonly: "
      &  Boolean'Image (Get_Data_Source_Readonly (Connection))
      );
      Put_Line
      (  "Data base name [ASCII]: "
      &  Get_Database_Name (Connection)
      );
      Put_Line
      (  "             [Unicode]: "
      &  To_UTF8 (Wide_String'(Get_Database_Name (Connection)))
      );
      Put_Line
      (  "Datetime literals: "
      &  Image (Get_Datetime_Literals (Connection))
      );
      Put_Line
      (  "DBMS name [ASCII]: "
      &  Get_DBMS_Name (Connection)
      );
      Put_Line
      (  "        [Unicode]: "
      &  To_UTF8 (Wide_String'(Get_DBMS_Name (Connection)))
      );
      Put_Line ("DBMS version: " & Get_DBMS_Version (Connection));
      Put_Line ("DDL index: " & Image (Get_DDL_Index (Connection)));
      Put_Line
      (  "Default TXN isolation: "
      &  Image (Get_Default_TXN_Isolation (Connection))
      );
      Put_Line
      (  "Describe parameter: "
      &  Boolean'Image (Get_Describe_Parameter (Connection))
      );
      Put_Line ("DM version: " & Get_DM_Version (Connection));
      Put_Line
      (  "Driver name [ASCII]: "
      &  Get_Driver_Name (Connection)
      );
      Put_Line
      (  "          [Unicode]: "
      &  To_UTF8 (Wide_String'(Get_Driver_Name (Connection)))
      );
      Put_Line ("Driver ODBC ver: " & Get_Driver_ODBC_Ver (Connection));
      Put_Line ("Driver ver: " & Get_Driver_Ver (Connection));
      Put_Line
      (  "Drop assertion: "
      &  Image (Get_Drop_Assertion (Connection))
      );
      Put_Line
      (  "Drop character set: "
      &  Image (Get_Drop_Character_Set (Connection))
      );
      Put_Line
      (  "Drop collation: "
      &  Image (Get_Drop_Collation (Connection))
      );
      Put_Line
      (  "Drop domain: "
      &  Image (Get_Drop_Domain (Connection))
      );
      Put_Line
      (  "Drop schema: "
      &  Image (Get_Drop_Schema (Connection))
      );
      Put_Line
      (  "Drop table: "
      &  Image (Get_Drop_Table (Connection))
      );
      Put_Line
      (  "Drop translation: "
      &  Image (Get_Drop_Translation (Connection))
      );
      Put_Line
      (  "Drop view: "
      &  Image (Get_Drop_View (Connection))
      );
      Put_Line
      (  "Dynamic cursor attributes 1: "
      &  Image (SQL_CA1'(Get_Dynamic_Cursor_Attributes (Connection)))
      );
      Put_Line
      (  "Dynamic cursor attributes 2: "
      &  Image (SQL_CA2'(Get_Dynamic_Cursor_Attributes (Connection)))
      );
      Put_Line
      (  "Expressions In ORDERBY: "
      &  Boolean'Image (Get_Expressions_In_Orderby (Connection))
      );
      Put_Line ("File usage: " & Image (Get_File_Usage (Connection)));
      Put_Line
      (  "Forward only cursor attributes 1: "
      &  Image
         (  SQL_CA1'(Get_Forward_Only_Cursor_Attributes (Connection))
      )  );
      Put_Line
      (  "Forward only cursor attributes 2: "
      &  Image
         (  SQL_CA2'(Get_Forward_Only_Cursor_Attributes (Connection))
      )  );
      Put_Line
      (  "Getdata extensions: "
      &  Image (Get_Getdata_Extensions (Connection))
      );
      Put_Line ("Group by: " & Image (Get_Group_By (Connection)));
      Put_Line
      (  "Identifier case: "
      &  Image (Get_Identifier_Case (Connection))
      );
      Put_Line
      (  "Identifier quote char: "
      &  Get_Identifier_Quote_Char (Connection)
      );
      Put_Line
      (  "Index keywords: "
      &  Image (Get_Index_Keywords (Connection))
      );
      Put_Line
      (  "Info schema view: "
      &  Image (Get_Info_Schema_Views (Connection))
      );
      Put_Line
      (  "Insert statement: "
      &  Image (Get_Insert_Statement (Connection))
      );
      Put_Line
      (  "Integrity: " & Boolean'Image (Get_Integrity (Connection))
      );
      Put_Line
      (  "Keyset cursor attributes 1: "
      &  Image (SQL_CA1'(Get_Keyset_Cursor_Attributes (Connection)))
      );
      Put_Line
      (  "Keyset cursor attributes 2: "
      &  Image (SQL_CA2'(Get_Keyset_Cursor_Attributes (Connection)))
      );
      Put_Line ("Keywords: " & Get_Keywords (Connection));
      Put_Line
      (  "Like escape clause: "
      &  Boolean'Image (Get_Like_Escape_Clause (Connection))
      );
      begin
         Put_Line
         (  "Max async concurrent statements:"
         &  Integer'Image
            (  Get_Max_Async_Concurrent_Statements (Connection)
         )  );
      exception
         when Error : Use_Error =>
           Put_Line ("Max async concurrent statements: Not supported");
      end;
      Put_Line
      (  "Max binary literl len:"
      &  Integer'Image (Get_Max_Binary_Literal_Len (Connection))
      );
      Put_Line
      (  "Max catalog name len:"
      &  Integer'Image (Get_Max_Catalog_Name_Len (Connection))
      );
      Put_Line
      (  "Max char literal len:"
      &  Integer'Image (Get_Max_Char_Literal_Len (Connection))
      );
      Put_Line
      (  "Max column name len:"
      &  Integer'Image (Get_Max_Column_Name_Len (Connection))
      );
      Put_Line
      (  "Max columns in GROUP BY:"
      &  Integer'Image (Get_Max_Columns_In_Group_By (Connection))
      );
      Put_Line
      (  "Max columns in index:"
      &  Integer'Image (Get_Max_Columns_In_Index (Connection))
      );
      Put_Line
      (  "Max columns in order by:"
      &  Integer'Image (Get_Max_Columns_In_Order_By (Connection))
      );
      Put_Line
      (  "Max columns in select:"
      &  Integer'Image (Get_Max_Columns_In_Select (Connection))
      );
      Put_Line
      (  "Max columns in table:"
      &  Integer'Image (Get_Max_Columns_In_Table (Connection))
      );
      Put_Line
      (  "Max concurrent activities:"
      &  Integer'Image (Get_Max_Concurrent_Activities (Connection))
      );
      Put_Line
      (  "Max cursor name len:"
      &  Integer'Image (Get_Max_Cursor_Name_Len (Connection))
      );
      Put_Line
      (  "Max driver connections:"
      &  Integer'Image (Get_Max_Driver_Connections (Connection))
      );
      Put_Line
      (  "Max identifier len:"
      &  Integer'Image (Get_Max_Identifier_Len (Connection))
      );
      Put_Line
      (  "Max index size:"
      &  Integer'Image (Get_Max_Index_Size (Connection))
      );
      Put_Line
      (  "Max procedure name len:"
      &  Integer'Image (Get_Max_Procedure_Name_Len (Connection))
      );
      Put_Line
      (  "Max row size:"
      &  Integer'Image (Get_Max_Row_Size (Connection))
      );
      Put_Line
      (  "Max row size includes long:"
      &  Boolean'Image (Get_Max_Row_Size_Includes_Long (Connection))
      );
      Put_Line
      (  "Max schema name len:"
      &  Integer'Image (Get_Max_Schema_Name_Len (Connection))
      );
      Put_Line
      (  "Max statement len:"
      &  Integer'Image (Get_Max_Statement_Len (Connection))
      );
      Put_Line
      (  "Max table name len:"
      &  Integer'Image (Get_Max_Table_Name_Len (Connection))
      );
      Put_Line
      (  "Max tables in select:"
      &  Integer'Image (Get_Max_Tables_In_Select (Connection))
      );
      Put_Line
      (  "Max user name len:"
      &  Integer'Image (Get_Max_User_Name_Len (Connection))
      );
      Put_Line
      (  "Multiple result sets: "
      &  Boolean'Image (Get_Multiple_Result_Sets (Connection))
      );
      Put_Line
      (  "Multiple active TXN: "
      &  Boolean'Image (Get_Multiple_Active_TXN (Connection))
      );
      Put_Line
      (  "Need long data Len: "
      &  Boolean'Image (Get_Need_Long_Data_Len (Connection))
      );
      Put_Line
      (  "Non nullable columns: "
      &  Image (Get_Non_Nullable_Columns (Connection))
      );
      Put_Line
      (  "Null collation: "
      &  Image (Get_Null_Collation (Connection))
      );
      Put_Line
      (  "Numeric functions: "
      &  Image (Get_Numeric_Functions (Connection))
      );
      Put_Line
      (  "ODBC interface conformance: "
      &  Image (Get_ODBC_Interface_Conformance (Connection))
      );
      Put_Line ("ODBC ver: " & Get_ODBC_Ver (Connection));
      Put_Line
      (  "OJ capabilities: "
      &  Image (Get_OJ_Capabilities (Connection))
      );
      Put_Line
      (  "Order by columns in select: "
      &  Boolean'Image (Get_Order_By_Columns_In_Select (Connection))
      );
      Put_Line
      (  "Param array row count: "
      &  Image (Get_Param_Array_Row_Count (Connection))
      );
      Put_Line
      (  "Param array selects: "
      &  Image (Get_Param_Array_Selects (Connection))
      );
      Put_Line ("Procedure term: " & Get_Procedure_Term (Connection));
      Put_Line
      (  "Procedures: "
      &  Boolean'Image (Get_Procedures (Connection))
      );
      Put_Line
      (  "Pos operations: "
      &  Image (Get_Pos_Operations (Connection))
      );
      Put_Line
      (  "Quoted identifier case: "
      &  Image (Get_Quoted_Identifier_Case (Connection))
      );
      Put_Line
      (  "Row updates: "
      &  Boolean'Image (Get_Row_Updates (Connection))
      );
      Put_Line ("Schema term: " & Get_Schema_Term (Connection));
      Put_Line
      (  "Schema usage: "
      &  Image (Get_Schema_Usage (Connection))
      );
      Put_Line
      (  "Scroll options: "
      &  Image (Get_Scroll_Options (Connection))
      );
      Put_Line
      (  "Search pattern escape: "
      &  Get_Search_Pattern_Escape (Connection)
      );
      Put_Line ("Server name: " & Get_Server_Name (Connection));
      Put_Line
      (  "Special characters: "
      &  Get_Special_Characters (Connection)
      );
      Put_Line
      (  "SQL Conformance: "
      &  Image (Get_SQL_Conformance (Connection))
      );
      Put_Line
      (  "SQL92 datetime functions: "
      &  Image (Get_SQL92_Datetime_Functions (Connection))
      );
      Put_Line
      (  "SQL92 foreign key delete rule: "
      &  Image (Get_SQL92_Foreign_Key_Delete_Rule (Connection))
      );
      Put_Line
      (  "SQL92 foreign key update rule: "
      &  Image (Get_SQL92_Foreign_Key_Update_Rule (Connection))
      );
      Put_Line ("SQL92 grant: " & Image (Get_SQL92_Grant (Connection)));
      Put_Line
      (  "SQL92 numeric value function: "
      &  Image (Get_SQL92_Numeric_Value_Functions (Connection))
      );
      Put_Line
      (  "SQL92 predicates: "
      &  Image (Get_SQL92_Predicates (Connection))
      );
      Put_Line
      (  "SQL92 relational join operations: "
      &  Image (Get_SQL92_Relational_Join_Operations (Connection))
      );
      Put_Line
      (  "SQL92 revoke: "
      &  Image (Get_SQL92_Revoke (Connection))
      );
      Put_Line
      (  "SQL92 row value construction: "
      &  Image (Get_SQL92_Row_Value_Constructor (Connection))
      );
      Put_Line
      (  "SQL92 string functions: "
      &  Image (Get_SQL92_String_Functions (Connection))
      );
      Put_Line
      (  "SQL92 value expression: "
      &  Image (Get_SQL92_Value_Expressions (Connection))
      );
      Put_Line
      (  "Standard CLI conformance: "
      &  Image (Get_Standard_CLI_Conformance (Connection))
      );
      Put_Line
      (  "Static cursor attributes 1: "
      &  Image (SQL_CA1'(Get_Static_Cursor_Attributes (Connection)))
      );
      Put_Line
      (  "Static cursor attributes 2: "
      &  Image (SQL_CA2'(Get_Static_Cursor_Attributes (Connection)))
      );
      Put_Line ("Subqueries: " & Image (Get_Subqueries (Connection)));
      Put_Line
      (  "String functions: "
      &  Image (Get_String_Functions (Connection))
      );
      Put_Line
      (  "System functions: "
      &  Image (Get_System_Functions (Connection))
      );
      Put_Line ("Table term: " & Get_Table_Term (Connection));
      Put_Line
      (  "Timedate add intervals: "
      &  Image (Get_Timedate_Add_Intervals (Connection))
      );
      Put_Line
      (  "Timedate diff intervals: "
      &  Image (Get_Timedate_Diff_Intervals (Connection))
      );
      Put_Line
      (  "Timedate functions: "
      &  Image (Get_Timedate_Functions (Connection))
      );
      Put_Line
      (  "TXN capable: "
      &  Image (Get_TXN_Capable (Connection))
      );
      Put_Line
      (  "TXN isolation option: "
      &  Image (Get_TXN_Isolation_Option (Connection))
      );
      Put_Line ("Union: " & Image (Get_Union (Connection)));
      Put_Line
      (  "User name [ASCII]: "
      &  Get_User_Name (Connection)
      );
      Put_Line
      (  "        [Unicode]: "
      &  To_UTF8 (Wide_String'(Get_User_Name (Connection)))
      );
      Put_Line
      (  "XOpen CLI year [ASCII]: "
      &  Get_XOpen_CLI_Year (Connection)
      );
      Put_Line
      (  "             [Unicode]: "
      &  To_UTF8 (Wide_String'(Get_XOpen_CLI_Year (Connection)))
      );
      Put_Line ("------------ connection diag record fields:");
      begin
         Put_Line
         (  "Class origin field: " & Get_Class_Origin (Connection, 1)
         );
      exception
         when End_Error =>
            Put_Line ("*** Class origin field: No data");
      end;
      begin
         Put_Line
         (  "Column number:"
         &  SQLINTEGER'Image (Get_Column_Number (Connection, 1))
         );
      exception
         when End_Error =>
            Put_Line ("*** Column number: No data");
      end;
      begin
         Put_Line
         (  "Connection name: " & Get_Connection_Name (Connection, 1)
         );
      exception
         when End_Error =>
            Put_Line ("*** Connection name: No data");
      end;
      begin
         Put_Line
         (  "Message text [ASCII]: " & Get_Message_Text (Connection, 1)
         );
         Put_Line
         (  "           [Unicode]: "
         &  To_UTF8 (Wide_String'(Get_Message_Text (Connection, 1)))
         );
      exception
         when End_Error =>
            Put_Line ("*** Message text: No data");
      end;
      begin
         Put_Line
         (  "Native:" & SQLINTEGER'Image (Get_Native (Connection, 1))
         );
      exception
         when End_Error =>
            Put_Line ("*** Native: No data");
      end;
      begin
         Put_Line
         (  "Number:"
         &  SQLINTEGER'Image (Get_Number (Connection, 1))
         );
      exception
         when End_Error =>
            Put_Line ("*** Number: No data");
      end;
      begin
         Put_Line
         (  "Returncode:"
         &  SQLRETURN'Image (Get_Returncode (Connection, 1))
         );
      exception
         when End_Error =>
            Put_Line ("*** Returncode: No data");
      end;
      begin
         Put_Line
         (  "Row number:"
         &  SQLLEN'Image (Get_Row_Number (Connection, 1))
         );
      exception
         when End_Error =>
            Put_Line ("*** Row number: No data");
      end;
      begin
         Put_Line
         (  "Server name [ASCII]:" & Get_Server_Name (Connection, 1)
         );
         Put_Line
         (  "          [Unicode]:"
         &  To_UTF8 (Wide_String'(Get_Server_Name (Connection, 1)))
         );
      exception
         when End_Error =>
            Put_Line ("*** Server name: No data");
      end;
      begin
         Put_Line ("SQLSTATE:" & Get_SQLSTATE (Connection, 1));
      exception
         when End_Error =>
            Put_Line ("*** SQLSTATE: No data");
      end;
      begin
         Put_Line
         (  "Subclass oigin:"
         &  Get_Subclass_Origin (Connection, 1)
         );
      exception
         when End_Error =>
            Put_Line ("*** Subclass oigin: No data");
      end;
      Put_Line ("------------ supported ODBC functions:");
      declare
         type SQL_API_Array is array (Positive range <>) of SQL_API;
         List : SQL_API_Array :=
                (  SQL_API_SQLALLOCCONNECT,   SQL_API_SQLALLOCENV,
                   SQL_API_SQLALLOCSTMT,      SQL_API_SQLBINDCOL,
                   SQL_API_SQLCANCEL,         SQL_API_SQLCOLATTRIBUTE,
                   SQL_API_SQLCONNECT,        SQL_API_SQLDESCRIBECOL,
                   SQL_API_SQLDISCONNECT,     SQL_API_SQLERROR,
                   SQL_API_SQLEXECDIRECT,     SQL_API_SQLEXECUTE,
                   SQL_API_SQLFETCH,          SQL_API_SQLFREECONNECT,
                   SQL_API_SQLFREEENV,        SQL_API_SQLFREESTMT,
                   SQL_API_SQLGETCURSORNAME,  SQL_API_SQLNUMRESULTCOLS,
                   SQL_API_SQLPREPARE,        SQL_API_SQLROWCOUNT,
                   SQL_API_SQLSETCURSORNAME,  SQL_API_SQLSETPARAM,
                   SQL_API_SQLTRANSACT,       SQL_API_SQLBULKOPERATIONS,
                   SQL_API_SQLCOLUMNS,        SQL_API_SQLDRIVERCONNECT,
                   SQL_API_SQLGETDATA,        SQL_API_SQLGETFUNCTIONS,
                   SQL_API_SQLGETINFO,        SQL_API_SQLGETSTMTOPTION,
                   SQL_API_SQLGETTYPEINFO,    SQL_API_SQLPARAMDATA,
                   SQL_API_SQLPUTDATA,        SQL_API_SQLSETSTMTOPTION,
                   SQL_API_SQLSPECIALCOLUMNS, SQL_API_SQLSTATISTICS,
                   SQL_API_SQLTABLES,         SQL_API_SQLDATASOURCES,
                   SQL_API_SQLDESCRIBEPARAM,  SQL_API_SQLEXTENDEDFETCH,
                   SQL_API_SQLFOREIGNKEYS,    SQL_API_SQLMORERESULTS,
                   SQL_API_SQLNATIVESQL,      SQL_API_SQLNUMPARAMS,
                   SQL_API_SQLPARAMOPTIONS,   SQL_API_SQLPRIMARYKEYS,
                   SQL_API_SQLPROCEDURES,     SQL_API_SQLSETPOS,
                   SQL_API_SQLDRIVERS,        SQL_API_SQLBINDPARAMETER,
                   SQL_API_SQLALLOCHANDLESTD, SQL_API_SQLALLOCHANDLE,
                   SQL_API_SQLBINDPARAM,      SQL_API_SQLCLOSECURSOR,
                   SQL_API_SQLCOPYDESC,       SQL_API_SQLENDTRAN,
                   SQL_API_SQLFREEHANDLE,     SQL_API_SQLGETCONNECTATTR,
                   SQL_API_SQLGETDESCFIELD,   SQL_API_SQLGETDESCREC,
                   SQL_API_SQLGETDIAGFIELD,   SQL_API_SQLGETDIAGREC,
                   SQL_API_SQLGETENVATTR,     SQL_API_SQLGETSTMTATTR,
                   SQL_API_SQLSETCONNECTATTR, SQL_API_SQLSETDESCFIELD,
                   SQL_API_SQLSETDESCREC,     SQL_API_SQLSETENVATTR,
                   SQL_API_SQLSETSTMTATTR,    SQL_API_SQLFETCHSCROLL,
                   SQL_API_SQLTABLEPRIVILEGES,
                   SQL_API_SQLPROCEDURECOLUMNS,
                   SQL_API_SQLSETCONNECTOPTION,
                   SQL_API_SQLBROWSECONNECT,
                   SQL_API_SQLCOLUMNPRIVILEGES,
                   SQL_API_SQLSETSCROLLOPTIONS
                );
         subtype Functions_List is Integer range List'Range;
         function Image is
            new Enumeration_Image
                (  Functions_List,
                   (  "SQLALLOCCONNECT SQLALLOCENV SQLALLOCSTMT "
                   &  "SQLBINDCOL SQLCANCEL SQLCOLATTRIBUTE "
                   &  "SQLCONNECT SQLDESCRIBECOL SQLDISCONNECT "
                   &  "SQLERROR SQLEXECDIRECT SQLEXECUTE SQLFETCH "
                   &  "SQLFREECONNECT SQLFREEENV SQLFREESTMT "
                   &  "SQLGETCURSORNAME SQLNUMRESULTCOLS SQLPREPARE "
                   &  "SQLROWCOUNT SQLSETCURSORNAME SQLSETPARAM "
                   &  "SQLTRANSACT SQLBULKOPERATIONS SQLCOLUMNS "
                   &  "SQLDRIVERCONNECT SQLGETDATA SQLGETFUNCTIONS "
                   &  "SQLGETINFO SQLGETSTMTOPTION SQLGETTYPEINFO "
                   &  "SQLPARAMDATA SQLPUTDATA SQLSETSTMTOPTION "
                   &  "SQLSPECIALCOLUMNS SQLSTATISTICS SQLTABLES "
                   &  "SQLDATASOURCES SQLDESCRIBEPARAM "
                   &  "SQLEXTENDEDFETCH SQLFOREIGNKEYS SQLMORERESULTS "
                   &  "SQLNATIVESQL SQLNUMPARAMS SQLPARAMOPTIONS "
                   &  "SQLPRIMARYKEYS SQLPROCEDURES SQLSETPOS "
                   &  "SQLDRIVERS SQLBINDPARAMETER SQLALLOCHANDLESTD "
                   &  "SQLALLOCHANDLE SQLBINDPARAM SQLCLOSECURSOR "
                   &  "SQLCOPYDESC SQLENDTRAN SQLFREEHANDLE "
                   &  "SQLGETCONNECTATTR SQLGETDESCFIELD SQLGETDESCREC "
                   &  "SQLGETDIAGFIELD SQLGETDIAGREC SQLGETENVATTR "
                   &  "SQLGETSTMTATTR SQLSETCONNECTATTR "
                   &  "SQLSETDESCFIELD SQLSETDESCREC SQLSETENVATTR "
                   &  "SQLSETSTMTATTR SQLFETCHSCROLL "
                   &  "SQLTABLEPRIVILEGES SQLPROCEDURECOLUMNS "
                   &  "SQLSETCONNECTOPTION SQLBROWSECONNECT "
                   &  "SQLCOLUMNPRIVILEGES SQLSETSCROLLOPTIONS"
                )  );
      begin
         for Index in List'Range loop
            if Get_Functions (Connection, List (Index)) then
               Put_Line ("  " & Image (Index));
            end if;
         end loop;
      end;
      declare
         Command : aliased ODBC_Command (Connection'Access);
      begin
         Put_Line
         (  "Async enable: "
         &  Boolean'Image (Get_Async_Enable (Command))
         );
         Put_Line
         (  "Concurrency: "
         &  Image (Get_Concurrency (Command))
         );
         Put_Line
         (  "Cursor scrollable: "
         &  Boolean'Image (Get_Cursor_Scrollable (Command))
         );
         Put_Line
         (  "Cursor sensitivity: "
         &  Image (Get_Cursor_Sensitivity (Command))
         );
         Put_Line
         (  "Cursor type: "
         &  Image (Get_Cursor_Type (Command))
         );
         Put_Line
         (  "Enable Auto IPD: "
         &  Boolean'Image (Get_Enable_Auto_IPD (Command))
         );
         Put_Line
         (  "Keyset size:"
         &  SQLULEN'Image (Get_Keyset_Size (Command))
         );
         begin
            Put_Line
            (  "Max length:"
            &  SQLULEN'Image (Get_Max_Length (Command))
            );
         exception
            when Use_Error =>
               Put_Line ("*** Max length: Not supported");
         end;
         begin
            Put_Line
            (  "Max rows:"
            &  SQLULEN'Image (Get_Max_Rows (Command))
            );
         exception
            when Use_Error =>
               Put_Line ("*** Max rows: Not supported");
         end;
         begin
            Put_Line
            (  "Metadata ID: "
            &  Boolean'Image (Get_Metadata_ID (Command))
            );
         exception
            when Use_Error =>
               Put_Line ("*** Metadata ID: Not supported");
         end;
         Put_Line ("Noscan: " & Boolean'Image (Get_Noscan (Command)));
         begin
            Put_Line
            (  "Query timeout:"
            &  Duration'Image (Get_Query_Timeout (Command))
            );
         exception
            when Use_Error =>
               Put_Line ("*** Query timeout: Not supported");
         end;
         Put_Line
         (  "Retrieve data: "
         &  Boolean'Image (Get_Retrieve_Data (Command))
         );
         Put_Line
         (  "Row array size:"
         &  SQLULEN'Image (Get_Row_Array_Size (Command))
         );
      end;
      Set_Autocommit (Connection, False);
      Put_Line ("Listing data types:");
      declare
         Command : aliased ODBC_Command (Connection'Access);
         procedure Query (Data_Type : SQL_DATA_TYPE) is
         begin
            declare
               Info : Type_Info :=
                      Get_Type_Info (Command'Access, Data_Type);
            begin
               Put_Line (Info.Type_Name);
               Put_Line
               (  "  Data type: "
               &  Image (Info.Data_Type)
               &  " ="
               &  SQL_DATA_TYPE'Image (Data_Type)
               );
               Put_Line
               (  "  Column size:"
               &  SQLINTEGER'Image (Info.Column_Size)
               );
               Put_Line ("  Literal prefix: " &  Info.Literal_Prefix);
               Put_Line ("  Literal suffix: " & Info.Literal_Suffix);
               Put_Line
               (  "  Create parameters: "
               &  Info.Create_Parameters
               );
               Put_Line
               (  "  Nullable: "
               &  Boolean'Image (Info.Nullable)
               );
               Put_Line
               (  "  Case sensitive: "
               &  Boolean'Image (Info.Case_Sensitive)
               );
               Put_Line ("  Searchable: " &  Image (Info.Searchable));
               Put_Line
               (  "  Unsigned attribute: "
               &  Boolean'Image (Info.Unsigned_Attribute)
               );
               Put_Line
               (  "  Fixed prec scale: "
               &  Boolean'Image (Info.Fixed_Prec_Scale)
               );
               Put_Line
               (  "  Auto unique value: "
               &  Boolean'Image (Info.Auto_Unique_Value)
               );
               Put_Line ("  Local name: " & Info.Local_Name);
            end;
         exception
            when Constraint_Error =>
               Put_Line
               (  "Data type "
               &  Image (Data_Type)
               &  " ="
               &  SQL_DATA_TYPE'Image (Data_Type)
               &  " not supported"
               );
         end Query;
      begin
         Query (SQL_UTINYINT);
         Query (SQL_UBIGINT);
         Query (SQL_STINYINT);
         Query (SQL_SBIGINT);
         Query (SQL_ULONG);
         Query (SQL_USHORT);
         Query (SQL_SLONG);
         Query (SQL_SSHORT);
         Query (SQL_GUID);
         Query (SQL_WLONGVARCHAR);
         Query (SQL_WVARCHAR);
         Query (SQL_WCHAR);
         Query (SQL_BIT);
         Query (SQL_TINYINT);
         Query (SQL_BIGINT);
         Query (SQL_LONGVARBINARY);
         Query (SQL_VARBINARY);
         Query (SQL_BINARY);
         Query (SQL_LONGVARCHAR);
         Query (SQL_CHAR);
         Query (SQL_NUMERIC);
         Query (SQL_DECIMAL);
         Query (SQL_INTEGER);
         Query (SQL_SMALLINT);
         Query (SQL_FLOAT);
         Query (SQL_REAL);
         Query (SQL_DOUBLE);
         Query (SQL_DATETIME);
         Query (SQL_DATE);
         Query (SQL_TIME);
         Query (SQL_TIMESTAMP);
         Query (SQL_VARCHAR);
         Query (SQL_TYPE_DATE);
         Query (SQL_TYPE_TIME);
         Query (SQL_TYPE_TIMESTAMP);
         Query (SQL_DEFAULT);
         Query (SQL_INTERVAL_YEAR);
         Query (SQL_INTERVAL_MONTH);
         Query (SQL_INTERVAL_DAY);
         Query (SQL_INTERVAL_HOUR);
         Query (SQL_INTERVAL_MINUTE);
         Query (SQL_INTERVAL_SECOND);
         Query (SQL_INTERVAL_YEAR_TO_MONTH);
         Query (SQL_INTERVAL_DAY_TO_HOUR);
         Query (SQL_INTERVAL_DAY_TO_MINUTE);
         Query (SQL_INTERVAL_DAY_TO_SECOND);
         Query (SQL_INTERVAL_HOUR_TO_MINUTE);
         Query (SQL_INTERVAL_HOUR_TO_SECOND);
         Query (SQL_INTERVAL_MINUTE_TO_SECOND);
      end;
      Put_Line ("Listing existing tables:");
      declare
         Command : aliased ODBC_Command (Connection'Access);
      begin
         Get_Tables (Command);
         loop
            Fetch (Command);
            Put_Line
            (  "   Row number:"
            &  SQLULEN'Image (Get_Row_Number (Command))
            );
            Put_Line (Get_Data (Command'Access, 3, On_No_Result));
         end loop;
      exception
        when End_Error =>
           Put_Line ("   <<<< finished listing existing tables");
      end;
      Put_Line ("Creating tables:");
      declare
         Command : aliased ODBC_Command (Connection'Access);
         X1      : aliased SQLINTEGER_Parameter;
         X2      : aliased String_Parameter (100);
         X3      : aliased SQLDOUBLE_Parameter;
         X4      : aliased SQL_TIMESTAMP_STRUCT_Parameter;
--           X5      : aliased SQLBIGINT_Parameter;
      begin
         if Table_Exists (Command'Access, String'("test_table")) then
            Drop (Command, String'("test_table"));
         end if;
         if Table_Exists (Command'Access, String'("test_table")) then
            Raise_Exception
            (  Data_Error'Identity,
               "Tabble test_table still exists"
            );
         end if;
         declare
            function Get_Text return String is
               Info : Type_Info := Get_Type_Info
                                   (  Command'Access,
                                      SQL_LONGVARCHAR
                                   );
            begin
               if Info.Create_Parameters = "" then
                  return Info.Type_Name;
               else
                  return Info.Type_Name & "(100)";
               end if;
            end Get_Text;
            type Data_Type_Array is
               array (Positive range <>) of SQL_DATA_TYPE;
            function Query_Type
                     (  List : Data_Type_Array;
                        Text : String
                     )  return String is
            begin
               for Index in List'Range loop
                  begin
                     return Get_Type_Info
                            (  Command'Access,
                               List (Index)
                            ) .Type_Name;
                  exception
                     when Constraint_Error => -- Not supported
                        null;
                  end;
               end loop;
               Raise_Exception
               (  Data_Error'Identity,
                  "The data base does not support " & Text
               );
            end Query_Type;
            Int   : constant String := -- Name used for integer
                       Get_Type_Info
                       (  Command'Access, SQL_INTEGER
                       ) .Type_Name;
            Text  : constant String := Get_Text; -- LONGVARCHAR
            Real  : constant String :=           -- DOUBLE
                       Get_Type_Info
                       (  Command'Access, SQL_DOUBLE
                       ) .Type_Name;
            Stamp : constant String :=           -- timestamps
                       Get_Type_Info
                       (  Command'Access, SQL_TYPE_TIMESTAMP
                       ) .Type_Name;
--              Big   : constant String := -- Name used for bigint
--                         Query_Type
--                         (  (  SQL_UBIGINT, SQL_BIGINT, SQL_ULONG,
--                               SQL_INTEGER
--                            ),
--                            "large integers"
--                         );
            Create_Table : constant String :=
                       (  "CREATE TABLE test_table ("
                       &  "x1 " & Int & ","
                       &  "x2 " & Text & ","
                       &  "x3 " & Real & ","
                       &  "x4 " & Stamp
--                         &  "x5 " & Big
                       &  ")"
                       );
         begin
            Put_Line ("Creating table:" & Create_Table);
            Execute (Command, Create_Table);
         end;
         Put_Line ("Filling the table");
         Prepare
         (  Command,
            String'("INSERT INTO test_table VALUES (?,?,?,?)")
         );
         Put_Line
         (  "Parameters of the prepared statement:"
         &  Natural'Image (Num_Params (Command))
         );
         begin
            for No in 1..Num_Params (Command) loop
               declare
                  Info : Param_Description :=
                         Describe_Param (Command, No);
               begin
                  Put_Line
                  (  "  "
                  &  Integer'Image (No)
                  &  " Type = "
                  &  Image (Info.Data_Type)
                  &  " Size ="
                  &  SQLULEN'Image (Info.Parameter_Size)
                  &  " Digits ="
                  &  Natural'Image (Info.Decimal_Digits)
                  &  " Nullable ="
                  &  Image (Info.Nullable)
                  );
               end;
            end loop;
         exception
            when Use_Error =>
               Put_Line
               (  "Driver does not support parameters description"
               );
         end;
         Bind_Parameter (Command, 1, X1'Access);
         Bind_Parameter (Command, 2, X2'Access);
         Bind_Parameter (Command, 3, X3'Access);
         Bind_Parameter (Command, 4, X4'Access);
--           Bind_Parameter (Command, 5, X5'Access);
         for Index in SQLINTEGER range -10..10 loop
            X1.Value := Index;
            Set (X2, "value =" & SQLINTEGER'Image (X1.Value));
            X3.Value := SQLDOUBLE (X1.Value) * 123.0;
            Set (X4, Clock);
--              X5.Value := SQLBIGINT (Index);
            Execute (Command);
         end loop;
      end;
      Put_Line ("Reading data back from the table");
      declare
         Command : aliased ODBC_Command (Connection'Access);
      begin
         Execute (Command, String'("SELECT * FROM test_table"));
         Put_Line ("Describing colimns:");
         for Column in 1..Num_Result_Cols (Command) loop
            declare
               Description : Column_Description :=
                             Describe_Col (Command, Column);
            begin
               Put_Line
               (  "  "
               &  Integer'Image (Column)
               &  " "
               &  Description.Column_Name
               &  ", "
               &  Image (Description.Data_Type)
               &  ", Size"
               &  SQLULEN'Image (Description.Column_Size)
               &  ","
               &  Natural'Image (Description.Decimal_Digits)
               &  " digits, "
               &  Image (Description.Nullable)
               );
            end;
         end loop;
         loop
            Fetch (Command);
            Put
            (  "x1 ="
            &  SQLINTEGER'Image (Get_Data (Command'Access, 1, Never))
            );
            Put
            (  ", x2 = '"
            &  Get_Data (Command'Access, 2, Never)
            );
            Put
            (  "', x3 ="
            &  SQLDOUBLE'Image (Get_Data (Command'Access, 3, Never))
            );
            Put
            (  ", x4 ="
            &  Duration'Image
               (  Clock
               -  Time'(Get_Data (Command'Access, 4, Never))
               )
            &  "s ago"
            );
--              Put
--              (  "x5 ="
--              &  SQLBIGINT'Image (Get_Data (Command'Access, 5, Never))
--              );
            New_Line;
         end loop;
      exception
         when End_Error => -- No more rows
            null;
      end;
      Put_Line ("Reading data back from the table using bound results");
      declare
         Command : aliased ODBC_Command (Connection'Access);
         X1 : aliased SQLINTEGER;
         X3 : aliased SQLDOUBLE;
         X4 : aliased SQL_TIMESTAMP_STRUCT;
--           X5 : aliased SQLBIGINT;
      begin
         Execute (Command, String'("SELECT * FROM test_table"));
         Bind_Result (Command, 1, X1'Access);
         Bind_Result (Command, 3, X3'Access);
         Bind_Result (Command, 4, X4'Access);
--           Bind_Result (Command, 5, X5'Access);
         loop
            Fetch (Command);
            Put ("x1 =" & SQLINTEGER'Image (X1));
            Put (", x2 = '" & Get_Data (Command'Access, 2, Never));
            Put ("', x3 =" & SQLDOUBLE'Image (X3));
            Put
            (  ", x4 ="
            &  Duration'Image (Clock - To_Time (X4))
            &  "s ago"
            );
--              Put ("', x5 =" & SQLBIGINT'Image (X5));
            New_Line;
         end loop;
      exception
         when End_Error => -- No more rows
            null;
      end;
      Put_Line ("Testing Get_Data with a short buffer");
      declare
         Command : aliased ODBC_Command (Connection'Access);
         X1 : aliased SQLINTEGER;
         X2 : aliased String_Parameter (100);
         X3 : aliased SQLDOUBLE;
         X4 : aliased SQL_TIMESTAMP_STRUCT;
      begin
         Prepare
         (  Command,
            String'("SELECT * FROM test_table WHERE x2=?")
         );
         Bind_Parameter (Command, 1, X2'Access);
         Set (X2, "value = 7");
         Bind_Result (Command, 1, X1'Access);
         Bind_Result (Command, 3, X3'Access);
         Bind_Result (Command, 4, X4'Access);
         Execute (Command);
         loop
            Fetch (Command);
            Put ("X1 =" & SQLINTEGER'Image (X1));
            Put
            (  ", X2 = '"
            &  Get_Data (Command'Access, 2, Never, Block_Size => 2)
            );
            Put ("', X3 =" & SQLDOUBLE'Image (X3));
            Put
            (  ", X4 ="
            &  Duration'Image (Clock - To_Time (X4))
            &  "s ago"
            );
            New_Line;
         end loop;
      exception
         when End_Error => -- No more rows
            null;
      end;
      Put_Line ("Testing stream Get_Data");
      declare
         Command : aliased ODBC_Command (Connection'Access);
         X1 : aliased SQLINTEGER;
         X2 : aliased String_Parameter (100);
         X3 : aliased SQLDOUBLE;
         X4 : aliased SQL_TIMESTAMP_STRUCT;
         Value : String_Stream (100);
      begin
         Prepare
         (  Command,
            String'("SELECT * FROM test_table WHERE x2=?")
         );
         Bind_Parameter (Command, 1, X2'Access);
         Set (X2, "value = 7");
         Bind_Result (Command, 1, X1'Access);
         Bind_Result (Command, 3, X3'Access);
         Bind_Result (Command, 4, X4'Access);
         Execute (Command);
         loop
            Fetch (Command);
            Put ("X1 =" & SQLINTEGER'Image (X1));
            Get_Data (Command, Value, 2, Never, Block_Size => 2);
            Put (", X2 = '" & Get (Value));
            Put ("', X3 =" & SQLDOUBLE'Image (X3));
            Put
            (  ", X4 ="
            &  Duration'Image (Clock - To_Time (X4))
            &  "s ago"
            );
            Rewind (Value);
            New_Line;
         end loop;
      exception
         when End_Error => -- No more rows
            null;
      end;
      Put_Line ("Testing in-place string Get_Data");
      declare
         Command : aliased ODBC_Command (Connection'Access);
         X1 : aliased SQLINTEGER_Parameter := Create (7);
         X2 : String (1..100);
         Pointer : Integer;
      begin
         Prepare
         (  Command,
            String'("SELECT * FROM test_table WHERE x1=?")
         );
         Bind_Parameter (Command, 1, X1'Access);
         Execute (Command);
         loop
            Fetch (Command);
            Pointer := X2'First;
            Get_Data (Command, X2, Pointer, 2, Never);
            Put_Line ("X2 = '" & X2 (1..Pointer - 1) & ''');
         end loop;
      exception
         when End_Error => -- No more rows
            null;
      end;
      Put_Line ("Testing null parameters");
      declare
         Command : aliased ODBC_Command (Connection'Access);
      begin
         Prepare
         (  Command,
            String'("INSERT INTO test_table VALUES (666,?,?,?)")
         );
         Bind_Null (Command, 1);
         Bind_Null (Command, 2);
         Bind_Null (Command, 3);
         Execute (Command);
         Prepare
         (  Command,
            String'("SELECT * FROM test_table WHERE x1=666")
         );
         Execute (Command);
         Put_Line ("------------ command diag record fields:");
         begin
            Put_Line
            (  "Class origin field: " & Get_Class_Origin (Command, 1)
            );
         exception
            when End_Error =>
               Put_Line ("*** Class origin field: No data");
         end;
         begin
            Put_Line
            (  "Column number:"
            &  SQLINTEGER'Image (Get_Column_Number (Command, 1))
            );
         exception
            when End_Error =>
               Put_Line ("*** Column number: No data");
         end;
         begin
            Put_Line
            (  "Connection name: " & Get_Connection_Name (Command, 1)
            );
         exception
            when End_Error =>
               Put_Line ("*** Connection name: No data");
         end;
         begin
            Put_Line
            (  "Cursor row count: "
            &  SQLLEN'Image (Get_Cursor_Row_Count (Command, 1))
            );
         exception
            when End_Error =>
               Put_Line ("*** Cursor row count: No data");
         end;
         begin
            Put_Line
            (  "Dynamic function [ASCII]: "
            &  Get_Dynamic_Function (Command, 1)
            );
            Put_Line
            (  "               [Unicode]: "
            &  To_UTF8 (Wide_String'(Get_Dynamic_Function (Command, 1)))
            );
         exception
            when End_Error =>
               Put_Line ("*** Dynamic function: No data");
         end;
         begin
            Put_Line
            (  "Dynamic function code:"
            &  SQLINTEGER'Image (Get_Dynamic_Function_Code (Command, 1))
            );
         exception
            when End_Error =>
               Put_Line ("*** Dynamic function code: No data");
         end;
         begin
            Put_Line
            (  "Message text [ASCII]: " & Get_Message_Text (Command, 1)
            );
            Put_Line
            (  "           [Unicode]: "
            &  To_UTF8 (Wide_String'(Get_Message_Text (Command, 1)))
            );
         exception
            when End_Error =>
               Put_Line ("*** Message text: No data");
         end;
         begin
            Put_Line
            (  "Native:" & SQLINTEGER'Image (Get_Native (Command, 1))
            );
         exception
            when End_Error =>
               Put_Line ("*** Native: No data");
         end;
         begin
            Put_Line
            (  "Number:"
            &  SQLINTEGER'Image (Get_Number (Command, 1))
            );
         exception
            when End_Error =>
               Put_Line ("*** Number: No data");
         end;
         begin
            Put_Line
            (  "Returncode:"
            &  SQLRETURN'Image (Get_Returncode (Command, 1))
            );
         exception
            when End_Error =>
               Put_Line ("*** Returncode: No data");
         end;
         begin
            Put_Line
            (  "Row number:"
            &  SQLLEN'Image (Get_Row_Count (Command, 1))
            );
         exception
            when End_Error =>
               Put_Line ("*** Row count: No data");
         end;
         begin
            Put_Line
            (  "Server name [ASCII]:" & Get_Server_Name (Command, 1)
            );
            Put_Line
            (  "          [Unicode]:"
            &  To_UTF8 (Wide_String'(Get_Server_Name (Command, 1)))
            );
         exception
            when End_Error =>
               Put_Line ("*** Server name: No data");
         end;
         begin
            Put_Line ("SQLSTATE:" & Get_SQLSTATE (Command, 1));
         exception
            when End_Error =>
               Put_Line ("*** SQLSTATE: No data");
         end;
         begin
            Put_Line
            (  "Subclass oigin:"
            &  Get_Subclass_Origin (Command, 1)
            );
         exception
            when End_Error =>
               Put_Line ("*** Subclass oigin: No data");
         end;
         Fetch (Command);
         begin
            if "" /= Get_Data (Command'Access, 2, Never, False) then
               Raise_Exception (Data_Error'Identity, "X2 is not NULL");
            end if;
         exception
            when End_Error =>
               null;
         end;
      end;
      Put_Line ("Listinng DSNs:");
      declare
         procedure Put (DSN : DSN_Description) is
         begin
            Put_Line ("      " & DSN.Name & "    " & DSN.Description);
         end;
      begin
         begin
            Put_Line ("   All DSNs:");
            Put (Get_First_DSN (Environment, Any_DSN));
            loop
               Put (Get_Next_DSN (Environment));
            end loop;
         exception
            when End_Error => -- No more DSNs
               null;
         end;
         begin
            Put_Line ("   User DSNs:");
            Put (Get_First_DSN (Environment, User_DSN));
            loop
               Put (Get_Next_DSN (Environment));
            end loop;
         exception
            when End_Error => -- No more DSNs
               null;
         end;
         begin
            Put_Line ("   System DSNs:");
            Put (Get_First_DSN (Environment, System_DSN));
            loop
               Put (Get_Next_DSN (Environment));
            end loop;
         exception
            when End_Error => -- No more DSNs
               null;
         end;
      end;
      End_Transaction (Connection);
   end;
exception
   when Error : others =>
      Put_Line ("Fatal error: " & Exception_Information (Error));
end Test_ODBC_Bindings;
