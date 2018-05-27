------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--        G N O G A . S E R V E R . D A T A B A S E . S Q L I T E           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

--  This binding is to SQLite v3

--  You will need to add linker options for SQLite. This can be done as part
--  of gpr files, command line or even something like:
--    pragma Linker_Options ("-lsqlite3");

package Gnoga.Server.Database.SQLite is
   type SQLite_ID is access all Integer;

   type Connection is new Gnoga.Server.Database.Connection with private;
   type Connection_Access is access all Connection'Class;

   procedure Connect (C        : in out Connection;
                      Database : String);
   --  Initialize connection to SQLite database file

   function Connect (Database : String)
                     return Gnoga.Server.Database.Connection_Access;
   --  Create and initialize a connection object

   overriding
   procedure Disconnect (C : in out Connection);
   --  Disconnect from server

   overriding
   procedure Execute_Query (C : in out Connection; SQL : String);
   --  Execute an SQL Query with no result set

   overriding
   function Execute_Update (C : in out Connection; SQL : String)
                            return Natural;
   --  Executes a SQL Query and returns the number of affected rows

   overriding
   function Affected_Rows (C : Connection) return Natural;
   --  Returns the number of rows affected by an Execute_Query

   overriding
   function Insert_ID (C : Connection) return Natural;
   --  Returns the last value assigned to an auto increment field upon insert

   overriding
   function Error_Message (C : Connection) return String;
   --  Returns the last error message that has occurred on this connection

   overriding
   function List_Of_Tables (C : Connection) return Gnoga.Types.Data_Array_Type;
   --  Return an array of table names

   overriding
   function List_Fields_Of_Table (C          : Connection;
                                  Table_Name : String)
                                  return Gnoga.Types.Data_Array_Type;
   --  Return an array of field names for table

   overriding
   function Field_Descriptions (C : Connection; Table_Name : String)
                                return Field_Description_Array_Type;
   --  Return an array of Field_Description records describe the fields of
   --  a table

   overriding
   function Query (C : Connection; SQL : String)
                   return Gnoga.Server.Database.Recordset'Class;
   --  Execute query that returns Recordset

   overriding
   function ID_Field_String (C : Connection) return String;
   --  returns "id INTEGER PRIMARY KEY AUTOINCREMENT" the proper ID_Field
   --  creation string for SQLLite

   type Recordset (Server_ID : SQLite_ID) is
     new Gnoga.Server.Database.Recordset
     with private;

   overriding
   procedure Close (RS : in out Recordset);
   --  Close current recordset and free resources

   overriding
   procedure Next (RS : in out Recordset);
   --  Go to next row

   overriding
   function Next (RS : in out Recordset) return Boolean;
   --  Go to next row and return true if not End of Recordset

   overriding
   procedure Iterate
     (C       : in out Connection;
      SQL     : in String;
      Process : not null access
        procedure (RS : Gnoga.Server.Database.Recordset'Class));
   --  Iterate through all rows in the result set of the query

   overriding
   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access
        procedure (RS : Gnoga.Server.Database.Recordset'Class));
   --  Iterate through all rows in the recordset

   overriding
   procedure Iterate
     (C     : in out Connection;
      SQL   : String;
      Process : not null access procedure (Row : Gnoga.Types.Data_Map_Type));
   --  Iterate through all rows in the result set of the query

   overriding
   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access procedure (Row : Gnoga.Types.Data_Map_Type));
   --  Iterate through all rows in the recordset

   overriding
   function Number_Of_Rows (RS : Recordset) return Natural;
   --  Return number of rows in recordset
   --  This function is not available for SQLite Databases
   --  Call it will thrown an exception of Not_Implemented

   overriding
   function Number_Of_Fields (RS : Recordset) return Natural;
   --  Return number of fields in recordset

   overriding
   function Field_Name (RS : Recordset; Field_Number : Natural) return String;
   --  Return name of field

   overriding
   function Is_Null (RS : Recordset; Field_Number : Natural) return Boolean;
   overriding
   function Is_Null (RS : Recordset; Field_Name : String) return Boolean;
   --  return True if value of field is null
   --  For SQLite Is_Null is only reliable before Field_Value is taken

   overriding
   function Field_Value (RS           : Recordset;
                         Field_Number : Natural;
                         Handle_Nulls : Boolean := True)
                         return String;
   overriding
   function Field_Value (RS           : Recordset;
                         Field_Name   : String;
                         Handle_Nulls : Boolean := True)
                         return String;
   --  return value of field, if Handle_Nulls is true, Null values will
   --  return as empty Strings

   overriding
   function Field_Values (RS : Recordset) return Gnoga.Types.Data_Map_Type;
   --  return map of all values for current row, NULL values are set to
   --  an empty String

   overriding
   function Escape_String (C : Connection; S : String) return String;
   --  prepares a string for safe storage in a query

   procedure Full_Column_Names
     (C      : in out Connection;
      Active :        Boolean := True);
   function Full_Column_Names (C : in out Connection) return Boolean;
   procedure Short_Column_Names
     (C      : in out Connection;
      Active :        Boolean := True);
   function Short_Column_Names (C : in out Connection) return Boolean;
   --  The above functions determine the way SQLite assigns names to result
   --  columns of SELECT statements.
   --  Result columns are named by applying the following rules in order:
   --  * If there is an AS clause on the result, then the name of the column is
   --  the right-hand side of the AS clause.
   --  * If the result is a general expression, not a just the name of a source
   --  table column, then the name of the result is a copy of the expression
   --  text.
   --  * If the short_column_names pragma is ON, then the name of the result is
   --  the name of the source table column without the source table name
   --  prefix: COLUMN.
   --  * If both pragmas short_column_names and full_column_names are OFF then
   --  case (2) applies.
   --  * The name of the result column is a combination of the source table and
   --  source column name: TABLE.COLUMN
   --  Source: http://www.sqlite.org/pragma.html#pragma_full_column_names

   procedure Encoding (C : in out Connection; Value : String);
   --  This procedure sets the encoding that the main database will be created
   --  with if it is created by this session. The string "UTF-16" is
   --  interpreted as "UTF-16 encoding using native machine byte-ordering".
   --  It is not possible to change the text encoding of a database after
   --  it has been created and any attempt to do so will be silently ignored.
   --  Source: http://www.sqlite.org/pragma.html#pragma_full_column_names
   function Encoding (C : in out Connection) return String;
   --  If the main database has already been created, then this function
   --  returns the text encoding used by the main database, one of "UTF-8",
   --  "UTF-16le" (little-endian UTF-16 encoding) or "UTF-16be" (big-endian
   --  UTF-16 encoding). If the main database has not already been created,
   --  then the value returned is the text encoding that will be used to
   --  create the main database, if it is created by this session.
   --  Source: http://www.sqlite.org/pragma.html#pragma_full_column_names

   procedure UTF8_STring (C : in out Connection; Active : Boolean := True);
   function UTF8_STring (C : in out Connection) return Boolean;
   --  Property to treat String as UTF-8 (default) or treat String as Latin-1

private
   type Connection is new Gnoga.Server.Database.Connection with
      record
         Server_ID : aliased SQLite_ID := null;
         UTF8_STring : Boolean         := True;
         --  Consider string query parameter
         --  with UTF-8 encoding otherwise with Ada native Latin-1 encoding
      end record;

   type Recordset (Server_ID : SQLite_ID) is
     new Gnoga.Server.Database.Recordset with
      record
         Query_ID    : aliased SQLite_ID := null;
         Field_Count : Natural           := 0;
         Last_Result : Integer           := 0;
         First_Row   : Boolean           := False;
         UTF8_STring : Boolean           := True;
         --  Consider string query result
         --  with UTF-8 encoding otherwise with Ada native Latin-1 encoding
      end record;
end Gnoga.Server.Database.SQLite;
