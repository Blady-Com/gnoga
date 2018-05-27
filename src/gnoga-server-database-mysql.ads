------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . S E R V E R . D A T A B A S E . M Y S Q L           --
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

--  This binding is to MySQL

--  You will need to add linker options for MySQL. This can be done as part
--  of gpr files, command line or even something like:
--   pragma Linker_Options ("-lmysqlclient");

with Interfaces.C;

package Gnoga.Server.Database.MySQL is
   type MySQL_ID is access all Integer;

   type Connection is new Gnoga.Server.Database.Connection with private;
   type Connection_Access is access all Connection'Class;

   procedure Connect (C        : in out Connection;
                      Database : String;
                      Host     : String;
                      User     : String;
                      Password : String  := "";
                      Port     : Integer := 3306);
   --  Initialize connection to MySQL Server

   function Connect (Database : String;
                     Host     : String;
                     User     : String;
                     Password : String := "";
                     Port     : Integer := 3306)
                     return Connection_Access;
   --  Create and initialize a connection object

   overriding
   procedure Disconnect (C : in out Connection);
   --  Disconnect from server

   overriding
   procedure Execute_Query (C : in out Connection; SQL : String);
   --  Execute a SQL Query with no result set

   overriding
   function Execute_Update (C : in out Connection; SQL : String)
                            return Natural;
   --  Executes a SQL Query and returns the number of affected rows

   overriding
   function Affected_Rows (C : Connection) return Natural;
   --  Returns the number of rows affected by and Execute_Query

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
   --  returns "id INTEGER PRIMARY KEY AUTO_INCREMENT" the proper ID_Field
   --  creation string for SQLLite

   type Recordset (Server_ID : MySQL_ID) is new Gnoga.Server.Database.Recordset
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
      SQL     : in     String;
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
      SQL   : in     String;
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

private
   type Connection is new Gnoga.Server.Database.Connection with
      record
         Server_ID : MySQL_ID := null;
      end record;

   subtype Field_Data is Interfaces.C.char_array
     (0 .. Interfaces.C.size_t'Last);
   type Field_Access is access all Field_Data;
   type Row_Data is array (1 .. Natural'Last) of aliased Field_Access;
   type Row_Access is access all Row_Data;
   --  Used to access ROW data

   subtype Field_Length is Interfaces.C.unsigned_long;
   type List_of_Lengths is array (1 .. Natural'Last) of aliased Field_Length;
   type List_of_Lengths_Access is access all List_of_Lengths;
   --  Used to access Result Lengths

   type Recordset (Server_ID : MySQL_ID) is
     new Gnoga.Server.Database.Recordset with
      record
         Query_ID    : MySQL_ID               := null;
         Last_Row    : Row_Access             := null;
         Row_Count   : Natural                := 0;
         Field_Count : Natural                := 0;
         Lengths     : List_of_Lengths_Access := null;
      end record;

end Gnoga.Server.Database.MySQL;
