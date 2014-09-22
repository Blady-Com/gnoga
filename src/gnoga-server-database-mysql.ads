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
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

-- This binding is to MySQL

-- You will need to add linker options for MySQL. This can be done as part
-- of gpr files, command line or even something like:
--   pragma Linker_Options ("-L/usr/lib/mysql");
--   pragma Linker_Options ("-lmysqlclient");


with Ada.Finalization;

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

   procedure Disconnect (C : in out Connection);
   --  Disconnect from server

   procedure Execute_Query (C : in out Connection; SQL : String);
   --  Execute a SQL Query with no result set

   function Affected_Rows (C : Connection) return Natural;
   --  Returns the number of rows affected by and Execute_Query

   function Insert_ID (C : Connection) return Natural;
   --  Returns the last value assigned to an auto increment field upon insert

   function Error_Message (C : Connection) return String;
   --  Returns the last error message that has occured on this connection

   function List_Of_Tables (C : Connection) return Data_Array.Vector;
   --  Return an array of table names

   function List_Fields_Of_Table (C          : Connection;
                                  Table_Name : String)
                                  return Data_Array.Vector;
   --  Return an array of field names for table

   function Field_Descriptions (C : Connection; Table_Name : String)
                                return Field_Description_Array.Vector;
   --  Return an array of Field_Description records describe the fields of
   --  a table

   function Query (C : Connection; SQL : String)
                   return Gnoga.Server.Database.Recordset'Class;
   --  Execute query that returns Recordset

   function ID_Field_String (C : Connection) return String;
   --  returns "id INTEGER PRIMARY KEY AUTO_INCREMENT" the proper ID_Field
   --  creation string for SQLLite

   type Recordset (Server_ID : MySQL_ID) is new Gnoga.Server.Database.Recordset
     with private;

   procedure Close (RS : in out Recordset);
   --  Close current recordset and free resources

   procedure Next (RS : in out Recordset);
   --  Go to next row

   function Next (RS : Recordset) return Boolean;
   --  Go to next row and return true if not End of Recordset

   procedure Iterate
     (C       : in out Connection;
      SQL     : in String;
      Process : not null access
        procedure (RS : Gnoga.Server.Database.Recordset'Class));
   --  Iterate through all rows in the result set of the query

   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access
        procedure (RS : Gnoga.Server.Database.Recordset'Class));
   --  Iterate through all rows in the recordset

   procedure Iterate
     (C     : in out Connection;
      SQL   : String;
      Process : not null access procedure (Row : Data_Maps.Map));
   --  Iterate through all rows in the result set of the query

   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access procedure (Row : Data_Maps.Map));
   --  Iterate through all rows in the recordset

   function Number_Of_Rows (RS : Recordset) return Natural;
   --  Return number of rows in recordset

   function Number_Of_Fields (RS : Recordset) return Natural;
   --  Return number of fields in recordset

   function Field_Name (RS : Recordset; Field_Number : Natural) return String;
   --  Return name of field

   function Is_Null (RS : Recordset; Field_Number : Natural) return Boolean;
   function Is_Null (RS : Recordset; Field_Name : String) return Boolean;
   --  return True if value of field is null

   function Field_Value (RS           : Recordset;
                         Field_Number : Natural;
                         Handle_Nulls : Boolean := True)
                         return String;
   function Field_Value (RS           : Recordset;
                         Field_Name   : String;
                         Handle_Nulls : Boolean := True)
                         return String;
   --  return value of field, if Handle_Nulls is true, Null values will
   --  return as empty Strings

   function Field_Values (RS : Recordset) return Data_Maps.Map;
   --  return map of all values for current row, NULL values are set to
   --  an empty String

   function Escape_String (C : Connection; S : String) return String;
   --  prepares a string for safe storage in a query

private
   type Connection is new Gnoga.Server.Database.Connection with
      record
         Server_ID : MySQL_ID := null;
      end record;

   subtype Field_Data is String (1 .. Natural'Last);
   type Field_Access is access all Field_Data;
   type Row_Data is array (1 .. Natural'Last) of Field_Access;
   type Row_Access is access all Row_Data;
   --  Used to access ROW data

   type List_of_Lengths is array (1 .. Natural'Last) of Natural;
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
