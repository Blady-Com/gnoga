--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--        HTTP server.SQLite_Browser               Winter, 2014       --
--  Interface                                                         --
--                                Last revision :  13:51 30 May 2014  --
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

with SQLite;        use SQLite;
with Strings_Edit;  use Strings_Edit;

with Generic_Unbounded_Array;
with Object.Handle;
with Strings_Edit.Float_Edit;
with Strings_Edit.Integer_Edit;

package GNAT.Sockets.Connection_State_Machine.HTTP_Server.
        SQLite_Browser is
------------------------------------------------------------------------
-- Abstract_SQLite_Content -- The base type used to list SQLite3 DB
--
   type Abstract_SQLite_Content is abstract
      new Content_Source with private;
--
-- Get_Database -- The database
--
--    Content - The content object
--
-- Returns :
--
--    A handle to SQLite3 database
--
   function Get_Database (Content : Abstract_SQLite_Content)
      return Data_Base;
--
-- Get_Database_Path -- The database file path
--
--    Content - The content object
--
-- This procedure gets the database file path.
--
-- Returns :
--
--    The database path
--
   function Get_Database_Path (Content : Abstract_SQLite_Content)
      return String;
--
-- Set_Database -- Set the database from another content
--
--    Content - The content object
--    Source  - The object to take the database settings from
--
-- This procedure  should  be called  once  in  order  to  specifiy  the
-- database to work with.
--
   procedure Set_Database
             (  Content : in out Abstract_SQLite_Content;
                Source  : Abstract_SQLite_Content'Class
             );
--
-- Set_Database -- Set the database from database object and its path
--
--    Content   - The content object
--    Database  - The database object
--    File_Name - The database path
--
-- This procedure  should  be called  once  in  order  to  specifiy  the
-- database to work with.
--
   procedure Set_Database
             (  Content   : in out Abstract_SQLite_Content;
                Database  : Data_Base;
                File_Name : String
             );
--
-- Set_Database_Path -- Set the database using file path
--
--    Content   - The content object
--    File_Name - The SQLite3 database file
--    Flags     - Database file opening flags
--
-- This procedure  should  be called  once  in  order  to  specifiy  the
-- database to work with.
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - File open error
--
   procedure Set_Database_Path
             (  Content   : in out Abstract_SQLite_Content;
                File_Name : String;
                Flags     : Open_Flags := READONLY or FULLMUTEX
             );
------------------------------------------------------------------------
-- DB_Tables_Content -- The content that lists tables of the database
--
   type DB_Tables_Content is new Abstract_SQLite_Content with private;
--
-- Get -- Implements GNAT.Sockets.Connection_State_Machine.HTTP_Server..
--
   function Get (Content : access DB_Tables_Content) return String;
--
-- Get_Content_Page -- Page name from table
--
--    Content - The content object
--    Table   - The table name
--
-- This function returns  the name  of the page  that lists the contents
-- of the table.  It can be overridden  in order  to provide a different
-- naming.  The server must  use an instance of Table_Content object for
-- the page.
--
-- Returns :
--
--    <database-path>/<table-name>/content.htm
--
   function Get_Content_Page
            (  Content : DB_Tables_Content;
               Table   : String
            )  return String;
--
-- Get_Schema_Page -- Page name from table
--
--    Content - The content object
--    Table   - The table name
--
-- This function returns the name  of the page  that lists the schema of
-- the table.  It can  be  overridden in  order to provide  a  different
-- naming.  The server must use an instance of Schema_Content object for
-- the page.
--
-- Returns :
--
--    <database-path>/<table-name>/schema.htm
--
   function Get_Schema_Page
            (  Content : DB_Tables_Content;
               Table   : String
            )  return String;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : DB_Tables_Content
             );
   for DB_Tables_Content'Write use Write;
------------------------------------------------------------------------
-- DB_Query_Content -- The content used to query the database
--
--    Max_String_Column_Width -- Before image gets truncated
--
   type DB_Query_Content
        (  Max_String_Column_Width : Positive
        )  is new Abstract_SQLite_Content with private;
--
-- Get -- Implements GNAT.Sockets.Connection_State_Machine.HTTP_Server..
--
   function Get (Content : access DB_Query_Content) return String;
--
-- Get_Query_Action -- Action field of the query form
--
--    Content - The content object
--
-- This function  returns the field action  used for the database query.
-- It can be overridden  in order  to provide  a different  naming.  The
-- server  must  use an instance  of  DB_Query_Content  object  for  the
-- action.
--
-- Returns :
--
--    <database-path>/query.htm
--
   function Get_Query_Action (Content : DB_Query_Content) return String;
--
-- Set_Statement -- Set the SQLite3 statement to execute
--
--    Content   - The content object
--    Statement - The statement to use
--
-- This procedure  should be used  in order  to execute  a statement and
-- then show  its result set  as  a part of  the content.  The execution
-- happens when the content is sent using Send_Body.
--
   procedure Set_Statement
             (  Content   : in out DB_Query_Content;
                Statement : String
             );
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : DB_Query_Content
             );
   for DB_Query_Content'Write use Write;
------------------------------------------------------------------------
-- Abstract_Table_Content --  The base type used to list a table
--
   type Abstract_Table_Content is abstract
      new Abstract_SQLite_Content with private;
--
-- Get_Table -- The table name
--
--    Content - The content object
--
-- Returns :
--
--    The table name
--
   function Get_Table (Content : Abstract_Table_Content) return String;
--
-- Set_Table -- Set the database and the name of the table to work with
--
--    Content - The content object
--    Source  - The object to take table from
--
   procedure Set_Table
             (  Content : in out Abstract_Table_Content;
                Source  : Abstract_Table_Content'Class
             );
--
-- Set_Table -- Set the name of the table to work with
--
--    Content - The content object
--    Table   - The table name
--
-- This procedure should be called after Set_Database[_Path].
--
   procedure Set_Table
             (  Content : in out Abstract_Table_Content;
                Table   : String
             );
------------------------------------------------------------------------
-- Schema_Content -- The content that lists schema of a table
--
   type Schema_Content is new Abstract_Table_Content with private;
--
-- Get -- Implements GNAT.Sockets.Connection_State_Machine.HTTP_Server..
--
   function Get (Content : access Schema_Content) return String;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Schema_Content
             );
   for Schema_Content'Write use Write;
------------------------------------------------------------------------
-- Table_Content -- The content that lists a table
--
--    Max_String_Column_Width -- Before image gets truncated
--
   type Table_Content (Max_String_Column_Width : Positive) is
      new Abstract_Table_Content with private;
--
-- Get -- Implements GNAT.Sockets.Connection_State_Machine.HTTP_Server..
--
   function Get (Content : access Table_Content) return String;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Table_Content
             );
   for Table_Content'Write use Write;

private
   type String_Object (Length : Natural) is
      new Object.Entity with
   record
      Text : String (1..Length);
   end record;
   type String_Object_Ptr is access String_Object'Class;
   package String_Handles is
      new Object.Handle (String_Object, String_Object_Ptr);
   use String_Handles;
   function Get (Object : Handle) return String;
   procedure Set (Object : in out Handle; Value : String);

   type Abstract_SQLite_Content is abstract
      new Content_Source with
   record
      DB    : Data_Base;
      Path  : Handle;
      List  : Statement;
      Count : Natural := 0;
   end record;

   type DB_Query_Content
        (  Max_String_Column_Width : Positive
        )  is new Abstract_SQLite_Content with
   record
      Command           : Handle;
      Number_Of_Columns : Natural := 0;
      Column            : Natural := 0;
   end record;

   type DB_Tables_Content is
      new Abstract_SQLite_Content with null record;

   type Abstract_Table_Content is abstract
      new Abstract_SQLite_Content with
   record
      Table : Handle;
   end record;

   type Schema_Content is new Abstract_Table_Content with null record;

   subtype Column_Datatype is SQLite.Datatype
      range SQLITE_INTEGER..SQLITE_TEXT;
   type Column_Descriptor is record
      Datatype : Column_Datatype;
   end record;
   type Column_Descriptor_Array is
      array (Positive range <>) of Column_Descriptor;

   package Column_Descriptor_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Column_Descriptor,
             Object_Array_Type => Column_Descriptor_Array,
             Null_Element      => (Datatype => SQLite.SQLITE_INTEGER)
          );
   use Column_Descriptor_Arrays;

   type Table_Content (Max_String_Column_Width : Positive) is
      new Abstract_Table_Content with
   record
      Number_Of_Columns : Natural := 0;
      Column            : Natural := 0;
      Columns_List      : Column_Descriptor_Arrays.Unbounded_Array;
   end record;

   package Double_Edit is
      new Strings_Edit.Float_Edit (Interfaces.C.double);
   package Integer_64_Edit is
      new Strings_Edit.Integer_Edit (Integer_64);

   function Head (Justify : Alignment; Column : Positive) return String;

   function Integer_Image
            (  Result : Statement;
               Column : Positive
            )  return String;
   function Real_Image
            (  Result : Statement;
               Column : Positive
            )  return String;
   function String_Image
            (  Result    : Statement;
               Column    : Positive;
               Max_Width : Positive
            )  return String;
   function Tail
            (  Column      : Positive;
               Last_Column : Positive
            )  return String;

end GNAT.Sockets.Connection_State_Machine.HTTP_Server.SQLite_Browser;
