--                                                                    --
--  package APQ.Common              Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  15:03 28 Mar 2009  --
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

with Persistent.Data_Bank;   use Persistent.Data_Bank;

package APQ.Common is
   pragma Elaborate_Body (APQ.Common);

   type Root_Connection_Ptr is access Root_Connection_Type'Class;
   type Root_Query_Ptr is access Root_Query_Type'Class;
--
-- APQ_Data_Base -- Data base low-level interface object
--
   type APQ_Data_Base is
      new Ada.Finalization.Limited_Controlled with
   record
      Connection : Root_Connection_Ptr;
      Query      : Root_Query_Ptr;
      Shared     : Sharing_Type := Fully;
   end record;
--
-- Append_Quoted -- Adds a quoted string to the query
--
--    Data_Base - To the data base
--    Text      - The text
--    After     - Suffix to add after the text
--
   procedure Append_Quoted
             (  Data_Base : in out APQ_Data_Base;
                Text      : String;
                After     : String := ""
             );
--
-- Close_DB_Trace -- Close trace file
--
--    Data_Base - To the data base
--
-- Nothing happens if tracing is inactive.
--
   procedure Close_DB_Trace (Data_Base : in out APQ_Data_Base);
--
-- Commit -- Changes
--
--    Data_Base - To the data base
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Wrong access mode
--
   procedure Commit (Data_Base : in out APQ_Data_Base);
--
-- Connect -- To a data base
--
--    Data_Base      - The object
--    Server_Type    - Type of the server to connect to
--    Data_Base_Name - To connect to
--    User_Name      - To connect as
--    Password       - Of the user
--    Host_Name      - Of the server running the data base engine
--    Port_Number    - The TCP/IP port listened by the server
--
-- Exceptions :
--
--    Use_Error  - Password or other data might be wrong
--    Data_Error - Data base error
--
   procedure Connect
             (  Data_Base      : in out APQ_Data_Base;
                Server_Type    : Database_Type;
                Data_Base_Name : String;
                User_Name      : String;
                Password       : String;
                Host_Name      : String  := "localhost";
                Port_Number    : Natural := 0
             );
--
-- Drop -- Delete a table
--
--    Data_Base  - The data base
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
--    Seize_Write is called to perform the operation
--
   procedure Drop
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String
             );
--
-- Execute -- A prepared command
--
--    Data_Base - The data base
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Wrong access mode
--
   procedure Execute (Data_Base : APQ_Data_Base);
--
-- Finalize -- Destructor
--
--    Data_Base - The object to finalize
--
   procedure Finalize (Data_Base : in out APQ_Data_Base);
--
-- Get_ID -- Get the ID of newly inserted row
--
--    Data_Base   - The Data_Base
--    Table_Name  - The table into which the row was inserted
--    Column_Name - The column that identifies the row (primary key)
--
-- This function is called immediately after issuing INSERT  command  to
-- obtain the identifier of the row. The result can be  used  in  SELECT
-- with  the  name  of  the  table column identifying the row. Note, for
-- postreSQL it is not the OID, but serial.
--
-- Returns :
--
--    The ID
--
-- Exceptions :
--
--    Use_Error - The type is not supported by the data base
--
   function Get_ID
            (  Data_Base   : APQ_Data_Base;
               Table_Name  : String;
               Column_Name : String
            )  return Row_ID_Type;
--
-- ID_Type -- The name of the type used to identify a row
--
--    Data_Base - To the data base for which the type is queried
--
-- The  type  is  an  autoincremented  or globally unique type used as a
-- primary  table  key.  Its  referential  counterpart  is  returned  by
-- Ref_Type.
--
-- Returns :
--
--    The type name (to be used in SQL requests)
--
-- Exceptions :
--
--    Use_Error - The type is not supported by the data base
--
   function ID_Type (Data_Base : APQ_Data_Base) return String;
--
-- Integer_Type -- The name of the type used for integers
--
--    Data_Base - The data base for which the type is queried
--
-- Returns :
--
--    The type name (to be used in SQL requests)
--
-- Exceptions :
--
--    Use_Error  - The type is not supported by the data base
--
   function Integer_Type (Data_Base : APQ_Data_Base) return String;
--
-- New_ID -- The name of ID generator to used in INSERT
--
--    Data_Base - The Data_Base
--
-- Returns :
--
--    The name of (usually "new_id")
--
-- Exceptions :
--
--    Use_Error  - The type is not supported by the data base
--
   function New_ID (Data_Base : APQ_Data_Base) return String;
--
-- Open_DB_Trace -- Open trace file and start tracing
--
--    Data_Base - The data base
--    File_Name - To open
--    Mode      - The tracing mode
--
   procedure Open_DB_Trace
             (  Data_Base : APQ_Data_Base;
                File_Name : String;
                Mode      : Trace_Mode_Type := Trace_APQ
             );
--
-- Ref_Type -- The name of the type used to reference a row
--
--    Data_Base - The data base for which the type is queried
--
-- See ID_Type.
--
-- Returns :
--
--    The type name (to be used in SQL requests)
--
-- Exceptions :
--
--    Use_Error  - The type is not supported by the data base
--
   function Ref_Type (Data_Base : APQ_Data_Base) return String;
--
-- Roll_Back -- Changes
--
--    Data_Base - The data base
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Wrong access mode
--
   procedure Roll_Back (Data_Base : in out APQ_Data_Base);
--
-- Seize_Read/Write -- Start transaction
--
--    Data_Base - The data base
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Wrong access mode
--
   procedure Seize_Read  (Data_Base : in out APQ_Data_Base);
   procedure Seize_Write (Data_Base : in out APQ_Data_Base);
--
-- String_Type -- The name of the type used for variable length strings
--
--    Data_Base - To the data base for which the type is queried
--
-- Returns :
--
--    The type name (to be used in SQL requests)
--
-- Exceptions :
--
--    Use_Error  - The type is not supported by the data base
--
   function String_Type (Data_Base : APQ_Data_Base) return String;
--
-- Table_Exists -- The command
--
--    Data_Base  - The data base
--    Table_Name - The table name
--
-- Returns :
--
--    True if the table exists
--
   function Table_Exists
            (  Data_Base  : APQ_Data_Base;
               Table_Name : String
            )  return Boolean;
--
-- Timestamp_Now -- Pseudo-value now
--
--    Data_Base - The data base for which the value is queried
--
-- Returns :
--
--    Now (to be used in SQL requests)
--
-- Exceptions :
--
--    Use_Error  - The type is not supported by the data base
--
   function Timestamp_Now (Data_Base : APQ_Data_Base)
      return String;
--
-- Timestamp_Type -- The name of the type used for timestamps
--
--    Data_Base - The data base for which the type is queried
--
-- Returns :
--
--    The type name (to be used in SQL requests)
--
-- Exceptions :
--
--    Use_Error  - The type is not supported by the data base
--
   function Timestamp_Type (Data_Base : APQ_Data_Base)
      return String;
--
-- Value -- Of a result column
--
--    Data_Base - The data base for which the type is queried
--    Column    - The column number
--
-- Returns :
--
--    The value
--
-- Exceptions :
--
--    Data_Error - A data base error
--
   function Value
            (  Data_Base : APQ_Data_Base;
               Column    : Column_Index_Type
            )  return String;

end APQ.Common;
