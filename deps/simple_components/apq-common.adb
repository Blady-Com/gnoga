--                                                                    --
--  package APQ.Common              Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

with APQ.Client; -- Driver-specific client

package body APQ.Common is

   procedure Free is new
      Ada.Unchecked_Deallocation
      (  Root_Connection_Type'Class,
         Root_Connection_Ptr
      );
   procedure Free is new
      Ada.Unchecked_Deallocation
      (  Root_Query_Type'Class,
         Root_Query_Ptr
      );

   procedure Append_Quoted
             (  Data_Base : in out APQ_Data_Base;
                Text      : String;
                After     : String := ""
             )  is
   begin
      Append_Quoted
      (  Data_Base.Query.all,
         Data_Base.Connection.all,
         Text,
         After
      );
   end Append_Quoted;

   procedure Close_DB_Trace (Data_Base : in out APQ_Data_Base) is
   begin
      null;
--    Close_DB_Trace (Data_Base.Connection.all);
   end Close_DB_Trace;

   procedure Commit (Data_Base : in out APQ_Data_Base) is
   begin
      case Data_Base.Shared is
         when Fully =>
            Raise_Exception
            (  Use_Error'Identity,
               "Multiple commits are not allowed"
            );
         when Read_Only =>
            Data_Base.Shared := Fully;
         when Read_Write =>
            Data_Base.Shared := Fully;
            Commit_Work (Data_Base.Query.all, Data_Base.Connection.all);
      end case;
   exception
      when Use_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Data_Base.Query.all)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Commit;

   procedure Connect
             (  Data_Base      : in out APQ_Data_Base;
                Server_Type    : Database_Type;
                Data_Base_Name : String;
                User_Name      : String;
                Password       : String;
                Host_Name      : String  := "localhost";
                Port_Number    : Natural := 0
             )  is
   begin
      Data_Base.Connection := new APQ.Client.Connection_Type;
      Set_Host_Name (Data_Base.Connection.all, Host_Name);
      if Port_Number > 0 then
         Set_Port (Data_Base.Connection.all, Port_Number);
      end if;
      Set_DB_Name (Data_Base.Connection.all, Data_Base_Name);
      Set_User_Password (Data_Base.Connection.all, User_Name, Password);
      Connect (Data_Base.Connection.all);
      Data_Base.Query :=
         new Root_Query_Type'Class'
             (  New_Query (Data_Base.Connection.all)
             );
   exception
      when Not_Connected =>
         Raise_Exception
         (  Use_Error'Identity,
            Error_Message (Data_Base.Connection.all)
         );
   end Connect;

   procedure Drop
             (  Data_Base  : in out APQ_Data_Base;
                Table_Name : String
             )  is
   begin
      Seize_Write (Data_Base);
      Prepare (Data_Base.Query.all, "DROP TABLE " & Table_Name);
      Execute (Data_Base);
      Commit (Data_Base);
   exception
      when Data_Error =>
         Roll_Back (Data_Base);
      when others =>
         raise;
   end Drop;

   procedure Execute (Data_Base : APQ_Data_Base) is
   begin
      if Data_Base.Shared = Fully then
         Raise_Exception
         (  Use_Error'Identity,
            "No access to execute"
         );
      end if;
      Execute (Data_Base.Query.all, Data_Base.Connection.all);
   exception
      when Use_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Data_Base.Query.all)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Execute;

   procedure Finalize (Data_Base : in out APQ_Data_Base) is
   begin
      Free (Data_Base.Query);
      Free (Data_Base.Connection);
   end Finalize;

   function Get_ID
            (  Data_Base   : APQ_Data_Base;
               Table_Name  : String;
               Column_Name : String
            )  return Row_ID_Type is
   begin
      case Engine_Of (Data_Base.Connection.all) is
         when Engine_PostgreSQL =>
            Prepare
            (  Data_Base.Query.all,
               (  "SELECT "
               &  Column_Name
               &  " FROM "
               &  Table_Name
               &  " WHERE "
               &  Column_Name
               &  " = (SELECT currval('"
               &  Table_Name & "_" & Column_Name & "_seq'))"
            )  );
            Execute
            (  Data_Base.Query.all,
               Data_Base.Connection.all
            );
            Fetch (Data_Base.Query.all);
            return Value (Data_Base.Query.all, 1);
         when Engine_MySQL =>
            return Command_OID (Data_Base.Query.all);
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  Database_Type'Image
                  (  Engine_Of (Data_Base.Connection.all)
                  )
               &  " does not support row identification"
            )  );
      end case;
   exception
      when No_Tuple =>
         Raise_Exception (Data_Error'Identity, "Insertion failed");
      when Data_Error | Use_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Data_Base.Query.all)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get_ID;

   function ID_Type (Data_Base : APQ_Data_Base) return String is
   begin
      case Engine_Of (Data_Base.Connection.all) is
         when Engine_PostgreSQL =>
            return "SERIAL";
         when Engine_MySQL =>
            return "INTEGER NOT NULL AUTO_INCREMENT PRIMARY KEY";
         when Engine_SyBase | Engine_CT_Lib =>
            return "KEYCOL INT DEFAULT AUTOINCREMENT";
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  Database_Type'Image
                  (  Engine_Of (Data_Base.Connection.all)
                  )
               &  " does not support row identification"
            )  );
      end case;
   end ID_Type;

   function Integer_Type (Data_Base : APQ_Data_Base) return String is
   begin
      case Engine_Of (Data_Base.Connection.all) is
         when Engine_PostgreSQL =>
            return "INTEGER";
         when Engine_MySQL =>
            return "INTEGER";
         when Engine_SyBase | Engine_CT_Lib =>
            return "INT";
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  Database_Type'Image
                  (  Engine_Of (Data_Base.Connection.all)
                  )
               &  " does not support integers"
            )  );
      end case;
   end Integer_Type;

   function New_ID (Data_Base : APQ_Data_Base) return String is
   begin
      case Engine_Of (Data_Base.Connection.all) is
         when Engine_PostgreSQL =>
            return "DEFAULT";
         when Engine_MySQL =>
            return "NULL";
         when Engine_SyBase | Engine_CT_Lib =>
            return "NULL";
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  Database_Type'Image
                  (  Engine_Of (Data_Base.Connection.all)
                  )
               &  " does not support row identification"
            )  );
      end case;
   end New_ID;

   procedure Open_DB_Trace
             (  Data_Base : APQ_Data_Base;
                File_Name : String;
                Mode      : Trace_Mode_Type := Trace_APQ
             )  is
   begin
      Open_DB_Trace
      (  Data_Base.Connection.all,
         File_Name,
         Mode
      );
   end Open_DB_Trace;

   function Ref_Type (Data_Base : APQ_Data_Base) return String is
   begin
      case Engine_Of (Data_Base.Connection.all) is
         when Engine_PostgreSQL =>
            return "INTEGER";
         when Engine_MySQL =>
            return "INTEGER";
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  Database_Type'Image
                  (  Engine_Of (Data_Base.Connection.all)
                  )
               &  " does not support row identification"
            )  );
      end case;
   end Ref_Type;

   procedure Roll_Back (Data_Base : in out APQ_Data_Base) is
   begin
      case Data_Base.Shared is
         when Fully =>
            null;
         when Read_Only =>
            Data_Base.Shared := Fully;
         when Read_Write =>
            Data_Base.Shared := Fully;
            RollBack_Work
            (  Data_Base.Query.all,
               Data_Base.Connection.all
            );
      end case;
   exception
      when Use_Error =>
         raise;
      when SQL_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            Error_Message (Data_Base.Query.all)
         );
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Roll_Back;

   procedure Seize_Read (Data_Base : in out APQ_Data_Base) is
   begin
      case Data_Base.Shared is
         when Fully =>
            Data_Base.Shared := Read_Only;
         when Read_Only | Read_Write =>
            Raise_Exception
            (  Use_Error'Identity,
               "Nested read transaction"
            );
      end case;
   end Seize_Read;

   procedure Seize_Write (Data_Base : in out APQ_Data_Base) is
   begin
      case Data_Base.Shared is
         when Fully =>
            Data_Base.Shared := Read_Write;
            Begin_Work
            (  Data_Base.Query.all,
               Data_Base.Connection.all
            );
         when Read_Only | Read_Write =>
            Raise_Exception
            (  Use_Error'Identity,
               "Nested write transaction"
            );
      end case;
   end Seize_Write;

   function String_Type (Data_Base : APQ_Data_Base) return String is
   begin
      case Engine_Of (Data_Base.Connection.all) is
         when Engine_PostgreSQL =>
            return "TEXT";
         when Engine_MySQL =>
            return "MEDIUMTEXT";
         when Engine_SyBase | Engine_CT_Lib =>
            return "TEXT";
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  Database_Type'Image
                  (  Engine_Of (Data_Base.Connection.all)
                  )
               &  " does not support varying strings"
            )  );
      end case;
   end String_Type;

   function Table_Exists
            (  Data_Base  : APQ_Data_Base;
               Table_Name : String
            )  return Boolean is
   begin
      case Data_Base.Shared is
         when Fully | Read_Write =>
            Raise_Exception
            (  Use_Error'Identity,
               "Write transaction is required for Table_Exists"
            );
         when Read_Only =>
            null;
      end case;
      Prepare (Data_Base.Query.all, "SELECT * FROM " & Table_Name);
      Execute (Data_Base);
      Clear (Data_Base.Query.all);
      return True;
   exception
      when Use_Error =>
         raise;
      when others =>
         return False;
   end Table_Exists;

   function Timestamp_Now (Data_Base : APQ_Data_Base) return String is
   begin
      case Engine_Of (Data_Base.Connection.all) is
         when Engine_PostgreSQL =>
            return "now()";
         when Engine_MySQL =>
            return "now()";
         when Engine_SyBase | Engine_CT_Lib =>
            return "getdate()";
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  Database_Type'Image
                  (  Engine_Of (Data_Base.Connection.all)
                  )
               &  " does not support timestamps"
            )  );
      end case;
   end Timestamp_Now;

   function Timestamp_Type (Data_Base : APQ_Data_Base) return String is
   begin
      case Engine_Of (Data_Base.Connection.all) is
         when Engine_PostgreSQL =>
            return "TIMESTAMP";
         when Engine_MySQL =>
            return "TIMESTAMP";
         when Engine_SyBase | Engine_CT_Lib =>
            return "DATETIME";
         when others =>
            Raise_Exception
            (  Use_Error'Identity,
               (  Database_Type'Image
                  (  Engine_Of (Data_Base.Connection.all)
                  )
               &  " does not support timestamps"
            )  );
      end case;
   end Timestamp_Type;

   function Value
            (  Data_Base : APQ_Data_Base;
               Column    : Column_Index_Type
            )  return String is
   begin
      return Value (Data_Base.Query.all, Column);
   exception
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Value;

end APQ.Common;
