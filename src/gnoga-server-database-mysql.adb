------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--          G N O G A . S E R V E R . D A T A B A S E . M Y S Q L           --
--                                                                          --
--                                 B o d y                                  --
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

with Interfaces.C;

package body Gnoga.Server.Database.MySQL is

   --  Binding related specs

   Nul : constant Character := Character'First;
   --  nul terminator for binding to C string parameters

   type Ulonglong is new Natural;
   for Ulonglong'Size use 64;
   --  return type for Row related fields

   function Error_Message (S : Integer) return String;
   --  Return error message from database

   -------------
   -- Connect --
   -------------
   function Connect (Database : String;
                     Host     : String;
                     User     : String;
                     Password : String)
                     return Connection_Access
   is
      C : Connection_Access := new Connection;
   begin
      C.Connect (Database, Host, User, Password);
      return C;
   end Connect;

   procedure Connect (C        : in out Connection;
                      Database : String;
                      Host     : String;
                      User     : String;
                      Password : String)
   is
      function MYSQL_Init (mysql : Integer := 0) return Integer;
      pragma Import (C, MYSQL_Init, "mysql_init");

      function MYSQL_Real_Connect (mysql       : Integer := C.Server_ID;
                                   mHost       : String  := Host & Nul;
                                   mUser       : String  := User & Nul;
                                   Passwd      : String  := Password & Nul;
                                   Db          : Integer := 0;
                                   Port        : Integer := 0;
                                   Unix_Socket : Integer := 0;
                                   Clientflag  : Integer := 0)
                                  return Integer;
      pragma Import (C, MYSQL_Real_Connect, "mysql_real_connect");

      function MYSQL_Select_DB (mysql : Integer := C.Server_ID;
                                db    : String  := Database & Character'First)
                               return Integer;
      pragma Import (C, MYSQL_Select_DB, "mysql_select_db");

   begin
      C.Server_ID := MYSQL_Init;
      C.Server_ID := MYSQL_Real_Connect;
      if C.Server_ID = 0 then
         raise Connection_Error with
            "Connection to server " & Host & " has failed.";
      end if;

      if MYSQL_Select_DB /= 0 then
         raise Database_Error with Error_Message (C);
      end if;

   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (C : in out Connection) is
      procedure MYSQL_Close (mysql : Integer := C.Server_ID);
      pragma Import (C, MYSQL_Close, "mysql_close");

   begin
      if C.Server_ID /= 0 then
         MYSQL_Close;
         C.Server_ID := 0;
      end if;
   end Disconnect;


   -------------------
   -- Execute_Query --
   -------------------

   procedure Execute_Query (C : in out Connection; SQL : String) is
      function MYSQL_Real_Query (mysql : Integer := C.Server_ID;
                                 q     : String  := SQL;
                                 l     : Natural := SQL'Length)
                                 return Integer;
      pragma Import (C, MYSQL_Real_Query, "mysql_real_query");
   begin
      if C.Server_ID = 0 then
         raise Connection_Error;
      end if;

      if MYSQL_Real_Query /= 0 then
         raise Query_Error with SQL & " => " & Error_Message (C);
      end if;
   end Execute_Query;

   ---------------
   -- Insert_ID --
   ---------------

   function Insert_ID (C : Connection) return Natural is
      function MYSQL_Insert_Id (mysql : Integer := C.Server_ID)
                                return Ulonglong;
      pragma Import (C, MYSQL_Insert_Id, "mysql_insert_id");
   begin
      if C.Server_ID = 0 then
         raise Connection_Error;
      end if;

      return Natural (MYSQL_Insert_Id);
   end Insert_ID;

   -------------------
   -- Affected_Rows --
   -------------------

   function Affected_Rows (C : Connection) return Natural is
      function MYSQL_Affected_Rows (mysql : Integer := C.Server_ID)
                                    return Ulonglong;
      pragma Import (C, MYSQL_Affected_Rows, "mysql_affected_rows");
   begin
      if C.Server_ID = 0 then
         raise Connection_Error;
      end if;

      return Natural (MYSQL_Affected_Rows);
   end Affected_Rows;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (S : Integer) return String is
      subtype charbuf is
        Interfaces.C.char_array (1 .. Interfaces.C.size_t'Last);
      type charbuf_access is access all charbuf;

      function MYSQL_Error (mysql : Integer := S)
                            return charbuf_access;
      pragma Import (C, MYSQL_Error, "mysql_error");
   begin
      return Interfaces.C.To_Ada (MYSQL_Error.all);
   end Error_Message;

   function Error_Message (C : Connection) return String is
   begin
      return Error_Message (C.Server_ID);
   end Error_Message;

   --------------------
   -- List_Of_Tables --
   --------------------

   function List_Of_Tables (C : Connection) return Data_Array.Vector is
      Tables : Data_Array.Vector;
      RS     : Gnoga.Server.Database.Recordset'Class :=
        C.Query ("show tables");
   begin
      for I in 1 .. RS.Number_Of_Rows loop
         RS.Next;
         Tables.Append (RS.Field_Value (1));
      end loop;

      return Tables;
   end List_Of_Tables;

   --------------------------
   -- List_Fields_Of_Table --
   --------------------------

   function List_Fields_Of_Table (C          : Connection;
                                  Table_Name : String)
                                  return Data_Array.Vector
   is
      Fields : Data_Array.Vector;
      RS     : Gnoga.Server.Database.Recordset'Class :=
        C.Query ("describe " & Table_Name);
   begin
      for I in 1 .. RS.Number_Of_Rows loop
         RS.Next;
         Fields.Append (RS.Field_Value (1));
      end loop;

      return Fields;
   end List_Fields_Of_Table;


   ------------------------
   -- Field_Descriptions --
   ------------------------

   function Field_Descriptions (C : Connection; Table_Name : String)
                                return Field_Description_Array.Vector
   is
      use Ada.Strings.Unbounded;

      RS : Gnoga.Server.Database.Recordset'Class :=
        C.Query ("describe " & Table_Name);

      Descriptions : Field_Description_Array.Vector;
   begin
      while RS.Next loop
         declare
            Description : Field_Description;
         begin
            Description.Column_Name   :=
              To_Unbounded_String (RS.Field_Value (1));
            Description.Data_Type     :=
              To_Unbounded_String (RS.Field_Value (2));
            Description.Can_Be_Null   := RS.Field_Value (3) = "YES";
            Description.Default_Value :=
              To_Unbounded_String (RS.Field_Value (5));

            Descriptions.Append (Description);
         end;
      end loop;

      RS.Close;

      return Descriptions;
   end Field_Descriptions;

   ------------
   -- Query ---
   ------------

   function Query (C : Connection; SQL : String)
                   return Gnoga.Server.Database.Recordset'Class
   is
      RS : Recordset (C.Server_ID);

      function MYSQL_Real_Query (mysql : Integer := RS.Server_ID;
                                 q     : String  := SQL;
                                 l     : Natural := SQL'Length)
                                 return Integer;
      pragma Import (C, MYSQL_Real_Query, "mysql_real_query");

      function MYSQL_Store_Result (mysql : Integer := RS.Server_ID)
                                   return Integer;
      pragma Import (C, MYSQL_Store_Result, "mysql_store_result");

      function MYSQL_Num_Rows (Result : Integer := RS.Query_ID)
                               return Ulonglong;
      pragma Import (C, MYSQL_Num_Rows, "mysql_num_rows");

      function MYSQL_Num_Fields (Result : Integer := RS.Query_ID)
                                 return Natural;
      pragma Import (C, MYSQL_Num_Fields, "mysql_num_fields");
   begin
      if RS.Server_ID = 0 then
         raise Connection_Error;
      end if;

      if MYSQL_Real_Query /= 0 then
         raise Query_Error with SQL & " => " & Error_Message (RS.Server_ID);
      end if;

      RS.Query_ID := MYSQL_Store_Result;

      if RS.Query_ID = 0 then
         raise Empty_Recordset_Error;
      end if;

      RS.Row_Count := Natural (MYSQL_Num_Rows);
      RS.Field_Count := MYSQL_Num_Fields;

      return Recordset'Class (RS);
   end Query;

   ---------------------
   -- ID_Field_String --
   ---------------------

   function ID_Field_String (C : Connection) return String is
   begin
      return "id INTEGER PRIMARY KEY AUTO_INCREMENT";
   end ID_Field_String;

   -----------
   -- Close --
   -----------

   procedure Close (RS : in out Recordset) is
      procedure MYSQL_Free_Result  (Result : Integer := RS.Query_ID);
      pragma Import (C, MYSQL_Free_Result, "mysql_free_result");
   begin
      MYSQL_Free_Result;
   end Close;

   ----------
   -- Next --
   ----------

   procedure Next (RS : in out Recordset) is
   begin
      if not Next (RS) then
         raise End_Of_Recordset;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   function Next (RS : Recordset) return Boolean is
      R : access Recordset := RS'Unrestricted_Access;

      function MYSQL_Fetch_Row (Result : Integer := RS.Query_ID)
                                return Row_Access;
      pragma Import (C, MYSQL_Fetch_Row, "mysql_fetch_row");

      function MYSQL_Fetch_Lengths (Result : Integer := RS.Query_ID)
                                    return List_of_Lengths_Access;
      pragma Import (C, MYSQL_Fetch_Lengths, "mysql_fetch_lengths");
   begin
      R.Last_Row := MYSQL_Fetch_Row;
      if R.Last_Row = null then
         return False;
      else
         R.Lengths := MYSQL_Fetch_Lengths;
         return True;
      end if;
   end Next;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (C     : in out Connection;
      SQL   : String;
      Process : not null access
        procedure (RS : Gnoga.Server.Database.Recordset'Class))
   is
      RS : Gnoga.Server.Database.Recordset'Class := C.Query (SQL);
   begin
      RS.Iterate (Process);
   end Iterate;

   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access
        procedure (RS : Gnoga.Server.Database.Recordset'Class))
   is
   begin
      while RS.Next loop
         Process (RS);
      end loop;
   end Iterate;

   procedure Iterate
     (C     : in out Connection;
      SQL   : String;
      Process : not null access procedure (Row : Data_Maps.Map))
   is
      RS : Gnoga.Server.Database.Recordset'Class := C.Query (SQL);
   begin
      RS.Iterate (Process);
   end Iterate;

   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access procedure (Row : Data_Maps.Map)) is
   begin
      while RS.Next loop
         Process (RS.Field_Values);
      end loop;
   end Iterate;

   --------------------
   -- Number_Of_Rows --
   --------------------

   function Number_Of_Rows (RS : Recordset) return Natural is
   begin
      return RS.Row_Count;
   end Number_Of_Rows;

   ----------------------
   -- Number_Of_Fields --
   ----------------------

   function Number_Of_Fields (RS : Recordset) return Natural is
   begin
      return RS.Field_Count;
   end Number_Of_Fields;

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name
     (RS : Recordset;
      Field_Number : Natural)
      return String
   is
      type MYSQL_FIELD is record
         Name          : Field_Access;
         Org_Name      : Field_Access;
         Table         : Field_Access;
         Org_Table     : Field_Access;
         DB            : Field_Access;
         Catalog       : Field_Access;
         Default       : Field_Access;
         Create_Length : Natural;
         Max_Length    : Natural;
         Name_L        : Natural;
         Org_Name_L    : Natural;
         Table_L       : Natural;
         DB_L          : Natural;
         Catalog_L     : Natural;
         Default_L     : Natural;
      end record;
      pragma Convention (C, MYSQL_FIELD);

      type MYSQL_FIELD_Access is access all MYSQL_FIELD;

      function MYSQL_Fetch_Field_Direct
        (Result   : Integer := RS.Query_ID;
         FieldNum : Natural := Field_Number - 1)
         return MYSQL_FIELD_Access;
      pragma Import (C, MYSQL_Fetch_Field_Direct, "mysql_fetch_field_direct");

      Field : MYSQL_FIELD_Access := MYSQL_Fetch_Field_Direct;
   begin
      return Field.Name (1 .. Field.Name_L);
   end Field_Name;

   -----------------
   -- Field_Value --
   -----------------

   function Field_Value (RS           : Recordset;
                         Field_Number : Natural;
                         Handle_Nulls : Boolean := True)
                         return String
   is
   begin
      if RS.Last_Row = null then
         raise Empty_Row_Error;
      end if;

      if RS.Is_Null (Field_Number) then
         if Handle_Nulls then

            return "";
         else
            raise Null_Field;
         end if;
      end if;

      return RS.Last_Row (Field_Number)(1 .. RS.Lengths (Field_Number));
   end Field_Value;

   function Field_Value (RS           : Recordset;
                         Field_Name   : String;
                         Handle_Nulls : Boolean := True)
                         return String
   is
   begin
      for I in 1 .. RS.Field_Count loop
         if Field_Name = Gnoga.Server.Database.MySQL.Field_Name (RS, I) then
            return Field_Value (RS, I, Handle_Nulls);
         end if;
      end loop;

      raise No_Such_Field;
   end Field_Value;

   ------------------
   -- Field_Values --
   ------------------

   function Field_Values (RS : Recordset) return Data_Maps.Map is
      Row : Data_Maps.Map;
   begin
      for I in 1 .. RS.Field_Count loop
         Row.Insert (RS.Field_Name (I), RS.Field_Value (I));
      end loop;

      return Row;
   end Field_Values;


   -------------
   -- Is_Null --
   -------------

   function Is_Null (RS : Recordset; Field_Number : Natural) return Boolean is
   begin
      if RS.Last_Row = null then
         raise Empty_Row_Error;
      end if;

      if RS.Last_Row (Field_Number) = null then
         return True;
      else
         return False;
      end if;
   end Is_Null;

   function Is_Null (RS : Recordset; Field_Name : String) return Boolean is
   begin
      for I in 1 .. RS.Field_Count loop
         if Field_Name = Gnoga.Server.Database.MySQL.Field_Name (RS, I) then
            return RS.Is_Null (I);
         end if;
      end loop;

      raise No_Such_Field;
   end Is_Null;


   -------------------
   -- Escape_String --
   -------------------

   function Escape_String (C : Connection; S : String) return String is
      subtype  Buffer_Type is String (1 .. S'Length * 2 + 1); -- Min Buf Size

      Buf : Buffer_Type := (others => Character'First);

      function MYSQL_Real_Escape_String (mysql  : Integer := C.Server_ID;
                                         Buffer : Buffer_Type := Buf;
                                         Org    : String := S;
                                         Length : Natural := S'Length)
                                         return Natural;
      pragma Import (C, MYSQL_Real_Escape_String, "mysql_real_escape_string");

      Length : Natural := MYSQL_Real_Escape_String;
   begin
      return Buf (1 .. Length);
   end Escape_String;

end Gnoga.Server.Database.MySQL;
