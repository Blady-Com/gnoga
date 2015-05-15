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

   function Error_Message (S : MySQL_ID) return String;
   --  Return error message from database

   -------------
   -- Connect --
   -------------
   function Connect (Database : String;
                     Host     : String;
                     User     : String;
                     Password : String  := "";
                     Port     : Integer := 3306)
                     return Connection_Access
   is
      C : Connection_Access := new Connection;
   begin
      C.Connect (Database, Host, User, Password, Port);
      return C;
   end Connect;

   procedure Connect (C        : in out Connection;
                      Database : String;
                      Host     : String;
                      User     : String;
                      Password : String  := "";
                      Port     : Integer := 3306)
   is
      function MYSQL_Init (mysql : MySQL_ID := null) return MySQL_ID;
      pragma Import (C, MYSQL_Init, "mysql_init");

      Init_Result : MySQL_ID;

      function MYSQL_Real_Connect (mysql       : MySQL_ID;
                                   mHost       : String   := Host & Nul;
                                   mUser       : String   := User & Nul;
                                   Passwd      : String   := Password & Nul;
                                   Db          : Integer  := 0;
                                   Port        : Integer  := 0;
                                   Unix_Socket : Integer  := 0;
                                   Clientflag  : Integer  := 0)
                                  return MySQL_ID;
      pragma Import (C, MYSQL_Real_Connect, "mysql_real_connect");

      function MYSQL_Select_DB (mysql : MySQL_ID := C.Server_ID;
                                db    : String   := Database & Character'First)
                               return Integer;
      pragma Import (C, MYSQL_Select_DB, "mysql_select_db");
   begin
      Init_Result := MYSQL_Init;
      if Init_Result = null then
         raise Connection_Error with
            "Unable to initialize connection to MySQL Client Library.";
      end if;

      C.Server_ID := MYSQL_Real_Connect (Init_Result);
      if C.Server_ID = null then
         raise Connection_Error with Error_Message (Init_Result);
      end if;

      if MYSQL_Select_DB /= 0 then
         raise Database_Error with Error_Message (C);
      end if;

   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   overriding
   procedure Disconnect (C : in out Connection) is
      procedure MYSQL_Close (mysql : MySQL_ID := C.Server_ID);
      pragma Import (C, MYSQL_Close, "mysql_close");

   begin
      if C.Server_ID /= null then
         MYSQL_Close;
         C.Server_ID := null;
      end if;
   end Disconnect;

   -------------------
   -- Execute_Query --
   -------------------

   overriding
   procedure Execute_Query (C : in out Connection; SQL : String) is
      function MYSQL_Real_Query (mysql : MySQL_ID := C.Server_ID;
                                 q     : String   := SQL;
                                 l     : Natural := SQL'Length)
                                 return Integer;
      pragma Import (C, MYSQL_Real_Query, "mysql_real_query");
   begin
      if C.Server_ID = null then
         raise Connection_Error;
      end if;

      if MYSQL_Real_Query /= 0 then
         raise Query_Error with SQL & " => " & Error_Message (C);
      end if;
   end Execute_Query;

   --------------------
   -- Execute_Update --
   --------------------

   overriding
   function Execute_Update (C : in out Connection; SQL : String)
                            return Natural
   is
   begin
      Execute_Query (C, SQL);
      return Affected_Rows (C);
   end Execute_Update;

   ---------------
   -- Insert_ID --
   ---------------

   overriding
   function Insert_ID (C : Connection) return Natural is
      function MYSQL_Insert_Id (mysql : MySQL_ID := C.Server_ID)
                                return Ulonglong;
      pragma Import (C, MYSQL_Insert_Id, "mysql_insert_id");
   begin
      if C.Server_ID = null then
         raise Connection_Error;
      end if;

      return Natural (MYSQL_Insert_Id);
   end Insert_ID;

   -------------------
   -- Affected_Rows --
   -------------------

   overriding
   function Affected_Rows (C : Connection) return Natural is
      function MYSQL_Affected_Rows (mysql : MySQL_ID := C.Server_ID)
                                    return Ulonglong;
      pragma Import (C, MYSQL_Affected_Rows, "mysql_affected_rows");
   begin
      if C.Server_ID = null then
         raise Connection_Error;
      end if;

      return Natural (MYSQL_Affected_Rows);
   end Affected_Rows;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (S : MySQL_ID) return String is
      subtype charbuf is
        Interfaces.C.char_array (1 .. Interfaces.C.size_t'Last);
      type charbuf_access is access all charbuf;

      function MYSQL_Error (mysql : MySQL_ID := S)
                            return charbuf_access;
      pragma Import (C, MYSQL_Error, "mysql_error");
   begin
      return Interfaces.C.To_Ada (MYSQL_Error.all);
   end Error_Message;

   overriding
   function Error_Message (C : Connection) return String is
   begin
      return Error_Message (C.Server_ID);
   end Error_Message;

   --------------------
   -- List_Of_Tables --
   --------------------

   overriding
   function List_Of_Tables (C : Connection)
                            return Gnoga.Types.Data_Array_Type is
      Tables : Gnoga.Types.Data_Array_Type;
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

   overriding
   function List_Fields_Of_Table (C          : Connection;
                                  Table_Name : String)
                                  return Gnoga.Types.Data_Array_Type
   is
      Fields : Gnoga.Types.Data_Array_Type;
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

   overriding
   function Field_Descriptions (C : Connection; Table_Name : String)
                                return Field_Description_Array_Type
   is
      use Ada.Strings.Unbounded;

      RS : Gnoga.Server.Database.Recordset'Class :=
        C.Query ("describe " & Table_Name);

      Descriptions : Field_Description_Array_Type;
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

   overriding
   function Query (C : Connection; SQL : String)
                   return Gnoga.Server.Database.Recordset'Class
   is
      RS : Recordset (C.Server_ID);

      function MYSQL_Real_Query (mysql : MySQL_ID := RS.Server_ID;
                                 q     : String   := SQL;
                                 l     : Natural  := SQL'Length)
                                 return Integer;
      pragma Import (C, MYSQL_Real_Query, "mysql_real_query");

      function MYSQL_Store_Result (mysql : MySQL_ID := RS.Server_ID)
                                   return MySQL_ID;
      pragma Import (C, MYSQL_Store_Result, "mysql_store_result");

      function MYSQL_Num_Rows (Result : MySQL_ID := RS.Query_ID)
                               return Ulonglong;
      pragma Import (C, MYSQL_Num_Rows, "mysql_num_rows");

      function MYSQL_Num_Fields (Result : MySQL_ID := RS.Query_ID)
                                 return Natural;
      pragma Import (C, MYSQL_Num_Fields, "mysql_num_fields");
   begin
      if RS.Server_ID = null then
         raise Connection_Error;
      end if;

      if MYSQL_Real_Query /= 0 then
         raise Query_Error with SQL & " => " & Error_Message (RS.Server_ID);
      end if;

      RS.Query_ID := MYSQL_Store_Result;

      if RS.Query_ID = null then
         raise Empty_Recordset_Error;
      end if;

      RS.Row_Count := Natural (MYSQL_Num_Rows);
      RS.Field_Count := MYSQL_Num_Fields;

      return Recordset'Class (RS);
   end Query;

   ---------------------
   -- ID_Field_String --
   ---------------------

   overriding
   function ID_Field_String (C : Connection) return String is
   begin
      return "id INTEGER PRIMARY KEY AUTO_INCREMENT";
   end ID_Field_String;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (RS : in out Recordset) is
      procedure MYSQL_Free_Result  (Result : MySQL_ID := RS.Query_ID);
      pragma Import (C, MYSQL_Free_Result, "mysql_free_result");
   begin
      MYSQL_Free_Result;
   end Close;

   ----------
   -- Next --
   ----------

   overriding
   procedure Next (RS : in out Recordset) is
   begin
      if not Next (RS) then
         raise End_Of_Recordset;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   overriding
   function Next (RS : in out Recordset) return Boolean is
      R : access Recordset := RS'Unrestricted_Access;

      function MYSQL_Fetch_Row (Result : MySQL_ID := RS.Query_ID)
                                return Row_Access;
      pragma Import (C, MYSQL_Fetch_Row, "mysql_fetch_row");

      function MYSQL_Fetch_Lengths (Result : MySQL_ID := RS.Query_ID)
                                    return List_of_Lengths_Access;
      pragma Import (C, MYSQL_Fetch_Lengths, "mysql_fetch_lengths");
   begin
      R.Last_Row := MYSQL_Fetch_Row;
      if R.Last_Row = null then
         return False;
      else
         R.Lengths := MYSQL_Fetch_Lengths;
         if R.Lengths = null then
            raise Query_Error with Error_Message (RS.Query_ID);
         end if;
         return True;
      end if;
   end Next;

   -------------
   -- Iterate --
   -------------

   overriding
   procedure Iterate
     (C     : in out Connection;
      SQL   : in     String;
      Process : not null access
        procedure (RS : Gnoga.Server.Database.Recordset'Class))
   is
      RS : Gnoga.Server.Database.Recordset'Class := C.Query (SQL);
   begin
      RS.Iterate (Process);
   end Iterate;

   overriding
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

   overriding
   procedure Iterate
     (C     : in out Connection;
      SQL   : in     String;
      Process : not null access procedure (Row : Gnoga.Types.Data_Map_Type))
   is
      RS : Gnoga.Server.Database.Recordset'Class := C.Query (SQL);
   begin
      RS.Iterate (Process);
   end Iterate;

   overriding
   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access procedure (Row : Gnoga.Types.Data_Map_Type))
   is
   begin
      while RS.Next loop
         Process (RS.Field_Values);
      end loop;
   end Iterate;

   --------------------
   -- Number_Of_Rows --
   --------------------

   overriding
   function Number_Of_Rows (RS : Recordset) return Natural is
   begin
      return RS.Row_Count;
   end Number_Of_Rows;

   ----------------------
   -- Number_Of_Fields --
   ----------------------

   overriding
   function Number_Of_Fields (RS : Recordset) return Natural is
   begin
      return RS.Field_Count;
   end Number_Of_Fields;

   ----------------
   -- Field_Name --
   ----------------

   overriding
   function Field_Name
     (RS           : Recordset;
      Field_Number : Natural)
      return String
   is
      use type Interfaces.C.char_array;
      use type Interfaces.C.size_t;

      type MYSQL_FIELD is record
         Name          : Field_Access;
         Org_Name      : Field_Access;
         Table         : Field_Access;
         Org_Table     : Field_Access;
         DB            : Field_Access;
         Catalog       : Field_Access;
         Default       : Field_Access;
         Create_Length : Interfaces.C.unsigned_long;
         Max_Length    : Interfaces.C.unsigned_long;
         Name_L        : Interfaces.C.unsigned;
         Org_Name_L    : Interfaces.C.unsigned;
         Table_L       : Interfaces.C.unsigned;
         DB_L          : Interfaces.C.unsigned;
         Catalog_L     : Interfaces.C.unsigned;
         Default_L     : Interfaces.C.unsigned;
      end record;
      pragma Convention (C, MYSQL_FIELD);

      type MYSQL_FIELD_Access is access all MYSQL_FIELD;

      function MYSQL_Fetch_Field_Direct
        (Result   : MySQL_ID              := RS.Query_ID;
         FieldNum : Interfaces.C.unsigned :=
           Interfaces.C.unsigned (Field_Number - 1))
         return MYSQL_FIELD_Access;
      pragma Import (C, MYSQL_Fetch_Field_Direct, "mysql_fetch_field_direct");

      Field : MYSQL_FIELD_Access;
   begin
      Field := MYSQL_Fetch_Field_Direct;
      if Field = null then
         raise Query_Error with Error_Message (RS.Query_ID);
      end if;

      return Interfaces.C.To_Ada
        (Field.Name (0 .. Interfaces.C.size_t (Field.Name_L) - 1) &
           Interfaces.C.nul);
   end Field_Name;

   -----------------
   -- Field_Value --
   -----------------

   overriding
   function Field_Value (RS           : Recordset;
                         Field_Number : Natural;
                         Handle_Nulls : Boolean := True)
                         return String
   is
      use type Interfaces.C.char_array;
      use type Interfaces.C.size_t;
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

      return Interfaces.C.To_Ada
        (RS.Last_Row (Field_Number)
         (0 .. Interfaces.C.size_t (RS.Lengths (Field_Number)) - 1) &
           Interfaces.C.nul);
   end Field_Value;

   overriding
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

   overriding
   function Field_Values (RS : Recordset) return Gnoga.Types.Data_Map_Type is
      Row : Gnoga.Types.Data_Map_Type;
   begin
      for I in 1 .. RS.Field_Count loop
         Row.Insert (RS.Field_Name (I), RS.Field_Value (I));
      end loop;

      return Row;
   end Field_Values;

   -------------
   -- Is_Null --
   -------------

   overriding
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

   overriding
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

   overriding
   function Escape_String (C : Connection; S : String) return String is
      subtype  Buffer_Type is String (1 .. S'Length * 2 + 1); -- Min Buf Size

      Buf : Buffer_Type := (others => Character'First);

      function MYSQL_Real_Escape_String (mysql  : MySQL_ID    := C.Server_ID;
                                         Buffer : Buffer_Type := Buf;
                                         Org    : String      := S;
                                         Length : Natural     := S'Length)
                                         return Natural;
      pragma Import (C, MYSQL_Real_Escape_String, "mysql_real_escape_string");

      Length : Natural := MYSQL_Real_Escape_String;
   begin
      return Buf (1 .. Length);
   end Escape_String;

end Gnoga.Server.Database.MySQL;
