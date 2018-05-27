------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--        G N O G A . S E R V E R . D A T A B A S E . S Q L I T E           --
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
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Interfaces.C;
with Ada.Strings.UTF_Encoding.Strings;

package body Gnoga.Server.Database.SQLite is

   SQLITE_OK   : constant := 0;
   SQLITE_ROW  : constant := 100;
   SQLITE_DONE : constant := 101;

   --  Binding related specs

   Nul : constant Character := Character'First;
   --  nul terminator for binding to C string parameters

   type Ulonglong is new Natural;
   for Ulonglong'Size use 64;
   --  return type for Row related fields

   function Error_Message (S : SQLite_ID) return String;
   --  Return error message from database

   -------------
   -- Connect --
   -------------

   function Connect (Database : String)
                     return Gnoga.Server.Database.Connection_Access
   is
      C : constant Connection_Access := new Connection;
   begin
      C.Connect (Database);
      return Gnoga.Server.Database.Connection_Access (C);
   end Connect;

   procedure Connect (C        : in out Connection;
                      Database : in     String)
   is
      SQLITE_OPEN_READWRITE : constant := 16#2#;
      SQLITE_OPEN_CREATE    : constant := 16#4#;

      SQLITE_OPEN_NOMUTEX   : constant := 16#8000#;
      --  Can only be used if database access is protected in app

      SQLITE_OPEN_FULLMUTEX : constant := 16#10000#;
      --  Serializes access to database, all use safe from multiple threads

      function sqlite3_open (filename : String           := Database & Nul;
                             ppDb     : access SQLite_ID := C.Server_ID'Access;
                             flags    : Integer          :=
                               SQLITE_OPEN_READWRITE +
                                 SQLITE_OPEN_CREATE +
                                   SQLITE_OPEN_FULLMUTEX;
                             zVfs     : Integer          := 0)
                             return Integer;
      pragma Import (C, sqlite3_open, "sqlite3_open_v2");

      R : Integer;
   begin
      R := sqlite3_open;

      if R /= 0 then
         raise Connection_Error with
            "Connection to server " & Database & " has failed - " &
            Error_Message (C.Server_ID);
      end if;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   overriding
   procedure Disconnect (C : in out Connection) is
      procedure sqlite3_close (sqlite3 : SQLite_ID := C.Server_ID);
      pragma Import (C, sqlite3_close, "sqlite3_close");

   begin
      if C.Server_ID /= null then
         sqlite3_close;
         C.Server_ID := null;
      end if;
   end Disconnect;

   -------------------
   -- Execute_Query --
   -------------------

   overriding
   procedure Execute_Query (C : in out Connection; SQL : String) is
      LSQL : constant String := (if C.UTF8_STring then SQL else
                           Ada.Strings.UTF_Encoding.Strings.Encode (SQL));
      Q  : aliased SQLite_ID;
      P  : aliased SQLite_ID;

      function sqlite3_prepare
        (sqlite  : SQLite_ID      := C.Server_ID;
         sq      : String         := LSQL;
         l       : Natural        := LSQL'Length;
         ppStmt  : access SQLite_ID := Q'Access;
         ppzTail : access SQLite_ID := P'Access)
         return Integer;
      pragma Import (C, sqlite3_prepare, "sqlite3_prepare_v2");

      function sqlite3_step (sqlite : SQLite_ID := Q)
                             return Integer;
      pragma Import (C, sqlite3_step, "sqlite3_step");

      procedure sqlite3_finalize (sqlite : SQLite_ID := Q);
      pragma Import (C, sqlite3_finalize, "sqlite3_finalize");
   begin
      if C.Server_ID = null then
         raise Connection_Error;
      end if;

      if sqlite3_prepare /= SQLITE_OK then
         raise Query_Error with SQL & " => " & Error_Message (C.Server_ID);
      end if;

      declare
         result : constant Integer := sqlite3_step;
      begin
         if result /= SQLITE_OK and
           result /= SQLITE_ROW and
           result /= SQLITE_DONE
         then
            raise Query_Error with SQL & " => " &
               result'Img & " - " & Error_Message (C.Server_ID);
         end if;
      end;

      sqlite3_finalize;
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
      function sqlite3_last_insert_rowid (sqlite : SQLite_ID := C.Server_ID)
                                          return Ulonglong;
      pragma Import
        (C, sqlite3_last_insert_rowid, "sqlite3_last_insert_rowid");
   begin
      if C.Server_ID = null then
         raise Connection_Error;
      end if;

      return Natural (sqlite3_last_insert_rowid);
   end Insert_ID;

   -------------------
   -- Affected_Rows --
   -------------------

   overriding
   function Affected_Rows (C : Connection) return Natural is
      function sqlite3_changes (sqlite : SQLite_ID := C.Server_ID)
                                return Natural;
      pragma Import (C, sqlite3_changes, "sqlite3_changes");
   begin
      if C.Server_ID = null then
         raise Connection_Error;
      end if;

      return sqlite3_changes;
   end Affected_Rows;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (S : SQLite_ID) return String is
      subtype charbuf is
        Interfaces.C.char_array (1 .. Interfaces.C.size_t'Last);
      type charbuf_access is access all charbuf;

      function sqlite3_errmsg (sqlite3 : SQLite_ID := S)
                               return charbuf_access;
      pragma Import (C, sqlite3_errmsg, "sqlite3_errmsg");
   begin
      return Interfaces.C.To_Ada (sqlite3_errmsg.all);
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
        C.Query
          ("select name from sqlite_master where type='table' order by name");
   begin
      while RS.Next loop
         Tables.Append (RS.Field_Value (1));
      end loop;

      RS.Close;
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
        C.Query ("select * from " & Table_Name & " limit 1");

      function sqlite3_column_count
        (sqlite : SQLite_ID := Recordset (RS).Query_ID)
         return Natural;
      pragma Import (C, sqlite3_column_count, "sqlite3_column_count");
   begin
      for J in 1 .. sqlite3_column_count loop
         Fields.Append (RS.Field_Name (J));
      end loop;

      RS.Close;

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
        C.Query ("pragma table_info (" & Table_Name & ")");

      Descriptions : Field_Description_Array_Type;
   begin
      while RS.Next loop
         declare
            Description : Field_Description;
         begin
            Description.Column_Name   :=
              To_Unbounded_String (RS.Field_Value (2));
            Description.Data_Type     :=
              To_Unbounded_String (RS.Field_Value (3));
            Description.Can_Be_Null   := RS.Field_Value (4) = "1";
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
      LSQL : constant String := (if C.UTF8_STring then SQL else
                           Ada.Strings.UTF_Encoding.Strings.Encode (SQL));
      RS : Recordset (C.Server_ID);
      Q  : aliased SQLite_ID;
      P  : aliased SQLite_ID;

      function sqlite3_prepare
        (sqlite  : SQLite_ID        := C.Server_ID;
         sq      : String           := LSQL;
         l       : Natural          := LSQL'Length;
         ppStmt  : access SQLite_ID := Q'Access;
         ppzTail : access SQLite_ID := P'Access)
         return Integer;
      pragma Import (C, sqlite3_prepare, "sqlite3_prepare_v2");

      function sqlite3_step (sqlite : SQLite_ID := RS.Query_ID)
                             return Integer;
      pragma Import (C, sqlite3_step, "sqlite3_step");

      function sqlite3_column_count (sqlite : SQLite_ID := RS.Query_ID)
                                     return Natural;
      pragma Import (C, sqlite3_column_count, "sqlite3_column_count");

      R : Integer;
   begin
      if RS.Server_ID = null then
         raise Connection_Error;
      end if;

      if sqlite3_prepare /= SQLITE_OK then
         raise Query_Error with SQL & " => " & Error_Message (RS.Server_ID);
      end if;

      RS.Query_ID := Q;

      R := sqlite3_step;
      if R /= SQLITE_OK and
        R /= SQLITE_ROW and
        R /= SQLITE_DONE
      then
         raise Query_Error with SQL & " => " &
         R'Img & " - " & Error_Message (C.Server_ID);
      end if;

      RS.Last_Result := R;
      RS.First_Row := True;
      RS.UTF8_STring := C.UTF8_STring;

      RS.Field_Count := sqlite3_column_count;
      if RS.Field_Count = 0 then
         raise Empty_Recordset_Error;
      end if;

      return Recordset'Class (RS);
   end Query;

   ---------------------
   -- ID_Field_String --
   ---------------------

   overriding
   function ID_Field_String (C : Connection) return String is
      pragma Unreferenced (C);
   begin
      return "id INTEGER PRIMARY KEY AUTOINCREMENT";
   end ID_Field_String;

   -----------
   -- Close --
   -----------

   overriding
   procedure Close (RS : in out Recordset) is
      procedure sqlite3_finalize  (Result : SQLite_ID := RS.Query_ID);
      pragma Import (C, sqlite3_finalize, "sqlite3_finalize");
   begin
      sqlite3_finalize;
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
      R : constant access Recordset := RS'Unrestricted_Access;

      function sqlite3_step (sqlite : SQLite_ID := RS.Query_ID)
                             return Integer;
      pragma Import (C, sqlite3_step, "sqlite3_step");
   begin
      if RS.Last_Result = SQLITE_DONE then
         return False;
      end if;

      if RS.First_Row then
         R.First_Row := False;
         return True;
      end if;

      R.Last_Result := sqlite3_step;

      if RS.Last_Result = SQLITE_DONE then
         return False;
      else
         return True;
      end if;
   end Next;

   -------------
   -- Iterate --
   -------------

   overriding
   procedure Iterate
     (C       : in out Connection;
      SQL     : in     String;
      Process : not null access
        procedure (RS : Gnoga.Server.Database.Recordset'Class))
   is
      LSQL : constant String := (if C.UTF8_STring then SQL else
                           Ada.Strings.UTF_Encoding.Strings.Encode (SQL));
      RS : Gnoga.Server.Database.Recordset'Class := C.Query (LSQL);
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
      LSQL : constant String := (if C.UTF8_STring then SQL else
                           Ada.Strings.UTF_Encoding.Strings.Encode (SQL));
      RS : Gnoga.Server.Database.Recordset'Class := C.Query (LSQL);
   begin
      RS.Iterate (Process);
   end Iterate;

   overriding
   procedure Iterate
     (RS      : in out Recordset;
      Process : not null access procedure (Row : Gnoga.Types.Data_Map_Type)) is
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
      raise Not_Implemented
        with "SQLite does not support Number_Of_Rows";
      return 0;
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
      subtype charbuf is
        Interfaces.C.char_array (1 .. Interfaces.C.size_t'Last);
      type charbuf_access is access all charbuf;

      function sqlite3_column_name
        (sqlite : SQLite_ID := RS.Query_ID;
         N      : Integer   := Field_Number - 1)
        return charbuf_access;
      pragma Import (C, sqlite3_column_name, "sqlite3_column_name");
   begin
      return (if RS.UTF8_STring then Interfaces.C.To_Ada
              (sqlite3_column_name.all) else
                 Ada.Strings.UTF_Encoding.Strings.Decode
                (Interfaces.C.To_Ada (sqlite3_column_name.all)));
   exception
      when E : Ada.Strings.UTF_Encoding.Encoding_Error =>
         Log ("Error converting to String from " &
                Interfaces.C.To_Ada (sqlite3_column_name.all));
         Log (Ada.Exceptions.Exception_Information (E));
         return Interfaces.C.To_Ada (sqlite3_column_name.all);
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
      use Interfaces.C;

      subtype Field_Data is
        Interfaces.C.char_array (0 .. Interfaces.C.size_t'Last);
      type Field_Access is access all Field_Data;

      function sqlite3_column_text
        (sqlite : SQLite_ID := RS.Query_ID;
         iCol   : int       := int (Field_Number - 1))
         return Field_Access;
      pragma Import (C, sqlite3_column_text, "sqlite3_column_text");

      function sqlite3_column_bytes
        (sqlite : SQLite_ID := RS.Query_ID;
         iCol   : int       := int (Field_Number - 1))
         return int;
      pragma Import (C, sqlite3_column_bytes, "sqlite3_column_bytes");

   begin
      if RS.Is_Null (Field_Number) then
         if Handle_Nulls then
            return "";
         else
            raise Null_Field;
         end if;
      else
         declare
            Value : constant String := To_Ada (sqlite3_column_text.all
                           (0 .. size_t (sqlite3_column_bytes)));
         begin
            return (if RS.UTF8_STring then Value else
                         Ada.Strings.UTF_Encoding.Strings.Decode (Value));
         exception
            when E : Ada.Strings.UTF_Encoding.Encoding_Error =>
               Log ("Error converting to String from " & Value);
               Log (Ada.Exceptions.Exception_Information (E));
               return Value;
         end;
      end if;
   end Field_Value;

   overriding
   function Field_Value (RS           : Recordset;
                         Field_Name   : String;
                         Handle_Nulls : Boolean := True)
                         return String
   is
   begin
      for I in 1 .. RS.Field_Count loop
         if Field_Name = Gnoga.Server.Database.SQLite.Field_Name (RS, I) then
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
      function sqlite3_column_type (sqlite : SQLite_ID := RS.Query_ID;
                                    iCol   : Natural   := Field_Number - 1)
                                    return Integer;
      pragma Import (C, sqlite3_column_type, "sqlite3_column_type");
   begin
      if sqlite3_column_type = 5 then -- SQLITE_NULL
         return True;
      else
         return False;
      end if;
   end Is_Null;

   overriding
   function Is_Null (RS : Recordset; Field_Name : String) return Boolean is
   begin
      for I in 1 .. RS.Field_Count loop
         if Field_Name = Gnoga.Server.Database.SQLite.Field_Name (RS, I) then
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
      pragma Unreferenced (C);
      use Ada.Strings.Unbounded;

      New_String : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for J in S'First .. S'Last loop
         if S (J) = ''' then
            New_String := New_String & "''";
         else
            New_String := New_String & S (J);
         end if;
      end loop;

      return To_String (New_String);
   end Escape_String;

   -----------------------
   -- Full_Column_Names --
   -----------------------

   procedure Full_Column_Names
     (C      : in out Connection;
      Active :        Boolean := True)
   is
   begin
      C.Execute_Query ("PRAGMA full_column_names = " & Active'Img);
   end Full_Column_Names;
   function Full_Column_Names (C : in out Connection) return Boolean is
      RS : Gnoga.Server.Database.Recordset'Class :=
        C.Query ("PRAGMA full_column_names");
   begin
      if RS.Next then
         return
           Result : constant Boolean :=
             Boolean'Val (Integer'Value (RS.Field_Value (1)))
         do
            RS.Close;
         end return;
      else
         RS.Close;
         return False;
      end if;
   end Full_Column_Names;

   ------------------------
   -- Short_Column_Names --
   ------------------------

   procedure Short_Column_Names
     (C      : in out Connection;
      Active :        Boolean := True)
   is
   begin
      C.Execute_Query ("PRAGMA short_column_names = " & Active'Img);
   end Short_Column_Names;
   function Short_Column_Names (C : in out Connection) return Boolean is
      RS : Gnoga.Server.Database.Recordset'Class :=
        C.Query ("PRAGMA short_column_names");
   begin
      if RS.Next then
         return
           Result : constant Boolean :=
             Boolean'Val (Integer'Value (RS.Field_Value (1)))
         do
            RS.Close;
         end return;
      else
         RS.Close;
         return False;
      end if;
   end Short_Column_Names;

   --------------
   -- Encoding --
   --------------

   procedure Encoding (C : in out Connection; Value : String) is
   begin
      C.Execute_Query ("PRAGMA encoding = '" & Value & ''');
   end Encoding;
   function Encoding (C : in out Connection) return String is
      RS : Gnoga.Server.Database.Recordset'Class :=
        C.Query ("PRAGMA encoding");
   begin
      if RS.Next then
         return Result : constant String := RS.Field_Value (1) do
            RS.Close;
         end return;
      else
         RS.Close;
         return "";
      end if;
   end Encoding;

   -----------------
   -- UTF8_STring --
   -----------------

   procedure UTF8_STring (C : in out Connection; Active : Boolean := True) is
   begin
      C.UTF8_STring := Active;
   end UTF8_STring;
   function UTF8_STring (C : in out Connection) return Boolean is
   begin
      return C.UTF8_STring;
   end UTF8_STring;
end Gnoga.Server.Database.SQLite;
