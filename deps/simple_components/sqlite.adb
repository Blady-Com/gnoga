--                                                                    --
--  package SQLite                  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2009       --
--                                                                    --
--                                Last revision :  07:53 21 Jul 2016  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Ada.Unchecked_Conversion;

package body SQLite is
   use Data_Base_Handles;
   use Object;
   use Statement_Handles;
      -- Return codes
   SQLITE_OK         : constant := 0;
   SQLITE_ERROR      : constant := 1;
   SQLITE_INTERNAL   : constant := 2;
   SQLITE_PERM       : constant := 3;
   SQLITE_ABORT      : constant := 4;
   SQLITE_BUSY       : constant := 5;
   SQLITE_LOCKED     : constant := 6;
   SQLITE_NOMEM      : constant := 7;
   SQLITE_READONLY   : constant := 8;
   SQLITE_INTERRUPT  : constant := 9;
   SQLITE_IOERR      : constant := 10;
   SQLITE_CORRUPT    : constant := 11;
   SQLITE_NOTFOUND   : constant := 12;
   SQLITE_FULL       : constant := 13;
   SQLITE_CANTOPEN   : constant := 14;
   SQLITE_PROTOCOL   : constant := 15;
   SQLITE_EMPTY      : constant := 16;
   SQLITE_SCHEMA     : constant := 17;
   SQLITE_TOOBIG     : constant := 18;
   SQLITE_CONSTRAINT : constant := 19;
   SQLITE_MISMATCH   : constant := 20;
   SQLITE_MISUSE     : constant := 21;
   SQLITE_NOLFS      : constant := 22;
   SQLITE_AUTH       : constant := 23;
   SQLITE_FORMAT     : constant := 24;
   SQLITE_RANGE      : constant := 25;
   SQLITE_NOTADB     : constant := 26;
   SQLITE_ROW        : constant := 100;
   SQLITE_DONE       : constant := 101;
      -- Extended return codes
   SQLITE_IOERR_READ              : constant := SQLITE_IOERR  +  1*256;
   SQLITE_IOERR_SHORT_READ        : constant := SQLITE_IOERR  +  2*256;
   SQLITE_IOERR_WRITE             : constant := SQLITE_IOERR  +  3*256;
   SQLITE_IOERR_FSYNC             : constant := SQLITE_IOERR  +  4*256;
   SQLITE_IOERR_DIR_FSYNC         : constant := SQLITE_IOERR  +  5*256;
   SQLITE_IOERR_TRUNCATE          : constant := SQLITE_IOERR  +  6*256;
   SQLITE_IOERR_FSTAT             : constant := SQLITE_IOERR  +  7*256;
   SQLITE_IOERR_UNLOCK            : constant := SQLITE_IOERR  +  8*256;
   SQLITE_IOERR_RDLOCK            : constant := SQLITE_IOERR  +  9*256;
   SQLITE_IOERR_DELETE            : constant := SQLITE_IOERR  + 10*256;
   SQLITE_IOERR_BLOCKED           : constant := SQLITE_IOERR  + 11*256;
   SQLITE_IOERR_NOMEM             : constant := SQLITE_IOERR  + 12*256;
   SQLITE_IOERR_ACCESS            : constant := SQLITE_IOERR  + 13*256;
   SQLITE_IOERR_CHECKRESERVEDLOCK : constant := SQLITE_IOERR  + 14*256;
   SQLITE_IOERR_LOCK              : constant := SQLITE_IOERR  + 15*256;
   SQLITE_IOERR_CLOSE             : constant := SQLITE_IOERR  + 16*256;
   SQLITE_IOERR_DIR_CLOSE         : constant := SQLITE_IOERR  + 17*256;
   SQLITE_LOCKED_SHAREDCACHE      : constant := SQLITE_LOCKED +  1*256;

   type Raw_Address is mod 2**Standard'Address_Size;
   SQLITE_TRANSIENT : constant Raw_Address := Raw_Address'Last; -- (-1)

   function To_Address is
      new Ada.Unchecked_Conversion (Raw_Address, Address);

   function Message (Result : int) return String;

   procedure Check (Connection : SQLite_Handle; Result : int) is
      function Message return String is
         Code : constant String := '[' & Image (Integer (Result)) & ']';
      begin
         if Connection = null then
            return Message (Result) & ' ' & Code;
         else
            declare
               function Internal (Error : int)
                  return chars_ptr;
               pragma Import (C, Internal, "sqlite3_errstr");
               Text : constant chars_ptr := Internal (Result);
            begin
               if Text = Null_Ptr then
                  return Message (Result) & ' ' & Code;
               else
                  return Value (Text) & ' ' & Code;
               end if;
            end;
         end if;
      end Message;
   begin
      case Result is
         when SQLITE_OK =>
            return;
         when SQLITE_PERM | SQLITE_CANTOPEN =>
            Raise_Exception (Use_Error'Identity, Message);
         when SQLITE_ABORT    | SQLITE_BUSY | SQLITE_LOCKED |
              SQLITE_READONLY | SQLITE_AUTH =>
            Raise_Exception (Status_Error'Identity, Message);
         when SQLITE_NOTFOUND | SQLITE_ERROR =>
            Raise_Exception (End_Error'Identity, Message);
         when SQLITE_RANGE =>
            Raise_Exception (Constraint_Error'Identity, Message);
         when others =>
            Raise_Exception (Data_Error'Identity, Message);
      end case;
   end Check;

   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : double
             )  is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int;
                  Value     : double
               )  return int;
      pragma Import (C, Internal, "sqlite3_bind_double");
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
   begin
      Check
      (  Ptr (Object.Base).Handle,
         Internal (Object.Handle, int (Parameter), Value)
      );
   end Bind;

   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : int
             )  is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int;
                  Value     : int
               )  return int;
      pragma Import (C, Internal, "sqlite3_bind_int");
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
   begin
      Check
      (  Ptr (Object.Base).Handle,
         Internal (Object.Handle, int (Parameter), Value)
      );
   end Bind;

   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : Integer_64
             )  is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int;
                  Value     : Integer_64
               )  return int;
      pragma Import (C, Internal, "sqlite3_bind_int64");
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
   begin
      Check
      (  Ptr (Object.Base).Handle,
         Internal (Object.Handle, int (Parameter), Value)
      );
   end Bind;

   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : Stream_Element_Array
             )  is
      pragma Assert ((Stream_Element'Size) mod 8 = 0);
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
   begin
      if Value'Length = 0 then
         declare
            function Internal
                     (  Statement : SQLite_Handle;
                        Index     : int;
                        Value     : Address := Null_Address;
                        Length    : int     := 0;
                        Copy      : Address := Null_Address
                     )  return int; -- SQLITE_STATIC
            pragma Import (C, Internal, "sqlite3_bind_blob");
         begin
            Check
            (  Ptr (Object.Base).Handle,
               Internal (Object.Handle, int (Parameter))
            );
         end;
      else
         declare
            function Internal
                     (  Statement : SQLite_Handle;
                        Index     : int;
                        Value     : Address;
                        Length    : int;
                        Copy      : Raw_Address := SQLITE_TRANSIENT
                     )  return int;
            pragma Import (C, Internal, "sqlite3_bind_blob");
         begin
            Check
            (  Ptr (Object.Base).Handle,
               Internal
               (  Object.Handle,
                  int (Parameter),
                  Value (Value'First)'Address,
                  (Value'Length * Stream_Element'Size + 7) / 8
            )  );
         end;
      end if;
   end Bind;

   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : access Stream_Element_Array
             )  is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int;
                  Value     : Address;
                  Length    : int;
                  Copy      : Address := Null_Address
               )  return int; -- SQLITE_STATIC
      pragma Import (C, Internal, "sqlite3_bind_blob");
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
   begin
      Check
      (  Ptr (Object.Base).Handle,
         Internal
         (  Object.Handle,
            int (Parameter),
            Value (Value'First)'Address,
            (Value'Length * Stream_Element'Size + 7) / 8
      )  );
   end Bind;

   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : String
             )  is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int;
                  Value     : char_array;
                  Length    : int;
                  Copy      : Raw_Address := SQLITE_TRANSIENT
               )  return int;
      pragma Import (C, Internal, "sqlite3_bind_text");
      Data   : char_array renames To_C (Value);
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
   begin
      Check
      (  Ptr (Object.Base).Handle,
         Internal
         (  Object.Handle,
            int (Parameter),
            Data,
            Value'Length
      )  );
   end Bind;

   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : access String
             )  is
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
   begin
      if Value'Length = 0 then
         declare
            function Internal
                     (  Statement : SQLite_Handle;
                        Index     : int;
                        Value     : Address := Null_Address;
                        Length    : int     := 0;
                        Copy      : Address := Null_Address
                     )  return int; -- SQLITE_STATIC
            pragma Import (C, Internal, "sqlite3_bind_text");
         begin
            Check
            (  Ptr (Object.Base).Handle,
               Internal (Object.Handle, int (Parameter))
            );
         end;
      elsif Character'Size = char'Size and then char'Size = 8 then
         declare
            function Internal
                     (  Statement : SQLite_Handle;
                        Index     : int;
                        Value     : Address;
                        Length    : int;
                        Copy      : Address := Null_Address
                     )  return int; -- SQLITE_STATIC
            pragma Import (C, Internal, "sqlite3_bind_text");
         begin
            Check
            (  Ptr (Object.Base).Handle,
               Internal
               (  Object.Handle,
                  int (Parameter),
                  Value (Value'First)'Address,
                  Value'Length
            )  );
         end;
      else
         Bind (Command, Parameter, Value.all);
      end if;
   end Bind;

   procedure Bind
             (  Command   : Statement;
                Parameter : Positive;
                Value     : chars_ptr;
                Length    : size_t
             )  is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int;
                  Value     : chars_ptr;
                  Length    : int;
                  Copy      : Address := Null_Address
               )  return int; -- SQLITE_STATIC
      pragma Import (C, Internal, "sqlite3_bind_text");
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
   begin
      Check
      (  Ptr (Object.Base).Handle,
         Internal
         (  Object.Handle,
            int (Parameter),
            Value,
            int (Length)
      )  );
   end Bind;

   procedure Bind
             (  Command   : Statement;
                Parameter : Positive
             )  is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int
               )  return int;
      pragma Import (C, Internal, "sqlite3_bind_null");
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
   begin
      Check
      (  Ptr (Object.Base).Handle,
         Internal (Object.Handle, int (Parameter))
      );
   end Bind;

   function Column
            (  Command  : Statement;
               Position : Positive
            )  return double is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int
               )  return double;
      pragma Import (C, Internal, "sqlite3_column_double");
   begin
      return Internal (Ptr (Command.Handle).Handle, int (Position) - 1);
   end Column;

   function Column
            (  Command  : Statement;
               Position : Positive
            )  return int is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int
               )  return int;
      pragma Import (C, Internal, "sqlite3_column_int");
   begin
      return Internal (Ptr (Command.Handle).Handle, int (Position) - 1);
   end Column;

   function Column
            (  Command  : Statement;
               Position : Positive
            )  return Integer_64 is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int
               )  return Integer_64;
      pragma Import (C, Internal, "sqlite3_column_int64");
   begin
      return Internal (Ptr (Command.Handle).Handle, int (Position) - 1);
   end Column;

   function Column
            (  Command  : Statement;
               Position : Positive
            )  return String is
      function Bytes
               (  Statement : SQLite_Handle;
                  Index     : int
               )  return int;
      pragma Import (C, Bytes, "sqlite3_column_bytes");
      function Text
               (  Statement : SQLite_Handle;
                  Index     : int
               )  return chars_ptr;
      pragma Import (C, Text, "sqlite3_column_text");
      Handle : constant SQLite_Handle := Ptr (Command.Handle).Handle;
      Index  : constant int       := int (Position) - 1;
      Result : constant chars_ptr := Text (Handle, Index);
      Count  : constant int       := Bytes (Handle, Index);
   begin
      if Count = 0 then
         return "";
      else
         return Value (Result, size_t (Count));
      end if;
   end Column;

   function Column
            (  Command  : Statement;
               Position : Positive
            )  return Stream_Element_Array is
      pragma Assert ((Stream_Element'Size) mod 8 = 0);
      function Bytes
               (  Statement : SQLite_Handle;
                  Index     : int
               )  return int;
      pragma Import (C, Bytes, "sqlite3_column_bytes");
      Handle : constant SQLite_Handle := Ptr (Command.Handle).Handle;
      Index  : constant int := int (Position) - 1;
      Count  : constant int := Bytes (Handle, Index);
   begin
      if Count = 0 then
         return (1..0 => Stream_Element'First);
      else
         declare
            function Internal
                     (  Statement : SQLite_Handle;
                        Index     : int
                     )  return Address;
            pragma Import (C, Internal, "sqlite3_column_blob");
            subtype Result_Array is
                    Stream_Element_Array
                    (  1
                    .. Stream_Element_Offset
                       (  (Count * 8) / Stream_Element'Size
                    )  );
            Result : Result_Array;
            pragma Import (Ada, Result);
            for Result'Address use Internal (Handle, Index);
         begin
            return Result;
         end;
      end if;
   end Column;

   function Column_Count (Command : Statement) return Natural is
      function Internal (Statement : SQLite_Handle) return int;
      pragma Import (C, Internal, "sqlite3_column_count");
   begin
      return Natural (Internal (Ptr (Command.Handle).Handle));
   end Column_Count;

   function Column_Type
            (  Command  : Statement;
               Position : Positive
            )  return Datatype is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int
               )  return int;
      pragma Import (C, Internal, "sqlite3_column_type");
      Handle : constant SQLite_Handle := Ptr (Command.Handle).Handle;
      Index  : constant int := int (Position) - 1;
   begin
      return Datatype (Internal (Handle, Index));
   end Column_Type;

   procedure Exec (Base : Data_Base; Command : String) is
      Operation : constant Statement := Prepare (Base, Command);
   begin
      while Step (Operation) loop
         null;
      end loop;
   end Exec;

   procedure Finalize (Object : in out Data_Base_Object) is
      function Internal (sqlite3 : SQLite_Handle) return int;
      pragma Import (C, Internal, "sqlite3_close");
   begin
      if Object.Handle /= null then
         Check (Object.Handle, Internal (Object.Handle));
         Object.Handle := null;
      end if;
      Finalize (Entity (Object));
   exception
      when others =>
         Object.Handle := null;
         Finalize (Entity (Object));
         raise;
   end Finalize;

   procedure Finalize (Object : in out Statement_Object) is
      function Internal (sqlite3 : SQLite_Handle) return int;
      pragma Import (C, Internal, "sqlite3_finalize");
   begin
      if Object.Handle /= null then
         Check (Object.Handle, Internal (Object.Handle));
         Object.Handle := null;
      end if;
      Finalize (Entity (Object));
   exception
      when others =>
         Object.Handle := null;
         Finalize (Entity (Object));
         raise;
   end Finalize;

   function Is_Null
            (  Command  : Statement;
               Position : Positive
            )  return Boolean is
      function Internal
               (  Statement : SQLite_Handle;
                  Index     : int
               )  return int;
      pragma Import (C, Internal, "sqlite3_column_type");
   begin
      return
         Internal (Ptr (Command.Handle).Handle, int (Position) - 1) = 5;
   end Is_Null;

   function Is_Valid (Command : Statement) return Boolean is
   begin
      return Is_Valid (Command.Handle);
   end Is_Valid;

   function Last_Insert_Row (Base : Data_Base'Class) return Row_ID is
      function Internal (Statement : SQLite_Handle) return Row_ID;
      pragma Import (C, Internal, "sqlite3_last_insert_rowid");
   begin
      return Internal (Ptr (Base.Handle).Handle);
   end Last_Insert_Row;

   function Message (Result : int) return String is
   begin
      case Result is
         when SQLITE_OK =>
            return "Success";
         when SQLITE_ERROR =>
            return "SQL error or missing database";
         when SQLITE_INTERNAL =>
            return "Internal logic error in SQLite";
         when SQLITE_PERM =>
            return "Access permission denied";
         when SQLITE_ABORT =>
            return "Callback routine requested an abort";
         when SQLITE_BUSY =>
            return "The database file is locked";
         when SQLITE_LOCKED =>
            return "A table in the database is locked";
         when SQLITE_NOMEM =>
            return "A malloc() failed";
         when SQLITE_READONLY =>
            return "Attempt to write a readonly database";
         when SQLITE_INTERRUPT =>
            return "Operation terminated by sqlite3_interrupt()";
         when SQLITE_IOERR =>
            return "Some kind of disk I/O error occurred";
         when SQLITE_CORRUPT =>
            return "The database disk image is malformed";
         when SQLITE_NOTFOUND =>
            return "NOT USED. Table or record not found";
         when SQLITE_FULL =>
            return "Insertion failed because database is full";
         when SQLITE_CANTOPEN =>
            return "Unable to open the database file";
         when SQLITE_PROTOCOL =>
            return "NOT USED. Database lock protocol error";
         when SQLITE_EMPTY =>
            return "Database is empty";
         when SQLITE_SCHEMA =>
            return "The database schema changed";
         when SQLITE_TOOBIG =>
            return "String or BLOB exceeds size limit";
         when SQLITE_CONSTRAINT =>
            return "Abort due to constraint violation";
         when SQLITE_MISMATCH =>
            return "Data type mismatch";
         when SQLITE_MISUSE =>
            return "Library used incorrectly";
         when SQLITE_NOLFS =>
            return "Uses OS features not supported on host";
         when SQLITE_AUTH =>
            return "Authorization denied";
         when SQLITE_FORMAT =>
            return "Auxiliary database format error";
         when SQLITE_RANGE =>
            return "2nd parameter to sqlite3_bind out of range";
         when SQLITE_NOTADB =>
            return "File opened that is not a database file";
         when SQLITE_ROW =>
            return "sqlite3_step() has another row ready";
         when SQLITE_DONE =>
            return "sqlite3_step() has finished executing";
         when others =>
            return "Unknown error";
      end case;
   end Message;

   function Open
            (  File_Name : String;
               Flags     : Open_Flags :=
                              READWRITE or CREATE or FULLMUTEX
            )  return Data_Base is
      function Internal
               (  filename : char_array;
                  ppDb     : access SQLite_Handle;
                  flags    : Open_Flags;
                  zVfs     : Address := Null_Address
               )  return int;
      pragma Import (C, Internal, "sqlite3_open_v2");
      Result : Data_Base;
   begin
      Set (Result.Handle, new Data_Base_Object);
      Check
      (  null,
         Internal
         (  To_C (File_Name),
            Ptr (Result.Handle).Handle'Access,
            Flags
      )  );
      return Result;
   end Open;

   function Prepare
            (  Base    : Data_Base'Class;
               Command : String
            )  return Statement is
      function Internal
               (  db     : SQLite_Handle;
                  zSql   : char_array;
                  nByte  : int;
                  ppStmt : access SQLite_Handle;
                  pzTail : Address := Null_Address
               )  return int;
      pragma Import (C, Internal, "sqlite3_prepare_v2");
      Result : Statement;
   begin
      Set (Result.Handle, new Statement_Object);
      declare
         Object : Data_Base_Object'Class renames Ptr (Base.Handle).all;
      begin
         Check
         (  Object.Handle,
            Internal
            (  Object.Handle,
               To_C (Command),
               -1,
               Ptr (Result.Handle).Handle'Access
         )  );
         Set (Ptr (Result.Handle).Base, Ptr (Base.Handle));
      end;
      return Result;
   end Prepare;

   procedure Reset (Command : Statement) is
      function Internal (pStmt : SQLite_Handle) return int;
      pragma Import (C, Internal, "sqlite3_reset");
      Result : int;  -- Ignoring result code since sqlite3_reset would
   begin             -- repeat the last execution fault
      Result := Internal (Ptr (Command.Handle).Handle);
   end Reset;

   function Step (Command : Statement) return Boolean is
      function Internal (pStmt : SQLite_Handle) return int;
      pragma Import (C, Internal, "sqlite3_step");
      Object : Statement_Object'Class renames Ptr (Command.Handle).all;
      Result : constant int := Internal (Object.Handle);
   begin
      case Result is
         when SQLITE_ROW  =>
            return True;
         when SQLITE_DONE =>
            return False;
         when others =>
            Check (Ptr (Object.Base).Handle, Result);
            return False;
      end case;
   end Step;

   procedure Step (Command : Statement) is
   begin
      if Step (Command) then
         null;
      end if;
   end Step;

   function Table_Exists
            (  Base : Data_Base;
               Name : String
            )  return Boolean is
      Command : constant Statement :=
                Prepare
                (  Base,
                   (  "SELECT * FROM sqlite_master "
                   &  "WHERE type='table' AND name=?"
                )  );
   begin
      Bind (Command, 1, Name);
      return Step (Command);
   exception
      when End_Error =>
         return False;
   end Table_Exists;

end SQLite;
