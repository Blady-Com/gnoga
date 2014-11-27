--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_SQLite_Benchmark                       Luebeck            --
--  Benchmark SQLite vs B-trees                    Autumn, 2014       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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

with Ada.Calendar;           use Ada.Calendar;
with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Strings_Edit.Floats;    use Strings_Edit.Floats;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with SQLite;                 use SQLite;

with Ada.Numerics.Discrete_Random;
with Persistent.Blocking_Files.Transactional.Dump;
with Persistent.Memory_Pools.Dump;
with Test_String_B_Trees;

procedure Test_SQLite_Benchmark is
   Count   : constant := 1000;

   use Test_String_B_Trees.External;
   use Persistent.Blocking_Files;
   use Persistent.Blocking_Files.Transactional;
   use Persistent.Blocking_Files.Transactional.Dump;
   use Persistent.Memory_Pools;
   use Persistent.Memory_Pools.Dump;

   subtype Token_Length is Integer range 1..100;
   subtype Token_Letter is Character range 'A'..'Z';
   package Random_Letter is
      new Ada.Numerics.Discrete_Random (Token_Letter);
   use Random_Letter;
   package Random_Length is
      new Ada.Numerics.Discrete_Random (Token_Length);
   use Random_Length;

   type Benchmark_Type is mod 2**2;
   Benchmark_SQLite : constant Benchmark_Type := 2**0;
   Benchmark_B_Tree : constant Benchmark_Type := 2**1;
   Benchmark_All    : constant Benchmark_Type := Benchmark_Type'Last;

--   Benchmark : constant Benchmark_Type := Benchmark_B_Tree;
   Benchmark : constant Benchmark_Type := Benchmark_All;

   Letter_Dice  : Random_Letter.Generator;
   Letter_State : Random_Letter.State;
   Length_Dice  : Random_Length.Generator;
   Length_State : Random_Length.State;

   function Create_Key (Index : Natural) return String is
      Result : String (1..Random (Length_Dice));
   begin
      for Index in Result'Range loop
         Result (Index) := Random (Letter_Dice);
      end loop;
      return Result;
   end Create_Key;

   function Create_Value (Index : Natural) return String
      renames Create_Key;

   Start        : Time;
   Tree_Address : Byte_Index;
   Seconds      : Duration;
begin
   Reset (Letter_Dice, 1);
   Save  (Letter_Dice, Letter_State);
   Reset (Length_Dice, 1);
   Save  (Length_Dice, Length_State);
------------------------------------------------------------------------
   Put_Line ("Insertion test");
   if 0 /= (Benchmark and Benchmark_SQLite) then
      declare
         DB : Data_Base;
      begin
         DB := Open ("sqlite_benchmark.db");
         Exec (DB, "PRAGMA journal_mode=WAL; PRAGMA synchronous=FULL");
         Exec (DB, "DROP TABLE IF EXISTS test_table");
         Exec
         (  DB,
            "CREATE TABLE test_table (key TEXT PRIMARY KEY, value TEXT)"
         );
         declare
            Command : Statement :=
                      Prepare
                      (  DB,
                         "INSERT INTO test_table VALUES (?, ?)"
                      );
         begin
            Reset (Letter_Dice, Letter_State);
            Reset (Length_Dice, Length_State);
            Start := Clock;
            for Index in 1..Count loop
               Exec (DB, "BEGIN TRANSACTION;");
               declare
                  Key   : aliased String := Create_Key (Index);
                  Value : aliased String := Create_Value (Index);
               begin
                  Bind (Command, 1, Key'Access);
                  Bind (Command, 2, Value'Access);
                  Step (Command);
                  Reset (Command);
               end;
               Exec (DB, "COMMIT");
            end loop;
            Seconds := Clock - Start;
         end;
         Put_Line
         (  "SQLite "
         &  Image (Count)
         &  " prepared INSERTs, "
         &  Image (1000.0 * (Float (Seconds) / Float (Count)))
         &  "ms per operation"
         );
      end;
   end if;
   if 0 /= (Benchmark and Benchmark_B_Tree) then
      declare
         DB : aliased Persistent_Transactional_Array;
      begin
         Open
         (  Container => DB,
            Name      => "test_benchmark.db",
            Mode      => Create_Mode,
            Map_Size  => Get_Map_Size (Count),
            Hash_Size => 1024
         );
         declare
            Pool : aliased Persistent_Pool (DB'Access);
            Tree : B_Tree (Pool'Access);
         begin
            Reset (Letter_Dice, Letter_State);
            Reset (Length_Dice, Length_State);
            Start := Clock;
            for Index in 1..Count loop
               declare
                  Key   : aliased String := Create_Key (Index);
                  Value : aliased String := Create_Value (Index);
               begin
                  Add (Tree, Key, Value);
                  Commit (Pool);
               end;
            end loop;
            Seconds := Clock - Start;
            Tree_Address := Get_Root_Address (Tree);
            Put_Line
            (  "Blocking file "
            &  Image (Count)
            &  " committed insertions, "
            &  Image (1000.0 * (Float (Seconds) / Float (Count)))
            &  "ms per operation. "
            &  "File size"
            &  Block_Count'Image (Get_Block_Size (DB))
            );
         end;
      end;
   end if;
------------------------------------------------------------------------
   Put_Line ("Search test");
   if 0 /= (Benchmark and Benchmark_SQLite) then
      declare
         DB : Data_Base;
      begin
         DB := Open ("sqlite_benchmark.db", READWRITE or FULLMUTEX);
         Exec (DB, "PRAGMA journal_mode=WAL; PRAGMA synchronous=FULL");
         declare
            Command : Statement :=
                      Prepare
                      (  DB,
                         "SELECT value FROM test_table WHERE key=?"
                      );
         begin
            Reset (Letter_Dice, Letter_State);
            Reset (Length_Dice, Length_State);
            Start := Clock;
            for Index in 1..Count loop
               declare
                  Key   : aliased String := Create_Key (Index);
                  Value : aliased String := Create_Value (Index);
               begin
                  Bind (Command, 1, Key'Access);
                  Step (Command);
                  if Value /= Column (Command, 1) then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Error in SELECT, searched for "
                        &  Key
                        &  ", found "
                        &  Column (Command, 1)
                        &  ", expected "
                        &  Value
                     )  );
                  end if;
                  Reset (Command);
               end;
            end loop;
            Seconds := Clock - Start;
         end;
         Put_Line
         (  "SQLite "
         &  Image (Count)
         &  " prepared committed SELECTs, "
         &  Image (1000.0 * (Float (Seconds) / Float (Count)))
         &  "ms per operation"
         );
      end;
   end if;
   if 0 /= (Benchmark and Benchmark_B_Tree) then
      declare
         DB : aliased Persistent_Transactional_Array;
      begin
         Open
         (  Container => DB,
            Name      => "test_benchmark.db",
            Mode      => Read_Mode,
            Map_Size  => Get_Map_Size (Count),
            Hash_Size => 1024
         );
         declare
            Pool : aliased Persistent_Pool (DB'Access);
            Tree : B_Tree (Pool'Access);
         begin
            Set_Root_Address (Tree, Tree_Address);
            Reset (Letter_Dice, Letter_State);
            Reset (Length_Dice, Length_State);
            Start := Clock;
            for Index in 1..Count loop
               declare
                  Key   : aliased String := Create_Key (Index);
                  Value : aliased String := Create_Value (Index);
               begin
                  if Get (Tree, Key) /= Value then
                     Raise_Exception
                     (  Data_Error'Identity,
                        (  "Error in Find, searched for "
                        &  Key
                        &  ", found "
                        &  Get (Tree, Key)
                        &  ", expected "
                        &  Value
                     )  );
                  end if;
               end;
            end loop;
            Seconds := Clock - Start;
            Put_Line
            (  "Blocking file "
            &  Image (Count)
            &  " searches, "
            &  Image (1000.0 * (Float (Seconds) / Float (Count)))
            &  "ms per operation"
            );
         end;
      end;
   end if;
------------------------------------------------------------------------
   Put_Line ("Update test");
   if 0 /= (Benchmark and Benchmark_SQLite) then
      declare
         DB : Data_Base;
      begin
         DB := Open ("sqlite_benchmark.db", READWRITE or FULLMUTEX);
         Exec (DB, "PRAGMA journal_mode=WAL; PRAGMA synchronous=FULL");
         declare
            Command : Statement :=
                      Prepare
                      (  DB,
                         "UPDATE test_table SET value=? WHERE key=?"
                      );
         begin
            Reset (Letter_Dice, Letter_State);
            Reset (Length_Dice, Length_State);
            Start := Clock;
            for Index in 1..Count loop
               Exec (DB, "BEGIN TRANSACTION;");
               declare
                  Key   : aliased String := Create_Key (Index);
                  Value : aliased String := 2 * Create_Value (Index);
               begin
                  Bind (Command, 1, Value'Access);
                  Bind (Command, 2, Key'Access);
                  Step (Command);
                  Reset (Command);
               end;
               Exec (DB, "COMMIT");
            end loop;
            Seconds := Clock - Start;
         end;
         Put_Line
         (  "SQLite "
         &  Image (Count)
         &  " prepared committed UPDATESs, "
         &  Image (1000.0 * (Float (Seconds) / Float (Count)))
         &  "ms per operation"
         );
      end;
   end if;
   if 0 /= (Benchmark and Benchmark_B_Tree) then
      declare
         DB : aliased Persistent_Transactional_Array;
      begin
         Open
         (  Container => DB,
            Name      => "test_benchmark.db",
            Mode      => Read_Write_Mode,
            Map_Size  => Get_Map_Size (Count),
            Hash_Size => 1024
         );
         declare
            Pool : aliased Persistent_Pool (DB'Access);
            Tree : B_Tree (Pool'Access);
         begin
            Set_Root_Address (Tree, Tree_Address);
            Reset (Letter_Dice, Letter_State);
            Reset (Length_Dice, Length_State);
            Start := Clock;
            for Index in 1..Count loop
               declare
                  Key   : aliased String := Create_Key (Index);
                  Value : aliased String := 2 * Create_Value (Index);
               begin
                  Replace (Tree, Key, Value);
                  Commit (Pool);
               end;
            end loop;
            Seconds := Clock - Start;
            Put_Line
            (  "Blocking file "
            &  Image (Count)
            &  " committed updates, "
            &  Image (1000.0 * (Float (Seconds) / Float (Count)))
            &  "ms per operation"
            );
         end;
      end;
   end if;
------------------------------------------------------------------------
   Put_Line ("Delete test");
   if 0 /= (Benchmark and Benchmark_SQLite) then
      declare
         DB : Data_Base;
      begin
         DB := Open ("sqlite_benchmark.db", READWRITE or FULLMUTEX);
         Exec (DB, "PRAGMA journal_mode=WAL; PRAGMA synchronous=FULL");
         declare
            Command : Statement :=
                      Prepare
                      (  DB,
                         "DELETE FROM test_table WHERE key=?"
                      );
         begin
            Reset (Letter_Dice, Letter_State);
            Reset (Length_Dice, Length_State);
            Start := Clock;
            for Index in 1..Count / 2 loop
--               Exec (DB, "BEGIN TRANSACTION;");
               declare
                  Key   : aliased String := Create_Key (Index);
                  Value : aliased String := Create_Value (Index);
               begin
                  Bind (Command, 1, Key'Access);
                  Step (Command);
                  Reset (Command);
               end;
--               Exec (DB, "COMMIT");
            end loop;
            Seconds := Clock - Start;
         end;
         Put_Line
         (  "SQLite "
         &  Image (Count / 2)
         &  " prepared committed DELETEs, "
         &  Image (1000.0 * (Float (Seconds) / Float (Count / 2)))
         &  "ms per operation"
         );
      end;
   end if;
   if 0 /= (Benchmark and Benchmark_B_Tree) then
      declare
         DB : aliased Persistent_Transactional_Array;
      begin
         Open
         (  Container => DB,
            Name      => "test_benchmark.db",
            Mode      => Read_Write_Mode,
            Map_Size  => Get_Map_Size (Count),
            Hash_Size => 1024
         );
         declare
            Pool : aliased Persistent_Pool (DB'Access);
            Tree : B_Tree (Pool'Access);
         begin
            Set_Root_Address (Tree, Tree_Address);
            Reset (Letter_Dice, Letter_State);
            Reset (Length_Dice, Length_State);
            Start := Clock;
            for Index in 1..Count / 2 loop
               declare
                  Key   : aliased String := Create_Key (Index);
                  Value : aliased String := Create_Value (Index);
               begin
                  Remove (Tree, Key);
                  Commit (Pool);
               end;
            end loop;
            Seconds := Clock - Start;
            Put_Line
            (  "Blocking file "
            &  Image (Count / 2)
            &  " committed deletes, "
            &  Image (1000.0 * (Float (Seconds) / Float (Count / 2)))
            &  "ms per operation"
            );
         end;
      end;
   end if;
exception
   when Error : others =>
      Put_Line ("Fault: " & Exception_Information (Error));
end Test_SQLite_Benchmark;
