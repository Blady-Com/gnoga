--                                                                    --
--  procedure Test_B_Trees          Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Autumn, 2014       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Calendar;                  use Ada.Calendar;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;
with Persistent.Blocking_Files;     use Persistent.Blocking_Files;
with Persistent.Memory_Pools;       use Persistent.Memory_Pools;
with Persistent.Memory_Pools.Dump;  use Persistent.Memory_Pools.Dump;
with Strings_Edit;                  use Strings_Edit;
with Strings_Edit.Floats;           use Strings_Edit.Floats;
with Strings_Edit.Integers;         use Strings_Edit.Integers;
with System.Storage_Elements;       use System.Storage_Elements;
with Test_Integer_Sets;             use Test_Integer_Sets;

with Persistent.Blocking_Files.Text_IO;
with Persistent.Memory_Pools.Streams.External_B_Tree;
with Test_Integer_B_Trees;
with Test_String_B_Trees;

procedure Test_B_Trees is
   use Persistent.Blocking_Files.Text_IO;

   procedure Dump (Tree : Test_Integer_B_Trees.Internal.B_Tree) is
      use Test_Integer_B_Trees.Internal;
      This : Item_Ptr := Get_First (Tree);
   begin
      while This /= No_Item loop
         Put_Line
         (  Image (Get_Key (This))
         &  "->"
         &  Image (Get_Value (This))
         &  " at "
         &  Image (Get_Index (This))
         &  "/"
         &  Image (Get_Bucket_Size (This))
         &  Integer_Address'Image
            (  To_Integer (Get_Bucket_Address (This))
         )  );
         This := Get_Next (This);
      end loop;
   end Dump;

   procedure Dump (Tree : Test_Integer_B_Trees.External.B_Tree) is
      use Test_Integer_B_Trees.External;
      This : Item_Ptr := Get_First (Tree);
   begin
      while This /= No_Item loop
         Put_Line
         (  Image (Get_Key (This))
         &  "->"
         &  Image (Get_Value (This))
         &  " at "
         &  Image (Get_Index (This))
         &  "/"
         &  Image (Get_Bucket_Size (This))
         &  " "
         &  Image (Get_Bucket_Address (This))
         );
         This := Get_Next (This);
      end loop;
   end Dump;

   procedure Dump (Tree : Test_Integer_B_Trees.External_Ptr.B_Tree) is
      use Test_Integer_B_Trees.External_Ptr;
      This : Item_Ptr := Get_First (Tree);
   begin
      while This /= No_Item loop
         Put_Line
         (  Image (Get_Key (This))
         &  "->"
         &  Image (Integer (Get_Pointer (This)))
         &  " at "
         &  Image (Get_Index (This))
         &  "/"
         &  Image (Get_Bucket_Size (This))
         &  " "
         &  Image (Get_Bucket_Address (This))
         );
         This := Get_Next (This);
      end loop;
   end Dump;

   procedure Dump (Tree : Test_String_B_Trees.Internal.B_Tree) is
      use Test_String_B_Trees.Internal;
      This : Item_Ptr := Get_First (Tree);
   begin
      while This /= No_Item loop
         Put_Line
         (  Get_Key (This)
         &  "->"
         &  Image (Get_Value (This))
         &  " at "
         &  Image (Get_Index (This))
         &  "/"
         &  Image (Get_Bucket_Size (This))
         &  Integer_Address'Image
            (  To_Integer (Get_Bucket_Address (This))
         )  );
         This := Get_Next (This);
      end loop;
   end Dump;

   procedure Dump_Reversed
             (  Tree : Test_Integer_B_Trees.Internal.B_Tree
             )  is
      use Test_Integer_B_Trees.Internal;
      This : Item_Ptr := Get_Last (Tree);
   begin
      while This /= No_Item loop
         Put_Line
         (  Image (Get_Key (This))
         &  "->"
         &  Image (Get_Value (This))
         &  " at "
         &  Image (Get_Index (This))
         &  "/"
         &  Image (Get_Bucket_Size (This))
         &  Integer_Address'Image
            (  To_Integer (Get_Bucket_Address (This))
         )  );
         This := Get_Previous (This);
      end loop;
   end Dump_Reversed;

   type Integer_Array is array (Positive range <>) of Integer;

   function "+" (Tree : Test_Integer_B_Trees.Internal.B_Tree)
      return Integer_Array is
      use Test_Integer_B_Trees.Internal;
      This  : Item_Ptr := Get_First (Tree);
      Count : Natural  := 0;
   begin
      while This /= No_Item loop
         Count := Count + 1;
         This  := Get_Next (This);
      end loop;
      declare
         Result : Integer_Array (1..Count);
      begin
         Count := 1;
         This  := Get_First (Tree);
         while This /= No_Item loop
            This := Get_Next (This);
            Result (Count) := Get_Key (This);
            Count := Count + 1;
         end loop;
         return Result;
      end;
   end "+";

   function Image (List : Integer_Array) return String is
      Text    : String (1..2048);
      Pointer : Integer := 1;
   begin
      for Index in List'Range loop
         if Pointer > 1 then
            Put (Text, Pointer, ", ");
         end if;
         Put (Text, Pointer, List (Index));
      end loop;
      return Text (1..Pointer - 1);
   end Image;

   procedure Check
             (  T    : Test_Integer_B_Trees.Internal.B_Tree;
                List : Integer_Array
             )  is
      use Test_Integer_B_Trees.Internal;
      This : Item_Ptr := Get_First (T);
   begin
      for Index in List'Range loop
         declare
            Node : constant Integer := List (Index);
         begin
            if This = No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Missing key " & Image (Node)
               );
            elsif Node /= Get_Key (This) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Key "
                  &  Image (Get_Key (This))
                  &  " /= "
                  &  Image (Node)
                  &  " (expected)"
               )  );
            end if;
            This := Get_Next (This);
         end;
      end loop;
      if This /= No_Item then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Unexpected key " & Image (Get_Key (This))
         );
      end if;
   end Check;

   procedure Check
             (  T    : Test_Integer_B_Trees.External.B_Tree;
                List : Integer_Array
             )  is
      use Test_Integer_B_Trees.External;
      This : Item_Ptr := Get_First (T);
   begin
      for Index in List'Range loop
         declare
            Node : constant Integer := List (Index);
         begin
            if This = No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Missing key " & Image (Node)
               );
            elsif Node /= Get_Key (This) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Key "
                  &  Image (Get_Key (This))
                  &  " /= "
                  &  Image (Node)
                  &  " (expected)"
               )  );
            end if;
            This := Get_Next (This);
         end;
      end loop;
      if This /= No_Item then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Unexpected key " & Image (Get_Key (This))
         );
      end if;
   end Check;

   procedure Check
             (  T    : Test_Integer_B_Trees.External_Ptr.B_Tree;
                List : Integer_Array
             )  is
      use Test_Integer_B_Trees.External_Ptr;
      This : Item_Ptr := Get_First (T);
   begin
      for Index in List'Range loop
         declare
            Node : constant Integer := List (Index);
         begin
            if This = No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Missing key " & Image (Node)
               );
            elsif Node /= Get_Key (This) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Key "
                  &  Image (Get_Key (This))
                  &  " /= "
                  &  Image (Node)
                  &  " (expected)"
               )  );
            elsif Byte_Index (Node) /= Get_Pointer (This) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Key "
                  &  Image (Get_Key (This))
                  &  ", Pointer"
                  &  Byte_Index'Image (Get_Pointer (This))
                  &  " /= "
                  &  Image (Node)
                  &  " (expected)"
               )  );
            end if;
            This := Get_Next (This);
         end;
      end loop;
      if This /= No_Item then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Unexpected key " & Image (Get_Key (This))
         );
      end if;
   end Check;

   procedure Check
             (  T    : Persistent.Memory_Pools.Streams.
                       External_B_Tree.B_Tree;
                List : Integer_Array
             )  is
      use Persistent.Memory_Pools.Streams.External_B_Tree;
      This : Item_Ptr := Get_First (T);
   begin
      for Index in List'Range loop
         declare
            Node : constant Integer := List (Index);
         begin
            if This = No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Missing key " & Image (Node)
               );
            elsif Byte_Index (Node) /= Get_Key (This) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Key "
                  &  Image (Get_Key (This))
                  &  " /= "
                  &  Image (Node)
                  &  " (expected)"
               )  );
            elsif Byte_Index (Node) /= Get_Value (This) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Key "
                  &  Image (Get_Key (This))
                  &  ", Pointer"
                  &  Byte_Index'Image (Get_Value (This))
                  &  " /= "
                  &  Image (Node)
                  &  " (expected)"
               )  );
            end if;
            This := Get_Next (This);
         end;
      end loop;
      if This /= No_Item then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Unexpected key " & Image (Get_Key (This))
         );
      end if;
   end Check;

   procedure Check
             (  T     : Test_Integer_B_Trees.Tables.Table;
                Key   : Test_Integer_B_Trees.Keys;
                Value : Test_Integer_B_Trees.Values;
                List  : Integer_Array
             )  is
      use Test_Integer_B_Trees;
      use Test_Integer_B_Trees.Tables;
      This : Row_Ptr := Get_First (T, Key);
   begin
      for Index in List'Range loop
         declare
            Node : constant Integer := List (Index);
         begin
            if This = No_Row then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Missing key " & Image (Node)
               );
            elsif Byte_Index (Node) /= Get_Key (This, Key) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Key "
                  &  Image (Get_Key (This, Key))
                  &  " /= "
                  &  Image (Node)
                  &  " (expected)"
               )  );
            elsif Byte_Index (Node) /= Get_Value (This, Value) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Key "
                  &  Image (Get_Key (This, Key))
                  &  ", Pointer"
                  &  Byte_Index'Image (Get_Value (This, Value))
                  &  " /= "
                  &  Image (Node)
                  &  " (expected)"
               )  );
            end if;
            This := Get_Next (This, Key);
         end;
      end loop;
      if This /= No_Row then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Unexpected key " & Image (Get_Key (This, Key))
         );
      end if;
   end Check;

   procedure Check_Inf
             (  T : Persistent.Memory_Pools.Streams.
                    External_B_Tree.B_Tree;
                Key      : Integer;
                Expected : Integer := Integer'Last
             )  is
      use Persistent.Memory_Pools.Streams.External_B_Tree;
      Item : Item_Ptr;
   begin
      Item := Inf (T, Byte_Index (Key));
      if Item = No_Item then
         if Expected /= Integer'Last then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in Inf: searching for "
               &  Image (Key)
               &  " found nothing, expected"
               &  Image (Expected)
            )  );
         end if;
      elsif Expected = Integer'Last then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Inf: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Integer (Get_Key (Item)))
            & ", expected nothing"
         )  );
      elsif Integer (Get_Key (Item)) /= Expected then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Inf: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Integer (Get_Key (Item)))
            & ", expected "
            &  Image (Expected)
         )  );
      end if;
   end Check_Inf;

   procedure Check_Inf
             (  T        : Test_Integer_B_Trees.Internal.B_Tree;
                Key      : Integer;
                Expected : Integer := Integer'Last
             )  is
      use Test_Integer_B_Trees.Internal;
      Item : Item_Ptr;
   begin
      Item := Inf (T, Key);
      if Item = No_Item then
         if Expected /= Integer'Last then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in Inf: searching for "
               &  Image (Key)
               &  " found nothing, expected"
               &  Image (Expected)
            )  );
         end if;
      elsif Expected = Integer'Last then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Inf: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Get_Key (Item))
            & ", expected nothing"
         )  );
      elsif Get_Key (Item) /= Expected then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Inf: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Get_Key (Item))
            & ", expected "
            &  Image (Expected)
         )  );
      end if;
   end Check_Inf;

   procedure Check_Inf
             (  T        : Test_Integer_B_Trees.Tables.Table;
                Index    : Test_Integer_B_Trees.Keys;
                Key      : Integer;
                Expected : Integer := Integer'Last
             )  is
      use Test_Integer_B_Trees;
      use Test_Integer_B_Trees.Tables;
      Item : Row_Ptr;
   begin
      Item := Inf (T, Index, Byte_Index (Key));
      if Item = No_Row then
         if Expected /= Integer'Last then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in Inf: searching for "
               &  Image (Key)
               &  " found nothing, expected"
               &  Image (Expected)
            )  );
         end if;
      elsif Expected = Integer'Last then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Inf: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Get_Key (Item, Index))
            & ", expected nothing"
         )  );
      elsif Get_Key (Item, Index) /= Byte_Index (Expected) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Inf: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Get_Key (Item, Index))
            & ", expected "
            &  Image (Expected)
         )  );
      end if;
   end Check_Inf;

   procedure Check_Sup
             (  T        : Test_Integer_B_Trees.Tables.Table;
                Index    : Test_Integer_B_Trees.Keys;
                Key      : Integer;
                Expected : Integer := Integer'Last
             )  is
      use Test_Integer_B_Trees.Tables;
      Item : Row_Ptr;
   begin
      Item := Sup (T, Index, Byte_Index (Key));
      if Item = No_Row then
         if Expected /= Integer'Last then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in Sup: searching for "
               &  Image (Key)
               &  " found nothing, expected"
               &  Image (Expected)
            )  );
         end if;
      elsif Expected = Integer'Last then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Sup: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Get_Key (Item, Index))
            & ", expected nothing"
         )  );
      elsif Integer (Get_Key (Item, Index)) /= Expected then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Sup: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Integer (Get_Key (Item, Index)))
            & ", expected "
            &  Image (Expected)
         )  );
      end if;
   end Check_Sup;

   procedure Check_Sup
             (  T        : Persistent.Memory_Pools.Streams.
                           External_B_Tree.B_Tree;
                Key      : Integer;
                Expected : Integer := Integer'Last
             )  is
      use Persistent.Memory_Pools.Streams.External_B_Tree;
      Item : Item_Ptr;
   begin
      Item := Sup (T, Byte_Index (Key));
      if Item = No_Item then
         if Expected /= Integer'Last then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in Sup: searching for "
               &  Image (Key)
               &  " found nothing, expected"
               &  Image (Expected)
            )  );
         end if;
      elsif Expected = Integer'Last then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Sup: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Get_Key (Item))
            & ", expected nothing"
         )  );
      elsif Integer (Get_Key (Item)) /= Expected then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Sup: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Integer (Get_Key (Item)))
            & ", expected "
            &  Image (Expected)
         )  );
      end if;
   end Check_Sup;

   procedure Check_Sup
             (  T        : Test_Integer_B_Trees.Internal.B_Tree;
                Key      : Integer;
                Expected : Integer := Integer'Last
             )  is
      use Test_Integer_B_Trees.Internal;
      Item : Item_Ptr;
   begin
      Item := Sup (T, Key);
      if Item = No_Item then
         if Expected /= Integer'Last then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in Sup: searching for "
               &  Image (Key)
               &  " found nothing, expected"
               &  Image (Expected)
            )  );
         end if;
      elsif Expected = Integer'Last then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Sup: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Get_Key (Item))
            & ", expected nothing"
         )  );
      elsif Get_Key (Item) /= Expected then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in Sup: searching for "
            &  Image (Key)
            &  ", found "
            &  Image (Get_Key (Item))
            & ", expected "
            &  Image (Expected)
         )  );
      end if;
   end Check_Sup;

   Start  : Time;
   Stop   : Time;
   D1, D2 : Duration;
begin
   Put_Line ("Testing B-trees ...");
   declare
      use Test_Integer_B_Trees;
      use Test_Integer_B_Trees.Tables;
      Count : constant := 10_000;
      File  : aliased Persistent_Array;
   begin
      Put ("Testing external raw tables ...");
      Open (File, 10);
      declare
         Pool : aliased Persistent_Pool (File'Access);
         T    : Table (Pool'Access);

         procedure Check (From, To : Integer) is
            This : Row_Ptr := Get_First (T, Primary);
         begin
            for Node in From..To loop
               if This = No_Row then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Missing reverse key " & Image (Node)
                  );
               elsif Byte_Index (Node) /= Get_Key (This, Primary) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Key "
                     &  Image (Get_Key (This, Primary))
                     &  " /= "
                     &  Image (Node)
                     &  " (expected)"
                  )  );
               end if;
               This := Get_Next (This, Primary);
            end loop;
            if This /= No_Row then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Unexpected reverse key "
                  &  Image (Get_Key (This, Primary))
               )  );
            end if;
         end Check;
      begin
         for Index in 1..15 loop   --        4       8        12
            Add                    --  1 2 3   5 6 7   9 10 11  13 14 15
            (  T,
               (Byte_Index (Index), Byte_Index (Count - Index)),
               (others => Byte_Index (Index))
            );
         end loop;
         Remove (T, Primary, 8);
         Check
         (  T,
            Primary,
            1,
            (1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15)
         );
         Erase (T);
         Add (T, (1, Count - 1), (others => 1));
         Add (T, (3, Count - 3), (others => 3));
         Add (T, (5, Count - 5), (others => 5));
         Add (T, (2, Count - 2), (others => 2));
         Check (T, Primary, 1, (1, 2, 3, 5));
         Remove (T, Primary, 5);
         Check  (T, Primary, 1, (1, 2, 3));
         Remove (T, Primary, 3);
         Check  (T, Primary, 1, (1, 2));
         Remove (T, Primary, 2);
         Check  (T, Primary, 1, (1=> 1));
         Remove (T, Primary, 1);
         Erase  (T);

         Add (T, (1, Count - 1), (others => 1));
         Add (T, (3, Count - 3), (others => 3));
         Add (T, (5, Count - 5), (others => 5));
         Add (T, (0, Count - 0), (others => 0));
         Check (T, Primary, 1, (0, 1, 3, 5));
         Erase (T);

         Add (T, (10, Count - 10), (others => 10));
         Add (T, (20, Count - 20), (others => 20));
         Add (T, (30, Count - 30), (others => 30));
         Add (T, (40, Count - 40), (others => 40));
         Add (T, (50, Count - 50), (others => 50));
         Add (T, (60, Count - 60), (others => 60));
         Add (T, (70, Count - 70), (others => 70));
         Add (T, (11, Count - 11), (others => 11));
         Check (T, Primary, 1, (10, 11, 20, 30, 40, 50, 60, 70));
         Check_Sup (T, Primary, 1, 10);
         Check_Sup (T, Primary, 12, 20);
         Check_Sup (T, Primary, 39, 40);
         Check_Sup (T, Primary, 40, 40);
         Check_Sup (T, Primary, 69, 70);
         Check_Sup (T, Primary, 70, 70);
         Check_Sup (T, Primary, 71);
         Check_Inf (T, Primary, 1);
         Check_Inf (T, Primary, 10, 10);
         Check_Inf (T, Primary, 12, 11);
         Check_Inf (T, Primary, 39, 30);
         Check_Inf (T, Primary, 40, 40);
         Check_Inf (T, Primary, 69, 60);
         Check_Inf (T, Primary, 70, 70);
         Check_Inf (T, Primary, 71, 70);
         Erase (T);

         Add (T, (1, Count - 1), (others => 1));
         Add (T, (2, Count - 2), (others => 2));
         Add (T, (3, Count - 3), (others => 3));
         Add (T, (4, Count - 4), (others => 4));
         Check (T, Primary, 1, (1, 2, 3, 4));

         Remove (T, Primary, 1);
         Remove (T, Primary, 3);
         Remove (T, Primary, 2);
         Remove (T, Primary, 4);
--       Dump (T);
--       Dump_Reversed (T);
         Erase (T);
         if not Is_Empty (T) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Tree is not empty after erasing"
            );
         end if;

         Start := Clock;
         for Index in Count + 1..2 * Count loop
            Add
            (  T,
               (Byte_Index (Index), Count - Byte_Index (Index)),
               (others => Byte_Index (Index))
            );
         end loop;
         D1 := (Clock - Start) / Count;
         Check (Count + 1, 2 * Count);
--       Dump (T);
         Start := Clock;
         for Index in reverse 1..Count loop
            Add
            (  T,
               (Byte_Index (Index), Count - Byte_Index (Index)),
               (others => Byte_Index (Index))
            );
         end loop;
         D2 := (Clock - Start) / Count;
         Check (1, 2*Count);
--       Dump (T);
--       Dump_Reversed (T);
         declare
            use Integer_Sets;
            List : Set;

            procedure Check is
               This : Row_Ptr := Get_First (T, Primary);
               Node : Integer;
            begin
               for Item in 1..Get_Size (List) loop
                  Node := Integer (Get (List, Item));
                  if This = No_Row then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        "Missing key " & Image (Node)
                     );
                  elsif Byte_Index (Node) /= Get_Key (This, Primary) then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        (  "Key "
                        &  Image (Get_Key (This, Primary))
                        &  " /= "
                        &  Image (Node)
                        &  " (expected)"
                     )  );
                  end if;
                  This := Get_Next (This, Primary);
               end loop;
               if This /= No_Row then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Unexpected key " & Image (Get_Key (This, Primary))
                  );
               end if;
            end Check;
            Index : Integer := 1;
         begin
            for Index in 1..2 * Count loop
               Add (List, Number (Index));
            end loop;
            while Index <= Count loop
               Remove (T, Primary, Byte_Index (Index));
               Remove (List, Number (Index));
               Index := Index + 2;
            end loop;
            Check;
            for Right in 1..20 loop
               Index := Integer (Get (List, Get_Size (List)));
               Remove (T, Primary, Byte_Index (Index));
               Remove (List, Number (Index));
            end loop;
            Check;
            for Left in 1..10 loop
               Index := Integer (Get (List, 1));
               Remove (T, Primary, Byte_Index (Index));
               Remove (List, Number (Index));
               Check;
            end loop;
            Check;
            for Middle in 1..10 loop
               Index := Integer (Get (List, 10));
               Remove (T, Primary, Byte_Index (Index));
               Remove (List, Number (Index));
               Check;
            end loop;
            Check;
            Index := Integer (Get (List, Get_Size (List) / 2));
            declare
               Item : constant Row_Ptr :=
                      Find (T, Primary, Byte_Index (Index));
            begin
               if Get_Key (Item, Primary) /= Byte_Index (Index) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Found key "
                     &  Image (Get_Key (Item, Primary))
                     &  " /= "
                     &  Image (Index)
                     &  " (expected)"
                  )  );
               end if;
               if Get_Value (Item, 1) /= Byte_Index (Index) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Found value"
                     &  Byte_Index'Image (Get_Value (Item, 1))
                     &  " /= "
                     &  Image (Index)
                     &  " (expected)"
                  )  );
               end if;
               Replace (Item, 1, 300);
               if Get_Value (Item, 1) /= 300 then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Replaced value"
                     &  Byte_Index'Image (Get_Value (Item, 1))
                     &  " /= 300 (expected)"
                  )  );
               end if;
            end;
         end;
      end;
--        Dump (T);
--        for Index in Visited'Range loop
--           if not Visited (Index) then
--              Put_Line (Coverage_Paths'Image (Index) & " not visited");
--           end if;
--        end loop;
      Put_Line (" done");
   end;
------------------------------------------------------------------------
   declare
      use Persistent.Memory_Pools.Streams.External_B_Tree;
      Count : constant := 10_000;
      File  : aliased Persistent_Array;
   begin
      Put ("Testing external raw B-trees ...");
      Open (File, 10);
      declare
         Pool : aliased Persistent_Pool (File'Access);
         T    : B_Tree (Pool'Access);

         procedure Check (From, To : Integer) is
            This : Item_Ptr := Get_First (T);
         begin
            for Node in From..To loop
               if This = No_Item then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Missing reverse key " & Image (Node)
                  );
               elsif Byte_Index (Node) /= Get_Key (This) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Key "
                     &  Image (Get_Key (This))
                     &  " /= "
                     &  Image (Node)
                     &  " (expected)"
                  )  );
               end if;
               This := Get_Next (This);
            end loop;
            if This /= No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Unexpected reverse key " & Image (Get_Key (This))
               );
            end if;
         end Check;
      begin
         for Index in 1..15 loop   --        4       8        12
            Add                    --  1 2 3   5 6 7   9 10 11  13 14 15
            (  T,
               Byte_Index (Index),
               Byte_Index (Index)
            );
         end loop;
         Remove (T, 8);
         Check (T, (1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15));
         Erase (T);
         Add (T, 1, 1);
         Add (T, 3, 3);
         Add (T, 5, 5);
         Add (T, 2, 2);
         Check  (T, (1, 2, 3, 5));
         Remove (T, 5);
         Check  (T, (1, 2, 3));
         Remove (T, 3);
         Check  (T, (1, 2));
         Remove (T, 2);
         Check  (T, (1=> 1));
         Remove (T, 1);
         Erase  (T);

         Add (T, 1, 1);
         Add (T, 3, 3);
         Add (T, 5, 5);
         Add (T, 0, 0);
         Check (T, (0, 1, 3, 5));
         Erase (T);

         Add (T, 10, 10);
         Add (T, 20, 20);
         Add (T, 30, 30);
         Add (T, 40, 40);
         Add (T, 50, 50);
         Add (T, 60, 60);
         Add (T, 70, 70);
         Add (T, 11, 11);
         Check (T, (10, 11, 20, 30, 40, 50, 60, 70));
         Check_Sup (T, 1, 10);
         Check_Sup (T, 12, 20);
         Check_Sup (T, 39, 40);
         Check_Sup (T, 40, 40);
         Check_Sup (T, 69, 70);
         Check_Sup (T, 70, 70);
         Check_Sup (T, 71);
         Check_Inf (T, 1);
         Check_Inf (T, 10, 10);
         Check_Inf (T, 12, 11);
         Check_Inf (T, 39, 30);
         Check_Inf (T, 40, 40);
         Check_Inf (T, 69, 60);
         Check_Inf (T, 70, 70);
         Check_Inf (T, 71, 70);
         Erase (T);

         Add (T, 1, 1);
         Add (T, 2, 2);
         Add (T, 3, 3);
         Add (T, 4, 4);
         Check (T, (1, 2, 3, 4));

         Remove (T, 1);
         Remove (T, 3);
         Remove (T, 2);
         Remove (T, 4);
--       Dump (T);
--       Dump_Reversed (T);
         Erase (T);
         if not Is_Empty (T) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Tree is not empty after erasing"
            );
         end if;

         Start := Clock;
         for Index in Count + 1..2 * Count loop
            Add (T, Byte_Index (Index), Byte_Index (Index));
         end loop;
         D1 := (Clock - Start) / Count;
         Check (Count + 1, 2 * Count);
--       Dump (T);
         Start := Clock;
         for Index in reverse 1..Count loop
            Add (T, Byte_Index (Index), Byte_Index (Index));
         end loop;
         D2 := (Clock - Start) / Count;
         Check (1, 2*Count);
--       Dump (T);
--       Dump_Reversed (T);
         declare
            use Integer_Sets;
            List : Set;

            procedure Check is
               This : Item_Ptr := Get_First (T);
               Node : Integer;
            begin
               for Item in 1..Get_Size (List) loop
                  Node := Integer (Get (List, Item));
                  if This = No_Item then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        "Missing key " & Image (Node)
                     );
                  elsif Byte_Index (Node) /= Get_Key (This) then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        (  "Key "
                        &  Image (Get_Key (This))
                        &  " /= "
                        &  Image (Node)
                        &  " (expected)"
                     )  );
                  end if;
                  This := Get_Next (This);
               end loop;
               if This /= No_Item then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Unexpected key " & Image (Get_Key (This))
                  );
               end if;
            end Check;
            Index : Integer := 1;
         begin
            for Index in 1..2 * Count loop
               Add (List, Number (Index));
            end loop;
            while Index <= Count loop
               Remove (T, Byte_Index (Index));
               Remove (List, Number (Index));
               Index := Index + 2;
            end loop;
            Check;
            for Right in 1..20 loop
               Index := Integer (Get (List, Get_Size (List)));
               Remove (T, Byte_Index (Index));
               Remove (List, Number (Index));
            end loop;
            Check;
            for Left in 1..10 loop
               Index := Integer (Get (List, 1));
               Remove (T, Byte_Index (Index));
               Remove (List, Number (Index));
               Check;
            end loop;
            Check;
            for Middle in 1..10 loop
               Index := Integer (Get (List, 10));
               Remove (T, Byte_Index (Index));
               Remove (List, Number (Index));
               Check;
            end loop;
            Check;
            Index := Integer (Get (List, Get_Size (List) / 2));
            declare
               Item : constant Item_Ptr := Find (T, Byte_Index (Index));
            begin
               if Get_Key (Item) /= Byte_Index (Index) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Found key "
                     &  Image (Get_Key (Item))
                     &  " /= "
                     &  Image (Index)
                     &  " (expected)"
                  )  );
               end if;
               if Get_Value (Item) /= Byte_Index (Index) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Found value"
                     &  Byte_Index'Image (Get_Value (Item))
                     &  " /= "
                     &  Image (Index)
                     &  " (expected)"
                  )  );
               end if;
               Replace (Item, 300);
               if Get_Value (Item) /= 300 then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Replaced value"
                     &  Byte_Index'Image (Get_Value (Item))
                     &  " /= 300 (expected)"
                  )  );
               end if;
            end;
         end;
      end;
--        Dump (T);
--        for Index in Visited'Range loop
--           if not Visited (Index) then
--              Put_Line (Coverage_Paths'Image (Index) & " not visited");
--           end if;
--        end loop;
      Put_Line (" done");
   end;
------------------------------------------------------------------------
   declare
      use Test_Integer_B_Trees.External_Ptr;
      Count : constant := 10_000;
      File  : aliased Persistent_Array;
   begin
      Put ("Testing external pointer B-trees ...");
      Open (File, 10);
      declare
         Pool : aliased Persistent_Pool (File'Access);
         T    : B_Tree (Pool'Access);

         procedure Check (From, To : Integer) is
            This : Item_Ptr := Get_First (T);
         begin
            for Node in From..To loop
               if This = No_Item then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Missing reverse key " & Image (Node)
                  );
               elsif Node /= Get_Key (This) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Key "
                     &  Image (Get_Key (This))
                     &  " /= "
                     &  Image (Node)
                     &  " (expected)"
                  )  );
               end if;
               This := Get_Next (This);
            end loop;
            if This /= No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Unexpected reverse key " & Image (Get_Key (This))
               );
            end if;
         end Check;
      begin
         for Index in 1..15 loop   --        4       8        12
            Add                    --  1 2 3   5 6 7   9 10 11  13 14 15
            (  T,
               Index,
               Byte_Index (Index)
            );
         end loop;
         Remove (T, 8);
         Check (T, (1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15));
         Erase (T);
         Add (T, 1, 1);
         Add (T, 3, 3);
         Add (T, 5, 5);
         Add (T, 2, 2);
         Check  (T, (1, 2, 3, 5));
         Remove (T, 5);
         Check  (T, (1, 2, 3));
         Remove (T, 3);
         Check  (T, (1, 2));
         Remove (T, 2);
         Check  (T, (1=> 1));
         Remove (T, 1);
         Erase  (T);

         Add (T, 1, 1);
         Add (T, 3, 3);
         Add (T, 5, 5);
         Add (T, 0, 0);
         Check (T, (0, 1, 3, 5));
         Erase (T);

         Add (T, 10, 10);
         Add (T, 20, 20);
         Add (T, 30, 30);
         Add (T, 40, 40);
         Add (T, 50, 50);
         Add (T, 60, 60);
         Add (T, 70, 70);
         Add (T, 11, 11);
         Check (T, (10, 11, 20, 30, 40, 50, 60, 70));
         Erase (T);

         Add (T, 1, 1);
         Add (T, 2, 2);
         Add (T, 3, 3);
         Add (T, 4, 4);
         Check (T, (1, 2, 3, 4));

         Remove (T, 1);
         Remove (T, 3);
         Remove (T, 2);
         Remove (T, 4);
--       Dump (T);
--       Dump_Reversed (T);
         Erase (T);
         if not Is_Empty (T) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Tree is not empty after erasing"
            );
         end if;

         Start := Clock;
         for Index in Count + 1..2 * Count loop
            Add (T, Index, Byte_Index (Index));
         end loop;
         D1 := (Clock - Start) / Count;
         Check (Count + 1, 2 * Count);
--       Dump (T);
         Start := Clock;
         for Index in reverse 1..Count loop
            Add (T, Index, Byte_Index (Index));
         end loop;
         D2 := (Clock - Start) / Count;
         Check (1, 2*Count);
--       Dump (T);
--       Dump_Reversed (T);
         declare
            use Integer_Sets;
            List : Set;

            procedure Check is
               This : Item_Ptr := Get_First (T);
               Node : Integer;
            begin
               for Item in 1..Get_Size (List) loop
                  Node := Integer (Get (List, Item));
                  if This = No_Item then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        "Missing key " & Image (Node)
                     );
                  elsif Node /= Get_Key (This) then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        (  "Key "
                        &  Image (Get_Key (This))
                        &  " /= "
                        &  Image (Node)
                        &  " (expected)"
                     )  );
                  end if;
                  This := Get_Next (This);
               end loop;
               if This /= No_Item then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Unexpected key " & Image (Get_Key (This))
                  );
               end if;
            end Check;
            Index : Integer := 1;
         begin
            for Index in 1..2 * Count loop
               Add (List, Number (Index));
            end loop;
            while Index <= Count loop
               Remove (T, Index);
               Remove (List, Number (Index));
               Index := Index + 2;
            end loop;
            Check;
            for Right in 1..20 loop
               Index := Integer (Get (List, Get_Size (List)));
               Remove (T, Index);
               Remove (List, Number (Index));
            end loop;
            Check;
            for Left in 1..10 loop
               Index := Integer (Get (List, 1));
               Remove (T, Index);
               Remove (List, Number (Index));
               Check;
            end loop;
            Check;
            for Middle in 1..10 loop
               Index := Integer (Get (List, 10));
               Remove (T, Index);
               Remove (List, Number (Index));
               Check;
            end loop;
            Check;
            Index := Integer (Get (List, Get_Size (List) / 2));
            declare
               Item : constant Item_Ptr := Find (T, Index);
            begin
               if Get_Key (Item) /= Index then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Found key "
                     &  Image (Get_Key (Item))
                     &  " /= "
                     &  Image (Index)
                     &  " (expected)"
                  )  );
               end if;
               if Get_Pointer (Item) /= Byte_Index (Index) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Found value"
                     &  Byte_Index'Image (Get_Pointer (Item))
                     &  " /= "
                     &  Image (Index)
                     &  " (expected)"
                  )  );
               end if;
               Replace (Item, 300);
               if Get_Pointer (Item) /= 300 then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Replaced value"
                     &  Byte_Index'Image (Get_Pointer (Item))
                     &  " /= 300 (expected)"
                  )  );
               end if;
            end;
         end;
      end;
--        Dump (T);
--        for Index in Visited'Range loop
--           if not Visited (Index) then
--              Put_Line (Coverage_Paths'Image (Index) & " not visited");
--           end if;
--        end loop;
      Put_Line (" done");
   end;
------------------------------------------------------------------------
   declare
      use Test_Integer_B_Trees.External;
      Count : constant := 10_000;
      File  : aliased Persistent_Array;
   begin
      Put ("Testing external B-trees ...");
      Open (File, 10);
      declare
         Pool : aliased Persistent_Pool (File'Access);
         T    : B_Tree (Pool'Access);

         procedure Check (From, To : Integer) is
            This : Item_Ptr := Get_First (T);
         begin
            for Node in From..To loop
               if This = No_Item then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Missing reverse key " & Image (Node)
                  );
               elsif Node /= Get_Key (This) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Key "
                     &  Image (Get_Key (This))
                     &  " /= "
                     &  Image (Node)
                     &  " (expected)"
                  )  );
               end if;
               This := Get_Next (This);
            end loop;
            if This /= No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Unexpected reverse key " & Image (Get_Key (This))
               );
            end if;
         end Check;
      begin
         for Index in 1..15 loop   --        4       8        12
            Add (T, Index, Index); --  1 2 3   5 6 7   9 10 11  13 14 15
         end loop;
--    Dump (T);
         Remove (T, 8);
         Check (T, (1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15));
         Erase (T);
         Add (T, 1, 1);
         Add (T, 3, 3);
         Add (T, 5, 5);
         Add (T, 2, 2);
         Check (T, (1, 2, 3, 5));
         Remove  (T, 5);
         Check (T, (1, 2, 3));
         Remove  (T, 3);
         Check (T, (1, 2));
         Remove  (T, 2);
         Check (T, (1=> 1));
         Remove  (T, 1);
         Erase (T);

         Add (T, 1, 1);
         Add (T, 3, 3);
         Add (T, 5, 5);
         Add (T, 0, 0);
         Check (T, (0, 1, 3, 5));
         Erase (T);

         Add (T, 10, 1);
         Add (T, 20, 2);
         Add (T, 30, 3);
         Add (T, 40, 1);
         Add (T, 50, 2);
         Add (T, 60, 3);
         Add (T, 70, 3);
         Add (T, 11, 3);
         Check (T, (10, 11, 20, 30, 40, 50, 60, 70));
         Erase (T);

         Add (T, 1, 1);
         Add (T, 2, 2);
         Add (T, 3, 3);
         Add (T, 4, 4);
         Check (T, (1, 2, 3, 4));

         Remove (T, 1);
         Remove (T, 3);
         Remove (T, 2);
         Remove (T, 4);
--       Dump (T);
--       Dump_Reversed (T);
         Erase (T);
         if not Is_Empty (T) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Tree is not empty after erasing"
            );
         end if;

         Start := Clock;
         for Index in Count + 1..2 * Count loop
            Add (T, Index, Index);
         end loop;
         D1 := (Clock - Start) / Count;
         Check (Count + 1, 2 * Count);
--       Dump (T);
         Start := Clock;
         for Index in reverse 1..Count loop
            Add (T, Index, Index);
         end loop;
         D2 := (Clock - Start) / Count;
         Check (1, 2*Count);
--       Dump (T);
--       Dump_Reversed (T);
         declare
            use Integer_Sets;
            List : Set;

            procedure Check is
               This : Item_Ptr := Get_First (T);
               Node : Integer;
            begin
               for Item in 1..Get_Size (List) loop
                  Node := Integer (Get (List, Item));
                  if This = No_Item then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        "Missing key " & Image (Node)
                     );
                  elsif Node /= Get_Key (This) then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        (  "Key "
                        &  Image (Get_Key (This))
                        &  " /= "
                        &  Image (Node)
                        &  " (expected)"
                     )  );
                  end if;
                  This := Get_Next (This);
               end loop;
               if This /= No_Item then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Unexpected key " & Image (Get_Key (This))
                  );
               end if;
            end Check;
            Index : Integer := 1;
         begin
            for Index in 1..2 * Count loop
               Add (List, Number (Index));
            end loop;
            while Index <= Count loop
               Remove (T, Index);
               Remove (List, Number (Index));
               Index := Index + 2;
            end loop;
            Check;
            for Right in 1..20 loop
               Index := Integer (Get (List, Get_Size (List)));
               Remove (T, Index);
               Remove (List, Number (Index));
            end loop;
            Check;
            for Left in 1..10 loop
               Index := Integer (Get (List, 1));
               Remove (T, Index);
               Remove (List, Number (Index));
               Check;
            end loop;
            Check;
            for Middle in 1..10 loop
               Index := Integer (Get (List, 10));
               Remove (T, Index);
               Remove (List, Number (Index));
               Check;
            end loop;
            Check;
            Index := Integer (Get (List, Get_Size (List) / 2));
            declare
               Item : constant Item_Ptr := Find (T, Index);
            begin
               if Get_Key (Item) /= Index then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Found key "
                     &  Image (Get_Key (Item))
                     &  " /= "
                     &  Image (Index)
                     &  " (expected)"
                  )  );
               end if;
               if Get_Value (Item) /= Index then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Found value "
                     &  Image (Get_Value (Item))
                     &  " /= "
                     &  Image (Index)
                     &  " (expected)"
                  )  );
               end if;
               Replace (Item, 300);
               if Get_Value (Item) /= 300 then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Replaced value "
                     &  Image (Get_Value (Item))
                     &  " /= 300 (expected)"
                  )  );
               end if;
            end;
         end;
      end;
--        Dump (T);
--        for Index in Visited'Range loop
--           if not Visited (Index) then
--              Put_Line (Coverage_Paths'Image (Index) & " not visited");
--           end if;
--        end loop;
      Put_Line (" done");
   end;
------------------------------------------------------------------------
   declare
      use Test_String_B_Trees.Internal;
      Count : constant := 1_000;
      T : B_Tree;
   begin
      Put ("Testing indefinite B-tress ...");
      for Index in 1..Count loop
         Add (T, Image (Index), Index);
         declare
            This : Item_Ptr := Get_First (T);
         begin
            for Node in 1..Index loop
               if This = No_Item then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Missing key " & Image (Node)
                  );
               elsif Image (Get_Value (This)) /= Get_Key (This) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Key "
                     &  Get_Key (This)
                     &  " /= "
                     &  Image (Get_Value (This))
                     &  " (expected)"
                  )  );
               end if;
               This := Get_Next (This);
            end loop;
            if This /= No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Unexpected key " & Get_Key (This)
               );
            end if;
         end;
      end loop;
      for Index in 1..Count loop
         if not Is_In (T, Image (Index)) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Missing key " & Image (Index)
            );
         elsif Get (T, Image (Index)) /= Index then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Wrong key " & Image (Index) & " value"
            );
         end if;
      end loop;
      Put (" done");
   end;
------------------------------------------------------------------------
   declare
      use Test_Integer_B_Trees.Internal;
      Count : constant := 1_000;
      T : B_Tree;
      procedure Check (From, To : Integer) is
         This : Item_Ptr := Get_First (T);
      begin
         for Index in From..To loop
            if This = No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Missing key " & Image (Index)
               );
            elsif Index /= Get_Key (This) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Key "
                  &  Image (Get_Key (This))
                  &  " /= "
                  &  Image (Index)
                  &  " (expected)"
               )  );
            end if;
            This := Get_Next (This);
         end loop;
         if This /= No_Item then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Unexpected key " & Image (Get_Key (This))
            );
         end if;
      end Check;
   begin
      New_Line;
      Put ("Testing definite B-tress ...");
      for Index in 1..15 loop   --        4       8        12
         Add (T, Index, Index); --  1 2 3   5 6 7   9 10 11  13 14 15
      end loop;
      Remove (T, 8);
      Check (T, (1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15));
      Erase (T);

      Add (T, 1, 1);
      Add (T, 3, 3);
      Add (T, 5, 5);
      Add (T, 2, 2);
      Check  (T, (1, 2, 3, 5));
      Remove (T, 5);
      Check  (T, (1, 2, 3));
      Remove (T, 3);
      Check  (T, (1, 2));
      Remove (T, 2);
      Check  (T, (1=> 1));
      Remove (T, 1);
      Erase  (T);

      Add (T, 1, 1);
      Add (T, 3, 3);
      Add (T, 5, 5);
      Add (T, 0, 0);
      Check (T, (0, 1, 3, 5));
      Erase (T);

      Add (T, 10, 1);
      Add (T, 20, 2);
      Add (T, 30, 3);
      Add (T, 40, 1);
      Add (T, 50, 2);
      Add (T, 60, 3);
      Add (T, 70, 3);
      Add (T, 11, 3);
      Check (T, (10, 11, 20, 30, 40, 50, 60, 70));
      Check_Sup (T, 1, 10);
      Check_Sup (T, 12, 20);
      Check_Sup (T, 39, 40);
      Check_Sup (T, 40, 40);
      Check_Sup (T, 69, 70);
      Check_Sup (T, 70, 70);
      Check_Sup (T, 71);
      Check_Inf (T, 1);
      Check_Inf (T, 10, 10);
      Check_Inf (T, 12, 11);
      Check_Inf (T, 39, 30);
      Check_Inf (T, 40, 40);
      Check_Inf (T, 69, 60);
      Check_Inf (T, 70, 70);
      Check_Inf (T, 71, 70);
      Erase (T);

      Add (T, 1, 1);
      Add (T, 2, 2);
      Add (T, 3, 3);
      Add (T, 4, 4);
      Check (T, (1, 2, 3, 4));

      Remove (T, 1);
      Remove (T, 3);
      Remove (T, 2);
      Remove (T, 4);
--      Dump (T);
--      Dump_Reversed (T);
      Erase (T);
      if not Is_Empty (T) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Tree is not empty after erasing"
         );
      end if;
      for Index in 1..Count loop
         Add (T, Index, Index);
         Check (1, Index);
      end loop;
--    Dump (T);
      for Index in reverse -Count..0 loop
         Add (T, Index, Index);
         declare
            This : Item_Ptr := Get_First (T);
         begin
            for Node in Index..Count loop
               if This = No_Item then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Missing key " & Image (Node)
                  );
               elsif Node /= Get_Key (This) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Key "
                     &  Image (Get_Key (This))
                     &  " /= "
                     &  Image (Node)
                     &  " (expected)"
                  )  );
               end if;
               This := Get_Next (This);
            end loop;
            if This /= No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Unexpected key " & Image (Get_Key (This))
               );
            end if;
         end;
      end loop;
--      Dump (T);
--      Dump_Reversed (T);
      declare
         use Integer_Sets;
         function To (Index : Integer) return Number is
         begin
            return Number (1000 + Index);
         end To;
         function From (Index : Number) return Integer is
         begin
            return Integer (Index - 1000);
         end From;

         List : Set;

         procedure Check is
            This : Item_Ptr := Get_First (T);
            Node : Integer;
         begin
            for Item in 1..Get_Size (List) loop
               Node := From (Get (List, Item));
               if This = No_Item then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Missing key " & Image (Node)
                  );
               elsif Node /= Get_Key (This) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Key "
                     &  Image (Get_Key (This))
                     &  " /= "
                     &  Image (Node)
                     &  " (expected)"
                  )  );
               end if;
               This := Get_Next (This);
            end loop;
            if This /= No_Item then
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Unexpected key " & Image (Get_Key (This))
               );
            end if;
         end Check;
         Index : Integer := -Count;
      begin
         for Index in -Count..Count loop
            Add (List, To (Index));
         end loop;
         while Index <= Count loop
            Remove (T, Index);
            Remove (List, To (Index));
            Check;
            Index := Index + 2;
         end loop;
         for Right in 1..20 loop
            Index := From (Get (List, Get_Size (List)));
            Remove (T, Index);
            Remove (List, To (Index));
            Check;
         end loop;
         for Left in 1..10 loop
            Index := From (Get (List, 1));
            Remove (T, Index);
            Remove (List, To (Index));
            Check;
         end loop;
         for Middle in 1..10 loop
            Index := From (Get (List, 10));
            Remove (T, Index);
            Remove (List, To (Index));
            Check;
         end loop;
         Start := Clock;
         for Try in -Count..10 * Count loop
            Index := Integer (Seconds (Clock));
            Replace (T, Index, Index);
         end loop;
         Stop := Clock;
         Put (" done");
         New_Line;
         Put_Line
         (  "Randomized B-Tree update "
         &  Image (Float (Stop - Start) / Float (11 * Count) * 1.0E6)
         &  " us, per operation"
         );
         Put_Line
         (  "Adding ascending keys to persistent B-Tree "
         &  Image (Float (D1) * 1000.0)
         &  " ms, per operation"
         );
         Put_Line
         (  "Adding descending keys to persistent B-Tree "
         &  Image (Float (D2) * 1000.0)
         &  " ms, per operation"
         );
      end;
--        Dump (T);
--        for Index in Visited'Range loop
--           if not Visited (Index) then
--              Put_Line (Coverage_Paths'Image (Index) & " not visited");
--           end if;
--        end loop;
   end;
   Put_Line ("... Done");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_B_Trees;
