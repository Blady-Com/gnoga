--                                                                    --
--  procedure Test_B_Trees          Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Autumn, 2014       --
--                                                                    --
--                                Last revision :  18:00 18 Aug 2022  --
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
--with Persistent.Memory_Pools.Dump;  use Persistent.Memory_Pools.Dump;
with Strings_Edit;                  use Strings_Edit;
with Strings_Edit.Floats;           use Strings_Edit.Floats;
with Strings_Edit.Integers;         use Strings_Edit.Integers;
with System.Storage_Elements;       use System.Storage_Elements;
with Test_Integer_Sets;             use Test_Integer_Sets;

with Ada.Unchecked_Conversion;
with Interfaces;
with Long_Float_Waveform;
with Persistent.Blocking_Files.Text_IO;
with Persistent.Memory_Pools.Streams.External_B_Tree;
with Test_Integer_B_Trees;
with Test_String_B_Trees;
--with System.Exception_Traces;

procedure Test_B_Trees is
   use Persistent.Blocking_Files.Text_IO;

   function Get_Depth
            (  This  : Persistent.Memory_Pools.Streams.External_B_Tree.
                       Item_Ptr;
               Count : Natural := 20
            )  return String is
      use Persistent.Memory_Pools.Streams.External_B_Tree;
      Parent : Item_Ptr := Get_Left_Parent (This);
   begin
      if Parent = No_Item then
         Parent := Get_Right_Parent (This);
         if Parent = No_Item then
            return "";
         end if;
      end if;
      if Count = 0 then
         return
         (  "error at "
         &  Image (Get_Index (This))
         &  "/"
         &  Image (Get_Bucket_Size (This))
         &  " "
         &  Image (Get_Bucket_Address (This))
         );
      else
         return "|  " & Get_Depth (Parent, Count - 1);
      end if;
   end Get_Depth;

   function Get_Depth
            (  This  : Test_Integer_B_Trees.Internal.Item_Ptr;
               Count : Natural := 20
            )  return String is
      use Test_Integer_B_Trees.Internal;
      Parent : Item_Ptr := Get_Left_Parent (This);
   begin
      if Parent = No_Item then
         Parent := Get_Right_Parent (This);
         if Parent = No_Item then
            return "";
         end if;
      end if;
      if Count = 0 then
         return
         (  "error at "
         &  Image (Get_Index (This))
         &  "/"
         &  Image (Get_Bucket_Size (This))
         &  " "
         &  Integer_Address'Image
            (  To_Integer (Get_Bucket_Address (This))
         )  );
      else
         return "|  " & Get_Depth (Parent, Count - 1);
      end if;
   end Get_Depth;

   function Get_Parent
            (  This : Persistent.Memory_Pools.Streams.External_B_Tree.
                      Item_Ptr
            )  return String is
      use Persistent.Memory_Pools.Streams.External_B_Tree;
      Parent : Item_Ptr;
      Left   : constant Item_Ptr := Get_Left_Parent  (This);
      Right  : constant Item_Ptr := Get_Right_Parent (This);
      function Index_Image (Item : Item_Ptr) return String is
      begin
         if Item = No_Item then
            return "*";
         else
            return Image (Get_Index (Item));
         end if;
      end Index_Image;
   begin
      if Left = No_Item then
         if Right = No_Item then
            return "";
         else
            Parent := Right;
         end if;
      else
         Parent := Left;
      end if;
      return
      (  Character'Val (9)
      &  Get_Depth (Parent)
      &  "|<--"
      &  Image (Get_Bucket_Address (Parent))
      &  " "
      &  Index_Image (Left)
      &  "|"
      &  Index_Image (Right)
      &  "/"
      &  Image (Get_Bucket_Size (Parent))
      );
   end Get_Parent;

   function Get_Parent
            (  This : Test_Integer_B_Trees.Internal.Item_Ptr
            )  return String is
      use Test_Integer_B_Trees.Internal;
      Parent : Item_Ptr;
      Left   : constant Item_Ptr := Get_Left_Parent  (This);
      Right  : constant Item_Ptr := Get_Right_Parent (This);
      function Index_Image (Item : Item_Ptr) return String is
      begin
         if Item = No_Item then
            return "*";
         else
            return Image (Get_Index (Item));
         end if;
      end Index_Image;
   begin
      if Left = No_Item then
         if Right = No_Item then
            return "";
         else
            Parent := Right;
         end if;
      else
         Parent := Left;
      end if;
      return
      (  Character'Val (9)
      &  Get_Depth (Parent)
      &  "|<--"
      &  Integer_Address'Image
         (  To_Integer (Get_Bucket_Address (Parent))
         )
      &  " "
      &  Index_Image (Left)
      &  "|"
      &  Index_Image (Right)
      &  "/"
      &  Image (Get_Bucket_Size (Parent))
      );
   end Get_Parent;

   procedure Dump
             (  Tree  : Long_Float_Waveform.Waveform;
                Max   : Natural    := Natural'Last;
                First : Long_Float := Long_Float'First;
                Last  : Long_Float := Long_Float'Last;
                File  : File_Type  := Standard_Output
             )  is
      use Long_Float_Waveform;
      use Persistent.Memory_Pools.Streams.External_B_Tree;
      This  : Item_Ptr := Get_First (Tree);
      Count : Integer  := Max;
      Low   : Long_Float;
      High  : Long_Float;
   begin
      while This /= No_Item and then Count > 0 loop
         Get_Tag (This, Low, High);
         Low  := Long_Float'Max (Long_Float (Float'First), Low);
         High := Long_Float'Min (Long_Float (Float'Last),  High);
         Put_Line
         (  File,
            (  Image (Float (Get_Point (This).X))
            &  " ->"
            &  Image (Float (Get_Point (This).Y))
            &  Character'Val (9)
            &  "at "
            &  Image (Get_Index (This))
            &  "/"
            &  Image (Get_Bucket_Size (This))
            &  " "
            &  Image (Get_Bucket_Address (This))
            &  Get_Parent (This)
            &  " Min/Max "
            &  Image (Float (Low))
            &  ".."
            &  Image (Float (High))
            &  " at "
            &  Image (Get_Tag (This))
            &  ": "
            &  Image (Tree.Pool.File, Get_Tag (This), 8)
         )  );
         if Low < First or else High > Last then
            return;
         end if;
         Count := Count - 1;
         This  := Get_Next (This);
      end loop;
   end Dump;

   procedure Dump
             (  Tree  : Long_Float_Waveform.Waveform;
                Max   : Natural    := Natural'Last;
                First : Long_Float := Long_Float'First;
                Last  : Long_Float := Long_Float'Last;
                File  : String
             )  is
      This : File_Type;
   begin
      if Long_Float_Waveform.Is_Tagged (Tree) then
         Create (This, Out_File, File);
         Dump (Tree, Max, First, Last, This);
         Close (This);
         Put_Line ("Tree dump in " & File);
      else
         Put_Line ("Tree is not tagged");
      end if;
   end Dump;

   procedure Dump
             (  Tree : Persistent.Memory_Pools.Streams.External_B_Tree.
                       B_Tree;
                Max  : Natural := Natural'Last
             )  is
      use Persistent.Memory_Pools.Streams.External_B_Tree;
      This  : Item_Ptr := Get_First (Tree);
      Count : Integer  := Max;
   begin
      while This /= No_Item and then Count > 0 loop
         Put_Line
         (  "  "
         &  Byte_Index'Image (Get_Key (This))
         &  Character'Val (9)
         &  "->"
         &  Byte_Index'Image (Get_Value (This))
         &  Character'Val (9)
         &  "at "
         &  Image (Get_Index (This))
         &  "/"
         &  Image (Get_Bucket_Size (This))
         &  " "
         &  Image (Get_Bucket_Address (This))
         &  Get_Parent (This)
         );
         Count := Count - 1;
         This  := Get_Next (This);
      end loop;
   end Dump;

   procedure Dump
             (  Tree : Test_Integer_B_Trees.Internal.B_Tree;
                Max  : Natural := Natural'Last
             )  is
      use Test_Integer_B_Trees.Internal;
      This  : Item_Ptr := Get_First (Tree);
      Count : Integer  := Max;
   begin
      while This /= No_Item and then Count > 0 loop
         Put_Line
         (  "  "
         &  Image (Get_Key (This))
         &  Character'Val (9)
         &  "->"
         &  Image (Get_Value (This))
         &  Character'Val (9)
         &  "at "
         &  Image (Get_Index (This))
         &  "/"
         &  Image (Get_Bucket_Size (This))
         &  Integer_Address'Image
            (  To_Integer (Get_Bucket_Address (This))
            )
         &  Get_Parent (This)
         &  " Min/Max "
         &  Image (Get_Tag (This).Min)
         &  ".."
         &  Image (Get_Tag (This).Max)
         );
         Count := Count - 1;
         This  := Get_Next (This);
      end loop;
   end Dump;

   procedure Dump (Tree : Test_Integer_B_Trees.External.B_Tree) is
      use Test_Integer_B_Trees.External;
      This : Item_Ptr := Get_First (Tree);
   begin
      while This /= No_Item loop
         Put_Line
         (  "  "
         &  Image (Get_Key (This))
         &  Character'Val (9)
         &  "->"
         &  Image (Get_Value (This))
         &  Character'Val (9)
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
         (  "  "
         &  Image (Get_Key (This))
         &  Character'Val (9)
         &  "->"
         &  Image (Integer (Get_Pointer (This)))
         &  Character'Val (9)
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
         (  "  "
         &  Get_Key (This)
         &  Character'Val (9)
         &  "->"
         &  Image (Get_Value (This))
         &  Character'Val (9)
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
         (  "  "
         &  Image (Get_Key (This))
         &  Character'Val (9)
         &  "->"
         &  Image (Get_Value (This))
         &  Character'Val (9)
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
--   System.Exception_Traces.Trace_On
--   (  System.Exception_Traces.Every_Raise
--   );
   Put_Line ("Testing B-trees ...");
   declare
      use Test_Integer_B_Trees.Internal;
      T : B_Tree;

      function Visit_Item
               (  Tree : Test_Integer_B_Trees.Internal.B_Tree;
                  Key  : Integer;
                  Item : Test_Integer_B_Trees.Internal.Item_Ptr
               )  return Boolean is
         use Test_Integer_B_Trees.External_Ptr;
      begin
         Put_Line
         (  Image (Key)
         &  Character'Val (9) & Character'Val (9) & Character'Val (9)
         &  "at "
         &  Image (Get_Index (Item))
         &  "/"
         &  Image (Get_Bucket_Size (Item))
         &  Integer_Address'Image
            (  To_Integer (Get_Bucket_Address (Item))
            )
         &  Get_Parent (Item)
         &  Character'Val (9)
         &  " Min/Max "
         &  Image (Get_Tag (Item).Min)
         &  ".."
         &  Image (Get_Tag (Item).Max)
         );
         return True;
      end Visit_Item;

      function Visit_Range
               (  Tree : Test_Integer_B_Trees.Internal.B_Tree;
                  Item : Test_Integer_B_Trees.Internal.Item_Ptr
               )  return Bucket_Traversal is
         use Test_Integer_B_Trees.External_Ptr;
      begin
         Put_Line
         (  Image (Get_Key (Get_First (Get_Item (Item, 1))))
         &  ".."
         &  Image
            (  Get_Key
               (  Get_Last (Get_Item (Item, Get_Bucket_Size (Item)))
            )  )
         &  " ["
         &  Image (Get_Key (Get_Item (Item, 1)))
         &  ".."
         &  Image (Get_Key (Get_Item (Item, Get_Bucket_Size (Item))))
         &  "] "
         &  Character'Val (9)
         &  "at "
         &  Image (Get_Index (Item))
         &  "/"
         &  Image (Get_Bucket_Size (Item))
         &  Integer_Address'Image
            (  To_Integer (Get_Bucket_Address (Item))
            )
         &  Get_Parent (Item)
         &  Character'Val (9)
         &  " Min/Max "
         &  Image (Get_Tag (Item).Min)
         &  ".."
         &  Image (Get_Tag (Item).Max)
         );
         return Step_Over;
      end Visit_Range;

      procedure Traverse is
         new Test_Integer_B_Trees.Internal.Generic_Traverse;

      procedure Tag
                (  Root   : Item_Ptr;
                   Prefix : String  := "";
                   Print  : Boolean := True
                )  is
         use Test_Integer_B_Trees;
         Result : Min_Max := (Integer'Last, Integer'First);
         Length : Natural := Get_Bucket_Size (Root);
         Child  : Item_Ptr;
         This   : Min_Max;
      begin
         if Length > 0 then
            if Print then
               Put
               (  Prefix
               &  Image (Length)
               &  Integer_Address'Image
                  (  To_Integer (Get_Bucket_Address (Root))
                  )
               &  " "
               &  Character'Val (9)
               );
            end if;
            for Item in 1..Length loop
               if Print then
                  if Item /= 1 then
                     Put (" | ");
                  end if;
                  Put (Image (Get_Key (Get_Item (Root, Item))));
               end if;
               This.Min   := Get_Value (Get_Item (Root, Item));
               Result.Min := Integer'Min (Result.Min, This.Min);
               Result.Max := Integer'Max (Result.Max, This.Min);
            end loop;
            if Print then
               New_Line;
            end if;
            for Item in 1..Length loop
               Child := Get_Left_Child (Get_Item (Root, Item));
               if Child /= No_Item then
                  Tag (Child, Prefix & "   ", Print);
                  This := Get_Tag (Child);
                  Result.Min := Integer'Min (Result.Min, This.Min);
                  Result.Max := Integer'Max (Result.Max, This.Max);
               end if;
            end loop;
            Child := Get_Right_Child (Get_Item (Root, Length));
            if Child /= No_Item then
               Tag (Child, Prefix & "   ", Print);
               This := Get_Tag (Child);
               Result.Min := Integer'Min (Result.Min, This.Min);
               Result.Max := Integer'Max (Result.Max, This.Max);
            end if;
            Set_Tag (Root, Result);
         end if;
      end Tag;

      Value : constant Integer := 10;
      Count : Natural := 0;
      type Integer_Array is array (Positive range <>) of Integer;
      Found : Integer_Array (1..100);
      function Generate (From, To : Integer) return Integer_Array is
         Length : Natural := 0;
      begin
         for Index in From..To loop
            if 10 = (Index mod 20) then
               Length := Length + 1;
               Found (Length) := Index;
            end if;
         end loop;
         return Found (1..Length);
      end Generate;
      Expected : constant Integer_Array := Generate (100, 500);

      function Find_Item
               (  Tree : B_Tree;
                  Key  : Integer;
                  Item : Item_Ptr
               )  return Boolean is
      begin
         if Get_Value (Item) = Value then
            Count := Count + 1;
            Found (Count) := Key;
         end if;
         return True;
      end Find_Item;

      function Find_Range
               (  Tree : B_Tree;
                  Item : Item_Ptr
               )  return Bucket_Traversal is
         This : constant Test_Integer_B_Trees.Min_Max := Get_Tag (Item);
      begin
         if Value in This.Min..This.Max then
            return Step_In;
         else
            return Step_Over;
         end if;
      end Find_Range;

      procedure Find is
         new Test_Integer_B_Trees.Internal.Generic_Traverse
             (  Visit_Item  => Find_Item,
                Visit_Range => Find_Range
             );
   begin
      Put_Line ("Testing B-tree traversal...");
      for Index in 1..1000 loop
         Add (T, Index, Index mod 20);
      end loop;
      Tag (Get_Root (T), Print => True);
--      Traverse (T, Inf (T, 100), 500);
      Find (T, Inf (T, 100), 500);
      for Index in Expected'Range loop
         if (  Index > Count
            or else
               Found (Index) /= Expected (Index)
            )  then
            Raise_Exception
            (  Data_Error'Identity,
               "Not found " & Image (Expected (Index))
            );
         end if;
      end loop;
      if Count > Expected'Length then
         Raise_Exception
         (  Data_Error'Identity,
            "Not to be found " & Image (Found (Expected'Length + 1))
         );
      end if;
   end;
   declare
      use Long_Float_Waveform;
      File      : aliased Persistent_Array;
      Reference : Byte_Index;

      function Diff (Left, Right : Long_Float) return Boolean is
      begin
         return abs (Left - Right) >= 0.0001;
      end Diff;
   begin
      Put_Line ("Testing waveform ...");
      Open (File, 10);
      declare
         Pool : aliased Persistent_Pool (File'Access);
         T    : Waveform (Pool'Access);
         X, Y : Long_Float;
      begin
         for Index in 1..10 loop
            Add (T, Long_Float (Index),      Long_Float (Index));
            Add (T, Long_Float (21 - Index), Long_Float (Index));
         end loop;
         if Diff (1.0, Get (T, 1.0, None)) then
            Raise_Exception (Data_Error'Identity, "Get error at 1.0");
         end if;
         begin
            Y := Get (T, 1.5, None);
            Raise_Exception (Data_Error'Identity, "Found non-existing");
         exception
            when End_Error =>
               null;
         end;
         if Diff (1.0, Get (T, 1.5, Rightmost)) then
            Raise_Exception
            (  Data_Error'Identity,
               "Rightmost interpolation at 1.5 error"
            );
         end if;
         if Diff (1.5, Get (T, 1.5, Linear)) then
            Raise_Exception
            (  Data_Error'Identity,
               "Linear interpolation at 1.5 error"
            );
         end if;
         declare
            Result : Search_Outcome;
         begin
            Result := Get (T, 0.0);
            if Result.Kind_Of /= Less then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get at 0.5 error " &
                  Location_Type'Image (Result.Kind_Of)
               );
            end if;
            Result := Get (T, 21.0);
            if Result.Kind_Of /= Greater then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get at 21.0 error " &
                  Location_Type'Image (Result.Kind_Of)
               );
            end if;
            Result := Get (T, 5.5);
            if Result.Kind_Of /= Inside then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get at 5.5 error " &
                  Location_Type'Image (Result.Kind_Of)
               );
            elsif Diff (Result.X1, 5.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get at 5.5 error X1" & Long_Float'Image (Result.X1)
               );
            elsif Diff (Result.X2, 6.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get at 5.5 error X2" & Long_Float'Image (Result.X2)
               );
            elsif Diff (Result.Y1, 5.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get at 5.5 error Y1" & Long_Float'Image (Result.Y1)
               );
            elsif Diff (Result.Y2, 6.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get at 5.5 error Y2" & Long_Float'Image (Result.Y2)
               );
            end if;
         end;
         if Diff (Get_First_X (T), 1.0) then
            Raise_Exception
            (  Data_Error'Identity, "Get_First_X error"
            );
         end if;
         if Diff (Get_First_Y (T), 1.0) then
            Raise_Exception
            (  Data_Error'Identity, "Get_First_Y error"
            );
         end if;
         if Diff (Get_Last_X (T), 20.0) then
            Raise_Exception
            (  Data_Error'Identity, "Get_Last_X error"
            );
         end if;
         if Diff (Get_Last_Y (T), 1.0) then
            Raise_Exception
            (  Data_Error'Identity, "Get_Last_Y error"
            );
         end if;
         declare
            Y1, Y2 : Long_Float;
         begin
            Get_Convex (T, 5.5, 18.5, None, Y1, Y2);
            if Diff (Y1, 3.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get_Convex none error Y1 =" & Long_Float'Image (Y1)
               );
            end if;
            if Diff (Y2, 10.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get_Convex none error Y2 =" & Long_Float'Image (Y2)
               );
            end if;
            Get_Convex (T, 5.5, 8.5, Rightmost, Y1, Y2);
            if Diff (Y1, 5.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get_Convex rightmost error Y1 =" &
                  Long_Float'Image (Y1)
               );
            end if;
            if Diff (Y2, 8.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get_Convex rightmost error Y2 =" &
                  Long_Float'Image (Y2)
               );
            end if;
            Get_Convex (T, 5.5, 8.5, Linear, Y1, Y2);
            if Diff (Y1, 5.5) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get_Convex linear error Y1 =" &
                  Long_Float'Image (Y1)
               );
            end if;
            if Diff (Y2, 8.5) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Get_Convex linear error Y2 =" &
                  Long_Float'Image (Y2)
               );
            end if;
         end;
         declare
            X : Long_Float;
         begin
            X := Find (T, 2.0, 20.0, 6.0, Above, None);
            if Diff (X, 6.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find above none error X =" & Long_Float'Image (X)
               );
            end if;
            X := Find (T, 10.0, 20.0, 7.5, Below, None);
            if Diff (X, 14.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find below none error X =" & Long_Float'Image (X)
               );
            end if;
            X := Find (T, 1.0, 20.0, 5.0, 8.0, Inside, None);
            if Diff (X, 5.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find inside none error X =" & Long_Float'Image (X)
               );
            end if;
            X := Find (T, 5.0, 20.0, 5.0, 8.0, Outside, None);
            if Diff (X, 9.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find outside none error X =" & Long_Float'Image (X)
               );
            end if;

            X := Find (T, 2.0, 20.0, 6.5, Above, Rightmost);
            if Diff (X, 7.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find above rightmost error X =" &
                  Long_Float'Image (X)
               );
            end if;
            X := Find (T, 10.0, 20.0, 7.5, Below, Rightmost);
            if Diff (X, 14.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find below rightmost error X =" &
                  Long_Float'Image (X)
               );
            end if;
            X := Find (T, 1.0, 20.0, 5.5, 8.5, Inside, Rightmost);
            if Diff (X, 6.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find inside rightmost error X =" &
                  Long_Float'Image (X)
               );
            end if;
            X := Find (T, 5.0, 20.0, 5.0, 8.5, Outside, Rightmost);
            if Diff (X, 9.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find outside rightmost error X =" &
                  Long_Float'Image (X)
               );
            end if;

            X := Find (T, 2.0, 20.0, 6.5, Above, Linear);
            if Diff (X, 6.5) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find above linear error X =" & Long_Float'Image (X)
               );
            end if;
            X := Find (T, 10.0, 20.0, 7.5, Below, Linear);
            if Diff (X, 13.5) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find below linear error X =" & Long_Float'Image (X)
               );
            end if;
            X := Find (T, 1.0, 20.0, 5.5, 8.5, Inside, Linear);
            if Diff (X, 5.5) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find inside linear error X =" & Long_Float'Image (X)
               );
            end if;
            X := Find (T, 10.0, 20.0, 5.5, 8.5, Inside, Linear);
            if Diff (X, 12.5) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find inside linear error X =" & Long_Float'Image (X)
               );
            end if;
            X := Find (T, 5.0, 20.0, 5.0, 8.3, Outside, Linear);
            if Diff (X, 8.3) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find outside linear error X =" & Long_Float'Image (X)
               );
            end if;
            X := Find (T, 10.0, 20.0, 5.0, 10.0, Outside, Linear);
            if Diff (X, 16.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find outside linear error X =" & Long_Float'Image (X)
               );
            end if;
            X := Find (T, 10.0, 20.0, 5.3, 10.0, Outside, Linear);
            if Diff (X, 15.7) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Find outside linear error X =" & Long_Float'Image (X)
               );
            end if;
         end;
         Erase (T);
         for Index in 1..10_000 loop
            Add (T, Long_Float (Index), Long_Float (Index));
         end loop;
         declare
            Y1, Y2 : Long_Float;
         begin
            Get_Convex (T, 1.0, 10_000.0, None, Y1, Y2);
            if Diff (Y1, 1.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Convex none error Y1 =" & Long_Float'Image (Y1)
               );
            end if;
            if Diff (Y2, 10_000.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Convex none error Y2 =" & Long_Float'Image (Y2)
               );
            end if;
            Get_Convex (T, 10.5, 7_002.5, Linear, Y1, Y2);
            if Diff (Y1, 10.5) then
               Dump (T, File => "c:\temp\dump.txt");
               Raise_Exception
               (  Data_Error'Identity,
                  "Convex linear error Y1 =" & Long_Float'Image (Y1)
               );
            end if;
            if Diff (Y2, 7_002.5) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Convex none error Y2 =" & Long_Float'Image (Y2)
               );
            end if;
--          Dump (T, File=>"c:\temp\dump1.txt");
         end;
         Store (T, Reference);
      end;
      declare
         Pool : aliased Persistent_Pool (File'Access);
         T    : Waveform (Pool'Access);
      begin
         Restore (T, Reference);
--       Dump (T, File=>"c:\temp\dump2.txt");
         declare
            Y1, Y2 : Long_Float;
         begin
            Get_Convex (T, 1.0, 10_000.0, None, Y1, Y2);
            if Diff (Y1, 1.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Convex none error Y1 =" & Long_Float'Image (Y1)
               );
            end if;
            if Diff (Y2, 10_000.0) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Convex none error Y2 =" & Long_Float'Image (Y2)
               );
            end if;
            Get_Convex (T, 10.5, 7_002.5, Linear, Y1, Y2);
            if Diff (Y1, 10.5) then
               Dump (T, File => "c:\temp\dump.txt");
               Raise_Exception
               (  Data_Error'Identity,
                  "Convex linear error Y1 =" & Long_Float'Image (Y1)
               );
            end if;
            if Diff (Y2, 7_002.5) then
               Raise_Exception
               (  Data_Error'Identity,
                  "Convex none error Y2 =" & Long_Float'Image (Y2)
               );
            end if;
         end;
      end;
      --  declare
      --     Pool : aliased Persistent_Pool (File'Access);
      --     T    : Waveform (Pool'Access);
      --  begin
      --     for Index in 1..1_000_000 loop
      --        Add (T, Long_Float (Index), Long_Float (Index));
      --     end loop;
      --     declare
      --        Progress : Test_Integer_B_Trees.Indicator;
      --     begin
      --        Tag (T, Progress);
      --     end;
      --     return;
      --  end;
   end;
   declare
      use Test_Integer_B_Trees;
      use Test_Integer_B_Trees.Tables;
      Count : constant := 10_000;
      File  : aliased Persistent_Array;
   begin
      Put_Line ("Testing external raw tables ...");
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
         subtype U is Interfaces.Unsigned_16;
         type Min_Max is record
            Min : U;
            Max : U;
         end record;

         function Image (Value : U) return String is
         begin
            return Image (Integer (Value));
         end Image;

         function Get_Tag (Item : Item_Ptr) return Min_Max is
            function To_Min_Max is
               new Ada.Unchecked_Conversion (Byte_Index, Min_Max);
         begin
            return
               To_Min_Max
               (  Persistent.Memory_Pools.Streams.External_B_Tree.
                  Get_Tag
                  (  Item
               )  );
         end Get_Tag;

         Pool : aliased Persistent_Pool (File'Access);
         T    : B_Tree (Pool'Access);

         function Visit_Item
                  (  Tree : B_Tree;
                     Key  : Byte_Index;
                     Item : Item_Ptr
                  )  return Boolean is
         begin
            Put_Line
            (  Image (U (Key))
            &  Character'Val (9) & Character'Val (9) & Character'Val (9)
            &  "at "
            &  Image (Get_Index (Item))
            &  "/"
            &  Image (Get_Bucket_Size (Item))
            &  " "
            &  Image (Get_Bucket_Address (Item))
            &  Get_Parent (Item)
            &  Character'Val (9)
            &  " Min/Max "
            &  Image (Get_Tag (Item).Min)
            &  ".."
            &  Image (Get_Tag (Item).Max)
            );
            return True;
         end Visit_Item;

         function Visit_Range
                  (  Tree : B_Tree;
                     Item : Item_Ptr
                  )  return Bucket_Traversal is
            use Test_Integer_B_Trees.External_Ptr;
         begin
            Put_Line
            (  Image (U (Get_Key (Get_First (Get_Item (Item, 1)))))
            &  ".."
            &  Image
               (  U
                  (  Get_Key
                     (  Get_Last
                        (  Get_Item (Item, Get_Bucket_Size (Item))
               )  )  )  )
            &  " ["
            &  Image (U (Get_Key (Get_Item (Item, 1))))
            &  ".."
            &  Image
               (  U (Get_Key (Get_Item (Item, Get_Bucket_Size (Item))))
               )
            &  "] "
            &  Character'Val (9)
            &  "at "
            &  Image (Get_Index (Item))
            &  "/"
            &  Image (Get_Bucket_Size (Item))
            &  " "
            &  Image (Get_Bucket_Address (Item))
            &  " "
            &  Get_Parent (Item)
            &  Character'Val (9)
            &  " Min/Max "
            &  Image (Get_Tag (Item).Min)
            &  ".."
            &  Image (Get_Tag (Item).Max)
            );
            return Step_Over;
         end Visit_Range;

         procedure Traverse is new Generic_Traverse;

         procedure Tag (Root : Item_Ptr; Prefix : String := "") is
            use Test_Integer_B_Trees;
            Result : Min_Max := (U'Last, U'First);
            Length : constant Natural := Get_Bucket_Size (Root);
            Child  : Item_Ptr;
            This   : Min_Max;
            function From_Min_Max is
               new Ada.Unchecked_Conversion (Min_Max, Byte_Index);
         begin
            if Length > 0 then
               Put
               (  Prefix
               &  Image (Length)
               &  " "
               &  Image (Get_Bucket_Address (Root))
               &  "   "
               );
               for Item in 1..Length loop
                  if Item /= 1 then
                     Put (" | ");
                  end if;
                  Put (Image (U (Get_Key (Get_Item (Root, Item)))));
                  This.Min   := U (Get_Value (Get_Item (Root, Item)));
                  Result.Min := U'Min (Result.Min, This.Min);
                  Result.Max := U'Max (Result.Max, This.Min);
               end loop;
               New_Line;
               for Item in 1..Length loop
                  Child := Get_Left_Child (Get_Item (Root, Item));
                  if Child /= No_Item then
                     Tag (Child, Prefix & "   ");
                     This := Get_Tag (Child);
                     Result.Min := U'Min (Result.Min, This.Min);
                     Result.Max := U'Max (Result.Max, This.Max);
                  end if;
               end loop;
               Child := Get_Right_Child (Get_Item (Root, Length));
               if Child /= No_Item then
                  Tag (Child, Prefix & "   ");
                  This := Get_Tag (Child);
                  Result.Min := U'Min (Result.Min, This.Min);
                  Result.Max := U'Max (Result.Max, This.Max);
               end if;
               Set_Tag (Root, From_Min_Max (Result));
            end if;
         end Tag;

         Value : constant Byte_Index := 10;
         Count : Natural := 0;
         type Byte_Index_Array is
            array (Positive range <>) of Byte_Index;
         Found : Byte_Index_Array (1..100);
         function Generate (From, To : Byte_Index)
            return Byte_Index_Array is
            Length : Natural := 0;
         begin
            for Index in Byte_Index range From..To loop
               if 10 = (Index mod 20) then
                  Length := Length + 1;
                  Found (Length) := Index;
               end if;
            end loop;
            return Found (1..Length);
         end Generate;
         Expected : constant Byte_Index_Array := Generate (100, 500);

         function Find_Item
                  (  Tree : B_Tree;
                     Key  : Byte_Index;
                     Item : Item_Ptr
                  )  return Boolean is
         begin
            if Get_Value (Item) = Value then
               Count := Count + 1;
               Found (Count) := Key;
            end if;
            return True;
         end Find_Item;

         function Find_Range
                  (  Tree : B_Tree;
                     Item : Item_Ptr
                  )  return Bucket_Traversal is
            This : constant Min_Max := Get_Tag (Item);
         begin
            if Value in Byte_Index (This.Min)..Byte_Index (This.Max)
            then
               return Step_In;
            else
               return Step_Over;
            end if;
         end Find_Range;

         procedure Find is
            new Generic_Traverse
                (  Visit_Item  => Find_Item,
                   Visit_Range => Find_Range
                );
      begin
         Put_Line ("Testing B-tree traversal...");
         for Index in Byte_Index range 1..1000 loop
            Add (T, Index, Index mod 20);
         end loop;
         Tag (Get_Root (T));
         Dump (T);
         Traverse (T, Inf (T, 100), 500);
         Find (T, Inf (T, 100), 500);
         for Index in Expected'Range loop
            if (  Index > Count
               or else
                  Found (Index) /= Expected (Index)
               )  then
               Raise_Exception
               (  Data_Error'Identity,
                  "Not found" & Byte_Index'Image (Expected (Index))
               );
            end if;
         end loop;
         if Count > Expected'Length then
            Raise_Exception
            (  Data_Error'Identity,
               "Not to be found" &
               Byte_Index'Image (Found (Expected'Length + 1))
            );
         end if;
      end;
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

      function Visit_Item
               (  Tree : B_Tree;
                  Key  : Integer;
                  Item : Item_Ptr
               )  return Boolean is
      begin
         Put_Line (Image (Key));
         return True;
      end Visit_Item;

      function Visit_Range
               (  Tree : B_Tree;
                  Item : Item_Ptr
               )  return Bucket_Traversal is
      begin
         Put_Line
         (  Image (Get_Key (Get_Item (Item, 1)))
         &  ".."
         &  Image (Get_Key (Get_Item (Item, Get_Bucket_Size (Item))))
         );
         return Step_Over;
      end Visit_Range;

      procedure Traverse is
         new Test_Integer_B_Trees.External_Ptr.Generic_Traverse;
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
         Erase (T);
         for Index in 1..1_400 loop
            Add (T, Index, Byte_Index (Index));
         end loop;
         Traverse (T, Inf (T, 340), 1_300);
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
