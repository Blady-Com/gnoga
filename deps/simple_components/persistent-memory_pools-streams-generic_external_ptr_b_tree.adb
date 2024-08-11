--                                                                    --
--  package                          Copyright (c) Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Streams.            Luebeck            --
--        Generic_External_Ptr_B_Tree              Autumn, 2014       --
--  Implementation                                                    --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

with Persistent.Blocking_Files.Text_IO;
use  Persistent.Blocking_Files.Text_IO;

with Persistent.Memory_Pools.Streams;
use  Persistent.Memory_Pools.Streams;

with Persistent.Memory_Pools.Dump;
use  Persistent.Memory_Pools.Dump;

--  with Ada.Text_IO,
--       Strings_Edit.Integers,
--       Persistent.Blocking_Files.Text_IO;

package body Persistent.Memory_Pools.Streams.
             Generic_External_Ptr_B_Tree is

   Left_Half     : constant Positive := (Width + 1) / 2 - 1;
   Right_Half    : constant Positive := Width - Left_Half;

   type Byte_Index_Array is array (Positive range <>) of Byte_Index;

   function To_Child (No : Integer) return Block_Offset is
      pragma Inline (To_Child);
   begin
      return Child_Offset + (Block_Offset (No) - 1) * 8;
   end To_Child;

   function To_Key (No : Integer) return Block_Offset is
      pragma Inline (To_Key);
   begin
      return Key_Offset + (Block_Offset (No) - 1) * 8;
   end To_Key;

   function To_Value (No : Integer) return Block_Offset is
      pragma Inline (To_Value);
   begin
      return Value_Offset + (Block_Offset (No) - 1) * 8;
   end To_Value;

   function Get_Child
            (  Block : Block_Type;
               No    : Positive
            )  return Byte_Index is
      pragma Inline (Get_Child);
   begin
      return Get (Block, To_Child (No));
   end Get_Child;

   function Get_Key
            (  Block : Block_Type;
               No    : Positive
            )  return Byte_Index is
      pragma Inline (Get_Key);
   begin
      return Get (Block, To_Key (No));
   end Get_Key;

   function Get_Length (Block : Block_Type) return Natural is
      pragma Inline (Get_Length);
   begin
      return Natural (Unsigned_16'(Get (Block, Length_Offset)));
   end Get_Length;

   function Get_Node_Tag (Block : Block_Type) return Byte_Index is
      pragma Inline (Get_Node_Tag);
   begin
      return Get (Block, Node_Tag_Offset);
   end Get_Node_Tag;

   function Get_Parent_Address (Block : Block_Type) return Byte_Index is
      pragma Inline (Get_Parent_Address);
   begin
      return Get (Block, Parent_Address_Offset);
   end Get_Parent_Address;

   function Get_Parent_Index (Block : Block_Type) return Natural is
      pragma Inline (Get_Parent_Index);
   begin
      return Natural (Unsigned_16'(Get (Block, Parent_Index_Offset)));
   end Get_Parent_Index;

   function Get_Pointer
            (  Block : Block_Type;
               No    : Positive
            )  return Byte_Index is
      pragma Inline (Get_Pointer);
   begin
      return Get (Block, To_Value (No));
   end Get_Pointer;

   function Get_Root_Address (Container : B_Tree) return Byte_Index is
      Lock : Holder (Container.Pool);
   begin
      return Container.Root_Bucket;
   end Get_Root_Address;

   function New_Key
            (  Pool : access Persistent_Pool'Class;
               Key  : Key_Type
            )  return Byte_Index is
      Stream : aliased Unchecked_Output_Stream (Pool);
   begin
      Output_Key (Stream'Access, Key);
      return Get_First (Stream);
   end New_Key;

   procedure Set_Child
             (  Block : in out Block_Type;
                No    : Positive;
                Child : Byte_Index
             )  is
   begin
      Put (Block, To_Child (No), Child);
   end Set_Child;

   procedure Set_Key
             (  Block : in out Block_Type;
                No    : Positive;
                Key   : Byte_Index
             )  is
   begin
      Put (Block, To_Key (No), Key);
   end Set_Key;

   procedure Set_Length
             (  Block  : in out Block_Type;
                Length : Natural
             )  is
   begin
      Put (Block, Length_Offset, Unsigned_16 (Length));
   end Set_Length;

   procedure Set_Node_Tag
             (  Block : in out Block_Type;
                Tag   : Byte_Index
             )  is
   begin
      Put (Block, Node_Tag_Offset, Tag);
   end Set_Node_Tag;

   procedure Set_Parent_Address
             (  Block  : in out Block_Type;
                Parent : Byte_Index
             )  is
   begin
      Put (Block, Parent_Address_Offset, Parent);
   end Set_Parent_Address;

   procedure Set_Parent_Index
             (  Block : in out Block_Type;
                Index : Positive
             )  is
   begin
      Put (Block, Parent_Index_Offset, Unsigned_16 (Index));
   end Set_Parent_Index;

   procedure Set_Pointer
             (  Block   : in out Block_Type;
                No      : Positive;
                Pointer : Byte_Index
             )  is
   begin
      Put (Block, To_Value (No), Pointer);
   end Set_Pointer;

--     procedure Dump
--               (  Pool   : B_Tree'Class;
--                  Node   : Byte_Index;
--                  Prefix : String := ""
--               )  is
--        use Ada.Text_IO;
--        use Strings_Edit.Integers;
--        use Persistent.Blocking_Files.Text_IO;
--        Length : Natural;
--        Child  : Byte_Index;
--     begin
--        Put (Prefix);
--        Put ("at ");
--        Put (Image (Node));
--        declare
--           Block : Block_Type renames Load (Pool.Pool.File, Node).all;
--        begin
--           Length := Get_Length (Block);
--           if Get_Parent_Address (Block) /= 0 then
--              Put (" [");
--              Put (Image (Get_Parent_Address (Block)));
--              Put ("|");
--              Put (Image (Get_Parent_Index (Block)));
--              Put ("]");
--           end if;
--        end;
--        Put (":");
--        declare
--           Key : Byte_Index;
--        begin
--           for Index in 1..Length loop
--              declare
--                 Block : Block_Type renames
--                         Load (Pool.Pool.File, Node).all;
--              begin
--                 Child := Get_Child (Block, Index);
--                 if Child = 0 then
--                    Put (" .");
--                 else
--                    Put (' ' & Image (Child));
--                 end if;
--                 Put (" /");
--                 Key := Get_Key (Block, Index);
--              end;
--              Put (Image (Get_Key (Pool, Key)));
--              Put ("\");
--           end loop;
--           declare
--              Block : Block_Type renames Load (Pool.Pool.File, Node).all;
--           begin
--              Child := Get_Child (Block, Length + 1);
--              if Child = 0 then
--                 Put (" .");
--              else
--                 Put (' ' & Image (Child));
--              end if;
--           end;
--        end;
--        New_Line;
--        for Index in 1..Length + 1 loop
--           declare
--              Block : Block_Type renames Load (Pool.Pool.File, Node).all;
--           begin
--              Child := Get_Child (Block, Index);
--           end;
--           if Child /= 0 then
--              Dump (Pool, Child, "  " & Prefix & Image (Index) & ".");
--           end if;
--        end loop;
--     end Dump;

   procedure Set_Parent
             (  File      : access Persistent_Array'Class;
                Child     : Byte_Index;
                Parent    : Byte_Index;
                Index     : Positive
             )  is
   begin
      if Child /= 0 then
         declare
            Block : Block_Type renames Update (File, Child).all;
         begin
            Set_Parent_Address (Block, Parent);
            Set_Parent_Index   (Block, Index);
         end;
      end if;
   end Set_Parent;

   procedure Copy
             (  File       : access Persistent_Array'Class;
                To_Node    : Byte_Index;
                To_First   : Positive;
                To_Last    : Natural;
                From_Node  : Byte_Index;
                From_First : Positive;
                Tail       : Natural := 1
             )  is
      From_Last : constant Integer := From_First + To_Last - To_First;
      Children  : Byte_Index_Array (To_First..To_Last + Tail);
      Keys   : Byte_Array (To_Key (To_First)..To_Key (To_Last) + 7);
      Values : Byte_Array (To_Value (To_First)..To_Value (To_Last) + 7);
   begin
      declare
         Block : Block_Type renames Load (File, From_Node).all;
      begin
         Keys :=
            Block
            (  To_Key (From_First)
            .. To_Key (From_Last) + 7
            );
         Values :=
            Block
            (  To_Value (From_First)
            .. To_Value (From_Last) + 7
            );
         for Index in Children'Range loop
            Children (Index) :=
               Get_Child (Block, From_First + Index - To_First);
         end loop;
      end;
      declare
         Block : Block_Type renames Update (File, To_Node).all;
      begin
         Block (Keys'Range)   := Keys;
         Block (Values'Range) := Values;
         for Index in Children'Range loop
            Set_Child (Block, Index, Children (Index));
         end loop;
      end;
      for Index in Children'Range loop
         if Children (Index) /= 0 then
            Set_Parent (File, Children (Index), To_Node, Index);
         end if;
      end loop;
   end Copy;

   procedure Copy
             (  File       : access Persistent_Array'Class;
                To_Node    : Byte_Index;
                To_Index   : Positive;
                From_Node  : Byte_Index;
                From_Index : Positive;
                Increment  : Boolean := False
             )  is
      Key     : Byte_Index;
      Pointer : Byte_Index;
   begin
      declare
         Block : Block_Type renames Load (File, From_Node).all;
      begin
         Key := Get_Key (Block, From_Index);
         Pointer := Get_Pointer (Block, From_Index);
      end;
      declare
         Block : Block_Type renames Update (File, To_Node).all;
      begin
         Set_Key (Block, To_Index, Key);
         Set_Pointer (Block, To_Index, Pointer);
         if Increment then
            Set_Length (Block, Get_Length (Block) + 1);
         end if;
      end;
   end Copy;

   procedure Move
             (  File       : access Persistent_Array'Class;
                Node       : Byte_Index;
                To_First   : Positive;
                To_Last    : Natural;
                From_First : Positive;
                Tail       : Natural := 1
             )  is
      pragma Inline (Move);
      From_Last : constant Integer := From_First + To_Last - To_First;
      Children  : Byte_Index_Array (To_First..To_Last + Tail);
   begin
      declare
         Block : Block_Type renames Update (File, Node).all;
      begin
         Block (To_Key (To_First)..To_Key (To_Last) + 7) :=
            Block (To_Key (From_First)..To_Key (From_Last) + 7);
         Block (To_Value (To_First)..To_Value (To_Last) + 7) :=
            Block (To_Value (From_First)..To_Value (From_Last) + 7);
         Block (To_Child (To_First)..To_Child (To_Last + Tail) + 7) :=
            Block
            (  To_Child (From_First)
            .. To_Child (From_Last + Tail) + 7
            );
         for Index in Children'Range loop
            Children (Index) := Get_Child (Block, Index);
         end loop;
      end;
      for Index in Children'Range loop
         if Children (Index) /= 0 then -- The same parent only index
            Set_Parent_Index           -- change
            (  Update (File, Children (Index)).all,
               Index
            );
         end if;
      end loop;
   end Move;

   function Is_Empty (Container : B_Tree) return Boolean is
      Lock : Holder (Container.Pool);
   begin
      if Container.Root_Bucket = 0 then
         return True;
      else
         declare
            Block : Block_Type renames
                    Load (Container.Pool.File, Container.Root_Bucket).all;
         begin
            return Get_Length (Block) = 0;
         end;
      end if;
   end Is_Empty;

   function Is_In (Container : B_Tree; Key : Key_Type)
      return Boolean is
      Lock  : Holder (Container.Pool);
      Node  : Byte_Index;
      Index : Integer;
   begin
      Search (Container, Container.Root_Bucket, Key, Node, Index);
      return Node /= 0 and then Index > 0;
   end Is_In;

   procedure Add
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Pointer   : Byte_Index
             )  is
      Pool      : Persistent_Pool'Class renames Container.Pool.all;
      Lock      : Holder (Container.Pool);
      Key_Index : Byte_Index := 0;
   begin
      Key_Index := New_Key (Container.Pool, Key);
      if Container.Root_Bucket = 0 then
         New_Root (Container, Key_Index, Pointer, 0, 0);
      else
         declare
            Node  : Byte_Index;
            Index : Integer;
         begin
            Search (Container, Container.Root_Bucket, Key, Node, Index);
            if Index < 0 then
               Insert
               (  Key       => Key,
                  Key_Index => Key_Index,
                  Pointer   => Pointer,
                  Child     => 0,
                  Parent    => (  Container'Unchecked_Access,
                                  Node,
                                 -Index
               )               );
            else
               Raise_Exception (Constraint_Error'Identity, Key_In_Use);
            end if;
         end;
      end if;
   exception
      when others =>
         if Key_Index /= 0 then
            Unchecked_Deallocate (Pool, Key_Index);
         end if;
         raise;
   end Add;

   procedure Erase
             (  Container : in out B_Tree;
                Root      : Byte_Index
             )  is
      Pool   : Persistent_Pool'Class renames Container.Pool.all;
      Length : Integer;
   begin
      if Root /= 0 then
         declare
            Block : Block_Type renames Load (Pool.File, Root).all;
         begin
            Length := Get_Length (Block);
            declare
               Ptr : Byte_Index_Array (1..Length * 2);
            begin
               for Index in 1..Length loop
                  Ptr (Index) := Get_Key (Block, Index);
                  Ptr (Length + Index) := Get_Pointer (Block, Index);
               end loop;
            end;
         end;
         for Index in 1..Length + 1 loop
            Erase
            (  Container,
               Get_Child (Load (Pool.File, Root).all, Index)
            );
         end loop;
         Unchecked_Deallocate (Pool, Root);
      end if;
   end Erase;

   procedure Erase (Container : in out B_Tree) is
      Lock : Holder (Container.Pool);
   begin
      Erase (Container, Container.Root_Bucket);
      Container.Root_Bucket := 0;
   end Erase;

   procedure Finalize (Container : in out B_Tree) is
      use Ada.Finalization;
   begin
--        if (  Exception_Identity
--              (  GNAT.Most_Recent_Exception.Occurrence
--              )
--           /= Null_ID
--           )
--        then
--           Ada.Text_IO.Put_Line
--           (  "Finalize entered with exception "
--           &  Exception_Information
--              (  GNAT.Most_Recent_Exception.Occurrence
--           )  );
--        end if;
      if Is_Writable (Container.Pool.File.all) then
         Erase (Container);
      end if;
      Finalize (Limited_Controlled (Container));
   end Finalize;

   function Find
            (  Container : B_Tree;
               Block     : Block_Type;
               Key       : Key_Type
            )  return Integer is
      pragma Inline (Find);
      Size : constant Natural := Get_Length (Block);
      From : Natural := 0;
      To   : Natural := Size + 1;
      This : Natural;
   begin
      if Size = 0 then
         return -1;
      end if;
      declare
         Keys : constant Byte_Array :=
                   Block
                   (  Key_Offset
                   .. Key_Offset + Block_Offset (Size) * 8 - 1
                   );
      begin
         loop
            This := (From + To) / 2;
            declare
               Current : constant Key_Type :=
                  Get_Key
                  (  Container,
                     Get
                     (  Keys,
                        Keys'First + (Block_Offset (This) - 1) * 8
                  )  );
            begin
               if Key = Current then
                  return This;
               elsif Key < Current then
                  if This - From <= 1 then
                     return -This;
                  end if;
                  To := This;
               else
                  if To - This <= 1 then
                     return - This - 1;
                  end if;
                  From := This;
               end if;
            end;
         end loop;
      end;
   end Find;

   function Find (Container : B_Tree; Key : Key_Type) return Item_Ptr is
      Lock  : Holder (Container.Pool);
      Node  : Byte_Index;
      Index : Integer;
   begin
      Search (Container, Container.Root_Bucket, Key, Node, Index);
      if Index <= 0 then
         return No_Item;
      else
         return (Container.Self, Node, Index);
      end if;
   end Find;

   type Children_Location is (Left, Right);

   procedure Generic_Traverse
             (  Container : B_Tree;
                From      : Item_Ptr;
                To        : Key_Type
             )  is
      Stop : Boolean  := False;
      This : Item_Ptr := From;
      Next : Item_Ptr;

      function Overshot (Children_At : Children_Location)
         return Boolean is
         Key  : constant Key_Type := Get_Key (Next);
         Skip : Item_Ptr;
      begin
         if To < Key then
            return True;
         end if;
         case Children_At is
            when Left =>
               Skip := Get_Left_Child (Next);
            when Right =>
               Skip := Get_Right_Child (This);
         end case;
         if Skip.Node /= 0 then
            case Visit_Range (Container, Skip) is
               when Quit =>
                  Stop := True;
                  return True;
               when Step_Over =>
                  null;
               when Step_In =>
                  loop
                     This := (Skip.Tree, Skip.Node, 1); -- The first
                     Skip := Get_Left_Child (This);     -- item in the
                     if Skip.Node = 0 then              -- bucket
                        Stop := not Visit_Item          -- Leaf visit it
                                    (  Container,       -- and return
                                       Get_Key (This),
                                       This
                                    );
                        return Stop;
                     end if;
                     case Visit_Range (Container, Skip) is
                        when Quit =>
                           Stop := True;
                           return True;
                        when Step_Over =>
                           This := Skip;
                           return False;
                        when Step_In =>
                           null;
                     end case;
                  end loop;
            end case;
         end if;
         Stop := not Visit_Item (Container, Key, Next);
         This := Next;
         return Stop;
      end Overshot;
   begin
      if This = No_Item then
         return;
      end if;
      declare
         Key : constant Key_TYpe := Get_Key (This);
      begin
         if To < Key or else not Visit_Item (Container, Key, This) then
            return;
         end if;
      end;
      loop
         Next := Get_Item (This, This.Index + 1);
         if Next.Node = 0 then
            declare -- Searching fpr a right parent item
               Current : Item_Ptr := This;
            begin
               loop
                  Next := Get_Right_Parent (Current);
                  exit when Next.Node /= 0;
                  Next := Get_Left_Parent (Current);
                  exit when Next.Node = 0;
                  Current := Next;
               end loop;
            end;
            if Next.Node = 0 or else Overshot (Right) then
               if Stop then
                  return;
               end if;
               Next := Get_Right_Child (This);
               if Next.Node = 0 or else Overshot (Left) or else Stop
               then
                  return;
               end if;
            end if;
         else
            if Overshot (Right) then
               exit when Stop;
               Next := Get_Right_Child (This);
               if Next.Node = 0 or else Overshot (Left) or else Stop
               then
                  return;
               end if;
            end if;
         end if;
      end loop;
   end Generic_Traverse;

   function Get
            (  Container : B_Tree;
               Key       : Key_Type
            )  return Byte_Index is
      Lock  : Holder (Container.Pool);
      Node  : Byte_Index;
      Index : Integer;
   begin
      Search (Container, Container.Root_Bucket, Key, Node, Index);
      if Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      return Get_Pointer (Load (Container.Pool.File, Node).all, Index);
   end Get;

   function Get_Bucket_Address (Item : Item_Ptr) return Byte_Index is
   begin
      return Item.Node;
   end Get_Bucket_Address;

   function Get_Bucket_Size (Item : Item_Ptr) return Natural is
   begin
      if Item.Node = 0 then
         return 0;
      else
         declare
            Pool : Persistent_Pool'Class renames Item.Tree.Pool.all;
            Lock : Holder (Item.Tree.Pool);
         begin
            return Get_Length (Load (Pool.File, Item.Node).all);
         end;
      end if;
   end Get_Bucket_Size;

   function Get_First
            (  Container : B_Tree;
               Root      : Byte_Index
            )  return Item_Ptr is
      Pool : Persistent_Pool'Class renames Container.Pool.all;
      This : Byte_Index := Root;
      Next : Byte_Index;
   begin
      if (  This = 0
         or else
            Get_Length (Load (Pool.File, This).all) = 0
         )
      then
         return No_Item;
      else
         loop
            Next := Get_Child (Load (Pool.File, This).all, 1);
            if Next = 0 then
               return (Container.Self, This, 1);
            end if;
            This := Next;
         end loop;
      end if;
   end Get_First;

   function Get_First (Container : B_Tree) return Item_Ptr is
      Lock : Holder (Container.Pool);
   begin
      return Get_First (Container, Container.Root_Bucket);
   end Get_First;

   function Get_First (Item : Item_Ptr) return Item_Ptr is
      Left : Item_Ptr;
   begin
      if Item.Node = 0 then
         return No_Item;
      else
         Left := Get_Left_Child (Item);
         if Left.Node = 0 then
            return Item;
         else
            return Get_First (Left.Tree.all, Left.Node);
         end if;
      end if;
   end Get_First;

   function Get_Index (Item : Item_Ptr) return Positive is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      return Item.Index;
   end Get_Index;

   function Get_Index
            (  Container : B_Tree;
               Key       : Key_Type
            )  return Byte_Index is
      Lock  : Holder (Container.Pool);
      Node  : Byte_Index;
      Index : Integer;
   begin
      Search (Container, Container.Root_Bucket, Key, Node, Index);
      if Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      return Get_Pointer (Load (Container.Pool.File, Node).all, Index);
   end Get_Index;

   function Get_Item (Item : Item_Ptr; Index : Positive)
      return Item_Ptr is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      declare
         Pool  : Persistent_Pool'Class renames Item.Tree.Pool.all;
         Lock  : Holder (Item.Tree.Pool);
         Block : Block_Type renames Load (Pool.File, Item.Node).all;
      begin
         if Index > Get_Length (Block) then
            return No_Item;
         else
            return (Item.Tree, Item.Node, Index);
         end if;
      end;
   end Get_Item;

   function Get_Key
            (  Container : B_Tree;
               Key       : Byte_Index
            )  return Key_Type is
      Stream : aliased Unchecked_Input_Stream (Container.Pool);
   begin
      Open (Stream, Key);
      return Input_Key (Stream'Access);
   end Get_Key;

   function Get_Key (Item : Item_Ptr) return Key_Type is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      declare
         Pool  : Persistent_Pool'Class renames Item.Tree.Pool.all;
         Lock  : Holder (Item.Tree.Pool);
         Block : Block_Type renames Load (Pool.File, Item.Node).all;
      begin
         if Item.Index > Get_Length (Block) then
            Raise_Exception (Constraint_Error'Identity, Bad_Key_Index);
         end if;
         return Get_Key (Item.Tree.all, Get_Key (Block, Item.Index));
      end;
   end Get_Key;

   function Get_Key_Address (Item : Item_Ptr) return Byte_Index is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      declare
         Pool  : Persistent_Pool'Class renames Item.Tree.Pool.all;
         Lock  : Holder (Item.Tree.Pool);
         Block : Block_Type renames Load (Pool.File, Item.Node).all;
      begin
         if Item.Index > Get_Length (Block) then
            Raise_Exception (Constraint_Error'Identity, Bad_Key_Index);
         end if;
         return Get_Key (Block, Item.Index);
      end;
   end Get_Key_Address;

   function Get_Last
            (  Container : B_Tree;
               Root      : Byte_Index
            )  return Item_Ptr is
      Pool   : Persistent_Pool'Class renames Container.Pool.all;
      This   : Byte_Index := Root;
      Next   : Byte_Index;
      Length : Integer;
   begin
      if This = 0 then
         return No_Item;
      end if;
      Length := Get_Length (Load (Pool.File, This).all);
      if Length < 0 then
         return No_Item;
      else
         loop
            Next :=
               Get_Child
               (  Load (Pool.File, This).all,
                  Length + 1
               );
            if Next = 0 then
               if Length = 0 then
                  return No_Item;
               else
                  return (Container.Self, This, Length);
               end if;
            end if;
            This   := Next;
            Length := Get_Length (Load (Pool.File, This).all);
         end loop;
      end if;
   end Get_Last;

   function Get_Last (Container : B_Tree) return Item_Ptr is
      Lock : Holder (Container.Pool);
   begin
      return Get_Last (Container, Container.Root_Bucket);
   end Get_Last;

   function Get_Last (Item : Item_Ptr) return Item_Ptr is
      Right : Item_Ptr;
   begin
      if Item.Node = 0 then
         return No_Item;
      else
         Right := Get_Right_Child (Item);
         if Right.Node = 0 then
            return Item;
         else
            return Get_Last (Right.Tree.all, Right.Node);
         end if;
      end if;
   end Get_Last;

   function Get_Left_Child (Item : Item_Ptr) return Item_Ptr is
   begin
      if Item.Node = 0 then
         return No_Item;
      end if;
      declare
         Pool   : Persistent_Pool'Class renames Item.Tree.Pool.all;
         Lock   : Holder (Item.Tree.Pool);
         Block  : Block_Type_Ref := Load (Pool.File, Item.Node);
         Length : Integer := Get_Length (Block.all);
         Result : Item_Ptr;
      begin
         if Item.Index > Length then
            Raise_Exception (Constraint_Error'Identity, Bad_Left_Index);
         end if;
         Result.Tree := Item.Tree;
         Result.Node := Get_Child (Block.all, Item.Index);
         if Result.Node = 0 then
            return No_Item;
         end if;
         Block  := Load (Pool.File, Result.Node);
         Length := Get_Length (Block.all);
         if Length = 0 then
            return No_Item;
         else
            Result.Index := Length;
            return Result;
         end if;
      end;
   end Get_Left_Child;

   function Get_Left_Parent (Item : Item_Ptr) return Item_Ptr is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      declare
         Pool  : Persistent_Pool'Class renames Item.Tree.Pool.all;
         Lock  : Holder (Item.Tree.Pool);
         Block : Block_Type renames Load (Pool.File, Item.Node).all;
         Node  : constant Byte_Index := Get_Parent_Address (Block);
      begin
         if Node = 0 then
            return No_Item;
         else
            declare
               Index : constant Natural := Get_Parent_Index (Block);
            begin
               if Index = 1 then
                  return No_Item;
               else
                  return (Item.Tree, Node, Index - 1);
               end if;
            end;
         end if;
      end;
   end Get_Left_Parent;

   function Get_Next
            (  Container : B_Tree;
               Node      : Byte_Index;
               Index     : Positive
            )  return Item_Ptr is
   begin
      if Node = 0 then
         return No_Item;
      end if;
      declare
         Pool   : Persistent_Pool'Class renames Container.Pool.all;
         Block  : Block_Type_Ref := Load (Pool.File, Node);
         Length : constant Integer := Get_Length (Block.all);
      begin
         if Index > Length then
            Raise_Exception (Constraint_Error'Identity, Bad_Next_Index);
         end if;
         declare
            First : Item_Ptr :=
                       Get_First
                       (  Container,
                          Get_Child (Block.all, Index + 1)
                       );
         begin
            if First.Node = 0 or else First.Node = Node then
               if Index = Length then
                  Block := Load (Pool.File, Node);
                  First.Tree := Container.Self;
                  First.Node := Get_Parent_Address (Block.all);
                  while First.Node /= 0 loop
                     First.Index := Get_Parent_Index (Block.all);
                     Block := Load (Pool.File, First.Node);
                     if First.Index <= Get_Length (Block.all) then
                        return First;
                     end if;
                     First.Node := Get_Parent_Address (Block.all);
                  end loop;
                  return No_Item;
               else
                  return (Container.Self, Node, Index + 1);
               end if;
            else
               return First;
            end if;
         end;
      end;
   end Get_Next;

   function Get_Next (Item : Item_Ptr) return Item_Ptr is
      Lock : Holder (Item.Tree.Pool);
   begin
      return Get_Next (Item.Tree.all, Item.Node, Item.Index);
   end Get_Next;

   function Get_Pointer (Item : Item_Ptr) return Byte_Index is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      declare
         Lock  : Holder (Item.Tree.Pool);
         Block : Block_Type renames
                 Load (Item.Tree.Pool.File, Item.Node).all;
      begin
         if Item.Index > Get_Length (Block) then
            Raise_Exception (Constraint_Error'Identity, Bad_Val_Index);
         end if;
         return Get_Pointer (Block, Item.Index);
      end;
   end Get_Pointer;

   function Get_Previous
            (  Container : B_Tree;
               Node      : Byte_Index;
               Index     : Positive
            )  return Item_Ptr is
   begin
      if Node = 0 then
         return No_Item;
      end if;
      declare
         Pool   : Persistent_Pool'Class renames Container.Pool.all;
         Block  : Block_Type_Ref   := Load (Pool.File, Node);
         Length : constant Integer := Get_Length (Block.all);
      begin
         if Index > Length then
            Raise_Exception (Constraint_Error'Identity, Bad_Prev_Index);
         end if;
         declare
            Last : Item_Ptr :=
                      Get_Last
                      (  Container,
                         Get_Child (Block.all, Index)
                      );
         begin
            if Last.Node = 0 or else Last.Node = Node then
               if Index = 1 then
                  Block := Load (Pool.File, Node);
                  Last.Tree := Container.Self;
                  Last.Node := Get_Parent_Address (Block.all);
                  while Last.Node /= 0 loop
                     Last.Index := Get_Parent_Index (Block.all);
                     if Last.Index > 1 then
                        return
                        (  Container.Self,
                           Last.Node,
                           Last.Index - 1
                        );
                     end if;
                     Block := Load (Pool.File, Last.Node);
                     Last.Node := Get_Parent_Address (Block.all);
                   end loop;
                   return No_Item;
               else
                  return (Container.Self, Node, Index - 1);
               end if;
            else
               return Last;
            end if;
         end;
      end;
   end Get_Previous;

   function Get_Previous (Item : Item_Ptr) return Item_Ptr is
      Lock : Holder (Item.Tree.Pool);
   begin
      return Get_Previous (Item.Tree.all, Item.Node, Item.Index);
   end Get_Previous;

   function Get_Right_Child (Item : Item_Ptr) return Item_Ptr is
   begin
      if Item.Node = 0 then
         return No_Item;
      end if;
      declare
         Pool   : Persistent_Pool'Class renames Item.Tree.Pool.all;
         Lock   : Holder (Item.Tree.Pool);
         Block  : Block_Type_Ref := Load (Pool.File, Item.Node);
         Length : Integer := Get_Length (Block.all);
         Result : Item_Ptr;
      begin
         if Item.Index > Length then
            Raise_Exception (Constraint_Error'Identity, Bad_Righ_Index);
         end if;
         Result.Tree := Item.Tree;
         Result.Node := Get_Child (Block.all, Item.Index + 1);
         if Result.Node = 0 then
            return No_Item;
         end if;
         Block  := Load (Pool.File, Result.Node);
         Length := Get_Length (Block.all);
         if Length = 0 then
            return No_Item;
         else
            Result.Index := 1;
            return Result;
         end if;
      end;
   end Get_Right_Child;

   function Get_Right_Parent (Item : Item_Ptr) return Item_Ptr is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      declare
         Pool  : Persistent_Pool'Class renames Item.Tree.Pool.all;
         Lock  : Holder (Item.Tree.Pool);
         Block : Block_Type renames Load (Pool.File, Item.Node).all;
         Node  : constant Byte_Index := Get_Parent_Address (Block);
      begin
         if Node = 0 then
            return No_Item;
         else
            declare
               Index : constant Natural := Get_Parent_Index (Block);
            begin
               if Index > Get_Length (Load (Pool.File, Node).all) then
                  return No_Item;
               else
                  return (Item.Tree, Node, Index);
               end if;
            end;
         end if;
      end;
   end Get_Right_Parent;

   function Get_Root (Item : Item_Ptr) return Item_Ptr is
      Container : B_Tree'Class renames Item.Tree.all;
   begin
      if Item.Node = 0 then
         return No_Item;
      else
         declare
            Pool  : Persistent_Pool'Class renames Container.Pool.all;
            Lock  : Holder (Container.Pool);
            Block : Block_Type_Ref := Load (Pool.File, Item.Node);
            Node  : Byte_Index := Item.Node;
            Next  : Byte_Index := Get_Parent_Address (Block.all);
         begin
            while Next /= 0 loop
               Node  := Next;
               Block := Load (Pool.File, Node);
               Next  := Get_Parent_Address (Block.all);
            end loop;
            return (Item.Tree, Node, 1);
         end;
      end if;
   end Get_Root;

   function Get_Root (Container : B_Tree) return Item_Ptr is
      Lock : Holder (Container.Pool);
   begin
      if Container.Root_Bucket = 0 then
         return No_Item;
      else
         return (Container.Self, Container.Root_Bucket, 1);
      end if;
   end Get_Root;

   function Get_Self (Container : B_Tree) return B_Tree_Ptr is
   begin
      return Container.Self;
   end Get_Self;

   function Get_Self (Item : Item_Ptr) return B_Tree_Ptr is
   begin
      return Item.Tree;
   end Get_Self;

   function Get_Tag (Item : Item_Ptr) return Byte_Index is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      declare
         Pool : Persistent_Pool'Class renames Item.Tree.Pool.all;
         Lock : Holder (Item.Tree.Pool);
      begin
         return Get_Node_Tag (Load (Pool.File, Item.Node).all);
      end;
   end Get_Tag;

   function Inf (Container : B_Tree; Key : Key_Type) return Item_Ptr is
      Lock  : Holder (Container.Pool);
      Node  : Byte_Index;
      Index : Integer;
   begin
      Search (Container, Container.Root_Bucket, Key, Node, Index);
      if Index = 0 then
         return No_Item;
      elsif Index > 0 then
         return (Container.Self, Node, Index);
      else
         declare
            Block : constant Block_Type_Ref :=
                    Load (Container.Pool.File, Node);
         begin
            if -Index > Get_Length (Block.all) then
               if Index < -1 then
                  return (Container.Self, Node, -Index - 1);
               else
                  return No_Item;
               end if;
            else
               return Get_Previous (Container, Node, -Index);
            end if;
         end;
      end if;
   end Inf;

   procedure Initialize (Container : in out B_Tree) is
      use Ada.Finalization;
      Lock : Holder (Container.Pool);
   begin
      Initialize (Limited_Controlled (Container));
   end Initialize;

   procedure Insert
              (  Container : in out B_Tree;
                 Parent    : Byte_Index;
                 Key       : Key_Type;
                 Key_Index : Byte_Index;
                 Pointer   : Byte_Index;
                 Child     : Byte_Index
             )  is
   begin
      if Parent = 0 then
         Insert
         (  (Container.Self, 0, 1),
            Key,
            Key_Index,
            Pointer,
            Child
         );
      else
         declare
            Index : constant Integer :=
                    Find
                    (  Container,
                       Load (Container.Pool.File, Parent).all,
                       Key
                   );
         begin
            if Index > 0 then
               Raise_Exception (Data_Error'Identity, Reinsert_Error);
            end if;
            Insert
            (  (Container.Self, Parent, -Index),
               Key,
               Key_Index,
               Pointer,
               Child
            );
         end;
      end if;
   end Insert;

   procedure Insert
              (  Container : in out B_Tree;
                 Parent    : Byte_Index;
                 Key_Index : Byte_Index;
                 Pointer   : Byte_Index;
                 Child     : Byte_Index
             )  is
   begin
      Insert
      (  Container,
         Parent,
         Get_Key (Container, Key_Index),
         Key_Index,
         Pointer,
         Child
      );
   end Insert;

   procedure Insert
             (  Parent    : Item_Ptr;
                Key       : Key_Type;
                Key_Index : Byte_Index;
                Pointer   : Byte_Index;
                Child     : Byte_Index
             )  is
      Container : B_Tree'Class renames Parent.Tree.all;
      Ancestor  : Byte_Index;
      Length    : Integer;
      Split     : Integer;
      Index     : Integer := Parent.Index;
   begin
      if Parent.Node = 0 then -- Creating new root node
         Raise_Exception (Data_Error'Identity, No_Node_Insert);
      end if;
      declare
         Block : Block_Type renames
                 Load (Container.Pool.File, Parent.Node).all;
      begin
         Length := Get_Length (Block);
         if Length < Width then -- Have place in the bucket
            Move
            (  Container.Pool.File,
               Parent.Node,
               Index  + 1,
               Length + 1,
               Index
            );
            Update
            (  Container,
               Parent.Node,
               Index,
               Key_Index,
               Pointer,
               Child,
               True
            );
            return;
         end if;
         Ancestor := Get_Parent_Address (Block);
         Split    := Get_Parent_Index   (Block);
      end;
      if Index = Width + 1 then
         declare
            Left  : Byte_Index;
            Space : Natural;
         begin
            if Ancestor /= 0 and then Split >= 2 then
               Split := Split - 1;
               Left  := Get_Child
                        (  Load (Container.Pool.File, Ancestor).all,
                           Split
                        );
               if Left /= 0 then
                  Space :=
                     Get_Length (Load (Container.Pool.File, Left).all);
                  if Space < Width then -- Underfilled left
                     Copy
                     (  Container.Pool.File, -- Append pair from the
                        Left,                -- parent to the left node
                        Space + 1,
                        Ancestor,
                        Split,
                        True            -- Increment the left's length
                     );
                     Copy
                     (  Container.Pool.File, -- Copy the first child
                        Left,                -- from the right node as
                        Space + 2,           -- the last child of the
                        Space + 1,           -- left node
                        Parent.Node,
                        1
                     );
                     Copy
                     (  Container.Pool.File, -- Move the first pair from
                        Ancestor,            -- the right node up to the
                        Split,               -- parent at the split
                        Parent.Node,
                        1
                     );
                     Move
                     (  Container.Pool.File, -- Move pairs and children
                        Parent.Node,         -- of the right node to the
                        1,                   -- left
                        Width - 1,
                        2,
                        0
                     );
                     Update
                     (  Container,   -- Insert the key and value at the
                        Parent.Node, -- end of the right node
                        Width,
                        Key_Index,
                        Pointer,
                        Child
                     );
                     return;
                  end if;
               end if;
            end if;
         end;
      end if;
      if Index = 1 then
         declare
            Right : Byte_Index;
            Space : Natural;
            Block : Block_Type renames
                    Load (Container.Pool.File, Ancestor).all;
         begin
            if Ancestor /= 0 and then Split <= Get_Length (Block) then
               Right := Get_Child (Block, Split + 1);
               if Right /= 0 then
                  Space :=
                     Get_Length (Load (Container.Pool.File, Right).all);
                  if Space < Width then
                     --
                     -- Fill right sibling without splitting the bucket
                     --
                     --                         Index = 1
                     --                    K5               K+ < K1
                     --         [ * | * | L | R | * ]      /
                     --                  /     \         C+
                     --   K1 K2 K3 K4   /       \ K6 K7
                     -- [C1|C2|C3|C4|C5]        [C6|C7|C8|  |  ]   |
                     --                                            |
                     --                    K4                      V
                     --         [ * | * | L | R | * ]
                     --                  /     \
                     --   K+ K1 K2 K3   /       \ K5 K6 K7
                     -- [C+|C1|C2|C3|C4]        [C5|C6|C7|C8|  ]
                     --
                     Move
                     (  Container.Pool.File, -- Move items of the right
                        Right,               -- node to the right
                        2,
                        Space + 1,
                        1
                     );
                     Copy
                     (  Container.Pool.File, -- Move last child from the
                        Right,               -- node to the beginning of
                        1,                   -- the right node
                        0,
                        Parent.Node,
                        Width + 1
                     );
                     Copy
                     (  Container.Pool.File, -- Move pair from the
                        Right,               -- parent node at the
                        1,                   -- split to the first place
                        Ancestor,            -- of the right node
                        Split,
                        True            -- Increment right node length
                     );
                     Copy
                     (  Container.Pool.File, -- Move last pair of the
                        Ancestor,            -- node to the parent node
                        Split,               -- at the split
                        Parent.Node,
                        Width
                     );
                     Move
                     (  Container.Pool.File, -- Move items in the node
                        Parent.Node,         -- to the right
                        2,
                        Width,
                        1
                     );
                     Update
                     (  Container,      -- Insert the new key, value
                        Parent.Node,    -- and the child at the index
                        1,              -- 1, which is now free
                        Key_Index,
                        Pointer,
                        Child
                     );
                     return;
                  end if;
               end if;
            end if;
         end;
      end if;
      declare
         New_Node : constant Byte_Index :=
                    Unchecked_Allocate (Container.Pool.all, Node_Size);
      begin
         declare
            Block : Block_Type renames
                    Update (Container.Pool.File, New_Node).all;
         begin
            Set_Length (Block, Left_Half);
            Set_Node_Tag (Block, 0);
         end;
         Set_Length
         (  Update (Container.Pool.File, Parent.Node).all,
            Right_Half
         );
         if Index = Left_Half + 1 then -- Insert at the split
            --
            -- The new key is at the split        Left_Half = 2
            --                                   Right_Half = 2
            --   K1 K2 K3 K4     + K2 < K+ < K3       Index = 3
            -- [C1|C2|C3|C4|C5]         C+
            --                  K+
            --   K1 K2         /  K3 K4
            -- [C1|C2|C+|  |  ] [C3|C4|C5|  |  ]
            --
            if Ancestor = 0 then
               New_Root
               (  Container,
                  Key_Index,
                  Pointer,
                  New_Node,
                  Parent.Node
               );
            else
               Insert
               (  Container,
                  Ancestor,
                  Key,
                  Key_Index,
                  Pointer,
                  New_Node
               );
            end if;
            Copy
            (  Container.Pool.File, -- Copy half of node to the left
               New_Node,            -- node
               1,
               Left_Half,
               Parent.Node,
               1
            );
            Set_Child
            (  Update (Container.Pool.File, New_Node).all,
               Left_Half + 1,
               Child
            );
            Move               -- Shift right node to the left
            (  Container.Pool.File,
               Parent.Node,
               1,
               Right_Half,
               Left_Half + 1
            );
            Set_Parent
            (  Container.Pool.File,
               Child,
               New_Node,
               Left_Half + 1
            );
         elsif Index <= Left_Half then
            --
            -- The new key is left of the split   Left_Half = 2
            --                                   Right_Half = 2
            --   K1 K2 K3 K4     + K1 < K+ < K2       Index = 1
            -- [C1|C2|C3|C4|C5]        /
            --                       C+
            --
            --                  K2 (the last left key)
            --   K1 K+         /  K3 K4
            -- [C1|C+|C2|  |  ] [C3|C4|C5|  |  ]
            --
            declare
               Block : Block_Type renames
                       Load (Container.Pool.File, Parent.Node).all;
            begin
               if Ancestor = 0 then
                  New_Root
                  (  Container,
                     Get_Key     (Block, Left_Half),
                     Get_Pointer (Block, Left_Half),
                     New_Node,
                     Parent.Node
                  );
                  else
                     Insert
                     (  Container,
                        Ancestor,
                        Get_Key     (Block, Left_Half),
                        Get_Pointer (Block, Left_Half),
                        New_Node
                     );
                  end if;
            end;
            Copy
            (  Container.Pool.File,  -- Copy items under the insertion
               New_Node,             -- point from the old node
               1,
               Index - 1,
               Parent.Node,
               1,
               0
            );
            Copy
            (  Container.Pool.File,  -- Copy items after the insetion
               New_Node,             -- point
               Index + 1,
               Left_Half + 1,
               Parent.Node,
               Index
            );
            Update
            (  Container,       -- Insert key-value pair and the
               New_Node,        -- child
               Index,
               Key_Index,
               Pointer,
               Child
            );
            Move
            (  Container.Pool.File,  -- Move left items in the right
               Parent.Node,          -- node
               1,
               Right_Half,
               Left_Half + 1
            );
         else
            --
            -- The new key is right of the split  Left_Half = 2
            --                                   Right_Half = 2
            --   K1 K2 K3 K4     + K4 < K+            Index = 5
            -- [C1|C2|C3|C4|C5]        /
            --                       C+
            --
            --                  K3 (the first right key)
            --   K1 K2         /  K4 K+
            -- [C1|C2|C3|  |  ] [C4|C+|C5|  |  ]
            --
            declare
               Block : Block_Type renames
                       Load (Container.Pool.File, Parent.Node).all;
            begin
               if Ancestor = 0 then
                  New_Root
                  (  Container,
                     Get_Key     (Block, Left_Half + 1),
                     Get_Pointer (Block, Left_Half + 1),
                     New_Node,
                     Parent.Node
                  );
               else
                  Insert
                  (  Container,
                     Ancestor,
                     Get_Key     (Block, Left_Half + 1),
                     Get_Pointer (Block, Left_Half + 1),
                     New_Node
                  );
               end if;
            end;
            Copy
            (  Container.Pool.File,
               New_Node,
               1,
               Left_Half,
               Parent.Node,
               1
            );
            Index := Index - Left_Half - 1;
            Move
            (  Container.Pool.File,
               Parent.Node,
               1,
               Index - 1,
               Left_Half + 2,
               0
            );
            Move
            (  Container.Pool.File,
               Parent.Node,
               Index + 1,
               Right_Half,
               Index + Left_Half + 1
            );
            Update
            (  Container,   -- Insert key-value pair and the
               Parent.Node, -- child
               Index,
               Key_Index,
               Pointer,
               Child
            );
         end if;
      end;
   end Insert;

   procedure New_Root
             (  Container : in out B_Tree;
                Key       : Byte_Index;
                Pointer   : Byte_Index;
                Left      : Byte_Index;
                Right     : Byte_Index
             )  is
      Node  : constant Byte_Index :=
              Unchecked_Allocate (Container.Pool.all, Node_Size);
      Block : Block_Type renames Update (Container.Pool.File, Node).all;
   begin
      Set_Length (Block, 1);
      Set_Parent_Address (Block, 0);
      Set_Node_Tag       (Block, 0);
      Set_Parent_Index   (Block, 1);
      Set_Key     (Block, 1, Key);
      Set_Pointer (Block, 1, Pointer);
      Set_Child   (Block, 1, Left);
      Set_Child   (Block, 2, Right);
      Container.Root_Bucket := Node;
      Set_Parent (Container.Pool.File, Left,  Node, 1);
      Set_Parent (Container.Pool.File, Right, Node, 2);
   exception
      when others =>
         if Node /= 0 then
            Unchecked_Deallocate (Container.Pool.all, Node);
         end if;
         raise;
   end New_Root;

   procedure New_Root
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Pointer   : Byte_Index;
                Left      : Byte_Index;
                Right     : Byte_Index
             )  is
      Key_Index : constant Byte_Index := New_Key (Container.Pool, Key);
   begin
      New_Root (Container, Key_Index, Pointer, Left, Right);
   exception
      when others =>
         if Key_Index /= 0 then
            Unchecked_Deallocate (Container.Pool.all, Key_Index);
         end if;
         raise;
   end New_Root;

   procedure Remove
             (  Container : in out B_Tree;
                Node      : Byte_Index;
                Index     : Positive;
                Pointer   : out Byte_Index
             )  is
      Pool : Persistent_Pool'Class renames Container.Pool.all;
   begin
      if Node = 0 then
         Pointer := 0;
         return;
      end if;
      declare
         Block  : Block_Type renames Update (Pool.File, Node).all;
         Key    : Byte_Index;
         Length : Integer := Get_Length (Block);
      begin
         if Index > Length then
            Raise_Exception (Constraint_Error'Identity, Remove_Error);
         end if;
         Key     := Get_Key (Block, Index);
         Pointer := Get_Pointer (Block, Index);
         if Get_Child (Block, Index) = 0 then
            if Get_Child (Block, Index + 1) = 0 then
               if Length > 1 then
                  Set_Length (Block, Length - 1);
                  Move
                  (  Pool.File,
                     Node,
                     Index,
                     Length - 1,
                     Index + 1
                  );
               else
                  declare
                     Parent : constant Byte_Index :=
                              Get_Parent_Address (Block);
                     Index  : constant Positive :=
                              Get_Parent_Index (Block);
                  begin
                     if Parent = 0 then -- No parent
                        Set_Length (Block, 0);
                     else -- Remove at the parent
                        Set_Child
                        (  Update (Pool.File, Parent).all,
                           Index,
                           0
                        );
                        Unchecked_Deallocate (Pool, Node);
                     end if;
                  end;
               end if;
            else
               if Length > 1 then
                  Set_Length (Block, Length - 1);
                  Move
                  (  Pool.File,
                     Node,
                     Index,
                     Length - 1,
                     Index + 1
                  );
               else
                  declare
                     Parent : constant Byte_Index :=
                              Get_Parent_Address (Block);
                     Right  : constant Byte_Index :=
                              Get_Child (Block, 2);
                  begin
                     if Parent = 0 then -- No parent
                        Length :=
                           Get_Length (Load (Pool.File, Right).all);
                        Set_Length
                        (  Update (Pool.File, Node).all,
                           Length
                        );
                        Copy (Pool.File, Node, 1, Length, Right, 1);
                        Unchecked_Deallocate (Pool, Right);
                     else
                        Set_Parent
                        (  Pool.File,
                           Right,
                           Parent,
                           Get_Parent_Index (Block)
                        );
                        Unchecked_Deallocate (Pool, Node);
                     end if;
                  end;
               end if;
            end if;
         elsif Get_Child (Block, Index + 1) = 0 then
            declare
               Parent : constant Byte_Index :=
                        Get_Parent_Address (Block);
               Left   : constant Byte_Index := Get_Child (Block, 1);
            begin
               if Length > 1 then
                  Set_Length (Block, Length - 1);
                  Move
                  (  Pool.File,
                     Node,
                     Index,
                     Length - 1,
                     Index + 1
                  );
                  Set_Child
                  (  Update (Pool.File, Node).all,
                     Index,
                     Left
                  );
               else
                  if Parent = 0 then -- No parent
                     Length :=
                        Get_Length (Load (Pool.File, Left).all);
                     Set_Length
                     (  Update (Pool.File, Node).all,
                        Length
                     );
                     Copy (Pool.File, Node, 1, Length, Left, 1);
                     Unchecked_Deallocate (Pool, Left);
                  else
                     Set_Parent
                     (  Pool.File,
                        Left,
                        Parent,
                        Get_Parent_Index (Block)
                     );
                     Unchecked_Deallocate (Pool, Node);
                  end if;
               end if;
            end;
         else
            declare
               Left  : Byte_Index := Get_Child (Block, Index);
               Right : Byte_Index := Get_Child (Block, Index + 1);
            begin
               if (  Get_Length (Load (Pool.File, Left).all)
                  >  Get_Length (Load (Pool.File, Right).all)
                  )
               then
                  --
                  -- Rotate left          K-
                  --               [ * | L | R | * ]
                  --                    /     \
                  --     K1  K2  K3    /       \  K6
                  --  [ * | * | * | * ]        [ * | * |   |   ]
                  --                 \
                  --                  \  K4  K5                        |
                  --                  [ * | * | 0 |   ]                |
                  --                                                   V
                  --                      K5
                  --               [ * | L | R |   ]
                  --                    /     \
                  --     K1  K2  K3    /       \  K6
                  --  [ * | * | * | * ]        [ * | * |   |   ]
                  --                 \
                  --                  \  K4
                  --                  [ * | * |   |   ]
                  --
                  loop
                     declare
                        Block : Block_Type renames
                                Load (Pool.File, Left).all;
                     begin
                        Length := Get_Length (Block);
                        Right  := Get_Child (Block, Length + 1);
                     end;
                     exit when Right = 0;
                     Left := Right;
                  end loop;
                  Copy (Pool.File, Node, Index, Left, Length);
                  declare
                     Block : Block_Type renames
                             Update (Pool.File, Left).all;
                  begin
                     if Length = 1 then
                        Set_Parent
                        (  Pool.File,
                           0,
                           Get_Parent_Address (Block),
                           Get_Parent_Index (Block)
                        );
                        Unchecked_Deallocate (Pool, Left);
                     else
                        Set_Length (Block, Length - 1);
                     end if;
                  end;
               else
                  -- Rotate right         K-
                  --               [ * | L | R | * ]
                  --                    /     \
                  --     K1  K2        /       \  K6  K7
                  --  [ * | * | * |   ]        [ * | * | * |   ]
                  --                            /
                  --             K3  K4        /
                  --          [ 0 | * | * |   ]
                  --
                  --                      K3
                  --               [ * | L | R | * ]
                  --                    /     \
                  --     K1  K2        /       \  K6  K7
                  --  [ * | * | * |   ]        [ * | * | * |   ]
                  --                            /
                  --             K4            /
                  --          [ * | * |   |   ]
                  --
                  loop
                     declare
                        Block : Block_Type renames
                                Load (Pool.File, Right).all;
                     begin
                        Length := Get_Length (Block);
                        Left   := Get_Child (Block, Length + 1);
                     end;
                     exit when Left = 0;
                     Right := Left;
                  end loop;
                  Copy (Pool.File, Node, Index, Right, 1);
                  declare
                     Block : Block_Type renames
                             Update (Pool.File, Right).all;
                  begin
                     if Length = 1 then
                        Set_Parent
                        (  Pool.File,
                           0,
                           Get_Parent_Address (Block),
                           Get_Parent_Index (Block)
                        );
                        Unchecked_Deallocate (Pool, Right);
                     else
                        Set_Length (Block, Length - 1);
                        Move (Pool.File, Right, 1, Length - 1, 2);
                     end if;
                  end;
               end if;
            end;
         end if;
         Unchecked_Deallocate (Pool, Key);
      end;
   end Remove;

   procedure Remove
             (  Item    : in out Item_Ptr;
                Pointer : out Byte_Index
             )  is
      Container : B_Tree'Class renames Item.Tree.all;
      Lock      : Holder (Container.Pool);
   begin
      Remove (Container, Item.Node, Item.Index, Pointer);
      Item := No_Item;
   end Remove;

   procedure Remove (Item : in out Item_Ptr) is
      Container : B_Tree'Class renames Item.Tree.all;
      Pointer   : Byte_Index;
      Lock      : Holder (Container.Pool);
   begin
      Remove (Container, Item.Node, Item.Index, Pointer);
      Item := No_Item;
   end Remove;

   procedure Remove
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Pointer   : out Byte_Index
             )  is
      Lock  : Holder (Container.Pool);
      Node  : Byte_Index;
      Index : Integer;
   begin
      Search (Container, Container.Root_Bucket, Key, Node, Index);
      if Index > 0 then
         Remove (Container, Node, Index, Pointer);
      end if;
   end Remove;

   procedure Remove (Container : in out B_Tree; Key : Key_Type) is
      Replaced : Byte_Index;
      Node     : Byte_Index;
      Index    : Integer;
      Lock     : Holder (Container.Pool);
   begin
      Search (Container, Container.Root_Bucket, Key, Node, Index);
      if Index > 0 then
         Remove (Container, Node, Index, Replaced);
      end if;
   end Remove;

   procedure Replace
             (  Item     : Item_Ptr;
                Pointer  : Byte_Index;
                Replaced : out Byte_Index
             )  is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, Null_Replace);
      else
         declare
            Container : B_Tree'Class renames Item.Tree.all;
            Pool      : Persistent_Pool'Class renames
                        Container.Pool.all;
            Lock      : Holder (Container.Pool);
            Block     : Block_Type renames
                        Update (Pool.File, Item.Node).all;
         begin
            if Item.Index > Get_Length (Block) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  Replace_Error
               );
            end if;
            Replaced := Get_Pointer (Block, Item.Index);
            Set_Pointer (Block, Item.Index, Pointer);
         end;
      end if;
   end Replace;

   procedure Replace (Item : Item_Ptr; Pointer : Byte_Index) is
      Replaced : Byte_Index;
   begin
      Replace (Item, Pointer, Replaced);
   end Replace;

   procedure Replace
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Pointer   : Byte_Index;
                Replaced  : out Byte_Index
             )  is
      Pool : Persistent_Pool'Class renames Container.Pool.all;
      Lock : Holder (Container.Pool);
   begin
      if Container.Root_Bucket = 0 then
         New_Root (Container, Key, Pointer, 0, 0);
         Replaced := 0;
      else
         declare
            Node  : Byte_Index;
            Index : Integer;
         begin
            Search (Container, Container.Root_Bucket, Key, Node, Index);
            if Index > 0 then
               declare
                  Block : Block_Type renames
                          Update (Pool.File, Node).all;
               begin
                  Replaced := Get_Pointer (Block, Index);
                  Set_Pointer (Block, Index, Pointer);
               end;
            else
               declare
                  Key_Index : Byte_Index := 0;
               begin
                  Key_Index := New_Key (Container.Pool, Key);
                  Replaced := 0;
                  Insert
                  (  Key       => Key,
                     Key_Index => Key_Index,
                     Pointer   => Pointer,
                     Child     => 0,
                     Parent    => (  Container'Unchecked_Access,
                                     Node,
                                    -Index
                  )               );
               exception
                  when others =>
                     if Key_Index /= 0 then
                        Unchecked_Deallocate (Pool, Key_Index);
                     end if;
                     raise;
               end;
            end if;
         end;
      end if;
   end Replace;

   procedure Replace
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Pointer   : Byte_Index
             )  is
      Old : Byte_Index;
   begin
      Replace (Container, Key, Pointer, Old);
   end Replace;

   procedure Restore
             (  Container : in out B_Tree;
                Reference : Byte_Index
             )  is
   begin
      Set_Root_Address (Container, Reference);
   end Restore;

   procedure Search
             (  Container : B_Tree;
                Root      : Byte_Index;
                Key       : Key_Type;
                Node      : out Byte_Index;
                Index     : out Integer
             )  is
   begin
      Node := Root;
      if Node = 0 then
         Index := 0;
      else
         loop
            Index :=
               Find
               (  Container,
                  Load (Container.Pool.File, Node).all,
                  Key
               );
            exit when Index > 0;
            declare
               Next : constant Byte_Index :=
                      Get_Child
                      (  Load (Container.Pool.File, Node).all,
                        -Index
                      );
            begin
               exit when Next = 0;
               Node := Next;
            end;
         end loop;
      end if;
   end Search;

   procedure Set_Root_Address
             (  Container : in out B_Tree;
                Root      : Byte_Index
             )  is
      Lock : Holder (Container.Pool);
   begin
      Container.Root_Bucket := Root;
   end Set_Root_Address;

   procedure Set_Tag (Item : Item_Ptr; Tag : Byte_Index) is
   begin
      if Item.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, No_Item_Error);
      end if;
      declare
         Pool : Persistent_Pool'Class renames Item.Tree.Pool.all;
         Lock : Holder (Item.Tree.Pool);
      begin
         Set_Node_Tag (Update (Pool.File, Item.Node).all, Tag);
      end;
   end Set_Tag;

   procedure Store
             (  Container : in out B_Tree;
                Reference : out Byte_Index
             )  is
      Lock : Holder (Container.Pool);
   begin
      Reference := Container.Root_Bucket;
      Container.Root_Bucket := 0;
   end Store;

   function Sup (Container : B_Tree; Key : Key_Type) return Item_Ptr is
      Lock  : Holder (Container.Pool);
      Node  : Byte_Index;
      Index : Integer;
   begin
      Search (Container, Container.Root_Bucket, Key, Node, Index);
      if Index = 0 then
         return No_Item;
      elsif Index > 0 then
         return (Container.Self, Node, Index);
      else
         declare
            Block : constant Block_Type_Ref :=
                    Load (Container.Pool.File, Node);
         begin
            if -Index > Get_Length (Block.all) then
               if Index < -1 then
                  return Get_Next (Container, Node, -Index - 1);
               else
                  return No_Item;
               end if;
            else
               return (Container.Self, Node, -Index);
            end if;
         end;
      end if;
   end Sup;

   procedure Traverse
             (  Container : B_Tree;
                Iterator  : in out Abstract_Visitor'Class;
                From      : Item_Ptr;
                To        : Key_Type
             )  is
      function Visit_Item
               (  Container : B_Tree;
                  Key       : Key_Type;
                  Item      : Item_Ptr
               )  return Boolean is
      begin
         return Visit_Item
                (  Iterator'Unchecked_Access,
                   Container,
                   Key,
                   Item
                );
      end Visit_Item;

      function Visit_Range
               (  Container : B_Tree;
                  Item      : Item_Ptr
               )  return Bucket_Traversal is
      begin
         return Visit_Range
                (  Iterator'Unchecked_Access,
                   Container,
                   Item
                );
      end Visit_Range;

      procedure Walker is new Generic_Traverse;
   begin
      Walker (Container, From, To);
   end Traverse;

   procedure Update
             (  Container : in out B_Tree;
                Node      : Byte_Index;
                Index     : Positive;
                Key       : Byte_Index;
                Pointer   : Byte_Index;
                Child     : Byte_Index;
                Enlarge   : Boolean := False
             )  is
      Block : Block_Type renames Update (Container.Pool.File, Node).all;
   begin
      Set_Key   (Block, Index, Key);
      Set_Pointer (Block, Index, Pointer);
      Set_Child (Block, Index, Child);
      if Enlarge then
         Set_Length (Block, Get_Length (Block) + 1);
      end if;
      Set_Parent (Container.Pool.File, Child, Node, Index);
   end Update;

   procedure Update
             (  Container : in out B_Tree;
                Node      : Byte_Index;
                Index     : Positive;
                Key       : Key_Type;
                Pointer   : Byte_Index;
                Child     : Byte_Index;
                Enlarge   : Boolean := False
             )  is
      Key_Index : constant Byte_Index :=
                  New_Key (Container.Pool, Key);
   begin
      Update
      (  Container,
         Node,
         Index,
         Key_Index,
         Pointer,
         Child,
         Enlarge
      );
   exception
      when others =>
         if Key_Index /= 0 then
            Unchecked_Deallocate (Container.Pool.all, Key_Index);
         end if;
   end Update;

end Persistent.Memory_Pools.Streams.Generic_External_Ptr_B_Tree;
