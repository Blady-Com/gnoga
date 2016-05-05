--                                                                    --
--  package                          Copyright (c) Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Streams.            Luebeck            --
--        External_B_Tree.Generic_Table            Autumn, 2014       --
--  Implementation                                                    --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

with Ada.Unchecked_Deallocation;

package body Persistent.Memory_Pools.Streams.External_B_Tree.
             Generic_Table is

   Values_Offs : constant Block_Offset :=
                          (Key_Index'Pos (Key_Index'Last) + 1) * 8;

   procedure Add
             (  Container : in out Table;
                Keys      : Keys_Tuple;
                Values    : Values_Tuple
             )  is
      Lock : Holder (Container.Pool);
   begin
      Unchecked_Add (Container, Keys, Values);
   end Add;

   function Compare
            (  Container : Table;
               Index     : Key_Index;
               Left      : Byte_Index;
               Right     : Byte_Index
            )  return Outcome is
   begin
      if Left = Right then
         return Same;
      elsif Left < Right then
         return Before;
      else
         return After;
      end if;
   end Compare;

   procedure Erase (Container : in out Table) is
      Lock : Holder (Container.Pool);
   begin
      for Index in Key_Index'Range loop
         Unchecked_Erase (Container.Roots (Index).all);
      end loop;
      if Container.Address /= 0 then
         Unchecked_Deallocate (Container.Pool.all, Container.Address);
         Container.Address := 0;
      end if;
   end Erase;

   procedure Finalize (Container : in out Table) is
      use Ada.Finalization;
      procedure Free is
         new Ada.Unchecked_Deallocation (B_Tree'Class, B_Tree_Ptr);
   begin
      for Index in Key_Index'Range loop
         Free (Container.Roots (Index));
      end loop;
      Finalize (Limited_Controlled (Container));
   end Finalize;

   function Find
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Row_Ptr is
      Item : constant Item_Ptr :=
             Find (Container.Roots (Index).all, Key);
   begin
      if Item = No_Item then
         return No_Row;
      else
         return (Container.Self, Item.Node, Item.Index);
      end if;
   end Find;

   function Get
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Values_Tuple is
      Lock   : Holder (Container.Pool);
      Item   : Byte_Index;
      Values : Values_Tuple;
   begin
      Item := Unchecked_Get (Container.Roots (Index).all, Key);
      declare
         Block  : Block_Type renames
                  Load (Container.Pool.File, Item).all;
         Offset : Block_Offset := Get_Offset (Item) + Values_Offs;
      begin
         for Index in Values'Range loop
            Values (Index) := Get (Block, Offset);
            Offset := Offset + 8;
         end loop;
      end;
      return Values;
   end Get;

   function Get
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index;
               Column    : Value_Index
            )  return Byte_Index is
      Lock : Holder (Container.Pool);
      Item : Byte_Index;
   begin
      Item := Unchecked_Get (Container.Roots (Index).all, Key);
      return Get
             (  Load (Container.Pool.File, Item).all,
                (  Get_Offset (Item)
                +  Values_Offs
                +  Value_Index'Pos (Column) * 8
             )  );
   end Get;

   function Get_Bucket_Address (Row : Row_Ptr) return Byte_Index is
   begin
      return Row.Node;
   end Get_Bucket_Address;

   function Get_Bucket_Size (Row : Row_Ptr) return Natural is
   begin
      if Row.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, "No row");
      else
         return Get_Bucket_Size
                (  Item_Ptr'
                   (  Tree  => Row.Table.Roots (Key_Index'First),
                      Node  => Row.Node,
                      Index => Row.Index
                )  );
      end if;
   end Get_Bucket_Size;

   function Get_First
            (  Container : Table;
               Index     : Key_Index
            )  return Row_Ptr is
      Item : constant Item_Ptr :=
             Get_First (Container.Roots (Index).all);
   begin
      if Item = No_Item then
         return No_Row;
      else
         return (Container.Self, Item.Node, Item.Index);
      end if;
   end Get_First;

   function Get_Index (Row : Row_Ptr) return Positive is
   begin
      if Row.Node = 0 then
         Raise_Exception (Constraint_Error'Identity, "No item");
      else
         return Row.Index;
      end if;
   end Get_Index;

   function Get_Key
            (  Row   : Row_Ptr;
               Index : Key_Index
            )  return Byte_Index is
   begin
      return Get_Key
             (  Item_Ptr'
                (  Tree  => Row.Table.Roots (Index),
                   Node  => Row.Node,
                   Index => Row.Index
             )  );
   end Get_Key;

   procedure Get_Keys
             (  Block  : Block_Type;
                Offset : in out Block_Offset;
                Keys   : out Keys_Tuple
             )  is
   begin
      for Index in Keys'Range loop
         Keys (Index) := Get (Block, Offset);
         Offset := Offset + 8;
      end loop;
   end Get_Keys;

   function Get_Keys (Row : Row_Ptr) return Keys_Tuple is
      Lock   : Holder (Row.Table.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get_Value
               (  Item_Ptr'
                  (  Tree  => Row.Table.Roots (Key_Index'First),
                     Node  => Row.Node,
                     Index => Row.Index
               )  );
      Block  : Block_Type renames Load (Row.Table.Pool.File, This).all;
      Offset : Block_Offset := Get_Offset (This);
      Keys   : Keys_Tuple;
   begin
      Get_Keys (Block, Offset, Keys);
      return Keys;
   end Get_Keys;

   function Get_Last
            (  Container : Table;
               Index     : Key_Index
            )  return Row_Ptr is
      Item : constant Item_Ptr :=
             Get_Last (Container.Roots (Index).all);
   begin
      if Item = No_Item then
         return No_Row;
      else
         return (Container.Self, Item.Node, Item.Index);
      end if;
   end Get_Last;

   function Get_Length (Block : Block_Type) return Natural is
      pragma Inline (Get_Length);
   begin
      return Natural (Unsigned_16'(Get (Block, Length_Offset)));
   end Get_Length;

   function Get_Next
            (  Row   : Row_Ptr;
               Index : Key_Index
            )  return Row_Ptr is
      Item : constant Item_Ptr :=
             Get_Next
             (  Item_Ptr'
                (  Tree  => Row.Table.Roots (Index),
                   Node  => Row.Node,
                   Index => Row.Index
             )  );
   begin
      if Item = No_Item then
         return No_Row;
      else
         return (Row.Table, Item.Node, Item.Index);
      end if;
   end Get_Next;

   function Get_Previous
            (  Row   : Row_Ptr;
               Index : Key_Index
            )  return Row_Ptr is
      Item : constant Item_Ptr :=
             Get_Previous
             (  Item_Ptr'
                (  Tree  => Row.Table.Roots (Index),
                   Node  => Row.Node,
                   Index => Row.Index
             )  );
   begin
      if Item = No_Item then
         return No_Row;
      else
         return (Row.Table, Item.Node, Item.Index);
      end if;
   end Get_Previous;

   function Get_Root
            (  Row   : Row_Ptr;
               Index : Key_Index
            )  return Row_Ptr is
      Item : constant Item_Ptr :=
             Get_Root
             (  Item_Ptr'
                (  Tree  => Row.Table.Roots (Index),
                   Node  => Row.Node,
                   Index => Row.Index
             )  );
   begin
      if Item = No_Item then
         return No_Row;
      else
         return (Row.Table, Item.Node, Item.Index);
      end if;
   end Get_Root;

   function Get_Root_Address (Container : Table) return Byte_Index is
      Lock : Holder (Container.Pool);
   begin
      if Container.Roots (Key_Index'First).Root_Bucket /= 0 then
         if Container.Address = 0 then
            Container.Self.Address :=
               Unchecked_Allocate
               (  Container.Pool.all,
                  Container.Roots'Length * 8
               );
         end if;
         declare
            Block  : Block_Type renames
                     Update
                     (  Container.Pool.File,
                        Container.Address
                     ) .all;
            Offset : Block_Offset := Get_Offset (Container.Address);
         begin
            for Index in Container.Roots'Range loop
               Put (Block, Offset, Container.Roots (Index).Root_Bucket);
               Offset := Offset + 8;
            end loop;
         end;
      end if;
      return Container.Address;
   end Get_Root_Address;

   function Get_Value
            (  Row    : Row_Ptr;
               Column : Value_Index
            )  return Byte_Index is
      Lock : Holder (Row.Table.Pool);
      This : constant Byte_Index :=
             Unchecked_Get_Value
             (  Item_Ptr'
                (  Tree  => Row.Table.Roots (Key_Index'First),
                   Node  => Row.Node,
                   Index => Row.Index
             )  );
   begin
      return Get
             (  Load (Row.Table.Pool.File, This).all,
                (  Get_Offset (This)
                +  Values_Offs
                +  Value_Index'Pos (Column) * 8
             )  );
   end Get_Value;

   procedure Get_Values
             (  Block  : Block_Type;
                Offset : in out Block_Offset;
                Values : out Values_Tuple
             )  is
   begin
      for Index in Values'Range loop
         Values (Index) := Get (Block, Offset);
         Offset := Offset + 8;
      end loop;
   end Get_Values;

   function Get_Values (Row : Row_Ptr) return Values_Tuple is
      Lock   : Holder (Row.Table.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get_Value
               (  Item_Ptr'
                  (  Tree  => Row.Table.Roots (Key_Index'First),
                     Node  => Row.Node,
                     Index => Row.Index
               )  );
      Block  : Block_Type renames Load (Row.Table.Pool.File, This).all;
      Offset : Block_Offset := Get_Offset (This) + Values_Offs;
      Values : Values_Tuple;
   begin
      Get_Values (Block, Offset, Values);
      return Values;
   end Get_Values;

   function Inf
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Row_Ptr is
      Item : constant Item_Ptr :=
             Inf (Container.Roots (Index).all, Key);
   begin
      if Item = No_Item then
         return No_Row;
      else
         return (Container.Self, Item.Node, Item.Index);
      end if;
   end Inf;

   procedure Initialize (Container : in out Table) is
      use Ada.Finalization;
   begin
      Initialize (Limited_Controlled (Container));
      for Index in Container.Roots'Range loop
         Container.Roots (Index) :=
            new B_Tree (Container.Pool.all'Unchecked_Access);
      end loop;
   end Initialize;

   function Is_Empty (Container : Table) return Boolean is
   begin
      return Is_Empty (Container.Roots (Key_Index'First).all);
   end Is_Empty;

   function Is_In
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Boolean is
   begin
      return Is_In (Container.Roots (Index).all, Key);
   end Is_In;

   procedure Remove (Row : in out Row_Ptr) is
      Lock   : Holder (Row.Table.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get_Value
               (  Item_Ptr'
                  (  Tree  => Row.Table.Roots (Key_Index'First),
                     Node  => Row.Node,
                     Index => Row.Index
               )  );
      Block  : Block_Type renames Load (Row.Table.Pool.File, This).all;
      Offset : Block_Offset := Get_Offset (This) + Values_Offs;
      Keys   : Keys_Tuple;
   begin
      Get_Keys (Block, Offset, Keys);
      for Index in Keys'Range loop
         Unchecked_Remove (Row.Table.Roots (Index).all, Keys (Index));
      end loop;
      Unchecked_Deallocate (Row.Table.Pool.all, This);
      Row := No_Row;
   end Remove;

   procedure Remove
             (  Row    : in out Row_Ptr;
                Values : out Values_Tuple
             )  is
      Lock   : Holder (Row.Table.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get_Value
               (  Item_Ptr'
                  (  Tree  => Row.Table.Roots (Key_Index'First),
                     Node  => Row.Node,
                     Index => Row.Index
               )  );
      Block  : Block_Type renames Load (Row.Table.Pool.File, This).all;
      Offset : Block_Offset := Get_Offset (This) + Values_Offs;
      Keys   : Keys_Tuple;
   begin
      Get_Keys   (Block, Offset, Keys);
      Get_Values (Block, Offset, Values);
      for Index in Keys'Range loop
         Unchecked_Remove (Row.Table.Roots (Index).all, Keys (Index));
      end loop;
      Unchecked_Deallocate (Row.Table.Pool.all, This);
      Row := No_Row;
   end Remove;

   procedure Remove
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index
             )  is
      Lock : Holder (Container.Pool);
      Item : constant Item_Ptr :=
             Unchecked_Find (Container.Roots (Index).all, Key);
   begin
      if Item = External_B_Tree.No_Item then
         return;
      end if;
      declare
         This   : constant Byte_Index := Unchecked_Get_Value (Item);
         Block  : Block_Type renames
                  Load (Container.Pool.File, This).all;
         Offset : Block_Offset := Get_Offset (This) + Values_Offs;
         Keys   : Keys_Tuple;
      begin
         Get_Keys (Block, Offset, Keys);
         for Index in Keys'Range loop
            Unchecked_Remove
            (  Container.Roots (Index).all,
               Keys (Index)
            );
         end loop;
         Unchecked_Deallocate (Container.Pool.all, This);
      end;
   end Remove;

   procedure Remove
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Values    : out Values_Tuple
             )  is
      Lock : Holder (Container.Pool);
      Item : constant Item_Ptr :=
             Unchecked_Find (Container.Roots (Index).all, Key);
   begin
      if Item = External_B_Tree.No_Item then
         Values := (others => 0);
         return;
      end if;
      declare
         This   : constant Byte_Index := Unchecked_Get_Value (Item);
         Block  : Block_Type renames
                  Load (Container.Pool.File, This).all;
         Offset : Block_Offset := Get_Offset (This) + Values_Offs;
         Keys   : Keys_Tuple;
      begin
         Get_Keys (Block, Offset, Keys);
         Get_Values (Block, Offset, Values);
         for Index in Keys'Range loop
            Unchecked_Remove
            (  Container.Roots (Index).all,
               Keys (Index)
            );
         end loop;
         Unchecked_Deallocate (Container.Pool.all, This);
      end;
   end Remove;

   procedure Remove
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Keys      : out Keys_Tuple;
                Values    : out Values_Tuple
             )  is
      Lock : Holder (Container.Pool);
      Item : constant Item_Ptr :=
             Unchecked_Find (Container.Roots (Index).all, Key);
   begin
      if Item = External_B_Tree.No_Item then
         Keys   := (others => 0);
         Values := (others => 0);
         return;
      end if;
      declare
         This   : constant Byte_Index := Unchecked_Get_Value (Item);
         Block  : Block_Type renames
                  Load (Container.Pool.File, This).all;
         Offset : Block_Offset := Get_Offset (This) + Values_Offs;
      begin
         Get_Keys   (Block, Offset, Keys);
         Get_Values (Block, Offset, Values);
         for Index in Keys'Range loop
            Unchecked_Remove
            (  Container.Roots (Index).all,
               Keys (Index)
            );
         end loop;
         Unchecked_Deallocate (Container.Pool.all, This);
      end;
   end Remove;

   procedure Replace
             (  Row    : Row_Ptr;
                Column : Value_Index;
                Value  : Byte_Index
             )  is
      Lock   : Holder (Row.Table.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get_Value
               (  Item_Ptr'
                  (  Tree  => Row.Table.Roots (Key_Index'First),
                     Node  => Row.Node,
                     Index => Row.Index
               )  );
      Block  : Block_Type renames
               Update (Row.Table.Pool.File, This).all;
      Offset : constant Block_Offset :=
                        (  Get_Offset (This)
                        +  Values_Offs
                        +  Value_Index'Pos (Column) * 8
                        );
   begin
      Put (Block, Offset, Value);
   end Replace;

   procedure Replace
             (  Row      : Row_Ptr;
                Column   : Value_Index;
                Value    : Byte_Index;
                Replaced : out Byte_Index
             )  is
      Lock   : Holder (Row.Table.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get_Value
               (  Item_Ptr'
                  (  Tree  => Row.Table.Roots (Key_Index'First),
                     Node  => Row.Node,
                     Index => Row.Index
               )  );
      Block  : Block_Type renames
               Update (Row.Table.Pool.File, This).all;
      Offset : constant Block_Offset :=
                        (  Get_Offset (This)
                        +  Values_Offs
                        +  Value_Index'Pos (Column) * 8
                        );
   begin
      Replaced := Get (Block, Offset);
      Put (Block, Offset, Value);
   end Replace;

   procedure Replace (Row : Row_Ptr; Values : Values_Tuple) is
      Lock   : Holder (Row.Table.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get_Value
               (  Item_Ptr'
                  (  Tree  => Row.Table.Roots (Key_Index'First),
                     Node  => Row.Node,
                     Index => Row.Index
               )  );
      Block  : Block_Type renames
               Update (Row.Table.Pool.File, This).all;
      Offset : Block_Offset := Get_Offset (This) + Values_Offs;
   begin
      Set_Values (Block, Offset, Values);
   end Replace;

   procedure Replace
             (  Row      : Row_Ptr;
                Values   : Values_Tuple;
                Replaced : out Values_Tuple
             )  is
      Lock   : Holder (Row.Table.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get_Value
               (  Item_Ptr'
                  (  Tree  => Row.Table.Roots (Key_Index'First),
                     Node  => Row.Node,
                     Index => Row.Index
               )  );
      Block  : Block_Type renames
               Update (Row.Table.Pool.File, This).all;
      Offset : Block_Offset := Get_Offset (This) + Values_Offs;
   begin
      Get_Values (Block, Offset, Replaced);
      Offset := Get_Offset (This) + Values_Offs;
      Set_Values (Block, Offset, Values);
   end Replace;

   procedure Replace
             (  Container : in out Table;
                Keys      : Keys_Tuple;
                Values    : Values_Tuple
             )  is
      Lock  : Holder (Container.Pool);
      Item  : constant Item_Ptr :=
              Unchecked_Find
              (  Container.Roots (Key_Index'First).all,
                 Keys (Key_Index'First)
              );
      Index : Byte_Index;
   begin
      if Item = External_B_Tree.No_Item then -- Insert
         Unchecked_Add (Container, Keys, Values);
      else
         Index := Unchecked_Get_Value (Item);
         declare
            Block  : Block_Type renames
                     Update (Container.Pool.File, Index).all;
            Offset : Block_Offset := Get_Offset (Index) + Values_Offs;
         begin
            Set_Values (Block, Offset, Values);
         end;
      end if;
   end Replace;

   procedure Replace
             (  Container : in out Table;
                Keys      : Keys_Tuple;
                Values    : Values_Tuple;
                Replaced  : out Values_Tuple
             )  is
      Lock  : Holder (Container.Pool);
      Item  : constant Item_Ptr :=
              Unchecked_Find
              (  Container.Roots (Key_Index'First).all,
                 Keys (Key_Index'First)
              );
      Index : Byte_Index;
   begin
      if Item = External_B_Tree.No_Item then -- Insert
         Unchecked_Add (Container, Keys, Values);
         Replaced := (others => 0);
      else
         Index := Unchecked_Get_Value (Item);
         declare
            Block  : Block_Type renames
                     Update (Container.Pool.File, Index).all;
            Offset : Block_Offset := Get_Offset (Index) + Values_Offs;
         begin
            Get_Values (Block, Offset, Replaced);
            Offset := Get_Offset (Index) + Values_Offs;
            Set_Values (Block, Offset, Values);
         end;
      end if;
   end Replace;

   procedure Replace
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Column    : Value_Index;
                Value     : Byte_Index
             )  is
      Lock   : Holder (Container.Pool);
      Item   : constant Byte_Index :=
               Unchecked_Get
               (  Container.Roots (Index).all,
                  Key
               );
      Block  : Block_Type renames
               Update (Container.Pool.File, Item).all;
      Offset : constant Block_Offset :=
                        (  Get_Offset (Item)
                        +  Values_Offs
                        +  Value_Index'Pos (Column) * 8
                        );
   begin
      Put (Block, Offset, Value);
   end Replace;

   procedure Replace
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Column    : Value_Index;
                Value     : Byte_Index;
                Replaced  : out Byte_Index
             )  is
      Lock   : Holder (Container.Pool);
      Item   : constant Byte_Index :=
               Unchecked_Get
               (  Container.Roots (Index).all,
                  Key
               );
      Block  : Block_Type renames
               Update (Container.Pool.File, Item).all;
      Offset : constant Block_Offset :=
                        (  Get_Offset (Item)
                        +  Values_Offs
                        +  Value_Index'Pos (Column) * 8
                        );
   begin
      Replaced := Get (Block, Offset);
      Put (Block, Offset, Value);
   end Replace;

   procedure Set_Root_Address
             (  Container : in out Table;
                Root      : Byte_Index
             )  is
      Lock   : Holder (Container.Pool);
      Block  : Block_Type renames
               Load (Container.Pool.File, Root).all;
      Offset : Block_Offset := Get_Offset (Root);
   begin
      for Index in Container.Roots'Range loop
         Container.Roots (Index).Root_Bucket := Get (Block, Offset);
         Offset := Offset + 8;
      end loop;
      Container.Address := Root;
   end Set_Root_Address;

   procedure Set_Keys
             (  Block  : in out Block_Type;
                Offset : in out Block_Offset;
                Keys   : Keys_Tuple
             )  is
   begin
      for Index in Keys'Range loop
         Put (Block, Offset, Keys (Index));
         Offset := Offset + 8;
      end loop;
   end Set_Keys;

   procedure Set_Values
             (  Block  : in out Block_Type;
                Offset : in out Block_Offset;
                Values : Values_Tuple
             )  is
   begin
      for Index in Values'Range loop
         Put (Block, Offset, Values (Index));
         Offset := Offset + 8;
      end loop;
   end Set_Values;

   function Sup
            (  Container : Table;
               Index     : Key_Index;
               Key       : Byte_Index
            )  return Row_Ptr is
      Item : constant Item_Ptr :=
             Sup (Container.Roots (Index).all, Key);
   begin
      if Item = No_Item then
         return No_Row;
      else
         return (Container.Self, Item.Node, Item.Index);
      end if;
   end Sup;

   procedure Unchecked_Add
             (  Container : in out Table;
                Keys      : Keys_Tuple;
                Values    : Values_Tuple
             )  is
      Pool : Persistent_Pool'Class renames Container.Pool.all;
      Item : Byte_Index;
   begin
      Item :=
         Unchecked_Allocate (Pool, (Values'Length + Keys'Length) * 8);
      for Index in Keys'Range loop
         begin
            Unchecked_Add
            (  Container.Roots (Index).all,
               Keys (Index),
               Item
            );
         exception
            when Constraint_Error => -- Roll back added keys
               if Key_Index'Pos (Index) > 0 then
                  for Added in Keys'First..Key_Index'Pred (Index) loop
                     Unchecked_Remove
                     (  Container.Roots (Added).all,
                        Keys (Added)
                     );
                  end loop;
               end if;
               Unchecked_Deallocate (Pool, Item);
               raise;
         end;
      end loop;
      declare
         Block  : Block_Type renames Update (Pool.File, Item).all;
         Offset : Block_Offset := Get_Offset (Item);
      begin
         Set_Keys   (Block, Offset, Keys);
         Set_Values (Block, Offset, Values);
      end;
   end Unchecked_Add;

   procedure Update
             (  Row     : Row_Ptr;
                Handler : in out Update_Handler'Class
             )  is
      Lock   : Holder (Row.Table.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get_Value
               (  Item_Ptr'
                  (  Tree  => Row.Table.Roots (Key_Index'First),
                     Node  => Row.Node,
                     Index => Row.Index
               )  );
      Block  : Block_Type renames
               Update (Row.Table.Pool.File, This).all;
      Offset : Block_Offset := Get_Offset (This);
      Keys   : Keys_Tuple;
      Values : Values_Tuple;
   begin
      Get_Keys   (Block, Offset, Keys);
      Get_Values (Block, Offset, Values);
      Update (Handler, Keys, Values);
      Offset := Get_Offset (This) + Values_Offs;
      Set_Values (Block, Offset, Values);
   end Update;

   procedure Update
             (  Container : in out Table;
                Index     : Key_Index;
                Key       : Byte_Index;
                Handler   : in out Update_Handler'Class
             )  is
      Lock   : Holder (Container.Pool);
      This   : constant Byte_Index :=
               Unchecked_Get
               (  Container.Roots (Index).all,
                  Key
               );
      Block  : Block_Type renames
               Update (Container.Pool.File, This).all;
      Offset : Block_Offset := Get_Offset (This);
      Keys   : Keys_Tuple;
      Values : Values_Tuple;
   begin
      Get_Keys   (Block, Offset, Keys);
      Get_Values (Block, Offset, Values);
      Update (Handler, Keys, Values);
      Offset := Get_Offset (This) + Values_Offs;
      Set_Values (Block, Offset, Values);
   end Update;

end Persistent.Memory_Pools.Streams.External_B_Tree.Generic_Table;
