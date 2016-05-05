--                                                                    --
--  package                          Copyright (c) Dmitry A. Kazakov  --
--     Generic_Indefinite_B_Tree                   Luebeck            --
--  Implementation                                 Spring, 2014       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Ada.Tags;           use Ada.Tags;

with Ada.Unchecked_Deallocation;
--with Ada.Text_IO, Strings_Edit.Integers, System.Storage_Elements;

package body Generic_Indefinite_B_Tree is

   Left_Half  : constant Positive := (Width + 1) / 2 - 1;
   Right_Half : constant Positive := Width - Left_Half;

--     function Image (Node : Node_Ptr) return String is
--        use System.Storage_Elements;
--     begin
--        if Node = null then
--           return "0";
--        else
--           declare
--              Text : String :=
--                 Integer_Address'Image (To_Integer (Node.all'Address));
--           begin
--              return Text (Text'First + 1..Text'Last);
--           end;
--        end if;
--     end Image;
--
--     procedure Dump (Node : Node_Ptr; Prefix : String := "") is
--        use Ada.Text_IO;
--        use Strings_Edit.Integers;
--        use System.Storage_Elements;
--     begin
--        Put (Prefix);
--        Put ("at ");
--        Put (Image (Node));
--        if Node.Parent.Node /= null then
--           Put (" [");
--           Put (Image (Node.Parent.Node));
--           Put ("|");
--           Put (Image (Node.Parent.Index));
--           Put ("]");
--        end if;
--        Put (":");
--        for Index in 1..Node.Length loop
--           if Node.Children (Index) = null then
--              Put (" .");
--           else
--              Put (' ' & Image (Node.Children (Index)));
--           end if;
--           Put (" /");
--           Put (Ada.Strings.Unbounded.To_String (Image (Node.Pairs (Index).Key.all)));
--           Put ("\");
--        end loop;
--        if Node.Children (Node.Length + 1) = null then
--           Put (" .");
--        else
--           Put (' ' & Image (Node.Children (Node.Length + 1)));
--        end if;
--        New_Line;
--        for Index in 1..Node.Length + 1 loop
--           if Node.Children (Index) /= null then
--              Dump
--              (  Node.Children (Index),
--                 "  " & Prefix & Image (Index) & "."
--              );
--           end if;
--        end loop;
--     end Dump;

   procedure Copy
             (  To_Node    : Node_Ptr;
                To_First   : Positive;
                To_Last    : Natural;
                From_Node  : in out Node_Type;
                From_First : Positive;
                Tail       : Natural := 1
             )  is
      pragma Inline (Copy);
      From_Last : constant Integer := From_First + To_Last - To_First;
   begin
      To_Node.Pairs (To_First..To_Last) :=
         From_Node.Pairs (From_First..From_Last);
      To_Node.Children (To_First..To_Last + Tail) :=
         From_Node.Children (From_First..From_Last + Tail);
      for Index in To_First..To_Last + Tail loop
         if To_Node.Children (Index) /= null then
            To_Node.Children (Index).Parent := (To_Node, Index);
         end if;
      end loop;
   end Copy;

   procedure Move
             (  Node       : Node_Ptr;
                To_First   : Positive;
                To_Last    : Natural;
                From_First : Positive;
                Tail       : Natural := 1
             )  is
      pragma Inline (Move);
      From_Last : constant Integer := From_First + To_Last - To_First;
   begin
      Node.Pairs (To_First..To_Last) :=
         Node.Pairs (From_First..From_Last);
      Node.Children (To_First..To_Last + Tail) :=
         Node.Children (From_First..From_Last + Tail);
      for Index in To_First..To_Last + Tail loop
         if Node.Children (Index) /= null then
            Node.Children (Index).Parent := (Node, Index);
         end if;
      end loop;
   end Move;

   procedure Free is
      new Ada.Unchecked_Deallocation (Key_Type, Key_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (Object_Type, Object_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (Node_Type, Node_Ptr);

   function Is_Empty (Container : B_Tree) return Boolean is
   begin
      return Container.Root = null or else Container.Root.Length = 0;
   end Is_Empty;

   function Is_In (Container : B_Tree; Key : Key_Type)
      return Boolean is
      Node  : Node_Ptr;
      Index : Integer;
   begin
      Search (Container.Root, Key, Node, Index);
      return Node /= null and then Index > 0;
   end Is_In;

   procedure Add
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Value     : Object_Type
             )  is
   begin
      if Container.Root = null then
         Container.Root := new Node_Type;
         declare
            Root : Node_Type renames Container.Root.all;
            Pair : Pair_Type :=
                   (  Key   => new Key_Type'(Key),
                      Value => new Object_Type'(Value)
                   );
         begin
            Root.Length    := 1;
            Root.Pairs (1) := Pair;
         exception
            when others =>
               Free (Pair.Key);
               Free (Pair.Value);
               raise;
         end;
      else
         declare
            Node  : Node_Ptr;
            Index : Integer;
         begin
            Search (Container.Root, Key, Node, Index);
            if Index < 0 then
               declare
                  Pair : Pair_Type :=
                         (  Key   => new Key_Type'(Key),
                            Value => new Object_Type'(Value)
                         );
               begin
                  Insert
                  (  Container => Container,
                     Parent    => (Node, -Index),
                     Child     => null,
                     Pair      => Pair
                  );
               exception
                  when others =>
                     Free (Pair.Key);
                     Free (Pair.Value);
                     raise;
               end;
            else
               Raise_Exception
               (  Constraint_Error'Identity,
                  "The key is already in use"
               );
            end if;
         end;
      end if;
   end Add;

   procedure Erase (Root : in out Node_Ptr) is
   begin
      if Root /= null then
         declare
            Node : Node_Type := Root.all;
         begin
            for Index in 1..Node.Length loop
               Free (Node.Pairs (Index).Key);
               Free (Node.Pairs (Index).Value);
            end loop;
            for Index in 1..Node.Length + 1 loop
               Erase (Node.Children (Index));
            end loop;
         end;
         Free (Root);
      end if;
   end Erase;

   procedure Erase (Container : in out B_Tree) is
   begin
      Erase (Container.Root);
   end Erase;

   procedure Finalize (Container : in out B_Tree) is
   begin
      Erase (Container);
   end Finalize;

   function Find
            (  Pairs : Pair_Array;
               Size  : Natural;
               Key   : Key_Type
            )  return Integer is
      pragma Inline (Find);
      From : Natural := 0;
      To   : Natural := Size + 1;
      This : Natural;
   begin
      if Size = 0 then
         return -1;
      end if;
      loop
         This := (From + To) / 2;
         if Key = Pairs (This).Key.all then
            return This;
         elsif Key < Pairs (This).Key.all then
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
      end loop;
   end Find;

   function Find (Container : B_Tree; Key : Key_Type) return Item_Ptr is
      Node  : Node_Ptr;
      Index : Integer;
   begin
      Search (Container.Root, Key, Node, Index);
      if Index <= 0 then
         return No_Item;
      else
         return (Node, Index);
      end if;
   end Find;

   function Get
            (  Container : B_Tree;
               Key       : Key_Type
            )  return Object_Type is
      Item : constant Item_Ptr := Find (Container, Key);
   begin
      if Item.Node = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No item"
         );
      end if;
      return Item.Node.Pairs (Item.Index).Value.all;
   end Get;

   function Get_Bucket_Address
            (  Item : Item_Ptr
            )  return System.Address is
   begin
      if Item.Node = null then
         return System.Null_Address;
      else
         return Item.Node.all'Address;
      end if;
   end Get_Bucket_Address;

   function Get_Bucket_Size (Item : Item_Ptr) return Natural is
   begin
      if Item.Node = null then
         return 0;
      else
         return Item.Node.Length;
      end if;
   end Get_Bucket_Size;

   function Get_First (Root : Node_Ptr) return Item_Ptr is
      This : Node_Ptr := Root;
      Next : Node_Ptr;
   begin
      if This = null or else This.Length = 0 then
         return No_Item;
      else
         loop
            Next := This.Children (1);
            if Next = null then
               return (This, 1);
            end if;
            This := Next;
         end loop;
      end if;
   end Get_First;

   function Get_First (Container : B_Tree) return Item_Ptr is
   begin
      return Get_First (Container.Root);
   end Get_First;

   function Get_Index (Item : Item_Ptr) return Positive is
   begin
      if Item.Node = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No item"
         );
      end if;
      return Item.Index;
   end Get_Index;

   function Get_Key (Item : Item_Ptr) return Key_Type is
   begin
      if Item.Node = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No item"
         );
      end if;
      declare
         Node : Node_Type renames Item.Node.all;
      begin
         if Item.Index > Node.Length then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Key with illegal item index"
            );
         end if;
         return Node.Pairs (Item.Index).Key.all;
      end;
   end Get_Key;

   function Get_Last (Root : Node_Ptr) return Item_Ptr is
      This : Node_Ptr := Root;
      Next : Node_Ptr;
   begin
      if This = null or else This.Length < 0 then
         return No_Item;
      else
         loop
            Next := This.Children (This.Length + 1);
            if Next = null then
               if This.Length = 0 then
                  return No_Item;
               else
                  return (This, This.Length);
               end if;
            end if;
            This := Next;
         end loop;
      end if;
   end Get_Last;

   function Get_Last (Container : B_Tree) return Item_Ptr is
   begin
      return Get_Last (Container.Root);
   end Get_Last;

   function Get_Next (Item : Item_Ptr) return Item_Ptr is
   begin
      if Item.Node = null then
         return No_Item;
      end if;
      declare
         Node : Node_Type renames Item.Node.all;
      begin
         if Item.Index > Node.Length then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Next with illegal item index"
            );
         end if;
         declare
            First : Item_Ptr :=
                    Get_First (Node.Children (Item.Index + 1));
         begin
            if First.Node = null or else First.Node = Item.Node then
               if Item.Index = Node.Length then
                  First := Node.Parent;
                  while First.Node /= null loop
                     if First.Index <= First.Node.Length then
                        return First;
                     end if;
                     First := First.Node.Parent;
                  end loop;
                  return No_Item;
               else
                  return (Item.Node, Item.Index + 1);
               end if;
            else
               return First;
            end if;
         end;
      end;
   end Get_Next;

   function Get_Previous (Item : Item_Ptr) return Item_Ptr is
   begin
      if Item.Node = null then
         return No_Item;
      end if;
      declare
         Node : Node_Type renames Item.Node.all;
      begin
         if Item.Index > Node.Length then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Previous to illegal item index"
            );
         end if;
         declare
            Last : Item_Ptr := Get_Last (Node.Children (Item.Index));
         begin
            if Last.Node = null or else Last.Node = Item.Node then
               if Item.Index = 1 then
                  Last := Node.Parent;
                  while Last.Node /= null loop
                     if Last.Index > 1 then
                        return (Last.Node, Last.Index - 1);
                     end if;
                     Last := Last.Node.Parent;
                   end loop;
                   return No_Item;
               else
                  return (Item.Node, Item.Index - 1);
               end if;
            else
               return Last;
            end if;
         end;
      end;
   end Get_Previous;

   function Get_Root (Item : Item_Ptr) return Item_Ptr is
   begin
      if Item.Node = null then
         return No_Item;
      else
         declare
            Node : Node_Ptr := Item.Node;
         begin
            while Node.Parent.Node /= null loop
               Node := Node.Parent.Node;
            end loop;
            return (Node, 1);
         end;
      end if;
   end Get_Root;

   function Get_Value (Item : Item_Ptr) return Object_Type is
   begin
      if Item.Node = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "No item"
         );
      end if;
      declare
         Node : Node_Type renames Item.Node.all;
      begin
         if Item.Index > Node.Length then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Value with illegal item index"
            );
         end if;
         return Node.Pairs (Item.Index).Value.all;
      end;
   end Get_Value;

   function Inf (Container : B_Tree; Key : Key_Type) return Item_Ptr is
      Node  : Node_Ptr;
      Index : Integer;
   begin
      Search (Container.Root, Key, Node, Index);
      if Index = 0 then
         return No_Item;
      else
         return (Node, abs Index);
      end if;
   end Inf;

   procedure Insert
             (  Container : in out B_Tree;
                Parent    : Node_Ptr;
                Pair      : Pair_Type;
                Child     : Node_Ptr
             )  is
   begin
      if Parent = null then
         Insert (Container, No_Item, Pair, Child);
      else
         declare
            Index : constant Integer :=
                    Find (Parent.Pairs, Parent.Length, Pair.Key.all);
         begin
            if Index > 0 then
               Raise_Exception
               (  Data_Error'Identity,
                  "Re-inserting a key"
               );
            end if;
            Insert (Container, (Parent, -Index), Pair, Child);
         end;
      end if;
   end Insert;

   procedure Insert
             (  Container : in out B_Tree;
                Parent    : Item_Ptr;
                Pair      : Pair_Type;
                Child     : Node_Ptr
             )  is
      procedure New_Root
                (  Pair  : Pair_Type;
                   Left  : Node_Ptr;
                   Right : Node_Ptr
                )  is
         New_Node : constant Node_Ptr := new Node_Type;
         Root     : Node_Type renames New_Node.all;
      begin
         Root.Length       := 1;
         Root.Pairs (1)    := Pair;
         Root.Children (1) := Left;
         Root.Children (2) := Right;
         Container.Root    := New_Node;
      end New_Root;
   begin
      if Parent.Node = null then -- Creating new root node
         Raise_Exception
         (  Data_Error'Identity,
            "Inserting at null node"
         );
      end if;
      declare
         Right : Node_Type renames Parent.Node.all;
         Index : Integer := Parent.Index;
      begin
         if Right.Length < Width then -- Have place in the bucket
            Move (Parent.Node, Index + 1, Right.Length + 1, Index);
            Right.Pairs (Index) := Pair;
            Right.Children (Index) := Child;
            Right.Length := Right.Length + 1;
            if Child /= null then
               Child.Parent := (Parent.Node, Index);
            end if;
         elsif Index = Width + 1 and then Underfilled_Left (Right) then
            declare
               Split : constant Integer := Right.Parent.Index - 1;
               This  : Node_Type renames Right.Parent.Node.all;
               Left  : Node_Type renames This.Children (Split).all;
            begin
               Left.Pairs (Left.Length + 1) := This.Pairs (Split);
               Left.Length := Left.Length + 1;
               Copy
               (  This.Children (Split),
                  Left.Length + 1,
                  Left.Length,
                  Right,
                  1
               );
               This.Pairs (Split) := Right.Pairs (1);
               Move (Parent.Node, 1, Width - 1, 2, 0);
               Right.Pairs (Width) := Pair;
               Right.Children (Width) := Child;
               if Child /= null then
                  Child.Parent := (Parent.Node, Width);
               end if;
            end;
         elsif Index = 1 and then Underfilled_Right (Right) then
            declare
               Split : constant Integer := Right.Parent.Index;
               Other : constant Node_Ptr :=
                       Right.Parent.Node.Children (Split + 1);
            begin
               Move (Other, 2, Other.Length + 1, 1);
               Other.Length := Other.Length + 1;
               Copy (Other, 1, 0, Right, Width + 1);
               Other.Pairs (1) := Right.Parent.Node.Pairs (Split);
               Right.Parent.Node.Pairs (Split) :=
                  Parent.Node.Pairs (Width);
               Move (Parent.Node, 2, Width, 1);
               Right.Pairs (1) := Pair;
               Right.Children (1) := Child;
               if Child /= null then
                  Child.Parent := (Parent.Node, 1);
               end if;
            end;
         else
            declare
               New_Node : constant Node_Ptr := new Node_Type;
               Left     : Node_Type renames New_Node.all;
            begin
               Left.Length  := Left_Half;
               Right.Length := Right_Half;
               if Index = Left_Half + 1 then
                  if Right.Parent.Node = null then
                     New_Root (Pair, New_Node, Parent.Node);
                     Left.Parent  := (Container.Root, 1);
                     Right.Parent := (Container.Root, 2);
                  else
                     Insert
                     (  Container,
                        Right.Parent.Node,
                        Pair,
                        New_Node
                     );
                  end if;
                  Copy (New_Node, 1, Left_Half, Right, 1);
                  Left.Children (Left_Half + 1) := Child;
                  Move (Parent.Node, 1, Right_Half, Left_Half + 1);

                  if Child /= null then
                     Child.Parent := (New_Node, Left_Half + 1);
                  end if;
               elsif Index <= Left_Half then
                  if Right.Parent.Node = null then
                     New_Root
                     (  Right.Pairs (Left_Half),
                        New_Node,
                        Parent.Node
                     );
                     Left.Parent  := (Container.Root, 1);
                     Right.Parent := (Container.Root, 2);
                  else
                     Insert
                     (  Container,
                        Right.Parent.Node,
                        Right.Pairs (Left_Half),
                        New_Node
                     );
                  end if;
                  Copy (New_Node, 1, Index - 1, Right, 1, 0);
                  Copy
                  (  New_Node,
                     Index + 1,
                     Left_Half + 1,
                     Right,
                     Index
                  );
                  Left.Pairs (Index) := Pair;
                  Left.Children (Index) := Child;

                  Move (Parent.Node, 1, Right_Half, Left_Half + 1);

                  if Child /= null then
                     Child.Parent := (New_Node, Index);
                  end if;
               else
                  if Right.Parent.Node = null then
                     New_Root
                     (  Right.Pairs (Left_Half + 1),
                        New_Node,
                        Parent.Node
                     );
                     Left.Parent  := (Container.Root, 1);
                     Right.Parent := (Container.Root, 2);
                  else
                     Insert
                     (  Container,
                        Right.Parent.Node,
                        Right.Pairs (Left_Half + 1),
                        New_Node
                     );
                  end if;
                  Copy (New_Node, 1, Left_Half, Right, 1);

                  Index := Index - Left_Half - 1;
                  Move
                  (  Parent.Node,
                     1,
                     Index - 1,
                     Left_Half + 2,
                     0
                  );
                  Move
                  (  Parent.Node,
                     Index + 1,
                     Right_Half,
                     Index + Left_Half + 1
                  );

                  Right.Pairs (Index) := Pair;
                  Right.Children (Index) := Child;

                  if Child /= null then
                     Child.Parent := (Parent.Node, Index);
                  end if;
               end if;
            end;
         end if;
      end;
   end Insert;

   procedure Remove (Item : in out Item_Ptr) is
   begin
      if Item.Node = null then
         return;
      elsif Item.Index > Item.Node.Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Removing an item with illegal index"
         );
      end if;
      declare
         Node  : Node_Type renames Item.Node.all;
         Index : constant Integer := Item.Index;
      begin
         if Node.Children (Index) = null then
            if Node.Children (Index + 1) = null then
               if Node.Length > 1 then
                  Move (Item.Node, Index, Node.Length - 1, Index + 1);
                  Node.Length := Node.Length - 1;
               elsif Node.Parent.Node = null then -- No parent
                  Node.Length := 0;
               else -- Remove at the parent
                  Node.Parent.Node.Children (Node.Parent.Index) := null;
                  Free (Item.Node);
               end if;
            else
               if Node.Length > 1 then
                  Move (Item.Node, Index, Node.Length - 1, Index + 1);
                  Node.Length := Node.Length - 1;
               else
                  declare
                     Right : Node_Ptr := Node.Children (2);
                  begin
                     if Node.Parent.Node = null then -- No parent
                        Node.Length := Right.Length;
                        Copy (Item.Node, 1, Node.Length, Right.all, 1);
                        Free (Right);
                     else
                        Right.Parent := Node.Parent;
                        Node.Parent.Node.Children (Node.Parent.Index) :=
                           Right;
                        Free (Item.Node);
                     end if;
                  end;
               end if;
            end if;
         elsif Node.Children (Index + 1) = null then
            if Node.Length > 1 then
               declare
                  Left : constant Node_Ptr := Node.Children (Index);
               begin
                  Move (Item.Node, Index, Node.Length - 1, Index + 1);
                  Node.Children (Index) := Left;
                  Node.Length := Node.Length - 1;
               end;
            else
               declare
                  Left : Node_Ptr := Node.Children (1);
               begin
                  if Node.Parent.Node = null then -- No parent
                     Node.Length := Left.Length;
                     Copy (Item.Node, 1, Node.Length, Left.all, 1);
                     Free (Left);
                  else
                     Left.Parent := Node.Parent;
                     Node.Parent.Node.Children (Node.Parent.Index) :=
                        Left;
                     Free (Item.Node);
                  end if;
               end;
            end if;
         elsif (  Node.Children (Index).Length
               >  Node.Children (Index + 1).Length
               )  then
            declare
               Left : Node_Ptr := Node.Children (Index);
            begin
               while Left.Children (Left.Length + 1) /= null loop
                  Left := Left.Children (Left.Length + 1);
               end loop;
               Node.Pairs (Index) := Left.Pairs (Left.Length);
               if Left.Length = 1 then
                  Left.Parent.Node.Children (Left.Parent.Index) := null;
                  Free (Left);
               else
                  Left.Length := Left.Length - 1;
               end if;
            end;
         else
            declare
               Right : Node_Ptr := Node.Children (Index + 1);
            begin
               while Right.Children (1) /= null loop
                  Right := Right.Children (1);
               end loop;
               Node.Pairs (Index) := Right.Pairs (1);
               if Right.Length = 1 then
                  Right.Parent.Node.Children (Right.Parent.Index) :=
                     null;
                  Free (Right);
               else
                  Move (Right, 1, Right.Length - 1, 2);
                  Right.Length := Right.Length - 1;
               end if;
            end;
         end if;
      end;
      Item.Node  := null;
      Item.Index := 1;
   end Remove;

   procedure Remove (Container : in out B_Tree; Key : Key_Type) is
      Item : Item_Ptr := Find (Container, Key);
   begin
      Remove (Item);
   end Remove;

   procedure Replace (Item : Item_Ptr; Value : Object_Type) is
   begin
      if Item.Node = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Replacing an non-item"
         );
      else
         declare
            Node : Node_Type renames Item.Node.all;
         begin
            if Item.Index <= Node.Length then
               declare
                  Ptr : Object_Ptr renames
                           Node.Pairs (Item.Index).Value;
                  Old : Object_Ptr := Ptr;
               begin
                  Ptr := new Object_Type'(Value);
                  Free (Old);
               end;
            else
               Raise_Exception
               (  Constraint_Error'Identity,
                  "Replacing an item with wrong index"
               );
            end if;
         end;
      end if;
   end Replace;

   procedure Replace
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Value     : Object_Type
             )  is
   begin
      if Container.Root = null then
         declare
            Pair : Pair_Type :=
                   (  Key   => new Key_Type'(Key),
                      Value => new Object_Type'(Value)
                   );
         begin
            Insert (Container, No_Item, Pair, null);
         exception
            when others =>
               Free (Pair.Key);
               Free (Pair.Value);
               raise;
         end;
      else
         declare
            Node  : Node_Ptr;
            Index : Integer;
         begin
            Search (Container.Root, Key, Node, Index);
            if Index > 0 then
               declare
                  Ptr : Object_Ptr renames Node.Pairs (Index).Value;
                  Old : Object_Ptr := Ptr;
               begin
                  Ptr := new Object_Type'(Value);
                  Free (Old);
               end;
            else
               declare
                  Pair : Pair_Type :=
                         (  Key   => new Key_Type'(Key),
                            Value => new Object_Type'(Value)
                         );
               begin
                  Insert (Container, (Node, -Index), Pair, null);
               exception
                  when others =>
                     Free (Pair.Key);
                     Free (Pair.Value);
                     raise;
               end;
            end if;
         end;
      end if;
   end Replace;

   procedure Search
             (  Root  : Node_Ptr;
                Key   : Key_Type;
                Node  : out Node_Ptr;
                Index : out Integer
             )  is
   begin
      Node := Root;
      if Node = null then
         Index := 0;
      else
         loop
            Index := Find (Node.Pairs, Node.Length, Key);
            exit when Index > 0;
            declare
               Next : constant Node_Ptr := Node.Children (-Index);
            begin
               exit when Next = null;
               Node := Next;
            end;
         end loop;
      end if;
   end Search;

   function Sup (Container : B_Tree; Key : Key_Type) return Item_Ptr is
      Node  : Node_Ptr;
      Index : Integer;
   begin
      Search (Container.Root, Key, Node, Index);
      if Index = 0 then
         return No_Item;
      elsif Index > 0 then
         return (Node, Index);
      else
         return Get_Next ((Node, -Index));
      end if;
   end Sup;

   function Underfilled_Left (Right : Node_Type) return Boolean is
   begin
      if Right.Parent.Node = null then
         return False;
      else
         declare
            Index : Integer := Right.Parent.Index;
         begin
            if Index < 2 then
               return False;
            else
               Index := Index - 1;
               declare
                  Parent : Node_Type renames Right.Parent.Node.all;
               begin
                  if Parent.Children (Index) = null then
                     return False;
                  else
                     return Parent.Children (Index).Length < Width;
                  end if;
               end;
            end if;
         end;
      end if;
   end Underfilled_Left;

   function Underfilled_Right (Left : Node_Type) return Boolean is
   begin
      if Left.Parent.Node = null then
         return False;
      else
         declare
            Parent : Node_Type renames Left.Parent.Node.all;
            Index  : Integer := Left.Parent.Index;
         begin
            if Index > Parent.Length then
               return False;
            else
               Index := Index + 1;
               if Parent.Children (Index) = null then
                  return False;
               else
                  return Parent.Children (Index).Length < Width;
               end if;
            end if;
         end;
      end if;
   end Underfilled_Right;

   function "=" (Left, Right : Pair_Type) return Boolean is
   begin
      return
      (  Left.Key.all = Right.Key.all
      and then
         Left.Value.all = Right.Value.all
      );
   end "=";

   function "=" (Left, Right : B_Tree) return Boolean is
      Item_1 : Item_Ptr := Get_First (Left);
      Item_2 : Item_Ptr := Get_First (Right);
   begin
      while Item_1.Node /= null loop
         if (  Item_2.Node = null
            or else
               (  Item_1.Node.Pairs (Item_1.Index)
               /= Item_2.Node.Pairs (Item_2.Index)
            )  )
         then
            return False;
         end if;
         Item_1 := Get_Next (Item_1);
         Item_2 := Get_Next (Item_2);
      end loop;
      return Item_2.Node = null;
   end "=";

end Generic_Indefinite_B_Tree;
