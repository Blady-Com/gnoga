--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Directed_Graph                      Luebeck            --
--  Implementation                                 Winter, 2009       --
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

with Ada.Numerics;  use Ada.Numerics;

with Ada.Unchecked_Conversion;

package body Generic_Directed_Graph is
   use Node_Arrays;
   use Node_Address_Sets;
   use Node_Sets;

   Offset : Storage_Offset := -1;   -- Offset to the object's dope
   Blocks : Address := Null_Address;
--
-- Deref -- Node to its address conversion
--
   function Deref (Item : Node) return Address;

   type Nodes_Set (Size : Natural) is limited record
      Last : Natural;
      List : Nodes_Array (1..Size);
   end record;
   type Nodes_Set_Ptr is access Nodes_Set;
   for Nodes_Set_Ptr'Storage_Pool use Pool;
   procedure Free is
     new Ada.Unchecked_Deallocation (Nodes_Set, Nodes_Set_Ptr);
--
-- Add -- Node into a set
--
   procedure Add
             (  Set          : in out Address;
                Item         : Node;
                Minimal_Size : Positive
             );
   procedure Add
             (  Set          : in out Nodes_Set_Ptr;
                Item         : Node;
                Minimal_Size : Positive
             );
   pragma Inline (Add);
--
-- Find -- Binary search for a node
--
--    Vector - The nodes array
--    Size   - Of the array
--    Item   - To search for
--
-- Returns :
--
--    Node postion if found, negated would be position if not found
--
   function Find
            (  Vector : Nodes_Array;
               Size   : Positive;
               Item   : Node
            )  return Integer;
--
-- Find -- Location
--
   function Find (Set : Nodes_Set_Ptr; Item : Node) return Natural;
--
-- Is_In -- Membership test
--
   function Is_In (Set : Nodes_Set; Item : Node) return Boolean;
   function Is_In (Set : Nodes_Set_Ptr; Item : Node) return Boolean;
   pragma Inline (Is_In);
--
-- Remove -- Node from a set
--
   procedure Remove (Set : Nodes_Set_Ptr; Item : Node);
   procedure Remove (Set : Address; Item : Node);
   pragma Inline (Remove);

   type Node_Header is limited record
      Parents  : Address;
      Children : Address;
   end record;
   type Node_Header_Ptr is access all Node_Header;

   Header_Size : constant Storage_Offset :=
                    Node_Header'Max_Size_In_Storage_Elements;

   function Header is
      new Ada.Unchecked_Conversion (Address, Node_Header_Ptr);

   function Nodes is
      new Ada.Unchecked_Conversion (Address, Nodes_Set_Ptr);

   procedure Add
             (  Set          : in out Nodes_Set_Ptr;
                Item         : Node;
                Minimal_Size : Positive
             )  is
   begin
      if Set = null then
         Set := new Nodes_Set (Minimal_Size);
         Set.Last := 1;
         Set.List (1) := Item;
      elsif Set.Last = 0 then
         Set.Last := 1;
         Set.List (1) := Item;
      else
         declare
            Location : Integer := Find (Set.List, Set.Last, Item);
         begin
            if Location < 0 then
               Location := -Location;
               if Set.Size = Set.Last then
                  declare
                     Ptr : constant Nodes_Set_Ptr :=
                              new Nodes_Set
                                  (  Set.Size
                                  +  Natural'Max
                                     (  Minimal_Size,
                                        (  (  Set.Size
                                           *  (100 + Increment)
                                           )
                                        /  100
                                  )  )  );
                  begin
                     Ptr.List (1..Location - 1) :=
                        Set.List (1..Location - 1);
                     Ptr.List (Location + 1..Set.Size + 1) :=
                        Set.List (Location..Set.Size);
                     Free (Set);
                     Set := Ptr;
                  end;
               else
                  Set.List (Location + 1..Set.Last + 1) :=
                     Set.List (Location..Set.Last);
               end if;
               Set.List (Location) := Item;
               Set.Last := Set.Last + 1;
            elsif Set.List (Location) /= Item then
               raise Argument_Error;
            end if;
         end;
      end if;
   end Add;

   procedure Add
             (  Set          : in out Address;
                Item         : Node;
                Minimal_Size : Positive
             )  is
      This : Nodes_Set_Ptr := Nodes (Set);
   begin
      Add (This, Item, Minimal_Size);
      Set := This.all'Address;
   end Add;

   procedure Allocate
             (  Pool            : in out Node_Storage_Pool;
                Storage_Address : out Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             )  is
      Header_Alignment : constant Storage_Count :=
         Storage_Count'Max (Node_Header'Alignment, Alignment);
      Header_Offset    : constant Storage_Offset :=
         Header_Size + (-Header_Size) mod Header_Alignment;
   begin
      Allocate
      (  Pool.Host.all,
         Storage_Address,
         Size + Header_Offset,
         Header_Alignment
      );
      declare
         This : Node_Header renames Header (Storage_Address).all;
      begin
         This.Parents  := Null_Address;
         This.Children := Null_Address;
         if Offset < 0 then
            --
            -- The  offset  to  the  object  address  according  to  the
            -- attribute X'Address is unknown. For this reason the block
            -- allocated is added to the list  of  allocated  blocks  to
            -- determine the offset later.
            --
            if Blocks = Null_Address then
               This.Parents  := Storage_Address;
               This.Children := Storage_Address;
               Blocks        := Storage_Address;
            else
               declare
                  Head : Node_Header renames Header (Blocks).all;
                  Tail : Node_Header renames Header (Head.Parents).all;
               begin
                  This.Parents  := Head.Parents;
                  This.Children := Blocks;
                  Tail.Children := Storage_Address;
                  Head.Parents  := Storage_Address;
               end;
            end if;
         end if;
      end;
      Storage_Address := Storage_Address + Header_Offset;
   end Allocate;

   procedure Connect
             (  Parent  : Node;
                Child   : Node;
                Acyclic : Boolean := True
             )  is
   begin
      if (  Parent = null
         or else
            Child = null
         or else
            (  Acyclic
            and then
               (  Child = Parent
               or else
                  Is_Ancestor (Parent => Child, Child => Parent)
         )  )  )
      then
         raise Constraint_Error;
      else
         Add
         (  Header (Deref (Parent)).Children,
            Child,
            Minimal_Children_Size
         );
         begin
            Add
            (  Header (Deref (Child)).Parents,
               Parent,
               Minimal_Parents_Size
            );
         exception
            when others =>
               Remove (Header (Deref (Parent)).Children, Child);
               raise;
         end;
      end if;
   end Connect;

   procedure Deallocate
             (  Pool            : in out Node_Storage_Pool;
                Storage_Address : in Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             )  is
      Header_Alignment : constant Storage_Count :=
         Storage_Count'Max (Node_Header'Alignment, Alignment);
      Header_Offset    : constant Storage_Offset :=
         Header_Size + (-Header_Size) mod Header_Alignment;
   begin
      if Offset < 0 then
         --
         -- The node is deallocated before placement of any  other nodes
         -- in any graphs. It is removed  from  the  list  of  allocated
         -- blocks.
         --
         if Blocks = Null_Address then
            raise Program_Error;
         end if;
         declare
            Freed : constant Address := Storage_Address - Header_Offset;
            This  : Node_Header renames Header (Freed).all;
         begin
            if This.Parents = Freed then
               Blocks := Null_Address;
            else
               if Blocks = Freed then
                  Blocks := This.Children;
               end if;
               Header (This.Parents).Children := This.Children;
               Header (This.Children).Parents := This.Parents;
            end if;
         end;
      else
         --
         -- Checking  for  dangling pointers. No deallocated item can be
         -- in any of the lists.
         --
         declare
            Ptr  : Nodes_Set_Ptr;
            This : Node_Header renames
                   Header (Storage_Address - Header_Offset).all;
         begin
            if This.Parents /= Null_Address then
               Ptr := Nodes (This.Parents);
               if Ptr.Last > 0 then
                  raise Program_Error;
               end if;
               Free (Ptr);
            end if;
            if This.Children /= Null_Address then
               Ptr := Nodes (This.Children);
               if Ptr.Last > 0 then
                  raise Program_Error;
               end if;
               Free (Ptr);
            end if;
         end;
      end if;
      Deallocate
      (  Pool.Host.all,
         Storage_Address - Header_Offset,
         Size + Header_Offset,
         Header_Alignment
      );
   end Deallocate;

   procedure Delete
             (  Vertex   : in out Node;
                Subgraph : Subgraph_Type := Any
             )  is
      Visited : Node_Address_Sets.Set;
      Queued  : Unbounded_Array;
      Current : Node     := Vertex;
      Count   : Positive := 1;
   begin
      if Vertex = null then
         return;
      end if;
      loop
         Add (Visited, Current);
         declare
            This : Node_Header renames Header (Deref (Current)).all;
         begin
            if This.Parents /= Null_Address then
               declare
                  Parents : Nodes_Set renames Nodes (This.Parents).all;
                  Parent  : Node;
               begin
                  for Index in 1..Parents.Last loop
                     Parent := Parents.List (Index);
                     if not Is_In (Visited, Parent) then
                        Remove
                        (  Header (Deref (Parent)).Children,
                           Current
                        );
                        if (Subgraph and Ancestor) /= 0 then
                           Put (Queued, Count, Parent);
                           Count := Count + 1;
                        end if;
                     end if;
                  end loop;
                  Parents.Last := 0;
               end;
            end if;
            if This.Children /= Null_Address then
               declare
                  Children : Nodes_Set renames
                             Nodes (This.Children).all;
                  Child    : Node;
               begin
                  for Index in 1..Children.Last loop
                     Child := Children.List (Index);
                     if not Is_In (Visited, Child) then
                        Remove
                        (  Header (Deref (Child)).Parents,
                           Current
                        );
                        if (Subgraph and Descendant) /= 0 then
                           Put (Queued, Count, Child);
                           Count := Count + 1;
                        end if;
                     end if;
                  end loop;
                  Children.Last := 0;
               end;
            end if;
         end;
         if Vertex /= Current or else (Subgraph and Self) /= 0 then
            Free (Current);
         end if;
         exit when Count = 1;
         Count   := Count - 1;
         Current := Get (Queued, Count);
      end loop;
   end Delete;

   function Deref (Item : Node) return Address is
      function Node_To_Address is
         new Ada.Unchecked_Conversion (Node, Address);
   begin
      if Item = null then
         raise Constraint_Error;
      end if;
      if Offset < 0 then
         --
         -- Searching  for the memory block closest to the given address
         -- from the left. The offset between Item and the block address
         -- is the size of the dope plus the size of the header.
         --
         if Blocks = Null_Address then
            raise Program_Error;
         end if;
         Offset := Storage_Offset'Last;
         declare
            Addr    : constant Address := Node_To_Address (Item);
            Current : Address := Blocks;
         begin
            loop
               if Current < Addr then
                  Offset := Storage_Offset'Min (Offset, Addr - Current);
               end if;
               declare
                  This : Node_Header renames Header (Current).all;
               begin
                  Current := This.Children;
                  This.Parents  := Null_Address;
                  This.Children := Null_Address;
               end;
               exit when Current = Blocks;
            end loop;
         end;
         if Offset = Storage_Offset'Last then
            raise Program_Error;
         end if;
      end if;
      return Node_To_Address (Item) - Offset;
   end Deref;

   procedure Disconnect (Parent : Node; Child : Node) is
      This : Node_Header renames Header (Deref (Parent)).all;
      That : Node_Header renames Header (Deref (Child)).all;
   begin
      Remove (This.Children, Child);
      Remove (That.Parents, Parent);
   end Disconnect;

   function Find
            (  Vector : Nodes_Array;
               Size   : Positive;
               Item   : Node
            )  return Integer is
      From : Natural := 0;
      To   : Natural := Size + 1;
      This : Natural;
   begin
      loop
         This := (From + To) / 2;
         if Item = Vector (This) then
            return This;
         elsif Item.all'Address < Vector (This).all'Address then
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

   function Find (Set : Nodes_Set_Ptr; Item : Node) return Natural is
   begin
      if Set /= null and then Set.Last > 0 then
         declare
            Location : constant Integer :=
                       Find (Set.List, Set.Last, Item);
         begin
            if Location > 0 then
               return Location;
            end if;
         end;
      end if;
      return 0;
   end Find;

   function Find_Child (Parent : Node; Child : Node) return Natural is
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if Child = null then
         raise Constraint_Error;
      elsif This.Children = Null_Address then
         return 0;
      else
         return Find (Nodes (This.Children), Child);
      end if;
   end Find_Child;

   function Find_Parent (Parent : Node; Child : Node) return Natural is
      This : Node_Header renames Header (Deref (Child)).all;
   begin
      if Parent = null then
         raise Constraint_Error;
      elsif This.Parents = Null_Address then
         return 0;
      else
         return Find (Nodes (This.Parents), Parent);
      end if;
   end Find_Parent;

   function Get_Child (Parent : Node; Child : Positive) return Node is
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if This.Children = Null_Address then
         raise Constraint_Error;
      else
         declare
            Set : Nodes_Set renames Nodes (This.Children).all;
         begin
            if Child > Set.Last then
               raise Constraint_Error;
            else
               return Set.List (Child);
            end if;
         end;
      end if;
   end Get_Child;

   function Get_Children (Parent : Node) return Nodes_Array is
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if This.Children = Null_Address then
         return (1..0 => null);
      else
         declare
            Children : Nodes_Set renames Nodes (This.Children).all;
         begin
            return Children.List (1..Children.Last);
         end;
      end if;
   end Get_Children;

   function Get_Children (Parent : Node) return Node_Sets.Set is
      This   : Node_Header renames Header (Deref (Parent)).all;
      Result : Node_Sets.Set;
   begin
      if This.Children /= Null_Address then
         declare
            Children : Nodes_Set renames Nodes (This.Children).all;
         begin
            for Index in 1..Children.Last loop
               Add (Result, Children.List (Index));
            end loop;
         end;
      end if;
      return Result;
   end Get_Children;

   function Get_Children_Number (Parent : Node) return Natural is
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if This.Children = Null_Address then
         return 0;
      else
         return Nodes (This.Children).Last;
      end if;
   end Get_Children_Number;

   function Get_Parent (Child : Node; Parent : Positive) return Node is
      This : Node_Header renames Header (Deref (Child)).all;
   begin
      if This.Parents = Null_Address then
         raise Constraint_Error;
      else
         declare
            Set : Nodes_Set renames Nodes (This.Parents).all;
         begin
            if Parent > Set.Last then
               raise Constraint_Error;
            else
               return Set.List (Parent);
            end if;
         end;
      end if;
   end Get_Parent;

   function Get_Parents (Child : Node) return Nodes_Array is
      This : Node_Header renames Header (Deref (Child)).all;
   begin
      if This.Parents = Null_Address then
         return (1..0 => null);
      else
         declare
            Parents : Nodes_Set renames Nodes (This.Parents).all;
         begin
            return Parents.List (1..Parents.Last);
         end;
      end if;
   end Get_Parents;

   function Get_Parents (Child : Node) return Node_Sets.Set is
      This   : Node_Header renames Header (Deref (Child)).all;
      Result : Node_Sets.Set;
   begin
      declare
         Parents : Nodes_Set renames Nodes (This.Parents).all;
      begin
         for Index in 1..Parents.Last loop
            Add (Result, Parents.List (Index));
         end loop;
      end;
      return Result;
   end Get_Parents;

   function Get_Parents_Number (Child : Node) return Natural is
      This : Node_Header renames Header (Deref (Child)).all;
   begin
      if This.Parents = Null_Address then
         return 0;
      else
         return Nodes (This.Parents).Last;
      end if;
   end Get_Parents_Number;

   function Is_Ancestor (Parent : Node; Child : Node) return Boolean is
      Visited : Node_Address_Sets.Set;
      Queued  : Unbounded_Array;
      Current : Node     := Parent;
      Count   : Positive := 1;
   begin
      if Child = null then
         raise Constraint_Error;
      end if;
      loop
         declare
            This : Node_Header renames Header (Deref (Current)).all;
         begin
            Add (Visited, Current);
            if This.Children /= Null_Address then
               declare
                  Successor : Node;
                  Children  : Nodes_Set renames
                              Nodes (This.Children).all;
               begin
                  if Is_In (Children, Child) then
                     return True;
                  end if;
                  for Index in 1..Children.Last loop
                     Successor := Children.List (Index);
                     if not Is_In (Visited, Successor) then
                        Put (Queued, Count, Successor);
                        Count := Count + 1;
                     end if;
                  end loop;
               end;
            end if;
         end;
         exit when Count = 1;
         Count   := Count - 1;
         Current := Get (Queued, Count);
      end loop;
      return False;
   end Is_Ancestor;

   function Is_Connected (Vertex : Node) return Boolean is
      This : Node_Header renames Header (Deref (Vertex)).all;
   begin
      return
      (  (  This.Parents /= Null_Address
         and then
            Nodes (This.Parents).Last > 0
         )
      or else
         (  This.Children /= Null_Address
         and then
            Nodes (This.Children).Last > 0
      )  );
   end Is_Connected;

   function Is_Descendant (Child : Node; Parent : Node)
      return Boolean is
      Visited : Node_Address_Sets.Set;
      Queued  : Unbounded_Array;
      Current : Node     := Child;
      Count   : Positive := 1;
   begin
      if Parent = null then
         raise Constraint_Error;
      end if;
      loop
         declare
            This : Node_Header renames Header (Deref (Current)).all;
         begin
            Add (Visited, Current);
            if This.Parents /= Null_Address then
               declare
                  Predecessor : Node;
                  Parents     : Nodes_Set renames
                                Nodes (This.Parents).all;
               begin
                  if Is_In (Parents, Parent) then
                     return True;
                  end if;
                  for Index in 1..Parents.Last loop
                     Predecessor := Parents.List (Index);
                     if not Is_In (Visited, Predecessor) then
                        Put (Queued, Count, Predecessor);
                        Count := Count + 1;
                     end if;
                  end loop;
               end;
            end if;
         end;
         exit when Count = 1;
         Count   := Count - 1;
         Current := Get (Queued, Count);
      end loop;
      return False;
   end Is_Descendant;

   function Is_In (Set : Nodes_Set_Ptr; Item : Node) return Boolean is
   begin
      return
      (  Set /= null
      and then
         Set.Last > 0
      and then
         Find (Set.List, Set.Last, Item) > 0
      );
   end Is_In;

   function Is_In (Set : Nodes_Set; Item : Node) return Boolean is
   begin
      return Set.Last > 0 and then Find (Set.List, Set.Last, Item) > 0;
   end Is_In;

   function Is_Sibling (Left, Right : Node) return Boolean is
      This : Node_Header renames Header (Deref (Left)).all;
      That : Node_Header renames Header (Deref (Right)).all;
   begin
      if (  This.Parents = Null_Address
         or else
            That.Parents = Null_Address
         )
      then
         return False;
      end if;
      declare
         This_Parents : Nodes_Set renames Nodes (This.Parents).all;
         That_Parents : Nodes_Set renames Nodes (That.Parents).all;
      begin
         if This_Parents.Last > That_Parents.Last then
            for Index in 1..That_Parents.Last loop
               if Is_In (This_Parents, That_Parents.List (Index)) then
                  return True;
               end if;
            end loop;
         else
            for Index in 1..This_Parents.Last loop
               if Is_In (That_Parents, This_Parents.List (Index)) then
                  return True;
               end if;
            end loop;
         end if;
         return False;
      end;
   end Is_Sibling;

   function Precedes (Left, Right : Node) return Boolean is
   begin
      return
      (  Right /= null
      and then
         (Left = null or else Less (Left, Right))
      );
   end Precedes;

   function Related (Parent : Node; Child : Node) return Boolean is
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if Child = null then
         raise Constraint_Error;
      elsif This.Children = Null_Address then
         return False;
      else
         return Is_In (Nodes (This.Children), Child);
      end if;
   end Related;

   procedure Remove (Set : Nodes_Set_Ptr; Item : Node) is
   begin
      if Set.Last > 0 then
         declare
            Location : constant Integer :=
                       Find (Set.List, Set.Last, Item);
         begin
            if Location > 0 then
               Set.List (Location..Set.Last - 1) :=
                  Set.List (Location + 1..Set.Last);
               Set.Last := Set.Last - 1;
            end if;
         end;
      end if;
   end Remove;

   procedure Remove (Set : Address; Item : Node) is
   begin
      Remove (Nodes (Set), Item);
   end Remove;

   procedure Remove (Vertex : Node) is
      This : Node_Header renames Header (Deref (Vertex)).all;
   begin
      if This.Parents = Null_Address then
         if This.Children = Null_Address then
            return;
         end if;
         declare
            Children : Nodes_Set_Ptr renames Nodes (This.Children);
         begin
            for Child in 1..Children.Last loop
               Remove
               (  Header (Deref (Children.List (Child))).Parents,
                  Vertex
               );
            end loop;
            Children.Last := 0;
         end;
      elsif This.Children = Null_Address then
         declare
            Parents : Nodes_Set_Ptr renames Nodes (This.Parents);
         begin
            for Parent in 1..Parents.Last loop
               Remove
               (  Header (Deref (Parents.List (Parent))).Children,
                  Vertex
               );
            end loop;
            Parents.Last := 0;
         end;
      else
         declare
            Parents  : Nodes_Set_Ptr renames Nodes (This.Parents);
            Children : Nodes_Set_Ptr renames Nodes (This.Children);
         begin
            for Child in 1..Children.Last loop
               Remove
               (  Header (Deref (Children.List (Child))).Parents,
                  Vertex
               );
            end loop;
            for Parent in 1..Parents.Last loop
               Remove
               (  Header (Deref (Parents.List (Parent))).Children,
                  Vertex
               );
               for Child in 1..Children.Last loop
                  Add
                  (  Header (Deref (Parents.List (Parent))).Children,
                     Children.List (Child),
                     Minimal_Children_Size
                  );
                  Add
                  (  Header (Deref (Children.List (Child))).Parents,
                     Parents.List (Parent),
                     Minimal_Parents_Size
                  );
               end loop;
            end loop;
            Parents.Last  := 0;
            Children.Last := 0;
         end;
      end if;
   end Remove;

   function Same (Left, Right : Node) return Boolean is
   begin
      return
      (  Right = Left
      or else
         (  Left /= null
         and then
            Right /= null
         and then
            Equal (Left, Right)
      )  );
   end Same;

   function Storage_Size (Pool : Node_Storage_Pool)
      return Storage_Count is
   begin
      return Storage_Size (Pool.Host.all);
   end Storage_Size;

   function "<" (Left, Right : Node) return Boolean is
   begin
      return
      (  Right /= null
      and then
         (Left = null or else Left.all'Address < Right.all'Address)
      );
   end "<";

end Generic_Directed_Graph;
