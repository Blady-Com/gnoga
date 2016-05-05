--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Directed_Weighted_Graph             Luebeck            --
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

package body Generic_Directed_Weighted_Graph is
   use Node_Address_Sets;
   use Node_Arrays;
   use Node_Sets;
------------------------------------------------------------------------
-- Generic_Nodes_Set -- The package to handle sets of nodes internally
--
   generic
      type Key (<>) is limited private;
      type Node is private;
      type Nodes_Array is array (Positive range <>) of Node;
      with procedure Finalize (Left : in out Node) is <>;
      with function Equal
                    (  Left  : access Key;
                       Right : Node
                    )  return Boolean is <>;
      with function Less
                    (  Left  : access Key;
                       Right : Node
                    )  return Boolean is <>;
   package Generic_Nodes_Set is
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
                   Item         : access Key;
                   Data         : Node;
                   Minimal_Size : Positive
                );
      procedure Add
                (  Set          : in out Nodes_Set_Ptr;
                   Item         : access Key;
                   Data         : Node;
                   Minimal_Size : Positive
                );
      --
      -- Find -- Binary search for a node
      --
      --    Vector - The nodes array
      --    Size   - Of the array
      --    Item   - To search for
      --
      -- Returns :
      --
      --    Node postion if found,  negated  would  be  position  if not
      --    found
      --
      function Find
               (  Vector : Nodes_Array;
                  Size   : Positive;
                  Item   : access Key
               )  return Integer;
      --
      -- Find -- Location
      --
      function Find
               (  Set  : Nodes_Set_Ptr;
                  Item : access Key
               )  return Natural;
      --
      -- Is_In -- Membership test
      --
      function Is_In
               (  Set  : Nodes_Set;
                  Item : access Key
               )  return Boolean;
      function Is_In
               (  Set  : Nodes_Set_Ptr;
                  Item : access Key
               )  return Boolean;
      --
      -- Nodes -- Conversion of address to pointer
      --
      function Nodes is
         new Ada.Unchecked_Conversion (Address, Nodes_Set_Ptr);
      --
      -- Remove -- Node from a set
      --
      procedure Remove (Set : Nodes_Set_Ptr; Item : access Key);
      procedure Remove (Set : Nodes_Set_Ptr; Item : Positive);
      procedure Remove (Set : Address; Item : access Key);

   private
      pragma Inline (Add);
      pragma Inline (Is_In);
      pragma Inline (Remove);
   end Generic_Nodes_Set;

   type Weight_Ptr is access Weight_Type;
   for Weight_Ptr'Storage_Pool use Pool;

   type Pair is record
      Target : Node;
      Weight : Weight_Ptr;
   end record;
   type Pairs_Array is array (Positive range <>) of Pair;

   function Equal_By_Key
            (  Left  : access Node_Type;
               Right : Node
            )  return Boolean is
      pragma Inline (Equal_By_Key);
   begin
      return Equal (Left, Right);
   end Equal_By_Key;

   function Equal_By_Key
            (  Left  : access Node_Type;
               Right : Pair
            )  return Boolean is
      pragma Inline (Equal_By_Key);
   begin
      return Equal (Left, Right.Target);
   end Equal_By_Key;

   function Equal_By_Key
            (  Left  : access Weight_Type;
               Right : Pair
            )  return Boolean is
      pragma Inline (Equal_By_Key);
   begin
      return Equal (Left, Right.Weight);
   end Equal_By_Key;

   procedure Finalize (Left : in out Node) is
      pragma Inline (Finalize);
   begin
      null;
   end Finalize;

   procedure Finalize (Left : in out Pair) is
      pragma Inline (Finalize);
   begin
      null;
   end Finalize;

   procedure Free is
      new Ada.Unchecked_Deallocation (Weight_Type, Weight_Ptr);

   procedure Free (Left : in out Pair) is
      pragma Inline (Free);
   begin
      Free (Left.Weight);
   end Free;

   function Less_By_Key
            (  Left  : access Node_Type;
               Right : Node
            )  return Boolean is
      pragma Inline (Less_By_Key);
   begin
      return Less (Left, Right);
   end Less_By_Key;

   function Less_By_Key
            (  Left  : access Node_Type;
               Right : Pair
            )  return Boolean is
      pragma Inline (Less_By_Key);
   begin
      return Less (Left, Right.Target);
   end Less_By_Key;

   function Less_By_Key
            (  Left  : access Weight_Type;
               Right : Pair
            )  return Boolean is
      pragma Inline (Less_By_Key);
   begin
      return Less (Left, Right.Weight);
   end Less_By_Key;

   package body Generic_Nodes_Set is
      procedure Add
                (  Set          : in out Nodes_Set_Ptr;
                   Item         : access Key;
                   Data         : Node;
                   Minimal_Size : Positive
                )  is
      begin
         if Set = null then
            Set := new Nodes_Set (Minimal_Size);
            Set.Last := 1;
            Set.List (1) := Data;
         elsif Set.Last = 0 then
            Set.Last := 1;
            Set.List (1) := Data;
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
                  Set.List (Location) := Data;
                  Set.Last := Set.Last + 1;
               elsif Set.List (Location) /= Data then
                  raise Argument_Error;
               else
                  Finalize (Set.List (Location));
                  Set.List (Location) := Data;
               end if;
            end;
         end if;
      end Add;

      procedure Add
                (  Set          : in out Address;
                   Item         : access Key;
                   Data         : Node;
                   Minimal_Size : Positive
                )  is
         This : Nodes_Set_Ptr := Nodes (Set);
      begin
         Add (This, Item, Data, Minimal_Size);
         Set := This.all'Address;
      end Add;

      function Find
               (  Vector : Nodes_Array;
                  Size   : Positive;
                  Item   : access Key
               )  return Integer is
         From : Natural := 0;
         To   : Natural := Size + 1;
         This : Natural;
      begin
         loop
            This := (From + To) / 2;
            if Equal (Item, Vector (This)) then
               return This;
            elsif Less (Item, Vector (This)) then
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

      function Find
               (  Set  : Nodes_Set_Ptr;
                  Item : access Key
               )  return Natural is
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

      function Is_In
               (  Set  : Nodes_Set_Ptr;
                  Item : access Key
               )  return Boolean is
      begin
         return
         (  Set /= null
         and then
            Set.Last > 0
         and then
            Find (Set.List, Set.Last, Item) > 0
         );
      end Is_In;

      function Is_In
               (  Set  : Nodes_Set;
                  Item : access Key
               )  return Boolean is
      begin
         return
         (  Set.Last > 0
         and then
            Find (Set.List, Set.Last, Item) > 0
         );
      end Is_In;

      procedure Remove (Set : Nodes_Set_Ptr; Item : Positive) is
      begin
         Finalize (Set.List (Item));
         Set.List (Item..Set.Last - 1) := Set.List (Item + 1..Set.Last);
         Set.Last := Set.Last - 1;
      end Remove;

      procedure Remove (Set : Nodes_Set_Ptr; Item : access Key) is
      begin
         if Set.Last > 0 then
            declare
               Location : constant Integer :=
                          Find (Set.List, Set.Last, Item);
            begin
               if Location > 0 then
                  Remove (Set, Location);
               end if;
            end;
         end if;
      end Remove;

      procedure Remove (Set : Address; Item : access Key) is
      begin
         Remove (Nodes (Set), Item);
      end Remove;

   end Generic_Nodes_Set;
--
-- Maps_Of_Nodes -- Set of (Node, Weight) ordered by Node
--
   package Maps_Of_Nodes is
      new Generic_Nodes_Set
          (  Node        => Pair,
             Key         => Node_Type,
             Nodes_Array => Pairs_Array,
             Finalize    => Free,
             Equal       => Equal_By_Key,
             Less        => Less_By_Key
          );
--
-- Maps_Of_Weights -- Set of (Node, Weight) ordered by Weight
--
   package Maps_Of_Weights is
      new Generic_Nodes_Set
          (  Node        => Pair,
             Key         => Weight_Type,
             Nodes_Array => Pairs_Array,
             Equal       => Equal_By_Key,
             Less        => Less_By_Key
          );
--
-- Sets_Of_Nodes -- Set of Node
--
   package Sets_Of_Nodes is
      new Generic_Nodes_Set
          (  Node        => Node,
             Key         => Node_Type,
             Nodes_Array => Nodes_Array,
             Equal       => Equal_By_Key,
             Less        => Less_By_Key
          );

   Offset : Storage_Offset := -1;   -- Offset to the object's dope
   Blocks : Address := Null_Address;
--
-- Deref -- Node to its address conversion
--
   function Deref (Item : Node) return Address;

   type Node_Header is limited record
      Parents  : Address;
      Children : Address;
      Weights  : Address;
   end record;
   type Node_Header_Ptr is access all Node_Header;

   Header_Size : constant Storage_Offset :=
                    Node_Header'Max_Size_In_Storage_Elements;

   function Header is
      new Ada.Unchecked_Conversion (Address, Node_Header_Ptr);

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
         This.Weights  := Null_Address;
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

   procedure Classify
             (  Parent : Node;
                Weight : Weight_Type;
                Lower  : out Natural;
                Upper  : out Positive
             )  is
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if This.Weights = Null_Address then
         Lower := 0;
         Upper := 1;
      else
         declare
            use Maps_Of_Weights;
            Key   : aliased Weight_Type := Weight;
            Index : constant Natural :=
                       Find (Nodes (This.Weights), Key'Access);
         begin
            if Index > 0 then
               Lower := Index;
               Upper := Index;
            else
               Lower := -Index;
               Upper := Lower + 1;
            end if;
         end;
      end if;
   end Classify;

   procedure Connect
             (  Parent  : Node;
                Child   : Node;
                Weight  : Weight_Type;
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
         declare
            Data : Pair := (Child, new Weight_Type'(Weight));
         begin
            Maps_Of_Nodes.Add
            (  Header (Deref (Parent)).Children,
               Data.Target,
               Data,
               Minimal_Children_Size
            );
            begin
               Maps_Of_Weights.Add
               (  Header (Deref (Parent)).Weights,
                  Data.Weight,
                  Data,
                  Minimal_Children_Size
               );
               begin
                  Sets_Of_Nodes.Add
                  (  Header (Deref (Child)).Parents,
                     Parent,
                     Parent,
                     Minimal_Parents_Size
                  );
               exception
                  when others =>
                     Maps_Of_Weights.Remove
                     (  Header (Deref (Parent)).Children,
                        Data.Weight
                     );
                     raise;
               end;
            exception
               when others =>
                  Maps_Of_Nodes.Remove
                  (  Header (Deref (Parent)).Children,
                     Child
                  );
                  raise;
            end;
         exception
            when others =>
               Free (Data.Weight);
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
            This : Node_Header renames
                   Header (Storage_Address - Header_Offset).all;
         begin
            if This.Parents /= Null_Address then
               declare
                  use Sets_Of_Nodes;
                  Parents : Nodes_Set_Ptr := Nodes (This.Parents);
               begin
                  if Parents.Last > 0 then
                     raise Program_Error;
                  end if;
                  Free (Parents);
               end;
            end if;
            if This.Children /= Null_Address then
               declare
                  use Maps_Of_Nodes;
                  Children : Nodes_Set_Ptr := Nodes (This.Children);
               begin
                  if Children.Last > 0 then
                     raise Program_Error;
                  end if;
                  Free (Children);
               end;
            end if;
            if This.Weights /= Null_Address then
               declare
                  use Maps_Of_Weights;
                  Weights : Nodes_Set_Ptr := Nodes (This.Weights);
               begin
                  if Weights.Last > 0 then
                     raise Program_Error;
                  end if;
                  Free (Weights);
               end;
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
                  Parent  : Node;
                  Parents : Sets_Of_Nodes.Nodes_Set renames
                            Sets_Of_Nodes.Nodes (This.Parents).all;
               begin
                  for Index in 1..Parents.Last loop
                     Parent := Parents.List (Index);
                     if not Is_In (Visited, Parent) then
                        --
                        -- Delete the node Current  from  the  lists  of
                        -- children and weights of the node parent
                        --
                        declare
                           use Maps_Of_Nodes;
                           That     : Node_Header renames
                                      Header (Deref (Parent)).all;
                           Children : constant Nodes_Set_Ptr :=
                                      Nodes (That.Children);
                           Index    : constant Natural :=
                                      Find (Children, Current);
                        begin
                           if Index > 0 then -- It is in the lists
                              Maps_Of_Weights.Remove
                              (  Maps_Of_Weights.Nodes (That.Weights),
                                 Children.List (Index).Weight
                              );
                              Remove (Children, Index);
                           end if;
                        end;
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
                  Child    : Node;
                  Children : Maps_Of_Nodes.Nodes_Set renames
                             Maps_Of_Nodes.Nodes (This.Children).all;
               begin
                  for Index in 1..Children.Last loop
                     Child := Children.List (Index).Target;
                     Free (Children.List (Index).Weight);
                     if not Is_In (Visited, Child) then
                        --
                        -- Delete  the  node  Current  from  the list of
                        -- parents of the node Child
                        --
                        Sets_Of_Nodes.Remove
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
               Maps_Of_Weights.Nodes (This.Weights).Last := 0;
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
      Maps_Of_Nodes.Remove (This.Children, Child);
      Sets_Of_Nodes.Remove (That.Parents, Parent);
   end Disconnect;

   function Find_Child (Parent : Node; Child : Node) return Natural is
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if Child = null then
         raise Constraint_Error;
      elsif This.Children = Null_Address then
         return 0;
      else
         declare
            use Maps_Of_Nodes;
            Children : constant Nodes_Set_Ptr := Nodes (This.Children);
            Index    : constant Natural := Find (Children, Child);
         begin
            if Index > 0 then
               return
                  Maps_Of_Weights.Find
                  (  Maps_Of_Weights.Nodes (This.Weights),
                     Children.List (Index).Weight
                  );
            else
               return 0;
            end if;
         end;
      end if;
   end Find_Child;

   function Find_Parent (Parent : Node; Child : Node) return Natural is
      use Sets_Of_Nodes;
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
      use Maps_Of_Weights;
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if This.Weights = Null_Address then
         raise Constraint_Error;
      else
         declare
            Weights : Nodes_Set renames Nodes (This.Weights).all;
         begin
            if Child > Weights.Last then
               raise Constraint_Error;
            else
               return Weights.List (Child).Target;
            end if;
         end;
      end if;
   end Get_Child;

   function Get_Children (Parent : Node) return Nodes_Array is
      use Maps_Of_Weights;
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if This.Weights = Null_Address then
         return (1..0 => null);
      else
         declare
            Weights : Nodes_Set renames Nodes (This.Weights).all;
            Result  : Nodes_Array (1..Weights.Last);
         begin
            for Index in Result'Range loop
               Result (Index) := Weights.List (Index).Target;
            end loop;
            return Result;
         end;
      end if;
   end Get_Children;

   function Get_Children (Parent : Node) return Node_Sets.Set is
      use Maps_Of_Nodes;
      This   : Node_Header renames Header (Deref (Parent)).all;
      Result : Node_Sets.Set;
   begin
      if This.Children /= Null_Address then
         declare
            Children : Nodes_Set renames Nodes (This.Children).all;
         begin
            for Index in 1..Children.Last loop
               Add (Result, Children.List (Index).Target);
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
         return Maps_Of_Nodes.Nodes (This.Children).Last;
      end if;
   end Get_Children_Number;

   function Get_Parent (Child : Node; Parent : Positive) return Node is
      use Sets_Of_Nodes;
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
      use Sets_Of_Nodes;
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
      use Sets_Of_Nodes;
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
         return Sets_Of_Nodes.Nodes (This.Parents).Last;
      end if;
   end Get_Parents_Number;

   function Get_Weight (Parent : Node; Child : Positive)
      return Weight_Type is
      use Maps_Of_Weights;
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if This.Weights = Null_Address then
         raise Constraint_Error;
      else
         declare
            Weights : Nodes_Set renames Nodes (This.Weights).all;
         begin
            if Child > Weights.Last then
               raise Constraint_Error;
            else
               return Weights.List (Child).Weight.all;
            end if;
         end;
      end if;
   end Get_Weight;

   function Get_Weight (Parent : Node; Child : Node)
      return Weight_Type is
      use Maps_Of_Nodes;
      This : Node_Header renames Header (Deref (Parent)).all;
   begin
      if This.Children = Null_Address then
         raise Constraint_Error;
      else
         declare
            Children : constant Nodes_Set_Ptr := Nodes (This.Children);
            Index    : constant Natural := Find (Children, Child);
         begin
            if Index > 0 then
               return Children.List (Index).Weight.all;
            else
               raise Constraint_Error;
            end if;
         end;
      end if;
   end Get_Weight;

   function Is_Ancestor (Parent : Node; Child : Node) return Boolean is
      use Maps_Of_Nodes;
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
                     Successor := Children.List (Index).Target;
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
            Sets_Of_Nodes.Nodes (This.Parents).Last > 0
         )
      or else
         (  This.Children /= Null_Address
         and then
            Maps_Of_Nodes.Nodes (This.Children).Last > 0
      )  );
   end Is_Connected;

   function Is_Descendant (Child : Node; Parent : Node)
      return Boolean is
      use Sets_Of_Nodes;
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

   function Is_Sibling (Left, Right : Node) return Boolean is
      use Sets_Of_Nodes;
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

   function Precedes
            (  Left  : Node;
               Right : Node
            )  return Boolean is
   begin
      return
      (  Right /= null
      and then
         (Left = null or else Less (Left, Right))
      );
   end Precedes;

   function Related (Parent : Node; Child : Node) return Boolean is
      use Maps_Of_Nodes;
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

end Generic_Directed_Weighted_Graph;
