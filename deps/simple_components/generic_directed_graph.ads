--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Directed_Graph                      Luebeck            --
--  Interface                                      Winter, 2009       --
--                                                                    --
--                                Last revision :  10:10 27 Dec 2009  --
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
--
--  This generic package provides directed graphs of nodes. Nodes can be
--  of any type. The type of the nodes is the package's formal parameter
--  Node_Type.
--
--  The nodes of a graph are never copied when inserted or removed  from
--  the  graph.  All  operations  are  referential.  A node can have any
--  number of children and parent nodes. The graph can be constrained to
--  be acyclic, in which case adding a child checks this constraint.
--
--     Node_Type             - The node type
--     Pool                  - The storage pool to use for the nodes
--     Minimal_Parents_Size  - Minimal additionally allocated size
--     Minimal_Children_Size - Minimal additionally allocated size
--     Increment             - By which the map is enlarged if necessary
--     Equal                 - Equivalence of the nodes in a set
--     Less                  - Order of the nodes in a set
--
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with System.Storage_Pools;     use System.Storage_Pools;

with Generic_Set;
with Generic_Unbounded_Array;
with Ada.Unchecked_Deallocation;

generic
   type Node_Type (<>) is limited private;
   Pool                  : in out Root_Storage_Pool'Class;
   Minimal_Parents_Size  : Positive := 16;
   Minimal_Children_Size : Positive := 16;
   Increment             : Natural  := 50;
   with function Equal (Left, Right : access Node_Type)
           return Boolean is <>;
   with function Less (Left, Right : access Node_Type)
           return Boolean is <>;
package Generic_Directed_Graph is
--
-- Node_Storage_Pool -- The type of a proxy pool that keeps the nodes.
--
--    Host - The pool to take the memory from
--
   type Node_Storage_Pool (Host : access Root_Storage_Pool'Class) is
      new Root_Storage_Pool with null record;
--
-- Allocate -- Overrides System.Storage_Pools...
--
   procedure Allocate
             (  Pool            : in out Node_Storage_Pool;
                Storage_Address : out Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             );
--
-- Deallocate -- Overrides System.Storage_Pools...
--
   procedure Deallocate
             (  Pool            : in out Node_Storage_Pool;
                Storage_Address : in Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             );
--
-- Storage_Size -- Overrides System.Storage_Pools...
--
   function Storage_Size (Pool : Node_Storage_Pool)
      return Storage_Count;
--
-- Node_Pool -- The pool of the graph nodes
--
   Node_Pool : Node_Storage_Pool (Pool'Access);
------------------------------------------------------------------------
-- Node -- A  reference  to  a  node  of  a graph. The node points to an
--         instance of the type node type. Nodes are  allocated  in  the
-- pool Node_Pool. When a node is deallocated it  is  checked  that  the
-- node is not in any graph, otherwise Program_Error is propagated.
--
   type Node is access Node_Type;
   for Node'Storage_Pool use Node_Pool;
   for Node'Size use Integer_Address'Size;
--
-- Subgraph_Type -- The type of a subgraph
--
--    Ancestor   - An ancestor node
--    Descendant - A descendant node
--    Self       - The node itself
--
   type Subgraph_Type is mod 2**3;
   Self       : constant Subgraph_Type := 2**0;
   Ancestor   : constant Subgraph_Type := 2**1;
   Descendant : constant Subgraph_Type := 2**2;
   Any        : constant Subgraph_Type := Subgraph_Type'Last;
--
-- Nodes_Array -- List of nodes
--
   type Nodes_Array is array (Positive range <>) of Node;
--
-- Connect -- Add a new arc in the graph
--
--    Parent  - The parent node
--    Child   - The child node
--    Acyclic - The constraint
--
-- This  procedure creates a directed arc from Parent to Child. When the
-- arc already exists, this operation is void. Additionally when Acyclic
-- is true, it is checked that the arc does not create a  cycle  in  the
-- graph, that is when Child would be an ancestor of Parent. When Parent
-- has a child equivalent to Child according to the provided  comparison
-- operation or else Child  has  a  parent  equivalent  to  Parent  then
-- Argument_Error is propagated.
--
-- Exceptions :
--
--    Argument_Error   - There is an equivalent edge in the graph
--    Constraint_Error - Parent  or  child  is  null or else Acyclic and
--                       Parent is descendant of Child
--
   procedure Connect
             (  Parent  : Node;
                Child   : Node;
                Acyclic : Boolean := True
             );
--
-- Delete -- A subgraph rooted in a node
--
--    Vertex   - The root node
--    Subgraph - Indicates the graph to remove
--
-- This procedure removes a subgraph rooted  in  Vertex.  The  parameter
-- Subgraph specifies which parts of the graph to be removed and freed.
--
   procedure Delete
             (  Vertex   : in out Node;
                Subgraph : Subgraph_Type := Any
             );
--
-- Disconnect -- Remove arc from the graph
--
--    Parent - A node
--    Child  - A node
--
-- The arc from Parent to Child if any is removed.
--
-- Exceptions :
--
--    Constraint_Error - Parent or Child is null
--
   procedure Disconnect (Parent : Node; Child : Node);
--
-- Find_Child -- Get the position of an immediate descendant
--
--    Parent - The node
--    Child  - The node
--
-- Returns :
--
--    Child's number or else 0 if it is unrelated to Parent
--
-- Exceptions :
--
--    Constraint_Error - Parent or Child is null
--
   function Find_Child (Parent : Node; Child : Node) return Natural;
--
-- Find_Parent -- Get the position of an immediate ancestor
--
--    Parent - The node
--    Child  - The node
--
-- Returns :
--
--    Parent's number or else 0 if it is unrelated to Child
--
-- Exceptions :
--
--    Constraint_Error - Parent or Child is null
--
   function Find_Parent (Parent : Node; Child : Node) return Natural;
--
-- Free -- A node
--
-- Exceptions :
--
--    Program_Error - Node  is  in  a graph (including the case when the
--                    node is a parent or a child of itself
--
   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node);
--
-- Get_Children -- Get immediate descendants of a node
--
--    Parent - The node
--
-- Returns :
--
--    The array of children
--
-- Exceptions :
--
--    Constraint_Error - Parent is null
--
   function Get_Children (Parent : Node) return Nodes_Array;
--
-- Get_Children_Number -- Get the number of immediate descendants
--
--    Parent - The node
--
-- Returns :
--
--    The number of children
--
-- Exceptions :
--
--    Constraint_Error - Parent is null
--
   function Get_Children_Number (Parent : Node) return Natural;
--
-- Get_Child -- Get an immediate descendant by its number
--
--    Parent - The node
--    Child  - The position of the child 1..Get_Children_Number
--
-- Returns :
--
--    The child node
--
-- Exceptions :
--
--    Constraint_Error - No such child or Parent is null
--
   function Get_Child (Parent : Node; Child : Positive) return Node;
--
-- Get_Parent -- Get an immediate ancestor by its number
--
--    Child  - The node
--    Parent - The position of the child 1..Get_Parents_Number
--
-- Returns :
--
--    The parent node
--
-- Exceptions :
--
--    Constraint_Error - No such parent or child is null
--
   function Get_Parent (Child : Node; Parent : Positive) return Node;
--
-- Get_Parents -- Get immediate ancestors of a node
--
--    Child - The node
--
-- Returns :
--
--    The array of parents
--
-- Exceptions :
--
--    Constraint_Error - Child is null
--
   function Get_Parents (Child : Node) return Nodes_Array;
--
-- Get_Parents_Number -- Get the number of immediate ancestors
--
--    Child - The node
--
-- Returns :
--
--    The number of parents
--
-- Exceptions :
--
--    Constraint_Error - Child is null
--
   function Get_Parents_Number (Child : Node) return Natural;
--
-- Is_Ancestor -- Check for a path in the graph
--
--    Parent - A node
--    Child  - A node
--
-- Returns :
--
--    True if there is a path from Parent to Child
--
-- Exceptions :
--
--    Constraint_Error - Parent or Child is null
--
   function Is_Ancestor (Parent : Node; Child : Node) return Boolean;
--
-- Is_Descendant -- Check for a path in the graph
--
--    Child  - A node
--    Parent - A node
--
-- Returns :
--
--    True if there is a path from Parent to Child
--
-- Exceptions :
--
--    Constraint_Error - Parent or Child is null
--
   function Is_Descendant (Child : Node; Parent : Node) return Boolean;
--
-- Is_Connected -- Check for a path in the graph
--
--    Vertex - A node
--
-- Returns :
--
--    True if there edges connecting the node Vertex
--
-- Exceptions :
--
--    Constraint_Error - Vertex is null
--
   function Is_Connected (Vertex : Node) return Boolean;
--
-- Is_Sibling -- Check if two nodes have a common parent
--
--    Left, Right - Nodes
--
-- Returns :
--
--    True if Left and Right have a common parent
--
-- Exceptions :
--
--    Constraint_Error - Left or Right is null
--
   function Is_Sibling (Left, Right : Node) return Boolean;
--
-- Precedes -- Node objects order
--
--    Left, Right - Nodes to compare
--
-- Returns :
--
--    True if Left precedes Right
--
   function Precedes (Left, Right : Node) return Boolean;
--
-- Related -- Graph relation
--
--    Parent - A node
--    Child  - A node
--
-- Returns :
--
--    True if Parent is an immediate ancestor of Child
--
-- Exceptions :
--
--    Constraint_Error - Parent or Child is null
--
   function Related (Parent : Node; Child : Node) return Boolean;
--
-- Remove -- A node from the graph
--
--    Vertex - The node to remove
--
-- This  procedure  removes  Vertex  from  the  graph. Each pair of arcs
-- leading from a parent of Vertex to a child of, is replaced by an  arc
-- from the parent to the child.
--
   procedure Remove (Vertex : Node);
--
-- Same -- Node objects equivalence
--
--    Left, Right - Nodes to compare
--
-- Returns :
--
--    True if Left precedes Right
--
   function Same (Left, Right : Node) return Boolean;
--
-- Node_Arrays -- Unbounded arrays of nodes (instantiation)
--
   package Node_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Node,
             Object_Array_Type => Nodes_Array,
             Null_Element      => null
          );
--
-- Node_Sets -- Sets of nodes
--
   package Node_Sets is
      new Generic_Set
          (  Object_Type  => Node,
             Null_Element => null,
             "="          => Same,
             "<"          => Precedes
          );

   function Get_Children (Parent : Node) return Node_Sets.Set;
   function Get_Parents (Child : Node) return Node_Sets.Set;

private
   pragma Inline (Precedes);
   pragma Inline (Same);

   function "<" (Left, Right : Node) return Boolean;
   pragma Inline ("<");

   package Node_Address_Sets is new Generic_Set (Node, null);

end Generic_Directed_Graph;
