--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Suffix_Tree                            Luebeck            --
--  Sample interface                               Winter, 2009       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

with Generic_Directed_Weighted_Graph;
with Generic_Address_Order;

package Test_Suffix_Tree is
   type Node_Type is null record;    -- Nodes have no contents
   type Default is access Node_Type; -- Default access type
   --
   -- Node_Order -- Ordering of nodes by their addresses
   --
   package Node_Order is new Generic_Address_Order (Node_Type);
   use Node_Order;
   --
   -- Ordering of the edge weights
   --
   function Equal (Left, Right : access Character) return Boolean;
   function Less  (Left, Right : access Character) return Boolean;
   --
   -- Directed graph of Node_Type weighted by Character values
   --
   package Character_Weighted_Graphs is
      new Generic_Directed_Weighted_Graph
          (  Node_Type   => Node_Type,
             Weight_Type => Character,
             Pool        => Default'Storage_Pool,
             Minimal_Parents_Size => 1
          );
   subtype Suffix_Tree is Character_Weighted_Graphs.Node;
   --
   -- Build -- Creates the suffix tree from a string
   --
   function Build (Text : String) return Suffix_Tree;
   --
   -- Print -- Outputs the tree
   --
   procedure Print (Tree : Suffix_Tree; Prefix : String := "");

end Test_Suffix_Tree;
