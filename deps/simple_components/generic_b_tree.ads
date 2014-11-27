--                                                                    --
--  package Generic_B_Tree          Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2014       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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
--  This package provides  an implementation of B trees. A  B tree is  a
--  map effective for large sets of keys. The  tree  is  balanced,  each
--  node has up to fixed number  of  keys. Keys in the nodes are sorted,
--  binary search is used to within a node.
--
--  The package is generic. It has the following generic parameters:
--
--     Key_Type    - The type of the key used to order/access the B-tree
--     Object_Type - The type of the elements
--     Width       - The number of keys stored in a tree node
--     <           - Relational operator on keys
--     =           - Relational operator on keys
--
--  The children nodes on the left of the key km have keys Kl <= Km. The
--  children nodes on the right of the key have keys Kr > Km
--
--                                         K7
--                                      [ * | * |   |   ]
--                                       /     \
--                        K3            /       \
--                     [ * | * |   |   ]        [ * | * |   |   ]
--                      /     \                  /    \
--      K1  K2        /        \  K4  K5  K6
--   [ 0 | 0 | 0 |   ]         [ 0 | 0 | 0 | 0 ]
--
with Ada.Finalization;
with System;

generic
   type Key_Type is private;
   type Object_Type is private;
   Width : Positive := 256;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
package Generic_B_Tree is
--
-- Item_Ptr -- Points to an item in the B tree
--
-- Item pointers are volatile,  a tree update operation can  potentially
-- invalidate any pointer.
--
   type Item_Ptr is private;
   No_Item : constant Item_Ptr;
--
-- Get_Bucket_Address -- The address of the item's bucket
--
--    Item - Pointer to the item
--
-- Returns :
--
--    Address of the bucket
--
   function Get_Bucket_Address (Item : Item_Ptr) return System.Address;
--
-- Get_Bucket_Size -- The number used slots in the item's bucket
--
--    Item - Pointer to the item
--
-- Returns :
--
--    Number of simbling items in the same bucket
--
   function Get_Bucket_Size (Item : Item_Ptr) return Natural;
--
-- Get_Index -- The position of the item in its bucket
--
--    Item - Pointer to the item
--
-- Returns :
--
--    The position 1..Get_Bucket_Size
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   function Get_Index (Item : Item_Ptr) return Positive;
--
-- Get_Key -- The key corresponding to the item
--
--    Item - Pointer to the item
--
-- Returns :
--
--    The key
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   function Get_Key (Item : Item_Ptr) return Key_Type;
--
-- Get_Next -- The next item
--
--    Item - Pointer to the item
--
-- Returns :
--
--    A pointer to the next item or no item
--
   function Get_Next (Item : Item_Ptr) return Item_Ptr;
--
-- Get_Previous -- The previous item
--
--    Item - Pointer to the item
--
-- Returns :
--
--    A pointer to the previous item or no item
--
   function Get_Previous (Item : Item_Ptr) return Item_Ptr;
--
-- Get_Root -- The root item of the tree
--
--    Item - Pointer to the item
--
-- Returns :
--
--    The first item in the root bucket or no item
--
   function Get_Root (Item : Item_Ptr) return Item_Ptr;
--
-- Get_Value -- The object corresponding to the item
--
--    Item - Pointer to the item
--
-- Returns :
--
--    The object
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   function Get_Value (Item : Item_Ptr) return Object_Type;
--
-- Remove -- The object and its key from the tree
--
--    Item - Pointer to the item
--
-- After  removal Item is set to No_Item.  Nothing  happens  if  Item is
-- already No_Item.
--
-- Exceptions :
--
--    Constraint_Error - Illegal pointer
--
   procedure Remove (Item : in out Item_Ptr);
--
-- Replace -- Change item value in the tree
--
--    Item  - Pointer to the item
--    Value - To replace the old value
--
-- This procedure replaces the item value.
--
-- Exceptions :
--
--    Constraint_Error - No item
--
   procedure Replace (Item : Item_Ptr; Value : Object_Type);
------------------------------------------------------------------------
--
-- B_Tree -- B tree object
--
   type B_Tree is new Ada.Finalization.Limited_Controlled with private;
--
-- Add -- Add a new item to the tree
--
--    Container - The tree to modify
--    Key       - Of the item to be added
--    Item      - The item to be added
--
-- This procedure adds Item to the tree.
--
-- Exceptions :
--
--    Constraint_Error - There  is already an item with the key equal to
--                       Key.
--
   procedure Add
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Value     : Object_Type
             );
--
-- Erase -- Remove all items from the B-tree
--
--    Container - The tree to erase
--
   procedure Erase (Container : in out B_Tree);
--
-- Finalize -- Destructor
--
--    Container - The tree
--
   procedure Finalize (Container : in out B_Tree);
--
-- Find -- Find an item in the tree
--
--    Container - The tree
--    Key       - To search for
--
-- Returns :
--
--    The pointer to found item or no item
--
   function Find (Container : B_Tree; Key : Key_Type) return Item_Ptr;
--
-- Get -- Get an item by its key
--
--    Container - The tree
--    Key       - The key
--
-- Returns :
--
--    The item
--
-- Exceptions :
--
--    Contraint_Error - Item not found
--
   function Get (Container : B_Tree; Key : Key_Type) return Object_Type;
--
-- Get_First -- The first item in the tree
--
--    Container - The tree
--
-- Returns :
--
--    The item with the least key or else no item
--
   function Get_First (Container : B_Tree) return Item_Ptr;
--
-- Get_Last -- The last item in the tree
--
--    Container - The tree
--
-- Returns :
--
--    The item with the biggest key or else no item
--
   function Get_Last (Container : B_Tree) return Item_Ptr;
--
-- Inf -- Find an item that is less than or equal to the key
--
--    Container - The tree
--    Key       - To search for
--
-- Returns :
--
--    The pointer to found item or no item
--
   function Inf (Container : B_Tree; Key : Key_Type) return Item_Ptr;
--
-- Is_Empty -- Test if the tree is empty
--
--    Container - The tree
--
-- Returns :
--
--    True if the B-tree contains no items
--
   function Is_Empty (Container : B_Tree) return Boolean;
--
-- Is_In -- Test if an item in the B-tree
--
--    Container - The set
--    Key       - To be searched for
--
-- Returns :
--
--    True if the item is in the set
--
   function Is_In (Container : B_Tree; Key : Key_Type) return Boolean;
--
-- Remove -- Remove element from the B-tree
--
--    Container - The set
--    Key       - Of the item to be removed
--
-- Nothing happens if the B-tree does not contain an item with Key.
--
   procedure Remove (Container : in out B_Tree; Key : Key_Type);
--
-- Replace -- Replace an item in the B-tree
--
--    Container - The tree to be modified
--    Key       - Of the item to be added / replaced
--    Value     - To be added / replaced
--
-- This procedure adds Item to the set. The element is replaced if it is
-- already in the B-tree.
--
   procedure Replace
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Value     : Object_Type
             );
--
-- Sup -- Find an item that is greater than or equal to the key
--
--    Container - The tree
--    Key       - To search for
--
-- Returns :
--
--    The pointer to found item or no item
--
   function Sup (Container : B_Tree; Key : Key_Type) return Item_Ptr;
--
-- = -- Comparison
--
--    Left  - A tree to compare
--    Right - Another tree
--
-- Returns :
--
--    True if both trees contain same items
--
   function "=" (Left, Right : B_Tree) return Boolean;

--     type Coverage_Paths is
--          (  Insert_Underfilled_Bucket,
--             Insert_Underfilled_Left_Sibling,
--             Insert_Underfilled_Right_Sibling,
--             Insert_Left_Root_Split,
--             Insert_Left_Bucket_Split,
--             Insert_Middle_Root_Split,
--             Insert_Middle_Split,
--             Insert_Right_Root_Split,
--             Insert_Right_Bucket_Split,
--             Remove_Root_Key,
--             Remove_Root_Bucket,
--             Remove_Childless_Bucket,
--             Remove_Child_On_The_Right,
--             Remove_Replacing_By_Right,
--             Remove_Replacing_By_Left,
--             Remove_Bucket_With_Left,
--             Remove_Bucket_With_Right,
--             Remove_Child_On_The_Left,
--             Remove_From_Left_Bucket_1,
--             Remove_From_Left_Bucket_2,
--             Remove_From_Right_Bucket_1,
--             Remove_From_Right_Bucket_2
--          );
--     Visited : array (Coverage_Paths) of Boolean := (others => False);
private
   type Node_Type;
   type Node_Ptr is access Node_Type;

   type Item_Ptr is record
      Node  : Node_Ptr;
      Index : Positive;
   end record;

   type Pair_Type is record
      Key   : Key_Type;
      Value : Object_Type;
   end record;

   type Pair_Array is array (Positive range <>) of Pair_Type;
   type Node_Ptr_Array is array (Positive range <>) of Node_Ptr;

   type Node_Type is record
      Length   : Integer := 0;
      Parent   : Item_Ptr;
      Pairs    : Pair_Array     (1..Width);
      Children : Node_Ptr_Array (1..Width + 1);
   end record;

   type B_Tree is new Ada.Finalization.Limited_Controlled with record
      Root : Node_Ptr;
   end record;
--
-- Erase -- Erase a subtree
--
--    Root - Of the subtree to erase
--
   procedure Erase (Root : in out Node_Ptr);
--
-- Get_First -- The first child item
--
--    Root - The node to start search at
--
-- Returns :
--
--    The item with the least key in the subtree or else no item
--
   function Get_First (Root : Node_Ptr) return Item_Ptr;
--
-- Get_Last -- The last child item
--
--    Root - The node to start search at
--
-- Returns :
--
--    The item with the greatest key in the subtree or else no item
--
   function Get_Last (Root : Node_Ptr) return Item_Ptr;
--
-- Search -- Search the tree with a key
--
--    Root  - The node to start search at
--    Key   - The key
--    Node  - Containing the key or the leaf node to insert it new
--    Index - Of the key or else negated position to insert it
--
   procedure Search
             (  Root  : Node_Ptr;
                Key   : Key_Type;
                Node  : out Node_Ptr;
                Index : out Integer
             );
--
-- Insert -- Insert an node right of the key
--
--    Container - The tree
--    Parent    - The location of insertion or parent node
--    Pair      - The key-value pair
--    Child     - The child nodes or null
--
   procedure Insert
             (  Container : in out B_Tree;
                Parent    : Item_Ptr;
                Pair      : Pair_Type;
                Child     : Node_Ptr
             );
   procedure Insert
             (  Container : in out B_Tree;
                Parent    : Node_Ptr;
                Pair      : Pair_Type;
                Child     : Node_Ptr
             );
--
-- Underfilled_Left -- Check if the left sibling is underfilled
--
--    Right - The right sibling node
--
-- Returns :
--
--    True if the left sibling bucket has free room
--
   function Underfilled_Left (Right : Node_Type) return Boolean;
--
-- Underfilled_Right -- Check if the right sibling is underfilled
--
--    Left - The left sibling node
--
-- Returns :
--
--    True if the right sibling bucket has free room
--
   function Underfilled_Right (Left : Node_Type) return Boolean;

   No_Item : constant Item_Ptr := (null, 1);

   pragma Inline (Add);
   pragma Inline (Get_First);
   pragma Inline (Get_Last);
   pragma Inline (Get_Next);
   pragma Inline (Get_Previous);
   pragma Inline (Is_In);

end Generic_B_Tree;
