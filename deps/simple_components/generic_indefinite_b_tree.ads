--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Indefinite_B_Tree                   Luebeck            --
--  Interface                                      Spring, 2014       --
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
--
--  This package resembles Generic_B_Tree, but can  be instantiated with
--  indefinite types.
--
with Ada.Finalization;
with System;

generic
   type Key_Type    (<>) is private;
   type Object_Type (<>) is private;
   type Tag_Type    (<>) is private;
   Width : Positive := 256;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
package Generic_Indefinite_B_Tree is
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
-- Get_First -- Get the child item with the least key
--
--    Item  - Pointer to an item
--
-- Returns :
--
--    The item with the least key or no item if Item is no item
--
   function Get_First (Item : Item_Ptr) return Item_Ptr;
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
-- Get_Item -- Get item by its position in its bucket
--
--    Item  - Pointer to an item in the bucket
--    Index - The position 1..Get_Bucket_Size
--
-- Returns :
--
--    The item or no item
--
-- Exceptions :
--
--    Constraint_Error - Item is no item
--
   function Get_Item (Item : Item_Ptr; Index : Positive)
      return Item_Ptr;
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
-- Get_Last -- Get the child item with the greatest key
--
--    Item  - Pointer to an item
--
-- Returns :
--
--    The item with the greatest key or no item if Item is no item
--
   function Get_Last (Item : Item_Ptr) return Item_Ptr;
--
-- Get_Left_Child -- Get immediate child item
--
--    Item  - Pointer to the item
--
-- This function returns the child with the greatest key less than the
-- item's key.
--
-- Returns :
--
--    The item or no item
--
   function Get_Left_Child (Item : Item_Ptr) return Item_Ptr;
--
-- Get_Left_Parent -- The parent
--
--    Item  - Pointer to the item
--
-- This function  returns the parent  item with the keys lesser than the
-- item's key.
--
-- Returns :
--
--    A pointer to the parent item or no item
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   function Get_Left_Parent (Item : Item_Ptr) return Item_Ptr;
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
-- Get_Right_Child -- Get immediate child item
--
--    Item  - Pointer to the item
--
-- This function returns  the child with  the least key greater than the
-- item's key.
--
-- Returns :
--
--    The item or no item
--
   function Get_Right_Child (Item : Item_Ptr) return Item_Ptr;
--
-- Get_Right_Parent -- The parent
--
--    Item - Pointer to the item
--
-- This function  returns the parent item with the keys greater than the
-- item's key.
--
-- Returns :
--
--    A pointer to the parent item or no item
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   function Get_Right_Parent (Item : Item_Ptr) return Item_Ptr;
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
-- Get_Tag -- The tag corresponding to the item's bucket
--
--    Item - Pointer to the item
--
-- Returns :
--
--    The tag
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   function Get_Tag (Item : Item_Ptr) return Tag_Type;
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
--
-- Set_Tag -- Set tag corresponding to the item's bucket
--
--    Item - Pointer to the item
--    Tag  - The tag to set
--
-- Tag  can be any value used extension purpose.  Initially it is set to
-- zero.
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   procedure Set_Tag (Item : Item_Ptr; Tag : Tag_Type);   
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
-- Get_Root -- The root item of the tree
--
--    Item - Pointer to the item
--
-- Returns :
--
--    The first item in the root bucket or no item
--
   function Get_Root (Container : B_Tree) return Item_Ptr;
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
--
-- Generic_Traverse -- Shallow tree traversal
--
--    Container - The tree to traverse
--    From      - The item to start at
--    To        - The key to stop at
--
-- This procedure traverses items  of the tree with the keys starting at
-- the item From less than or equal to To. The traversals is shallow, if
-- a bucket  contains items with  the keys in the range,  which includes
-- items of all subtrees,  then it is visited as a whole once. There are
-- two visitor functions:
--
--    Visit_Range - This function  is called  for  each  bucket of items
--                  within the range.  An item from the bucket is passed
--       to identify it.  The function returns False  to  stop traversal
--       immediately. All items are visited in the key-ascending order.
--
--    Visit_Item  - This function  is called  for each tree item that is
--                  not in a bucket for which Visit_Range is called. The
--       functions returns one of the following values:
--
--       Quite stops the traversal immediately;
--       Step_Over continues traversal to the next tree bucket;
--       Step_In continues traversal into the bucket.
--
   type Bucket_Traversal is (Quit, Step_Over, Step_In);
   generic
      with function Visit_Item
                    (  Container : B_Tree;
                       Key       : Key_Type;
                       Item      : Item_Ptr
                    )  return Boolean is <>;
      with function Visit_Range
                    (  Container : B_Tree;
                       Item      : Item_Ptr
                    )  return Bucket_Traversal is <>;
   procedure Generic_Traverse
             (  Container : B_Tree;
                From      : Item_Ptr;
                To        : Key_Type
             );
--
-- Abstract_Visitor -- Abstract visitor base type
--
   type Abstract_Visitor is abstract
      new Ada.Finalization.Limited_Controlled with null record;
--
-- Visit_Item -- Equivalent to the generic variant
--
   function Visit_Item
            (  Iterator  : access Abstract_Visitor;
               Container : B_Tree'Class;
               Key       : Key_Type;
               Item      : Item_Ptr
            )  return Boolean is abstract;
--
-- Visit_Range -- Equivalent to the generic variant
--
   function Visit_Range
            (  Iterator  : access Abstract_Visitor;
               Container : B_Tree'Class;
               Item      : Item_Ptr
            )  return Bucket_Traversal is abstract;
--
-- Traverse -- Non-generic tree traversal
--
--    Container - The tree to traverse
--    Iterator  - The iterator object to use
--    From      - The item to start at
--    To        - The key to stop at
--
-- This procedure traverses  items of the tree starting at the item From
-- less than or equal to To using the Iterator object.
--
   procedure Traverse
             (  Container : B_Tree;
                Iterator  : in out Abstract_Visitor'Class;
                From      : Item_Ptr;
                To        : Key_Type
             );

private
   type Node_Type;
   type Node_Ptr is access Node_Type;

   type Item_Ptr is record
      Node  : Node_Ptr;
      Index : Positive;
   end record;

   type Key_Ptr is access Key_Type;
   type Object_Ptr is access Object_Type;
   type Tag_Ptr is access Tag_Type;
   type Pair_Type is record
      Key   : Key_Ptr;
      Value : Object_Ptr;
   end record;
   function "=" (Left, Right : Pair_Type) return Boolean;

   type Pair_Array is array (Positive range <>) of Pair_Type;
   type Node_Ptr_Array is array (Positive range <>) of Node_Ptr;

   type Node_Type is record
      Length   : Integer := 0;
      Parent   : Item_Ptr;
      Tag      : Tag_Ptr;
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

end Generic_Indefinite_B_Tree;
