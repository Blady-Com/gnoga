--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Streams.            Luebeck            --
--        Generic_External_B_Tree                  Spring, 2014       --
--  Interface                                                         --
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
--  This package provides B-trees allocated in an external storage.
--
with Ada.Streams;  use Ada.Streams;
with System;       use System;

with Ada.Finalization;

generic
   type Key_Type (<>) is private;
   type Object_Type (<>) is private;
   with function Input_Key
                 (  Stream : access Root_Stream_Type'Class
                 )  return Key_Type is <>;
   with function Input_Value
                 (  Stream : access Root_Stream_Type'Class
                 )  return Object_Type is <>;
   with procedure Output_Key
                  (  Stream : access Root_Stream_Type'Class;
                     Key    : Key_Type
                  )  is <>;
   with procedure Output_Value
                  (  Stream : access Root_Stream_Type'Class;
                     Value  : Object_Type
                  )  is <>;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
package Persistent.Memory_Pools.Streams.Generic_External_B_Tree is
--
-- Abstract_Value_Access -- User-defined object to access value
--
   type Abstract_Value_Access is abstract
      new Ada.Finalization.Limited_Controlled with null record;
--
-- Get -- Value from stream
--
--    Consumer - The recieve the value
--    Stream   - The stream to get the value from
--
   procedure Get
             (  Consumer : in out Abstract_Value_Access;
                Stream   : in out Root_Stream_Type'Class
             )  is abstract;
--
-- Put -- Value into stream
--
--    Producer - The produce the value
--    Stream   - The stream to put the value into
--
   procedure Put
             (  Producer : in out Abstract_Value_Access;
                Stream   : in out Root_Stream_Type'Class
             )  is abstract;
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
--    Index of the bucket
--
   function Get_Bucket_Address (Item : Item_Ptr) return Byte_Index;
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
-- Get_Key_Address -- The address of the key corresponding to the item
--
--    Item - Pointer to the item
--
-- Returns :
--
--    The address (byte index) of the key
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   function Get_Key_Address (Item : Item_Ptr) return Byte_Index;
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
--    Item  - Pointer to the item
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
   function Get_Tag (Item : Item_Ptr) return Byte_Index;
--
-- Get_Value -- The value corresponding to the item
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
-- Get_Value -- Get value corresponding to the item
--
--    Item     - Pointer to the item
--    Consumer - The user object to receive the value
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   procedure Get_Value
             (  Item     : Item_Ptr;
                Consumer : in out Abstract_Value_Access'Class
             );
--
-- Get_Value_Address -- The address of the value
--
--    Item - Pointer to the item
--
-- Returns :
--
--    The address (byte index) of the value
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   function Get_Value_Address (Item : Item_Ptr) return Byte_Index;
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
-- Replace -- Change item value in the tree
--
--    Item     - Pointer to the item
--    Producer - Of the value to replace the old value
--
-- This procedure replaces the item value.
--
-- Exceptions :
--
--    Constraint_Error - No item
--
   procedure Replace
             (  Item     : Item_Ptr;
                Producer : in out Abstract_Value_Access'Class
             );
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
   procedure Set_Tag (Item : Item_Ptr; Tag : Byte_Index);
------------------------------------------------------------------------
--
-- B_Tree -- B tree object
--
--    Pool - The parent pool
--
   type B_Tree
        (  Pool : access Persistent_Pool'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
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
-- Add -- Add a new item to the tree
--
--    Container - The tree to modify
--    Key       - Of the item to be added
--    Producer  - Of the value corresponding to the key
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
                Producer  : in out Abstract_Value_Access'Class
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
-- Get -- Get an value by its key
--
--    Container - The tree
--    Key       - The key
--
-- Returns :
--
--    The value
--
-- Exceptions :
--
--    Contraint_Error - Item not found
--
   function Get (Container : B_Tree; Key : Key_Type) return Object_Type;
--
-- Get -- Get an value by its key
--
--    Container - The tree
--    Key       - The key
--    Consumer  - Of the value
--
-- Exceptions :
--
--    Contraint_Error - Item not found
--
   procedure Get
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Consumer  : in out Abstract_Value_Access'Class
             );
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
-- Get_Root_Address -- The byte index of the tree root
--
--    Container - The tree
--
-- The result of this function  is the byte index of the  root bucket of
-- the tree.  Note that the index may change  as the tree gets  updated.
-- The  tree root is set using Set_Root_Address when the tree object has
-- to be restored from the persistent storage.  Typically before  object
-- finalization its actual root is obtained  and stored somewhere in the
-- persistent storage.  When the storage  is re-opened the root index is
-- read from the storage,  a tree object is created and then initialized
-- using Set_Root_Address.
--
-- Returns :
--
--    The tree root byte index
--
   function Get_Root_Address (Container : B_Tree) return Byte_Index;
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
-- Initialize -- Constructor
--
--    Container - The tree
--
   procedure Initialize (Container : in out B_Tree);
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
-- This procedure adds Item to the set.  The value is replaced  if it is
-- already in the B-tree.
--
   procedure Replace
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Value     : Object_Type
             );
--
-- Replace -- Replace an item in the B-tree
--
--    Container - The tree to be modified
--    Key       - Of the item to be added / replaced
--    Producer  - Of the value to be added / replaced
--
-- This procedure adds Item to the set.  The value is replaced  if it is
-- already in the B-tree.
--
   procedure Replace
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Producer  : in out Abstract_Value_Access'Class
             );
--
-- Restore -- Restore tree stored using Store
--
--    Container - The tree
--    Reference - Obtained by Store
--
-- Exceptions :
--
--    Status_Error - The tree is not empty
--
   procedure Restore
             (  Container : in out B_Tree;
                Reference : Byte_Index
             );
--
-- Set_Root_Address -- Set the byte index of the tree root
--
--    Container - The tree
--    Root      - The byte index of the tree root
--
-- This procedure sets the root of the tree. See Get_Root_Address.
--
   procedure Set_Root_Address
             (  Container : in out B_Tree;
                Root      : Byte_Index
             );
--
-- Store -- Store the tree
--
--    Container - The tree
--    Reference - To the tree
--
-- This procedure stores the tree. It must be called before finalization
-- if the tree must persist. Otherwise it will be erased.  The tree  can
-- be restored using Restore:
--
--    declare
--       Tree : B_Tree (...); -- A new tree
--    begin
--       ... -- Work with the tree
--       Store (Tree, Reference);
--    end;
--    ...
--    declare
--       Tree : B_Tree (...); -- A tree
--    begin
--       Restore (Tree, Reference);
--       ... -- Continue to work with the tree
--    end;
--
   procedure Store
             (  Container : in out B_Tree;
                Reference : out Byte_Index
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
   type B_Tree_Ptr is access all B_Tree'Class;

   type Item_Ptr is record
      Tree  : B_Tree_Ptr;
      Node  : Byte_Index;
      Index : Positive;
   end record;

   Width : constant :=
                    (Byte_Count'Last - 4 - 2 - 2 - 8 - 8 - 8) / (3 * 8);

   Length_Offset         : constant := 2; -- Skips the block margin
   Parent_Index_Offset   : constant := Length_Offset         + 2;
   Parent_Address_Offset : constant := Parent_Index_Offset   + 2;
   Node_Tag_Offset       : constant := Parent_Address_Offset + 8;
   Key_Offset            : constant := Node_Tag_Offset       + 8;
   Value_Offset          : constant := Key_Offset   + 8 * Width;
   Child_Offset          : constant := Value_Offset + 8 * Width;
   Node_Size             : constant := Child_Offset + 8 * (Width + 1);

   pragma Assert (Node_Size <= Byte_Count'Last);

   type B_Tree
        (  Pool : access Persistent_Pool'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Self        : B_Tree_Ptr := B_Tree'Unchecked_Access;
      Root_Bucket : Byte_Index := 0;
   end record;
--
-- Erase -- Erase a subtree
--
--    Container - The tree
--    Root      - Of the subtree to erase
--
   procedure Erase
             (  Container : in out B_Tree;
                Root      : Byte_Index
             );
--
-- Get_First -- The first child item
--
--    Container - The tree
--    Root      - The node to start search at
--
-- Returns :
--
--    The item with the least key in the subtree or else no item
--
   function Get_First
            (  Container : B_Tree;
               Root      : Byte_Index
            )  return Item_Ptr;
--
-- Get_Key -- The key by its address
--
--    Container - The tree
--    Key       - The key address
--
-- Returns :
--
--    The key
--
   function Get_Key
            (  Container : B_Tree;
               Key       : Byte_Index
            )  return Key_Type;
--
-- Get_Last -- The last child item
--
--    Container - The tree
--    Root      - The node to start search at
--
-- Returns :
--
--    The item with the greatest key in the subtree or else no item
--
   function Get_Last
            (  Container : B_Tree;
               Root      : Byte_Index
            )  return Item_Ptr;
--
-- Get_Next -- The next item
--
--    Container - The tree
--    Node      - The node
--    Index     - The index
--
-- Returns :
--
--    The next item or no item
--
   function Get_Next
            (  Container : B_Tree;
               Node      : Byte_Index;
               Index     : Positive
            )  return Item_Ptr;
--
-- Get_Previous -- The previous item
--
--    Container - The tree
--    Node      - The node
--    Index     - The index
--
-- Returns :
--
--    The previous item or no item
--
   function Get_Previous
            (  Container : B_Tree;
               Node      : Byte_Index;
               Index     : Positive
            )  return Item_Ptr;
--
-- Get_Value -- The value by its address
--
--    Container - The tree
--    Value     - The value address
--
-- Returns :
--
--    The key
--
   function Get_Value
            (  Container : B_Tree;
               Value     : Byte_Index
            )  return Object_Type;
--
-- Find -- Search the bucket with a key
--
--    Container - The tree
--    Block     - The node
--    Key       - The key
--
-- Returns :
--
--    Key index, negative if not found
--
   function Find
            (  Container : B_Tree;
               Block     : Block_Type;
               Key       : Key_Type
            )  return Integer;
--
-- Search -- Search the tree with a key
--
--    Container - The tree
--    Root      - The node to start search at
--    Key       - The key
--    Node      - Containing the key or the leaf node to insert it new
--    Index     - Of the key or else negated position to insert it
--
   procedure Search
             (  Container : B_Tree;
                Root      : Byte_Index;
                Key       : Key_Type;
                Node      : out Byte_Index;
                Index     : out Integer
             );
--
-- Insert -- Insert an node right of the key
--
--    Container   - The tree
--    Parent      - The location of insertion or parent node
--    Key         - The key
--    Key_Index   - Address of the key
--    Value_Index - The value
--    Child       - The child node or 0
--
   procedure Insert
             (  Parent      : Item_Ptr;
                Key         : Key_Type;
                Key_Index   : Byte_Index;
                Value_Index : Byte_Index;
                Child       : Byte_Index
             );
--
-- Insert -- Insert an node right of the key
--
--    Container   - The tree
--    Parent      - The location of insertion or parent node
--  [ Key ]       - The key
--    Key_Index   - Address of the key
--    Value_Index - The value
--    Child       - The child node or 0
--
    procedure Insert
              (  Container   : in out B_Tree;
                 Parent      : Byte_Index;
                 Key         : Key_Type;
                 Key_Index   : Byte_Index;
                 Value_Index : Byte_Index;
                 Child       : Byte_Index
              );
    procedure Insert
              (  Container   : in out B_Tree;
                 Parent      : Byte_Index;
                 Key_Index   : Byte_Index;
                 Value_Index : Byte_Index;
                 Child       : Byte_Index
              );
--
-- New_Root -- Create new root
--
--    Container - The tree
--    Key       - The key
--    Value     - The value
--    Left      - The left child
--    Right     - The left child
--
   procedure New_Root
             (  Container : in out B_Tree;
                Key       : Byte_Index;
                Value     : Byte_Index;
                Left      : Byte_Index;
                Right     : Byte_Index
             );
   procedure New_Root
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Value     : Object_Type;
                Left      : Byte_Index;
                Right     : Byte_Index
             );
--
-- Remove -- Key from the bucket
--
--    Container - The tree
--    Node      - The node to update
--    Index     - Of the key-value pair to remove
--
   procedure Remove
             (  Container : in out B_Tree;
                Node      : Byte_Index;
                Index     : Positive
             );
--
-- Update -- The bucket at specified index
--
--    Container - The tree
--    Node      - The node to update
--    Index     - Of the key-value pair to change
--    Key       - The key
--    Value     - The value
--    Child     - The child
--    Enlarge   - Increase the bucket length
--
   procedure Update
             (  Container : in out B_Tree;
                Node      : Byte_Index;
                Index     : Positive;
                Key       : Byte_Index;
                Value     : Byte_Index;
                Child     : Byte_Index;
                Enlarge   : Boolean := False
             );
   procedure Update
             (  Container : in out B_Tree;
                Node      : Byte_Index;
                Index     : Positive;
                Key       : Key_Type;
                Value     : Object_Type;
                Child     : Byte_Index;
                Enlarge   : Boolean := False
             );

   No_Item : constant Item_Ptr := (null, 0, 1);

   pragma Inline (Add);
   pragma Inline (Get_First);
   pragma Inline (Get_Key);
   pragma Inline (Get_Last);
   pragma Inline (Get_Next);
   pragma Inline (Get_Previous);
   pragma Inline (Get_Value);
   pragma Inline (Is_In);

end Persistent.Memory_Pools.Streams.Generic_External_B_Tree;
