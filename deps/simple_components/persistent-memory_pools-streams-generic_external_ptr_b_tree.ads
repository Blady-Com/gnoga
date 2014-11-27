--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.Streams.            Luebeck            --
--        Generic_External_Ptr_B_Tree              Autumn, 2014       --
--  Interface                                                         --
--                                Last revision :  10:05 22 Nov 2014  --
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
   with function Input_Key
                 (  Stream : access Root_Stream_Type'Class
                 )  return Key_Type is <>;
   with procedure Output_Key
                  (  Stream : access Root_Stream_Type'Class;
                     Key    : Key_Type
                  )  is <>;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
package Persistent.Memory_Pools.Streams.Generic_External_Ptr_B_Tree is
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
-- Get_Pointer -- The pointer corresponding to the item
--
--    Item - Pointer to the item
--
-- Returns :
--
--    The address
--
-- Exceptions :
--
--    Constraint_Error - No or illegal item
--
   function Get_Pointer (Item : Item_Ptr) return Byte_Index;
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
-- Remove -- The object and its key from the tree
--
--    Item      - Pointer to the item
--  [ Pointer ] - The old pointer, or 0 if No_Item
--
-- After  removal Item is set to No_Item.  Nothing  happens  if  Item is
-- already No_Item.  Pointer when specified  is set to  the pointer from
-- the removed pair or 0 if no pair removed.
--
-- Exceptions :
--
--    Constraint_Error - Illegal pointer
--
   procedure Remove (Item : in out Item_Ptr);
   procedure Remove (Item : in out Item_Ptr; Pointer : out Byte_Index);
--
-- Replace -- Change item pointer in the tree
--
--    Item       - Pointer to the item
--    Pointer    - To replace the old pointer
--  [ Replaced ] - The old pointer
--
-- This procedure replaces the item value.
--
-- Exceptions :
--
--    Constraint_Error - No item
--
   procedure Replace (Item : Item_Ptr; Pointer : Byte_Index);
   procedure Replace
             (  Item     : Item_Ptr;
                Pointer  : Byte_Index;
                Replaced : out Byte_Index
             );
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
--    Pointer   - The pointer of the item to be added
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
                Pointer   : Byte_Index
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
-- Get -- Get pointer by its key
--
--    Container - The tree
--    Key       - The key
--
-- Returns :
--
--    The pointer corresponding to the key
--
-- Exceptions :
--
--    Contraint_Error - Item not found
--
   function Get (Container : B_Tree; Key : Key_Type) return Byte_Index;
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
--  [ Pointer ] - The removed pointer, or 0 if tree was not changed
--
-- Nothing happens if the B-tree does not contain an item with Key.
--
   procedure Remove (Container : in out B_Tree; Key : Key_Type);
   procedure Remove
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Pointer   : out Byte_Index
             );
--
-- Replace -- Replace an item in the B-tree
--
--    Container  - The tree to be modified
--    Key        - Of the item to be added / replaced
--    Pointer    - To be added / replaced
--  [ Replaced ] - The old pointer if replaced, 0 if added
--
-- This procedure adds Item to the set.  The value is replaced  if it is
-- already in the B-tree.
--
   procedure Replace
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Pointer   : Byte_Index
             );
   procedure Replace
             (  Container : in out B_Tree;
                Key       : Key_Type;
                Pointer   : Byte_Index;
                Replaced  : out Byte_Index
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

private
   type B_Tree_Ptr is access all B_Tree'Class;

   type Item_Ptr is record
      Tree  : B_Tree_Ptr;
      Node  : Byte_Index;
      Index : Positive;
   end record;

   Width : constant := (Max_Size - 4 - 2 - 2 - 8 - 8) / (3 * 8);

   Length_Offset         : constant := 2; -- Skips the block margin
   Parent_Index_Offset   : constant := Length_Offset + 2;
   Parent_Address_Offset : constant := Parent_Index_Offset + 2;
   Key_Offset            : constant := Parent_Address_Offset + 8;
   Value_Offset          : constant := Key_Offset   + 8 * Width;
   Child_Offset          : constant := Value_Offset + 8 * Width;
   Node_Size             : constant := Child_Offset + 8 * (Width + 1);

   pragma Assert (Node_Size <= Block_Byte_Size);

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
--    Container - The tree
--    Parent    - The location of insertion or parent node
--    Key       - The key
--    Key_Index - Address of the key
--    Pointer   - The pointer
--    Child     - The child node or 0
--
   procedure Insert
             (  Parent    : Item_Ptr;
                Key       : Key_Type;
                Key_Index : Byte_Index;
                Pointer   : Byte_Index;
                Child     : Byte_Index
             );
--
-- Insert -- Insert an node right of the key
--
--    Container - The tree
--    Parent    - The location of insertion or parent node
--  [ Key ]     - The key
--    Key_Index - Address of the key
--    Pointer   - The pointer
--    Child     - The child node or 0
--
    procedure Insert
              (  Container : in out B_Tree;
                 Parent    : Byte_Index;
                 Key       : Key_Type;
                 Key_Index : Byte_Index;
                 Pointer   : Byte_Index;
                 Child     : Byte_Index
              );
    procedure Insert
              (  Container : in out B_Tree;
                 Parent    : Byte_Index;
                 Key_Index : Byte_Index;
                 Pointer   : Byte_Index;
                 Child     : Byte_Index
              );
--
-- New_Root -- Create new root
--
--    Container - The tree
--    Key       - The key
--    Pointer   - The pointer
--    Left      - The left child
--    Right     - The left child
--
   procedure New_Root
             (  Container : in out B_Tree;
                Key       : Byte_Index;
                Pointer   : Byte_Index;
                Left      : Byte_Index;
                Right     : Byte_Index
             );
--
-- Remove -- Key from the bucket
--
--    Container - The tree
--    Node      - The node to update
--    Index     - Of the key-value pair to remove
--    Pointer   - Pointer from the removed pair
--
   procedure Remove
             (  Container : in out B_Tree;
                Node      : Byte_Index;
                Index     : Positive;
                Pointer   : out Byte_Index
             );
--
-- Update -- The bucket at specified index
--
--    Container - The tree
--    Node      - The node to update
--    Index     - Of the key-value pair to change
--    Key       - The key
--    Pointer   - The pointer
--    Child     - The child
--    Enlarge   - Increase the bucket length
--
   procedure Update
             (  Container : in out B_Tree;
                Node      : Byte_Index;
                Index     : Positive;
                Key       : Byte_Index;
                Pointer   : Byte_Index;
                Child     : Byte_Index;
                Enlarge   : Boolean := False
             );

   No_Item : constant Item_Ptr := (null, 0, 1);

   pragma Inline (Add);
   pragma Inline (Get_First);
   pragma Inline (Get_Key);
   pragma Inline (Get_Last);
   pragma Inline (Get_Next);
   pragma Inline (Get_Pointer);
   pragma Inline (Get_Previous);
   pragma Inline (Is_In);

end Persistent.Memory_Pools.Streams.Generic_External_Ptr_B_Tree;
