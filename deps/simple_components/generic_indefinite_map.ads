--                                                                    --
--  package Generic_Indefinite_Map  Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  22:41 09 Mar 2020  --
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
--  This package is similar to Generic_Map,  but also allows  indefinite
--  key and object types as the actual formal parameters.
--
with Ada.Finalization;

generic
   type Key_Type (<>) is private;
   type Object_Type (<>) is private;
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
package Generic_Indefinite_Map is
   pragma Preelaborate (Generic_Indefinite_Map);
   type Map is new Ada.Finalization.Controlled with private;
--
-- Add -- Add a new item to the map
--
--    Container - The set to be modified
--    Key       - Of the item to be added
--    Item      - The item to be added
--
-- This procedure adds Item to the map.
--
-- Exceptions :
--
--    Constraint_Error - There  is already an item with the key equal to
--                       Key.
--
   procedure Add
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type
             );
--
-- Add -- Add items from one map to another
--
--    Container - The set to be modified
--    Items     - To be added
--
-- This procedure adds the items from Items to  Container.  An  item  is
-- added  only  if  Container does not already has another item with the
-- equal key.
--
   procedure Add (Container : in out Map; Items : Map);
--
-- Create -- Create a new empty map
--
-- Returns :
--
--    The new set
--
   function Create return Map;
--
-- Erase -- Remove all items from the map
--
--    Container - The map
--
   procedure Erase (Container : in out Map);
--
-- Finalize -- Destructor
--
--    Container - The map
--
-- When  finalized  the  use  count  is  decreased.  Only  if it becomes
-- negative, the set is finally removed.
--
   procedure Finalize (Container : in out Map);
--
-- Find -- Find an item in the map
--
--    Container - The map
--    Key       - To search for
--
-- When search is successful the result is positive.  Otherwise,  it  is
-- negative and indicates the position the  item  should  be  placed  if
-- inserted into the map.
--
-- Returns :
--
--    [+]  The index of the item
--    [-]  The negated index of the position the item should be
--
   function Find (Container : Map; Key : Key_Type) return Integer;
--
-- Get -- Get an item by its key
--
--    Container - The map
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
   function Get (Container : Map; Key : Key_Type) return Object_Type;
--
-- Get -- Get an item by its index
--
--    Container - The map
--    Index     - The number of the item to get
--
-- Lesser keys have lesser numbers, so Get (Container, 1) gives the item
-- with the smallest key.
--
-- Returns :
--
--    The item
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Get (Container : Map; Index : Positive) return Object_Type;
--
-- Get_Key -- Get a key of some item by its index
--
--    Container - The map
--    Index     - The number of the item
--
-- Returns :
--
--    The key of the item
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Get_Key (Container : Map; Index : Positive) return Key_Type;
--
-- Get_Size -- Get the number of items in the map
--
--    Container - The map
--
-- Returns :
--
--    The number of elements
--
   function Get_Size (Container : Map) return Natural;
--
-- Is_Empty -- Test if the map is empty
--
--    Container - The map
--
-- Returns :
--
--    True if the map contains no items
--
   function Is_Empty (Container : Map) return Boolean;
--
-- Is_In -- Test if an item in the map
--
--    Container - The set
--    Key       - To be searched for
--
-- Returns :
--
--    True if the item is in the set
--
   function Is_In (Container : Map; Key : Key_Type) return Boolean;
--
-- Remove -- Remove element from the map
--
--    Container - The set
--    Key       - Of the item to be removed
--
-- Nothing happens if the map does not contain an item with Key.
--
   procedure Remove (Container : in out Map; Key : Key_Type);
--
-- Remove -- Remove elements of one map from another map
--
--    Container - The map
--    Items     - The map of items to be removed
--
-- If  an  item  from the map Items is not in the map Container, nothing
-- happens. Otherwise it is removed from Container.
--
   procedure Remove (Container : in out Map; Items : Map);
--
-- Remove -- Remove element from the map by its index
--
--    Container - The map
--    Index     - Of the item to be removed
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Remove (Container : in out Map; Index : Positive);
--
-- Replace -- Replace an item in the map
--
--    Container - The map to be modified
--    Key       - Of the item to be added / replaced
--    Item      - To be added / replaced
--
-- This procedure adds Item to the set. The element is replaced if it is
-- already in the map.
--
   procedure Replace
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type
             );
--
-- Replace -- Replace items from one map in another
--
--    Container - The map to be modified
--    Items     - To be added
--
-- This procedure  adds  the  items  from  the  map  Items  to  the  map
-- Container. An element is replaced if it is already in the map.
--
   procedure Replace (Container : in out Map; Items : Map);
--
-- Replace -- Replace an item in the map by its index
--
--    Container - The map to be modified
--    Index     - Of the item to be replaced
--    Item      - To be replaced
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Replace
             (  Container : in out Map;
                Index     : Positive;
                Item      : Object_Type
             );
--
-- = -- Comparison
--
--    Left  - A map
--    Right - A map
--
-- Returns :
--
--    True if both maps contain same items
--
   function "=" (Left, Right : Map) return Boolean;

private
   pragma Inline (Add);
   pragma Inline (Create);
   pragma Inline (Get_Size);
   pragma Inline (Is_Empty);
   pragma Inline (Is_In);

   type Key_Type_Ptr is access Key_Type;
   type Object_Type_Ptr is access Object_Type;
   type Token is record
      Key  : Key_Type_Ptr;
      Item : Object_Type_Ptr;
   end record;
   type Token_Array is array (Positive range <>) of Token;
   type Token_Array_Ptr is access Token_Array;
   type Data is record
      Size      : Natural := 0;
      Use_Count : Natural := 0;
      Vector    : Token_Array_Ptr := null;
   end record;
   type Data_Ptr is access Data;
--
-- The  set  wraps pointer to the structure Data. When set is copied the
-- pointer  is duplicated and the Use_Count is increased. A deep copy is
-- made only if a destructive operation is applied.
--
   type Map is new Ada.Finalization.Controlled with record
      Object : Data_Ptr := null;
   end record;
--
-- Adjust -- Assignment
--
--    Container - The new map to be adjusted
--
-- Assignment increases the use count. No deep copy made.
--
   procedure Adjust (Container : in out Map);
--
-- Clone -- Copy a map if it is currently shared
--
--    Container - The map
--
-- This procedure is called if a destructive operation has to be applied
-- to  the  map.  If  the use count is greater than zero it makes a deep
-- copy  and  decreases the  use  count.  The  copy  will be used by the
-- operation.
--
   procedure Clone (Container : in out Map);

end Generic_Indefinite_Map;
