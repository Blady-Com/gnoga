--                                                                    --
--  package Generic_Bounded_Map     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2003       --
--                                                                    --
--                                Last revision :  08:25 05 May 2020  --
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
--  This package defines the type Map simpilar to Generic_Map but having
--  limited size.  When a new  item is added to the map and the  map  is
--  full an item from a map's end is removed to make place.  The  map is
--  effective  when items added  are  almost  ordered  by  ascending  or
--  descending keys. Then the map acts as a LIFO.
--
with Ada.Finalization;

generic
   type Key_Type is private;
   type Object_Type is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
package Generic_Bounded_Map is
   pragma Preelaborate (Generic_Bounded_Map);
--
-- Overriding_Policy -- The map item replacing policy
--
   type Overriding_Policy is (Override_Least, Override_Greatest);
--
-- Map -- The map object
--
--    Size - The maximum number elements in the map
--
   type Map (Size : Positive) is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Add -- Add a new item to the map
--
--    Container - The set to be modified
--    Key       - Of the item to be added
--    Item      - The item to be added
--    Override  - Which item to override when the map is full
--  [ Index ]   - The index where the item was added
--
-- This procedure adds Item to the map.  The parameter Policy determines
-- which item to replace. It is ignored when the new item  is added to a
-- map's end.  In that case always  an item  from  the opposite  end  is
-- dropped.
--
-- Exceptions :
--
--    Constraint_Error - There  is already an item with the key equal to
--                       Key
--
   procedure Add
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type;
                Override  : Overriding_Policy
             );
   procedure Add
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type;
                Override  : Overriding_Policy;
                Index     : out Positive
             );
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
-- Lesser keys have lesser indices, so Get (Container, 1) gives the item
-- with the least key
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
-- The least key in the map has the index 1. The greatest key in the map
-- has the index Get_Size (Container).
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
--    The number of items in the map
--
   function Get_Size (Container : Map) return Natural;
--
-- Inf -- Find an item that is less than or equal to the key
--
--    Container - The tmap
--    Key       - To search for
--
-- Returns :
--
--    [+]  The index to the found item
--    [0]  The the key is less than any other key
--
   function Inf (Container : Map; Key : Key_Type) return Natural;
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
-- Is_Full -- Test if the map is full
--
--    Container - The map
--
-- Returns :
--
--    True if the map is full
--
   function Is_Full (Container : Map) return Boolean;
--
-- Is_In -- Test if an item in the map
--
--    Container - The set
--    Key       - To be searched for
--
-- Returns :
--
--    True if the item is in the map
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
--    Override  - Which item to override when the map is full
--  [ Index ]   - The index of the item added or replaced
--
-- This procedure adds Item to the map. The element is replaced if it is
-- already in the map.
--
   procedure Replace
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type;
                Override  : Overriding_Policy
             );
   procedure Replace
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type;
                Override  : Overriding_Policy;
                Index     : out Positive
             );
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
-- Sup -- Find an item that is greater than or equal to the key
--
--    Container - The tmap
--    Key       - To search for
--
-- Returns :
--
--    [+]  The index to the found item
--    [0]  The the key is greater than any other key
--
   function Sup (Container : Map; Key : Key_Type) return Natural;
--
-- Comparison -- Map with key
--
-- These function compare the range of keys in the map with a key.  When
-- the map is empty the result is True if the key is the first parameter
-- and False if the map is the first parameter.
--
   function "<"  (Container : Map; Key : Key_Type) return Boolean;
   function "<"  (Key : Key_Type; Container : Map) return Boolean;
   function "<=" (Container : Map; Key : Key_Type) return Boolean;
   function "<=" (Key : Key_Type; Container : Map) return Boolean;
   function ">=" (Container : Map; Key : Key_Type) return Boolean;
   function ">=" (Key : Key_Type; Container : Map) return Boolean;
   function ">"  (Container : Map; Key : Key_Type) return Boolean;
   function ">"  (Key : Key_Type; Container : Map) return Boolean;

private
   pragma Inline (Add);
   pragma Inline (Get_Size);
   pragma Inline (Is_Empty);
   pragma Inline (Is_Full);
   pragma Inline (Is_In);
   pragma Inline ("<", "<=", ">=", ">");

   type Token is record
      Key  : Key_Type;
      Item : Object_Type;
   end record;
   type Token_Array is array (Positive range <>) of Token;
   function Find
            (  Data  : Token_Array;
               Size  : Positive;
               First : Positive;
               Key   : Key_Type
            )  return Integer;
--
-- The  set  wraps pointer to the structure Data. When set is copied the
-- pointer  is duplicated and the Use_Count is increased. A deep copy is
-- made only if a destructive operation is applied.
--
   type Map (Size : Positive) is
      new Ada.Finalization.Limited_Controlled with
   record
      First : Positive := 1;
      Last  : Positive := 1;
      Empty : Boolean := True;
      Data  : Token_Array (1..Size);
   end record;
   function "mod" (Container : Map; Index : Positive) return Positive;
   pragma Inline ("mod");

end Generic_Bounded_Map;
