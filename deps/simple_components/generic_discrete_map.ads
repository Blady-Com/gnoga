--                                                                    --
--  package Generic_Discrete_Map    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  14:26 27 May 2012  --
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
--  This  package defines the type Map of discrete keys.  The package is
--  generic:
--
--     Key_Type     - The type of the key used to order/access the map
--     Object_Type  - The type of the items in the map
--     Minimal_Size - Minimal additionally allocated size
--     Increment    - By which the set is enlarged if necessary
--
--  The  parameter  Increment controls set size growth. When there is no
--  free space in the set then it is enlarged by Size * Increment / 100.
--  Here Size is the current set size. The  allocated  amount  of  items
--  cannot be less than the parameter Minimal_Size specifies. So it will
--  be the initial set size after the first element is put in.
--
with Ada.Finalization;
with Generic_Map;

generic
   type Key_Type is (<>);
   type Object_Type is private;
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
package Generic_Discrete_Map is
   pragma Preelaborate (Generic_Discrete_Map);
   type Map is new Ada.Finalization.Controlled with private;
--
-- Add -- Add a new item to the set
--
--    Container      - The set to be modified
--    Key / From, To - Key or range of keys for the item to be added
--    Item           - The item to be added
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
   procedure Add
             (  Container : in out Map;
                From, To  : Key_Type;
                Item      : Object_Type
             );
--
-- Add -- Add items from one set to another
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
-- Create -- Create a singleton set
--
--    Key / From, To - Key or a range of keys to add
--    Item            - The item to be added
--
-- Returns :
--
--    The new map containing Item
--
-- Exceptions :
--
--    Constaint_Error - From > To
--
   function Create (Key      : Key_Type; Item : Object_Type) return Map;
   function Create (From, To : Key_Type; Item : Object_Type) return Map;
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
-- Find -- Find an element or a range in the set
--
--    Container      - The set
--    Key / From, To - Key or a range of keys to search for
--
-- When search is successful the result is positive.  Otherwise,  it  is
-- negative and indicates the position the  item  should  be  placed  if
-- inserted into the list.
--
-- Returns :
--
--    [+]  The index of the range containing the all keys
--    [-]  The negated index of the position the item should be
--
-- Exceptions :
--
--    Constaint_Error - From..To is an empty or invalid interval
--
   function Find (Container : Map; Key      : Key_Type) return Integer;
   function Find (Container : Map; From, To : Key_Type) return Integer;
--
-- From -- Get the lower bound of a range of keys by its index
--
--    Container - The set
--    Index     - The index of the range
--
-- Returns :
--
--    The key
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function From (Container : Map; Index : Positive) return Key_Type;
--
-- Get -- Get an item by its index
--
--    Container - The map
--    Key       - The key of the item to get
--
-- Returns :
--
--    The item
--
-- Exceptions :
--
--    Contraint_Error - Wrong key
--
   function Get (Container : Map; Key : Key_Type) return Object_Type;
--
-- Get_Key -- Get a range of keys by its index
--
--    Container - The set
--    Index     - The index of the range
--    From      - The first element of the range
--    To        - The laset element of the range
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Get_Key
             (  Container : Map;
                Index     : Positive;
                From      : out Key_Type;
                To        : out Key_Type
             );
--
-- Get_Size -- Get the number of ranges in the map
--
--    Container - The map
--
-- Returns :
--
--    The number of ranges
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
-- Is_In -- Test if an item in the set
--
--    Container      - The set
--    Key / From, To - Key or a range of keys to test
--
-- Returns :
--
--    True if the item or range is in the set
--
   function Is_In (Container : Map; Key      : Key_Type) return Boolean;
   function Is_In (Container : Map; From, To : Key_Type) return Boolean;
--
-- Is_Not_In -- Test if a range of keys not in the map
--
--    Container - The set
--    From, To  - A range to test
--
-- Returns :
--
--    True if no item from the range is in the set
--
   function Is_Not_In (Container : Map; From, To : Key_Type)
      return Boolean;
--
-- Range_Get -- Get an item by its range index
--
--    Container - The map
--    Index     - The number of the item's range to get
--
-- Returns :
--
--    The item
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Range_Get (Container : Map; Index : Positive)
      return Object_Type;
--
-- Range_Remove -- Remove a range of keys from the map by its index
--
--    Container - The map
--    Index     - Of the range to remove
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Range_Remove (Container : in out Map; Index : Positive);
--
-- Range_Replace -- Replace a range of keys in the map by its index
--
--    Container - The map to be modified
--    Index     - Of the range to be replaced
--    Item      - To be replaced
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Range_Replace
             (  Container : in out Map;
                Index     : Positive;
                Item      : Object_Type
             );
--
-- Remove -- Remove element from the set
--
--    Container       - The set
--    Item / From, To - Key or a range of keys to remove
--
-- If the items are not in the set, nothing happens.
--
   procedure Remove (Container : in out Map; Key      : Key_Type);
   procedure Remove (Container : in out Map; From, To : Key_Type);
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
-- Replace -- Replace an item in the map
--
--    Container      - The map to be modified
--    Key / From, To - Key or a range of keys to add or replace
--    Item           - To be added / replaced
--
-- This procedure adds Item to the set. The element is replaced if it is
-- already in the map.
--
-- Exceptions :
--
--   Constraint_Error - From > To
--
   procedure Replace
             (  Container : in out Map;
                Key       : Key_Type;
                Item      : Object_Type
             );
   procedure Replace
             (  Container : in out Map;
                From, To  : Key_Type;
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
-- To -- Get the upper bound of a range of keys by its index
--
--    Container - The set
--    Index     - The index of the range
--
-- Returns :
--
--    The key
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function To (Container : Map; Index : Positive) return Key_Type;
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
   type Range_Type is record
      From : Key_Type'Base;
      To   : Key_Type'Base;
   end record;
   function Intersect (Left, Right : Range_Type) return Boolean;
   function Less (Left, Right : Range_Type) return Boolean;

   pragma Inline (Create);
   pragma Inline (Find);
   pragma Inline (From);
   pragma Inline (Get);
   pragma Inline (Get_Key);
   pragma Inline (Get_Size);
   pragma Inline (Intersect);
   pragma Inline (Is_Empty);
   pragma Inline (Is_In);
   pragma Inline (Less);
   pragma Inline (Range_Get);
   pragma Inline (Range_Remove);
   pragma Inline (Range_Replace);
   pragma Inline (To);

   pragma Assert (Key_Type'Base'Last /= Key_Type'Base'First);
   package Range_Maps is
      new Generic_Map
          (  Key_Type     => Range_Type,
             Object_Type  => Object_Type,
             Minimal_Size => Minimal_Size,
             Increment    => Increment,
             "="          => Intersect,
             "<"          => Less
          );
   type Map is new Range_Maps.Map with null record;

end Generic_Discrete_Map;
