--                                                                    --
--  package Generic_Set             Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  20:01 04 Apr 2016  --
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
--  This  package defines the type Set. An instance of the type is a set
--  of items. One can add and remove elements to the  set.  Elements  of
--  the  set can be accessed using the positive index. They are ordered,
--  so the set implementation may use binary search.  There  is  a  null
--  element, which is never included into the set and is  used  to  mark
--  free memory slots. The package is  generic.  It  has  the  following
--  generic parameters:
--
--     Object_Type  - The type of the set elements
--     Null_Element - The element that never included into a set
--     Minimal_Size - Minimal additionally allocated size
--     Increment    - By which the set is enlarged if necessary
--     <            - Relational operator on elements
--     =            - Relational operator on elements
--
--  The  parameter  Increment controls set size growth. When there is no
--  free space in the set then it is enlarged by Size * Increment / 100.
--  Here Size is the current set size. The  allocated  amount  of  items
--  cannot be less than the parameter Minimal_Size specifies. So it will
--  be the initial set size after the first element is put in.
--
with Ada.Finalization;

generic
   type Object_Type is private;
   Null_Element : Object_Type;
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
   with function "<" (Left, Right : Object_Type) return Boolean is <>;
   with function "=" (Left, Right : Object_Type) return Boolean is <>;
package Generic_Set is
   pragma Preelaborate (Generic_Set);
   type Set is new Ada.Finalization.Controlled with private;
--
-- Add -- Add a new item to the set
--
--    Container - The set to be modified
--    Item      - To be added
--
-- This procedure adds the element Item to the set. The element is added
-- only if it is not already in the set.
--
   procedure Add (Container : in out Set; Item : Object_Type);
--
-- Add -- Add items from one set to another
--
--    Container - The set to be modified
--    Items     - To be added
--
-- This procedure adds the elemenets from  the  set  Items  to  the  set
-- Container. An element is added only if it is not already in the set.
--
   procedure Add (Container : in out Set; Items : Set);
--
-- Create -- Create a new empty set
--
-- Returns :
--
--    The new set
--
   function Create return Set;
--
-- Erase -- Remove all elements from the set
--
--    Container - The set
--
   procedure Erase (Container : in out Set);
--
-- Finalize -- Destructor
--
--    Container - The set
--
-- When  finalized  the  use  count  is  decreased.  Only  if it becomes
-- negative, the set is finally removed.
--
   procedure Finalize (Container : in out Set);
--
-- Find -- Find an item in the set
--
--    Container - The set
--    Item      - To be searched for
--
-- When search is successful the result is positive.  Otherwise,  it  is
-- negative and indicates the position the  item  should  be  placed  if
-- inserted into the list.
--
-- Returns :
--
--    [+]  The index of the item
--    [-]  The negated index of the position the item should be
--
   function Find (Container : Set; Item : Object_Type)
      return Integer;
--
-- Get -- Get an element by its index
--
--    Container - The set
--    Index     - The number of the element to get
--
-- Lesser elements have lesser numbers, so Get (Container, 1) gives  the
-- smallest element of the set.
--
-- Returns :
--
--    The element
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Get (Container : Set; Index : Positive)
      return Object_Type;
--
-- Get_Size -- Get the number of elements in the set
--
--    Container - The set
--
-- Returns :
--
--    The number of elements
--
   function Get_Size (Container : Set) return Natural;
--
-- Insert -- Add a new item to the set
--
--    Container - The set to be modified
--    Item      - To be added and the result
--
-- This procedure adds the element Item to the set. The element is added
-- only if it is not already in the  set.  Otherwise  the  element  from
-- Container  (found  to  be  an equivalent of Item) is returned through
-- Item.
--
   procedure Insert (Container : in out Set; Item : in out Object_Type);
--
-- Insert -- Add a new item to the set
--
--    Container - The set to be modified
--    Item      - To be added and the result
--    Inserted  - The outcome of the operation
--
-- This procedure adds the element Item to the set. The element is added
-- only if it is not already in the set. Inserted is set to True if Item
-- is added to the set. Otherwise it is False. It  is  also  false  when
-- Item is Null_Element.
--
   procedure Insert
             (  Container : in out Set;
                Item      : Object_Type;
                Inserted  : out Boolean
             );
--
-- Is_Empty -- Test if the set is empty
--
--    Container - The set
--
-- Returns :
--
--    True if the set contains no items
--
   function Is_Empty (Container : Set) return Boolean;
--
-- Is_In -- Test if an item in the set
--
--    Container - The set
--    Item      - To be searched for
--
-- Returns :
--
--    True if the item is in the set
--
   function Is_In (Container : Set; Item : Object_Type)
      return Boolean;
--
-- Remove -- Remove element from the set
--
--    Container - The set
--    Item      - To be removed
--
-- If the item is not in the set, nothing happens.
--
   procedure Remove (Container : in out Set; Item : Object_Type);
--
-- Remove -- Remove element from the set
--
--    Container - The set
--    Item      - To be removed
--    Removed   - The outcome of the operation
--
-- If the item is not in the set, nothing happens. The parameter Removed
-- is  set  to  True  when  Item  was in the set. Otherwise it is set to
-- False.
--
   procedure Remove
             (  Container : in out Set;
                Item      : Object_Type;
                Removed   : out Boolean
             );
--
-- Remove -- Remove elements of one set from another set
--
--    Container - The set
--    Items     - The set of items to be removed
--
-- If  an  item  from the set Items is not in the set Container, nothing
-- happens. Otherwise it is removed from Container.
--
   procedure Remove (Container : in out Set; Items : Set);
--
-- Remove -- Remove element from the set by index
--
--    Container - The set
--    Index     - Of the item to be removed
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Remove (Container : in out Set; Index : Positive);
--
-- Replace -- Replace an item in the set
--
--    Container   - The set to be modified
--    Item        - To be added / replaced
--
-- This procedure  adds the  element  Item to the set.  The  element  is
-- replaced if it is already in the set.
--
   procedure Replace (Container : in out Set; Item : Object_Type);
--
-- Replace -- Replace an item in the set
--
--    Container - The set to be modified
--    Item      - To be added / replaced
--    Condition - Predicate checking exchange
--    Updated   - True if the container was changed
--
-- This  procedure  adds the  element Item to the set.  The  element  is
-- replaced if Condition returns True.
--
   type Exchange_Condition is
      access function (New_Element, Old_Element : Object_Type)
         return Boolean;
   procedure Replace
             (  Container : in out Set;
                Item      : Object_Type;
                Condition : Exchange_Condition;
                Updated   : out Boolean
             );
--
-- Replace -- Replace items from one set in another
--
--    Container - The set to be modified
--    Items     - To be added
--
-- This  procedure  adds  the  elements  from  the  set Items to the set
-- Container. An element is replaced if it is already in the set.
--
   procedure Replace (Container : in out Set; Items : Set);
--
-- and -- Intersection
--
--    Left  - A set
--    Right - A set
--
-- Returns :
--
--    Intersection of two sets
--
   function "and" (Left, Right : Set) return Set;
--
-- or -- Union
--
--    Left  - A set
--    Right - A set
--
-- Returns :
--
--    Union of two sets
--
   function "or" (Left, Right : Set) return Set;
--
-- xor -- Difference
--
--    Left  - A set
--    Right - A set
--
-- The result is the set of elements present in only one of two sets.
--
-- Returns :
--
--    Difference of two sets
--
   function "xor" (Left, Right : Set) return Set;
--
-- - -- Remove elements of one set from another
--
--    Left  - A set
--    Right - A set
--
-- Returns :
--
--    Left without elements of Right
--
   function "-" (Left, Right : Set) return Set;
--
-- = -- Comparison
--
--    Left  - A set
--    Right - A set
--
-- Returns :
--
--    True if both sets contain same items
--
   function "=" (Left, Right : Set) return Boolean;

private
   pragma Inline (Add);
   pragma Inline (Create);
   pragma Inline (Insert);
   pragma Inline (Is_Empty);
   pragma Inline (Is_In);
   pragma Inline (Remove);
   pragma Inline (Replace);

   type Object_Array is array (Positive range <>) of Object_Type;
   type Object_Array_Ptr is access Object_Array;
   type Data is record
      Size      : Natural := 0;
      Use_Count : Natural := 0;
      Vector    : Object_Array_Ptr := null;
   end record;
   type Data_Ptr is access Data;
--
-- The  set  wraps pointer to the structure Data. When set is copied the
-- pointer  is duplicated and the Use_Count is increased. A deep copy is
-- made only if a destructive operation is applied.
--
   type Set is new Ada.Finalization.Controlled with record
      Object : Data_Ptr := null;
   end record;
--
-- Adjust -- Assignment
--
--    Container - The new set to be adjusted
--
-- Assignment increases the use count. No deep copy made.
--
   procedure Adjust (Container : in out Set);
--
-- Clone -- Copy a set if it is currently shared
--
--    Container - The set
--
-- This procedure is called if a destructive operation has to be applied
-- to  the  set.  If  the use count is greater than zero it makes a deep
-- copy  and  decreases the  use  count.  The  copy  will be used by the
-- operation.
--
   procedure Clone (Container : in out Set);

end Generic_Set;
