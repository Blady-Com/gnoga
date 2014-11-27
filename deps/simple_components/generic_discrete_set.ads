--                                                                    --
--  package Generic_Discrete_Set    Copyright (c)  Dmitry A. Kazakov  --
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
--  This  package defines the type Set of discrete elements, escpecially
--  when the set should be large, e.g. a set of all integer values.  The
--  implementation  uses ranges integers rather than individual numbers.
--  The package is generic:
--
--     Object_Type  - The type of the set elements
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
with Generic_Set;

generic
   type Object_Type is (<>);
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
package Generic_Discrete_Set is
   pragma Preelaborate (Generic_Discrete_Set);
   type Set is new Ada.Finalization.Controlled with private;
--
-- Add -- Add a new item to the set
--
--    Container       - The set to be modified
--    Item / From, To - Item or a range of items to add
--
-- This procedure adds the element Item to the set. The element is added
-- only if it is not already in the set.
--
-- Exceptions :
--
--    Constaint_Error - From > To
--
   procedure Add (Container : in out Set; Item : Object_Type);
   procedure Add (Container : in out Set; From, To : Object_Type);
--
-- Add -- Add items from one set to another
--
--    Container - The set to be modified
--    Items     - To be added
--
-- This procedure adds the elemenets from  the  set  Items  to  the  set
-- Container. An element is added only if it is not already in the set.
--
-- Exceptions :
--
--    Constaint_Error - Illegal range
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
-- Create -- Create a singleton set
--
--    Item / From, To - Item or a range of items to add
--
-- Returns :
--
--    The new set containing Item
--
-- Exceptions :
--
--    Constaint_Error - Illegal range
--
   function Create (Item : Object_Type) return Set;
   function Create (From, To : Object_Type) return Set;
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
-- Find -- Find an element or a range in the set
--
--    Container       - The set
--    Item / From, To - Item or a range of items to search for
--
-- When search is successful the result is positive.  Otherwise,  it  is
-- negative and indicates the position the  item  should  be  placed  if
-- inserted into the list.
--
-- Returns :
--
--    [+]  The index of the range containing the all items
--    [-]  The negated index of the position the item should be
--
-- Exceptions :
--
--    Constaint_Error - From..To is an empty or illegal range
--
   function Find (Container : Set; Item : Object_Type) return Integer;
   function Find (Container : Set; From, To : Object_Type)
      return Integer;
--
-- From -- Get the lower bound of a range of elements by its index
--
--    Container - The set
--    Index     - The index of the range
--
-- Returns :
--
--    The element
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function From (Container : Set; Index : Positive) return Object_Type;
--
-- Get -- Get a range of elements by its index
--
--    Container - The set
--    Index     - The index of the range
--    From      - The first element of the range
--    To        - The laset element of the range
--
-- Lesser elements have lesser numbers, so Get (Container, 1) gives  the
-- range of containing the smallest element of the set.
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Get
             (  Container : Set;
                Index     : Positive;
                From      : out Object_Type;
                To        : out Object_Type
             );
--
-- Get_Size -- Get the number of ranges in the set
--
--    Container - The set
--
-- Returns :
--
--    The number of element ranges
--
   function Get_Size (Container : Set) return Natural;
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
--    Container       - The set
--    Item / From, To - Item or a range to test
--
-- Returns :
--
--    True if the item or range is in the set
--
   function Is_In (Container : Set; Item : Object_Type)
      return Boolean;
   function Is_In (Container : Set; From, To : Object_Type)
      return Boolean;
--
-- Is_Not_In -- Test if a range not in the set
--
--    Container - The set
--    From, To  - A range to test
--
-- Returns :
--
--    True if no item from the range is in the set
--
   function Is_Not_In (Container : Set; From, To : Object_Type)
      return Boolean;
--
-- Range_Remove -- Remove a range of elements from the set by index
--
--    Container - The set
--    Index     - Of the item to be removed
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Range_Remove (Container : in out Set; Index : Positive);
--
-- Remove -- Remove element from the set
--
--    Container       - The set
--    Item / From, To - Item or a range to remove
--
-- If the items are not in the set, nothing happens.
--
   procedure Remove (Container : in out Set; Item : Object_Type);
   procedure Remove (Container : in out Set; From, To : Object_Type);
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
-- To -- Get the upper bound of a range of elements by its index
--
--    Container - The set
--    Index     - The index of the range
--
-- Returns :
--
--    The element
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function To (Container : Set; Index : Positive) return Object_Type;
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
-- not -- Complement set
--
--    Left - A set
--
-- Returns :
--
--    Complement set
--
   function "not" (Left : Set) return Set;
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
   type Range_Type is record
      From : Object_Type'Base;
      To   : Object_Type'Base;
   end record;
   function Intersect (Left, Right : Range_Type) return Boolean;
   function Less (Left, Right : Range_Type) return Boolean;

   pragma Inline (Create);
   pragma Inline (Find);
   pragma Inline (From);
   pragma Inline (Get);
   pragma Inline (Intersect);
   pragma Inline (Is_Empty);
   pragma Inline (Is_In);
   pragma Inline (Less);
   pragma Inline (Range_Remove);
   pragma Inline (To);

   pragma Assert (Object_Type'Base'Last /= Object_Type'Base'First);
   package Range_Sets is
      new Generic_Set
          (  Object_Type  => Range_Type,
             Minimal_Size => Minimal_Size,
             Increment    => Increment,
             "="          => Intersect,
             "<"          => Less,
             Null_Element =>
                (Object_Type'Base'Last, Object_Type'Base'First)
          );
   type Set is new Range_Sets.Set with null record;

end Generic_Discrete_Set;
