--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Object.Handle.                              Luebeck            --
--        Generic_Handle_Set                       Spring, 2006       --
--  Interface                                                         --
--                                Last revision :  18:58 21 Mar 2016  --
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
--  This package is similar to the package Object.Handle.Generic_Set but
--  allows specification of any type derived from Object.Handle  as  the
--  type  of  handles.  An instance of the type is a set of objects. One
--  can add and remove objects to  the  set  using  either  pointers  or
--  handles to them. Elements of the  set  can  be  accessed  using  the
--  positive index. When an object is in the set it cannot be finalized,
--  so  its  placement there is counted as a reference. The package is a
--  generic   child   of   Object.Handle   with  the  following  generic
--  parameters:
--
--     Handle_Type        - The type of handles
--     Object_Ptr_Type    - Referential type used in the container
--     Null_Object_Ptr    - A value corresponding to no object
--     Ptr                - Handle to Object_Ptr_Type conversion
--     Ref                - Object_Ptr_Type to handle conversion
--     <                  - Object_Ptr_Type comparison
--     =                  - Object_Ptr_Type comparison
--     To_Object_Type_Ptr - To object pointer conversion
--     Minimal_Size       - Minimal additionally allocated size
--     Increment          - By which the set is enlarged if necessary
--
--  The  parameter Object_Ptr_Type is the type used to represent objects
--  in the container. Usually  it  is  a  record  containing  an  object
--  pointer (Object_Type_Ptr) and some additional data.  The  operations
--  defined on this type are Ptr, Ref, < and =. The parameter  Increment
--  controls  set  size  growth.  When there is no free space in the set
--  then  it  is  enlarged  by  Size * Increment / 100. Here Size is the
--  current  set size. The allocated amount of items cannot be less than
--  the parameter Minimal_Size specifies. So it will be the initial  set
--  size after the first element is put in.
--
with Ada.Finalization;

generic
   type Handle_Type is new Handle with private;
   type Object_Ptr_Type is private;
   Null_Object_Ptr : Object_Ptr_Type;
   with function Ptr (Object : Handle_Type)
      return Object_Ptr_Type is <>;
   with function Ref (Object : Object_Ptr_Type)
      return Handle_Type is <>;
   with function To_Object_Ptr (Object : Object_Ptr_Type)
      return Object_Type_Ptr is <>;
   with function "<" (Left, Right : Object_Ptr_Type)
      return Boolean is <>;
   with function "=" (Left, Right : Object_Ptr_Type)
      return Boolean is <>;
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
package Object.Handle.Generic_Handle_Set is
   type Set is new Ada.Finalization.Controlled with private;
--
-- Add -- Add a new item to the set
--
--    Container - The set to be modified
--    Item      - To be added (a pointer or handle to object)
--
-- This procedure adds the object specified by Item to the  set.  It  is
-- added only if it is not already in the set.
--
   procedure Add (Container : in out Set; Item : Object_Ptr_Type);
   procedure Add (Container : in out Set; Item : Handle_Type);
--
-- Add -- Add items from one set to another
--
--    Container - The set to be modified
--    Items     - To be added
--
-- This procedure adds the  objects  from  the  set  Items  to  the  set
-- Container. An object is added only if it is not already in the set.
--
   procedure Add (Container : in out Set; Items : Set);
--
-- Adjust -- Assignment
--
--    Container - The new set to be adjusted
--
-- Assignment increases the use count. No deep copy made.
--
   procedure Adjust (Container : in out Set);
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
--    Item      - To searched for (a pointer or handle to the object)
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
   function Find (Container : Set; Item : Object_Ptr_Type)
      return Integer;
   function Find (Container : Set; Item : Handle_Type)
      return Integer;
--
-- Get -- Get an object by its index
--
--    Container - The set
--    Index     - The number of the object to get
--
-- Returns :
--
--    A pointer to the object (see also Ref)
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Get (Container : Set; Index : Positive)
      return Object_Ptr_Type;
--
-- Get_Size -- Get the number of elements in the set
--
--    Container - The set
--
-- Returns :
--
--    The number of objects in the set
--
   function Get_Size (Container : Set) return Natural;
--
-- Is_Empty -- Test if the set is empty
--
--    Container - The set
--
-- Returns :
--
--    True if the set contains no objects
--
   function Is_Empty (Container : Set) return Boolean;
--
-- Is_In -- Test if an item in the set
--
--    Container - The set
--    Item      - To search for (a pointer or handle to the object)
--
-- Returns :
--
--    True if the object is in the set
--
   function Is_In (Container : Set; Item : Object_Ptr_Type)
      return Boolean;
   function Is_In (Container : Set; Item : Handle_Type)
      return Boolean;
--
-- Ref -- Get an object by its index
--
--    Container - The set
--    Index     - The number of the object to get
--
-- Returns :
--
--    A handle to the object
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Ref (Container : Set; Index : Positive) return Handle_Type;
--
-- Remove -- Remove element from the set
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
-- Remove -- Remove element from the set
--
--    Container - The set
--    Item      - To be removed (a pointer or handle to the object)
--
-- If the object is not in the set, nothing happens.
--
   procedure Remove (Container : in out Set; Item : Object_Ptr_Type);
   procedure Remove (Container : in out Set; Item : Handle_Type);
--
-- Remove -- Remove elements of one set from another set
--
--    Container - The set
--    Items     - The set of items to be removed
--
-- If  an object from the set Items is not in the set Container, nothing
-- happens. Otherwise it is removed from Container.
--
   procedure Remove (Container : in out Set; Items : Set);
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
   pragma Inline (Is_Empty);
   pragma Inline (Is_In);

   type Object_Array is array (Positive range <>) of Object_Ptr_Type;
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

end Object.Handle.Generic_Handle_Set;
