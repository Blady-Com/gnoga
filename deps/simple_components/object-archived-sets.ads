--                                                                    --
--  package Object.Archived.Sets    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2004       --
--                                                                    --
--                                Last revision :  15:03 28 Mar 2009  --
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

with Object.Handle.Generic_Set;

package Object.Archived.Sets is
--
-- Deposit_Set -- A set of objects
--
   type Deposit_Set is new Deposit_Container with private;
--
-- Add -- Implements Object.Archived...
--
-- The parameter Backward is ignored.
--
   procedure Add
             (  Container : in out Deposit_Set;
                Object    : Deposit_Ptr;
                Backward  : Boolean := False
             );
--
-- Erase -- Implements Object.Archived...
--
   procedure Erase (Container : in out Deposit_Set);
--
-- Get -- Implements Object.Archived...
--
   function Get
            (  Container : Deposit_Set;
               Index     : Positive
            )  return Deposit_Ptr;
--
-- Get_Size -- Implements Object.Archived...
--
   function Get_Size (Container : Deposit_Set) return Natural;
--
-- Is_Backward -- Implements Object.Archived...
--
   function Is_Backward
            (  Container : Deposit_Set;
               Object    : Deposit_Ptr
            )  return Boolean;
--
-- Is_In -- Implementation of Object.Archived...
--
   function Is_In
            (  Container : Deposit_Set;
               Object    : Deposit_Ptr
            )  return Boolean;
--
-- Remove -- Remove object from a set of objects
--
--    Set    - The set of objects
--    Object - To removed
--
-- If the object is not in the set, then nothing happens.
--
   procedure Remove
             (  Container : in out Deposit_Set;
                Object    : Deposit_Ptr
             );
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
   function "and" (Left, Right : Deposit_Set) return Deposit_Set;
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
   function "or" (Left, Right : Deposit_Set) return Deposit_Set;
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
   function "xor" (Left, Right : Deposit_Set) return Deposit_Set;
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
   function "=" (Left, Right : Deposit_Set) return Boolean;

private
   pragma Inline (Add);
   pragma Inline (Erase);
   pragma Inline (Get);
   pragma Inline (Get_Size);
   pragma Inline (Is_Backward);
   pragma Inline (Is_In);
   pragma Inline (Remove);
   pragma Inline ("and", "or", "xor", "=");
--
-- Deposit_Set -- Implementation
--
-- A sets of handles to objects is used for this. The Deposit_Set object
-- contains it as a component.
--
   package Deposit_Sets is new Handles.Generic_Set;
   type Deposit_Set is new Deposit_Container with record
      Set : Deposit_Sets.Set;
   end record;

end Object.Archived.Sets;
