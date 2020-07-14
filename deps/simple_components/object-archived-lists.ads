--                                                                    --
--  package Object.Archived.Lists   Copyright (c)  Dmitry A. Kazakov  --
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
--
--  This package implements a list object container. All objects in  the
--  list are enumerated by from 1.  The  index  is  Positive.  The  same
--  object can occupy several places in the list. The container  can  be
--  stored as a set of objects, where objects do not repeat, followed by
--  a list of values identifying the objects in the set.
--
with Generic_Map;
with Object.Handle.Generic_Unbounded_Array;

package Object.Archived.Lists is
--
-- Deposit_List -- An ordered list of objects
--
   type Deposit_List is new Deposit_Container with private;
--
-- Add -- Implements Object.Archived...
--
   procedure Add
             (  Container : in out Deposit_List;
                Object    : Deposit_Ptr;
                Backward  : Boolean := False
             );
--
-- Erase -- Implements Object.Archived...
--
   procedure Erase (Container : in out Deposit_List);
--
-- Get -- Implements Object.Archived...
--
   function Get
            (  Container : Deposit_List;
               Index     : Positive
            )  return Deposit_Ptr;
--
-- Get_Size -- Implements Object.Archived...
--
   function Get_Size (Container : Deposit_List) return Natural;
--
-- Get_Total -- The number of distinct objects
--
--    Container - The list
--
-- Returns :
--
--    The number of distinct objects in Container
--
   function Get_Total (Container : Deposit_List) return Natural;
--
-- Is_Backward -- Implements Object.Archived...
--
   function Is_Backward
            (  Container : Deposit_List;
               Object    : Deposit_Ptr
            )  return Boolean;
--
-- Is_First -- Appearance of an object in the list
--
--    Container - The list
--    Index     - Of the object (same as in Get)
--
-- This function returns True if Index is the least index of the  object
-- Get (Container, Index) would return.
--
-- Returns :
--
--    True if Index is the first appearance of the object it specifies
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   function Is_First
            (  Container : Deposit_List;
               Index     : Positive
            )  return Boolean;
--
-- Is_In -- Implementation of Object.Archived...
--
   function Is_In
            (  Container : Deposit_List;
               Object    : Deposit_Ptr
            )  return Boolean;

private
   pragma Inline (Is_In);
--
-- Deposit_List -- Implementation
--
-- An  object  of Deposit_List consists of an unbounded array of handles
-- to the contained objects and a map object pointer to  Deposit_Key  to
-- identify  objects  in  the  list.  The implementation is derived from
-- Entity to have handles. A Deposit_List encapsulates such a handle  to
-- provide an efficient assignment.
--
   package Object_Arrays is   -- Unbounded array of object handles
      new Handles.Generic_Unbounded_Array
          (  Index_Type  => Positive,
             Handle_Type => Handles.Handle
          );
   use Object_Arrays;

   function "<" (Left, Right : Deposit_Ptr) return Boolean;
   package Object_Map is      -- Object to key map
      new Generic_Map
          (  Key_Type    => Deposit_Ptr,
             Object_Type => Integer
          );
   use Object_Map;

   type Dependency_List is new Entity with record
      Size       : Natural := 0;
      Links      : Unbounded_Array;
      References : Map;
   end record;
   type Dependency_List_Ptr is access Dependency_List'Class;

   package List_Handles is    -- Handles to list implementations
      new Handle
          (  Object_Type     => Dependency_List,
             Object_Type_Ptr => Dependency_List_Ptr
          );
   use List_Handles;

   type Deposit_List is new Deposit_Container with record
      Handle : List_Handles.Handle;
   end record;

end Object.Archived.Lists;
