--                                                                    --
--  package Generic_Stack           Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2003       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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
--  This package defines a generic type Stack built  upon  an  unbounded
--  array. The package generic parameters are: 
--
--     Index_Type   - The array index type
--     Object_Type  - The element type
--     Array_Type   - The array type using Index_Type and Object_Type
--     Null_Element - To pad unused array elements
--     Get          - The function to get an array element
--     Put          - The procedure to store an array element
--
--  Any array type can be used for instantiation. For example, it can be
--  Unbounded_Array  from  an  instance  of Generic_Unbounded_Array. The
--  procedure Get may raise Constraint_Error in case any index for which
--  no Put was made. It is warranted that Put never  called  with  Index
--  incremented more than by one to its greatest at the moment value.  
--
with Ada.Finalization;

generic
   type Index_Type is (<>);
   type Object_Type is private;
   type Array_Type is limited private;
   Null_Element : Object_Type;
   with function Get
                 (  Container : Array_Type;
                    Index     : Index_Type
                 )  return Object_Type is <>;
   with procedure Put
                  (  Container : in out Array_Type;
                     Index     : Index_Type;
                     Element   : Object_Type
                  )  is <>;
package Generic_Stack is
   type Stack is new Ada.Finalization.Limited_Controlled with private;
--
-- Erase -- Pop all items
--
--    Container - The stack
--
-- The stack is emptied.
--
   procedure Erase (Container : in out Stack);
   pragma Inline (Erase);
--
-- Get -- Get stack item by index
--
--    Container - The stack
--    Index     - The item index
--
-- This  function  can  be  used  to  access  stack  items randomly. The
-- parameter  Index  specifies  the  item.  An item in the stack has the
-- index returned by Mark just before pushing the item onto the stack.
--
-- Returns :
--
--    The stack item
--
-- Exceptions :
--
--    Constraint_Error - Wrong index (out of stack)
--
   function Get
            (  Container : Stack;
               Index     : Index_Type
            )  return Object_Type;
   pragma Inline (Get);
--
-- Is_Empty -- Check if the stack contains items
--
--    Container - The stack
--
-- Returns :
--
--    True if the stack is empty
--
   function Is_Empty (Container : Stack) return Boolean;
   pragma Inline (Is_Empty);
--
-- Mark -- Get the stack top index
--
--    Container - The stack
--
-- The value returned by this function can be used in Release to pop all
-- the items pushed in between. 
--
-- Returns :
--
--    The index of the stack top
--
   function Mark (Container : Stack) return Index_Type;
   pragma Inline (Mark);
--
-- Pop -- Items
--
--    Container - The stack
--    Count     - The number of items to pop
--
-- If the stack does not contain enough items, it is emptied.
--
   procedure Pop (Container : in out Stack; Count : Natural := 1);
   pragma Inline (Pop);
--
-- Push -- Elements
--
--    Container - The stack
--    Item      - To push
--
   procedure Push (Container : in out Stack; Item : Object_Type);
   pragma Inline (Push);
--
-- Put -- Replace stack item by index
--
--    Container - The stack
--    Index     - The item index
--    Item      - The new item value
--
-- This procedure replaces the stack item specified by Index with Item. 
--
-- Returns :
--
--    The stack item
--
-- Exceptions :
--
--    Constraint_Error - Wrong index (out of stack)
--
   procedure Put
             (  Container : in out Stack;
                Index     : Index_Type;
                Element   : Object_Type
             );
   pragma Inline (Put);
--
-- Release -- Item to the given mark
--
--    Container - The stack
--    Mark      - Obtained from the function Mark
--
-- Nothing happens if the stack was already popped below the mark. 
--
   procedure Release (Container : in out Stack; Mark : Index_Type);
   pragma Inline (Release);
--
-- Top -- Get the topmost item
--
--    Container - The stack
--
-- Returns :
--
--    The topmost item
--
-- Exceptions :
--
--    Constraint_Error - The stack is empty
--
   function Top (Container : Stack) return Object_Type;
   pragma Inline (Top);

private
   type Stack is new Ada.Finalization.Limited_Controlled with record
      Top  : Index_Type := Index_Type'First;
      Data : Array_Type;
   end record;
end Generic_Stack;
