--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Segmented_Stack                     Luebeck            --
--  Interface                                      Winter, 2004       --
--                                                                    --
--                                Last revision :  10:25 26 Dec 2009  --
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
--  This package provides an implementation of a stack  built  from  the
--  segments of same size. The number  of  segments  is  unlimited.  New
--  segments are allocated as necessary. The package generic  parameters
--  are:
--
--     Index_Type   - The index type
--     Object_Type  - The type of the stack items
--     Null_Element - To pad unused items
--     Segment_Size - The number of items in a segment
--     Minimal_Size - Minimal additionally allocated segment slots
--     Increment    - In segment slots by stack growth
--
--  The   package   instantiates   Generic_Stack    under    the    name
--  Segmented_Stack.  This  instance can be then used as a stack package
--  (see  Generic_Stack). The parameters Index_Type and Null_Element are
--  used  for  instantiation. The parameter Increment controls the stack
--  growth. When there is  no  free  space  the  stack  is  enlarged  by
--  allocation a new segment. A reference to the segment is placed in an
--  array  of  the  segment  slots. That array is enlarged by allocating
--  Size * Increment / 100 new slots. Here Size is the current number of
--  slots.  The  allocated  amount  of  slots  cannot  be  less than the
--  parameter Minimal_Size specifies. So it will  be  the  initial  size
--  after the first segment gets allocated.
--
--  The package can be used as follows:
--
--     package Float_Stack is
--        new Generic_Segmented_Stack (Integer, Float);
--     use Float_Stack.Segmented_Stack;
--     ...
--     LIFO : Stack;
--
with Generic_Stack;
with Generic_Unbounded_Ptr_Array;

generic
   type Index_Type is (<>);
   type Object_Type is private;
   Null_Element : Object_Type;
   Segment_Size : Positive := 128;
   Minimal_Size : Positive := 64;
   Increment    : Natural  := 50;
package Generic_Segmented_Stack is
   type Segment is array (0..Segment_Size - 1) of Object_Type;
   type Segment_Ptr is access Segment;
   type Segment_Ptr_Array is array (Natural range <>) of Segment_Ptr;

   package Segment_Arrays is
      new Generic_Unbounded_Ptr_Array
          (  Index_Type            => Natural,
             Object_Type           => Segment,
             Object_Ptr_Type       => Segment_Ptr,
             Object_Ptr_Array_Type => Segment_Ptr_Array,
             Minimal_Size          => Minimal_Size,
             Increment             => Increment
          );
   function Get
            (  Container : Segment_Arrays.Unbounded_Ptr_Array;
               Index     : Index_Type
            )  return Object_Type;

   procedure Put
             (  Container : in out Segment_Arrays.Unbounded_Ptr_Array;
                Index     : Index_Type;
                Element   : Object_Type
             );
--
-- Segmented_Stack -- The segmented stack package (an instance to use)
--
   package Segmented_Stack is
      new Generic_Stack
          (  Index_Type   => Index_Type,
             Object_Type  => Object_Type,
             Array_Type   => Segment_Arrays.Unbounded_Ptr_Array,
             Null_Element => Null_Element
          );
private
   pragma Inline (Get);

end Generic_Segmented_Stack;
