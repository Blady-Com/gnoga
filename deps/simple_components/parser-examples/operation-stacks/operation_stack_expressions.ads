--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Operation_Stack_Expressions                 Luebeck            --
--  Interface                                      Winter, 2004       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

with Generic_Segmented_Stack;
with Parsers.Generic_Operation.Segmented_Stack;

package Operation_Stack_Expressions is
   --
   -- Integer_Stack -- Stacks of integers to keep arguments
   --
   package Integer_Stack is
      new Generic_Segmented_Stack
          (  Index_Type   => Integer,
             Object_Type  => Integer,
             Null_Element => 0
          ); 
   --
   -- Operations -- The set of operations
   --
   type Operations is (Add, Mul, Inc, Left_Bracket, Right_Bracket);
   function "and" (Left, Right : Operations) return Boolean;
   function Is_Commutative (Left, Right : Operations) return Boolean;
   function Is_Inverse (Operation : Operations) return Boolean;
   function Group_Inverse (Operation : Operations) return Operations;
   --
   -- Priorities -- The operation priorities
   --
   type Priorities is range 1..10;
   --
   -- Raw_Descriptors -- The raw operation stack descriptors
   --
   package Raw_Descriptors is
      new Parsers.Generic_Operation (Operations, Priorities);
   --
   -- Descriptor_Stacks -- Operation stack based on raw descriptors
   --
   package Descriptor_Stacks is new Raw_Descriptors.Segmented_Stack;
   --
   -- Use the package of operation stacks deployed there
   --
   use Descriptor_Stacks.Operation;
   --
   -- Expression_Stack -- Derived  from  abstract  operation  stack   to
   --                     provide implementation of operation calls. 
   --
   type Expression_Stack is new Stack with record
      Data : Integer_Stack.Segmented_Stack.Stack;
   end record;
   --
   -- Call -- Overrides to implement operators
   --
   procedure Call
             (  Stack     : in out Expression_Stack;
                Operation : Operations;
                Count     : Natural
             );
   --
   -- Enclose -- Overrides to implement brackets
   --
   procedure Enclose
             (  Stack : in out Expression_Stack;
                Left  : Operations;
                Right : Operations;
                Count : Natural
             );
end Operation_Stack_Expressions;
