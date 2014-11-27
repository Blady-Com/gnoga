--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Operation_Stack                        Luebeck            --
--  Implementation                                 Winter, 2004       --
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

with Operation_Stack_Expressions;  use Operation_Stack_Expressions;

procedure Test_Operation_Stack is
   use Operation_Stack_Expressions.Raw_Descriptors;
   use Operation_Stack_Expressions.Descriptor_Stacks.Operation;
   use Operation_Stack_Expressions.Integer_Stack.Segmented_Stack;

   Expression : Expression_Stack;
begin
   -- 1 + (2 + 3 + 4 * 5)++ + 6 * 7 + 8++
   Push_Start (Expression);
   Push (Expression.Data, 1);                      -- 1
   Push_Binary (Expression, Add, 5, 6);            -- +
   Push_Left_Bracket (Expression, Left_Bracket);   -- (
   Push (Expression.Data, 2);                      -- 2
   Push_Binary (Expression, Add, 5, 6);            -- +
   Push (Expression.Data, 3);                      -- 3
   Push_Binary (Expression, Add, 5, 6);            -- +
   Push (Expression.Data, 4);                      -- 4
   Push_Binary (Expression, Mul, 7, 8);            -- *
   Push (Expression.Data, 5);                      -- 5
   Push_Right_Bracket (Expression, Right_Bracket); -- )
   Push_Postfix (Expression, Inc, 9, 10);          -- ++
   Push_Binary (Expression, Add, 5, 6);            -- +
   Push (Expression.Data, 6);                      -- 6
   Push_Binary (Expression, Mul, 7, 8);            -- *
   Push (Expression.Data, 7);                      -- 7
   Push_Binary (Expression, Add, 5, 6);            -- +
   Push (Expression.Data, 8);                      -- 8
   Push_Postfix (Expression, Inc, 9, 10);          -- ++
   Push_End (Expression);
   if Top (Expression.Data) /= 78 then
      raise Constraint_Error;
   end if;
end Test_Operation_Stack;
