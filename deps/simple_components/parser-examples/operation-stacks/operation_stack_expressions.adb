--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Operation_Stack_Expressions                 Luebeck            --
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

package body Operation_Stack_Expressions is
   use Integer_Stack.Segmented_Stack;

   function "and" (Left, Right : Operations) return Boolean is
   begin
      return True;
   end "and";

   function Is_Commutative (Left, Right : Operations) return Boolean is
   begin
      return False;
   end Is_Commutative;

   function Is_Inverse (Operation : Operations) return Boolean is
   begin
      return False;
   end Is_Inverse;

   function Group_Inverse (Operation : Operations) return Operations is
   begin
      raise Program_Error;      
      return Inc;
   end Group_Inverse;

   procedure Call
             (  Stack     : in out Expression_Stack;
                Operation : Operations;
                Count     : Natural
             )  is
      L, R : Integer;
   begin
      if Count > 0 then
         R := Top (Stack.Data);
         Pop (Stack.Data);
      end if;
      if Count > 1 then
         L := Top (Stack.Data);
         Pop (Stack.Data);
      end if;
      case Operation is
         when Add =>
            Push (Stack.Data, L + R);
         when Mul =>
            Push (Stack.Data, L * R);
         when Inc =>
            Push (Stack.Data, R + 1);
         when others =>
            raise Constraint_Error;
      end case;
   end Call;

   procedure Enclose
             (  Stack : in out Expression_Stack;
                Left  : Operations;
                Right : Operations;
                Count : Natural
             )  is
   begin
      null;
   end Enclose;

end Operation_Stack_Expressions;
