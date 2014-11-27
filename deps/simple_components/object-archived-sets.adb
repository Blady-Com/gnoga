--                                                                    --
--  package Object.Archived.Sets    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Object.Archived.Sets is
   use Deposit_Sets;

   procedure Add
             (  Container : in out Deposit_Set;
                Object    : Deposit_Ptr;
                Backward  : Boolean := False
             )  is
   begin
      Add (Container.Set, Object);
   end Add;

   procedure Erase (Container : in out Deposit_Set) is
   begin
      Erase (Container.Set);
   end Erase;

   function Get (Container : Deposit_Set; Index : Positive)
      return Deposit_Ptr is
   begin
      return Get (Container.Set, Index);
   end Get;

   function Get_Size (Container : Deposit_Set) return Natural is
   begin
      return Get_Size (Container.Set);
   end Get_Size;

   function Is_Backward
            (  Container : Deposit_Set;
               Object    : Deposit_Ptr
            )  return Boolean is
   begin
      if Is_In (Container, Object) then
         raise Use_Error;
      else
         raise Constraint_Error;
      end if;
         pragma Warnings (Off);
      return False; -- Unreachable, but required
         pragma Warnings (On);
   end Is_Backward;

   function Is_In (Container : Deposit_Set; Object : Deposit_Ptr)
      return Boolean is
   begin
      return Is_In (Container.Set, Object);
   end Is_In;

   procedure Remove
             (  Container : in out Deposit_Set;
                Object    : Deposit_Ptr
             )  is
   begin
      Remove (Container.Set, Object);
   end Remove;

   function "and" (Left, Right : Deposit_Set) return Deposit_Set is
   begin
      return (Deposit_Container with Set => Left.Set and Right.Set);
   end "and";

   function "or" (Left, Right : Deposit_Set) return Deposit_Set is
   begin
      return (Deposit_Container with Set => Left.Set or Right.Set);
   end "or";

   function "xor" (Left, Right : Deposit_Set) return Deposit_Set is
   begin
      return (Deposit_Container with Set => Left.Set xor Right.Set);
   end "xor";

   function "=" (Left, Right : Deposit_Set) return Boolean is
   begin
      return Left.Set = Right.Set;
   end "=";

end Object.Archived.Sets;
