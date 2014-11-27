--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Address_Order                       Luebeck            --
--  Implementation                                 Winter, 2009       --
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

with System;  use System;

package body Generic_Address_Order is

   type Ptr is access all Item_Type;

   function Equal (Left, Right : access Item_Type) return Boolean is
   begin
      return Ptr (Right) = Ptr (Left);
   end Equal;

   function Less  (Left, Right : access Item_Type) return Boolean is
   begin
      return
      (  Ptr (Right) /= null
      and then
         (  Ptr (Left) = null
         or else
            Left.all'Address < Right.all'Address
      )  );
   end Less;

end Generic_Address_Order;
