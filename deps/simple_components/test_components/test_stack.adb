--                                                                    --
--  procedure Test_Stack            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2003       --
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
--
--  This is a small test procedure for the stack pool.
--
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Test_Stack_Item;  use Test_Stack_Item;

procedure Test_Stack is
   Ptr : Item_Ptr;
begin
   declare
      Snap : Stack_Mark;
   begin
      Ptr := new Item;
      Ptr := new Item;
      if not Is_Allocated (1) or not Is_Allocated (2) then 
         Raise_Exception
         (  Constraint_Error'Identity,
            "Allocation fault"
         );
      end if;
      declare
         Snap : Stack_Mark;
      begin
         Ptr := new Item;
         Ptr := new Item;
         if not (  Is_Allocated (1)
                and
                   Is_Allocated (2)
                and
                   Is_Allocated (3)
                and
                   Is_Allocated (4)
                )
         then 
            Raise_Exception
            (  Constraint_Error'Identity,
               "Allocation fault (2)"
            );
         end if;
      end;
      Put_Line ("The nested frame left");
      if Is_Allocated (3) or Is_Allocated (4) then 
         Raise_Exception
         (  Constraint_Error'Identity,
            "Deallocation fault (2)"
         );
      end if;
      Ptr := new Item;
   end;
   Put_Line ("The main frame left");
   if (  Is_Allocated (1)
      or Is_Allocated (2)
      or Is_Allocated (3)
      or Is_Allocated (4)
      or Is_Allocated (5)
      )
   then 
      Raise_Exception
      (  Constraint_Error'Identity,
         "Deallocation fault (3)"
      );
   end if;
end Test_Stack;
