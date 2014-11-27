--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Object.Archived.Iterators                   Luebeck            --
--  Implementation                                 Autumn, 2004       --
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

package body Object.Archived.Iterators is
   use Deposit_Stacks.Segmented_Stack;

   procedure Enumerate
             (  Iterator : in out References_Iterator'Class;
                Object   : Deposit'Class
             )  is
      Referent : Deposit_Ptr;
      Count    : Natural := 0;
   begin
      Get_Referents (Object, Iterator.Buffer);
      for Index in 1..Get_Size (Iterator.Buffer) loop 
         Referent := Get (Iterator.Buffer, Index);
         if not Is_In (Iterator.Referents.all, Referent) then
            Add (Iterator.Referents.all, Referent);
            Push (Iterator.List, Referent);
            Count := Count + 1;
         end if;
      end loop;
      Erase (Iterator.Buffer);
      while Count /= 0 loop
         Referent := Top (Iterator.List);
         Pop (Iterator.List);
         Count := Count - 1;
         On_Each (Iterator, Referent);
         Enumerate (Iterator, Referent.all);
      end loop;
   exception
      when others =>
         Pop (Iterator.List, Count);
         raise;
   end Enumerate; 

   procedure On_Each
             (  Iterator : in out References_Iterator;
                Referent : Deposit_Ptr
             )  is
   begin
      null;
   end On_Each;

end Object.Archived.Iterators;
