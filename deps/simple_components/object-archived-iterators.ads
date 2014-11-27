--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Object.Archived.Iterators                   Luebeck            --
--  Interface                                      Autumn, 2004       --
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

with Object.Archived.Sets;  use Object.Archived.Sets;

with Generic_Segmented_Stack;

package Object.Archived.Iterators is
   pragma Elaborate_Body (Object.Archived.Iterators);
--
-- References_Iterator -- Used to enumerate references
--
--    Referents - The list of links
--
-- This type is used to enumerate objects another object depends on. For
-- this  Enumerate  is called with the dependent object as the argument.
-- The  operation  results  in  storing  all  referent  objects  in  the
-- container Referent.  A  derived  type  may  override  On_Each  to  be
-- informed about  each  new  referent  object  about  to  be  put  into
-- Referents. On_Each may raise an exception  to  stop  the  enumeration
-- process.
--
   type References_Iterator
        (  Referents : access Deposit_Container'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
--
-- Enumerate -- References of an object
--
--    Iterator - The interation state object
--    Object   - To enumerate references from
--
-- This procedure is called to enumerate references of  an  object.  The
-- object  references  itself,  so  this same procedure is used for both
-- starting  the  process  and  continuing  it for each found reference.
-- Enumerate calls Get_Referents for Object and places all found objects
-- which  Object  depends  on into Iterator.Referents. A found object is
-- placed only once. The object itself is not put there. 
--
   procedure Enumerate
             (  Iterator : in out References_Iterator'Class;
                Object   : Deposit'Class
             );
--
-- On_Each -- Called each time a new object to be enumerated
--
--    Iterator - The interation state object
--    Referent - To be placed into Iterator.Referents
--
-- Excepions :
--
--    Any stops enumeration and propagates out of Enumerate
--
   procedure On_Each 
             (  Iterator : in out References_Iterator;
                Referent : Deposit_Ptr
             );

private
   type Stack_Index is new Integer;
   package Deposit_Stacks is
      new Generic_Segmented_Stack (Stack_Index, Deposit_Ptr, null);
   type References_Iterator
        (  Referents : access Deposit_Container'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Buffer : Deposit_Set;
      List   : Deposit_Stacks.Segmented_Stack.Stack;
   end record;

end Object.Archived.Iterators;
