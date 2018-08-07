--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Stack_Storage.Mark_And_Release              Luebeck            --
--  Implementation                                 Winter, 2003       --
--                                                                    --
--                                Last revision :  19:15 09 Jul 2018  --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Stack_Storage.Mark_And_Release is

   type General_Ptr is access all Pool_Object'Class;
   function To_Specific is
      new Ada.Unchecked_Conversion
          (  General_Ptr,
             Pool_Object_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Pool_Object'Class,
             Pool_Object_Ptr
          );

   procedure Initialize (Object : in out Pool_Object) is
   begin
      Object.Previous := Last_Allocated;
      Last_Allocated  := To_Specific (Object'Unchecked_Access);
   end Initialize;

   procedure Initialize (Snap : in out Pool_Mark) is
   begin
      Snap.Mark := Last_Allocated;
   end Initialize;

   procedure Finalize (Object : in out Pool_Object) is
      This : constant Pool_Object_Ptr :=
             To_Specific (Object'Unchecked_Access);
   begin
      if This /= Last_Allocated then
         raise Storage_Error;
      end if;
      Last_Allocated := Object.Previous;
   end Finalize;

   procedure Finalize (Snap : in out Pool_Mark) is
      Ptr : Pool_Object_Ptr;
   begin
      while Last_Allocated /= Snap.Mark loop
         Ptr := Last_Allocated;
         Free (Ptr);
      end loop;
   exception
      when others =>
         raise Storage_Error;
   end Finalize;

end Stack_Storage.Mark_And_Release;
