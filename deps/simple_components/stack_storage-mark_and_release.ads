--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Stack_Storage.Mark_And_Release              Luebeck            --
--  Interface                                      Winter, 2003       --
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
--
--  This package provides a mark and release storage pool. The type Pool
--  is a descendant of a segmented stack pool. The package  keeps  track
--  on all objects allocated in the pool to allow them  being  destroyed
--  as the frame they were allocated left. The package has  one  generic
--  parameter: 
--
--     Stack - The stack pool to use (Stack_Storage.Pool)
--
--  The  type  Pool_Object  is  used  as  the  base type for all objects
--  allocated on Stack. It  is  a  limited  controlled  type.  The  type
--  Pool_Object_Ptr  is  a  pool-specific  pointer to Pool_Object'Class.
--  Objects of type Pool_Mark are used for mark-and-release: 
--
--     declare
--        Snap : Pool_Mark;       -- Mark the pool state
--        Ptr  : Pool_Object_Ptr;
--     begin
--        Ptr := new Derived_Pool_Object;         -- Allocate
--        Ptr := new Another_Derived_Pool_Object; -- Allocate
--     end;                      -- Release all allocated objects 
--
--  If  some  of  the  pool  objects  are  destroyed  explicitly   using
--  Unchecked_Deallocation,  then  this  should  be done in exactly same
--  order as they were created and never under the latest pool mark. 
--
with Ada.Finalization;

generic
   Stack : in out Pool'Class;
package Stack_Storage.Mark_And_Release is
--
-- Pool_Object -- All the objects allocated  on  the  stack  has  to  be
--                descendants of this type. 
--
   type Pool_Object is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Pool_Object_Ptr -- Pool-specific  pointers  to  the  objects. All the
--                    descendants of Pool_Object have  to  be  allocated
-- dynamically  using  the  allocator  (new).  As  the  target  for  the
-- allocator Pool_Object_Ptr or its descendant has to be  specified.  It
-- is also possible to use some  other  access  type  but  only  if  its
-- storage pool is Stack. 
--
   type Pool_Object_Ptr is access Pool_Object'Class;
   for Pool_Object_Ptr'Storage_Pool use Stack;
--
-- Pool_Mark -- Pool snap-shot
--
   type Pool_Mark is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Finalize -- Destructor
--
--    Snap - A pool snap-shot
--
-- All objects allocated in the pool  since  construction  and  not  yet
-- destroyed are destroyed now. 
--
-- Exceptions :
--
--    Storage_Error
--
   procedure Finalize (Snap : in out Pool_Mark);
--
-- Finalize -- Destructor
--
--    Object - A pool object
--
-- Exceptions :
--
--    Storage_Error - Finalized out of allocation order
-- 
   procedure Finalize (Object : in out Pool_Object);
--
-- Initialize -- Constructor
--
--    Object - A pool object
--
   procedure Initialize (Object : in out Pool_Object);
--
-- Initialize -- Constructor
--
--    Snap - A pool snap-shot
--
   procedure Initialize (Snap : in out Pool_Mark);

private
   type Pool_Object is
      new Ada.Finalization.Limited_Controlled with
   record
      Previous : Pool_Object_Ptr;
   end record;

   type Pool_Mark is
      new Ada.Finalization.Limited_Controlled with
   record
      Mark : Pool_Object_Ptr;
   end record;
--
-- Last_Allocated -- The last object allocated in the pool
--
   Last_Allocated : Pool_Object_Ptr;
--
-- Kill_All -- Deletes all objects lingering in the pool
--
   Kill_All : Pool_Mark;
   
end Stack_Storage.Mark_And_Release;
