--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Generic_Shared_Object                       Spring, 2018       --
--  Interface                                                         --
--                                Last revision :  19:18 30 Apr 2018  --
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

with Synchronization.Interprocess.Mutexes;
use  Synchronization.Interprocess.Mutexes;

generic
   type Object_Type is private;
package Synchronization.Interprocess.Generic_Shared_Object is
--
-- Shared_Object -- An object  shared  between  several  processes.  The
--                  object  value can be get and  set concurrently.  The
-- shared object requires a mutex  for  interlocking.  The mutex  object
-- must be  placed in the shared environment  object before  the shared
-- object. For example:
--
--     type Shared is new Abstract_Shared_Environment with record
--        Lock : Mutex;  <--------------------------.
--        Data : Shared_Object; -- Uses this mutex _/
--     end record;
--
   type Shared_Object is new Abstract_Shared_Object with private;
--
-- Get -- The object value
--
--    Object - The shared object
--
-- Returns :
--
--    True if the event is signaled
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The object is not initialized
--
   function Get (Object : Shared_Object) return Object_Type;
--
-- Set -- The event to the non-signaled state
--
--    Object - The shared object
--    Value  - The new value
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The event is not initialized
--
   procedure Set
             (  Object : in out Shared_Object;
                Value  : Object_Type
             );
--
-- Generic_Call -- An operation on the object
--
--    Object - The shared object
--
-- The generic formal  parameter  is the operation  to be applied to the
-- object in a safe way.
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The event is not initialized
--
   generic
      with procedure Operation (Value : in out Object_Type);
   procedure Generic_Call (Object : in out Shared_Object);

private
   type Value_Ptr is access all Object_Type;
   type Mutex_Ptr is access all Mutex'Class;
   type Shared_Object is new Abstract_Shared_Object with record
      Lock  : Mutex_Ptr;
      Value : Value_Ptr;
   end record;
   procedure Finalize (Object : in out Shared_Object);
   function Get_Signature (Object : Shared_Object) return Unsigned_16;
   function Get_Size (Object : Shared_Object) return Storage_Count;
   procedure Map
             (  Object   : in out Shared_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
end Synchronization.Interprocess.Generic_Shared_Object;
