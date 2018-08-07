--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Mutexes                                     Spring, 2018       --
--  Interface                                                         --
--                                Last revision :  01:09 01 May 2018  --
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

with Ada.Task_Identification;  use Ada.Task_Identification;

package Synchronization.Interprocess.Mutexes is
--
-- Mutex -- A mutex  object.  The  mutex  must  be  a  component  of  an
--          environment object. The mutex can be  seized  and  released.
--          Only one task can hold the mutex. The same task can take  it
--          several times without deadlocking. If taken  multiple  times
--          the mutex must be released for each take.
--
   type Mutex is new Abstract_Shared_Object with private;
--
-- Get_Timeout -- The mutex seize timeout
--
--    Object - The mutex object
--
-- Returns :
--
--    The timeout used in the the Seize
--
   function Get_Timeout (Object : Mutex) return Duration;
--
-- Grab -- The mutex if not owned
--
--    Object - The mutex object
--
-- This procedure seizes the mutex if it is not owned or else  is  owned
-- by the caller. Is_Mine can be used after Grab in order to  verify  if
-- the mutex was seized. For example:
--
--     Grab (Resource);          -- Try to seize it without blocking
--     if Is_Mine (Resource) then
--        ...                    -- Use the resource
--        Release (Resource);    -- Note, it has to be released
--     end if;
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The mutex is not initialized
--
   procedure Grab (Object : in out Mutex);
--
-- Is_Mine -- Check if the mutex is owned by the caller
--
--    Object - The mutex object
--
-- Returns :
--
--    True if owned by the caller
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The mutex is not initialized
--
   function Is_Mine (Object : Mutex) return Boolean;
--
-- Is_Owned -- Check if the mutex is owned
--
--    Object - The mutex object
--
-- Returns :
--
--    True if owned
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The mutex is not initialized
--
   function Is_Owned (Object : Mutex) return Boolean;
--
-- Release -- The mutex
--
--    Object - The mutex object
--
-- Exceptions :
--
--    Data_Error      - System errors
--    Ownership_Error - The mutex is not owned by the caller
--    Status_Error    - The mutex is not initialized
--
   procedure Release (Object : in out Mutex);
--
-- Seize -- The mutex
--
--    Object    - The mutex object
--  [ Timeout ] - Time before Timeout_Error is propagated
--
-- Seize does not block when the task already owns the mutex. Each  call
-- to Seize shall be matched by a call to Release.
--
-- Exceptions :
--
--    Data_Error    - System errors
--    Status_Error  - The mutex is not initialized
--    Timeout_Error - Timed out
--
   procedure Seize (Object : in out Mutex);
   procedure Seize (Object : in out Mutex; Timeout : Duration);
--
-- Set_Timeout -- The mutex seize timeout
--
--    Object  - The mutex object
--    Timeout - The timeout to be used in the the Seize
--
   procedure Set_Timeout
             (  Object  : in out Mutex;
                Timeout : Duration
             );
--
-- Holder -- A mutex holder object.  This  is  a  helper  which  ensures
--           releasing of a mutex even upon exception propagation.
--
--    Resource - A pointer to the mutex to seize
--
   type Holder (Resource : access Mutex) is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Finalize -- Destruction
--
-- This  procedure releases the mutex. It shall be called by the derived
-- type if overridden.
--
   procedure Finalize (Object : in out Holder);
--
-- Initialize -- Construction
--
-- This  procedure  seizes  the mutex. It shall be called by the derived
-- type if overridden.
--
   procedure Initialize (Object : in out Holder);

private
   type Holder (Resource : access Mutex) is
      new Ada.Finalization.Limited_Controlled with null record;

   type Mutex_Data is record
      Mutex : aliased pthread_mutex_t := (Data => (others => 0));
      Count : Natural                 := 0;
      Owner : Unsigned_64             := 0;
   end record;
   type Mutex_Data_Ptr is access all Mutex_Data;
   type Mutex is new Abstract_Shared_Object with record
      Data    : Mutex_Data_Ptr;
      Timeout : Duration := 10.0;
   end record;
   procedure Finalize (Object : in out Mutex);
   function Get_Size (Object : Mutex) return Storage_Count;
   procedure Map
             (  Object   : in out Mutex;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
   procedure Unmap
             (  Object : in out Mutex;
                Shared : in out Abstract_Shared_Environment'Class;
                Owner  : Boolean
             );

end Synchronization.Interprocess.Mutexes;
