--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Mutexes                     Luebeck            --
--  Interface                                      Spring, 2008       --
--                                                                    --
--                                Last revision :  16:09 11 May 2008  --
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
--  This  package  provides  mutexes  and  helper  objects  to seize and
--  release them in a safe way.  The  implementation  supports  multiple
--  seizing of a mutex from the same task.
--
with Ada.Task_Identification;  use Ada.Task_Identification;

with Ada.Finalization;

package Synchronization.Mutexes is
--
-- Mutex -- A mutex object
--
   protected type Mutex is
   --
   -- Get_Owner -- The current owner of the mutex
   --
   -- Returns :
   --
   --    The owner's ID or Null_Task_ID
   --
      function Get_Owner return Task_ID;
   --
   -- Grab -- The mutex if not owned
   --
   -- This procedure seizes the mutex if it is  not  owned  or  else  is
   -- owned by the caller. Is_Mine can be used after Grab  in  order  to
   -- verify if the mutex was seized. For example:
   --
   --     Resource.Grab;            -- Try to seize it without blocking
   --     if Resource.Is_Mine then
   --        ...                    -- Use the resource
   --        Resource.Release;      -- Note, it has to be released
   --     end if;
   --
      procedure Grab;
   --
   -- Is_Mine -- Check if the mutex is owned by the caller
   --
   -- Returns :
   --
   --    True if owned by the caller
   --
      function Is_Mine return Boolean;
   --
   -- Is_Owned -- Check if the mutex is owned
   --
   -- Returns :
   --
   --    True if owned
   --
      function Is_Owned return Boolean;
   --
   -- Release -- The mutex
   --
   -- Exceptions :
   --
   --    Ownership_Error - The mutex is not owned by the caller
   --
      procedure Release;
   --
   -- Seize -- The mutex
   --
   -- Seize  does  not  block when the task already owns the mutex. Each
   -- call to Seize shall be matched by a call to Release.
   --
      entry Seize;

   private
      pragma Inline (Get_Owner);
      pragma Inline (Grab);
      pragma Inline (Is_Mine);
      pragma Inline (Is_Owned);

      entry Lounge;

      Owner : Task_ID := Null_Task_ID;
      Count : Natural := 0;
   end Mutex;
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
end Synchronization.Mutexes;
