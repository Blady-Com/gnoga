--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.                            Luebeck            --
--        Generic_Mutexes_Array                    Spring, 2008       --
--  Interface                                                         --
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
--  This  package  provides arrays of mutexe and helper objects to seize
--  and release them in a safe way. The implementation supports multiple
--  seizing  of  a  mutex  from  the same task. It also enforces seizing
--  mutexes in the order of their values. When a task attempts to  seize
--  a mutex which it does not owm, while it already owns a mutex with  a
--  higher position  number,  that  action  propageates  Ownership_Error
--  exception. The package also supports seizing and releasing  of  sets
--  of mutexes.
--
with Ada.Task_Identification;  use Ada.Task_Identification;

with Ada.Finalization;

generic
   type Mutex_Type is (<>);
package Synchronization.Generic_Mutexes_Array is
--
-- Mutexes_Set -- A set of mutexes. A mutex belongs to the set, when the
--                corresponding element of the array is True
--
   type Mutexes_Set is array (Mutex_Type) of Boolean;
--
-- Some  set-theoretic  operations provided to ease construction of sets
-- of mutexes
--
   function "or" (Left, Right : Mutex_Type) return Mutexes_Set;
   function "or" (Left : Mutexes_Set; Right : Mutex_Type)
      return Mutexes_Set;
   function "or" (Left : Mutex_Type; Right : Mutexes_Set)
      return Mutexes_Set;
   function "not" (Left : Mutex_Type) return Mutexes_Set;

   type Task_ID_Array is array (Mutex_Type) of Task_ID;
   type Counts_Array  is array (Mutex_Type) of Natural;
--
-- Mutex -- A mutex object
--
   protected type Mutexes_Array is
   --
   -- Get_Owner -- The current owner of a mutex
   --
   --    Mutex - A mutex from the array
   --
   -- Returns :
   --
   --    The owner's ID or Null_Task_ID
   --
      function Get_Owner (Mutex : Mutex_Type) return Task_ID;
   --
   -- Grab -- A mutex if not owned by anybody else
   --
   --    Mutex - To grab
   --
   -- This procedure seizes the mutex if it is  not  owned  or  else  is
   -- owned by the caller. Is_Mine can be used after Grab  in  order  to
   -- verify if the mutex was seized. For example:
   --
   --     Resource.Grab (ID);       -- Try to seize it without blocking
   --     if Resource.Is_Mine (ID) then
   --        ...                    -- Use the resource
   --        Resource.Release (ID); -- Note, it has to be released
   --     end if;
   --
   -- The output parameter Success can be used for the same purpose.
   --
   -- Exceptions :
   --
   --    Ownership_Error - A mutex is requested out of order
   --
      procedure Grab (Mutex : Mutex_Type);
      procedure Grab (Mutex : Mutex_Type;  Success : out Boolean);
   --
   -- Grab_All -- Mutexes if not owned by anybody else
   --
   --    Mutexes - To grab
   --    Success - The operation outcome
   --
   -- This procedure seizes the mutexes if  none  of  them  is  wned  by
   -- another task.  The  output  parameter  Success  indicates  if  the
   -- mutexes were seized. In this case Release shall be called for each
   -- of them later.
   --
   -- Exceptions :
   --
   --    Ownership_Error - A mutex is requested out of order
   --
      procedure Grab_All (Mutexes : Mutexes_Set; Success : out Boolean);
   --
   -- Is_Mine -- Check if the mutex is owned by the caller
   --
   --    Mutex[es] - A mutex or a set of
   --
   -- Returns :
   --
   --    True if all specified mutexes owned by the caller
   --
      function Is_Mine (Mutex   : Mutex_Type)  return Boolean;
      function Is_Mine (Mutexes : Mutexes_Set) return Boolean;
   --
   -- Is_Owned -- Check if a mutex is owned
   --
   --    Mutex - A mutex to test
   --
   -- Returns :
   --
   --    True if owned
   --
      function Is_Owned (Mutex : Mutex_Type) return Boolean;
   --
   -- Release -- A mutex
   --
   --    Mutex - The mutex to release
   --
   -- Exceptions :
   --
   --    Ownership_Error - The mutex is not owned by the caller
   --
      entry Release (Mutex : Mutex_Type);
   --
   -- Release_All -- A mutex
   --
   --    Mutexes - A set of mutexes to release
   --
   -- Exceptions :
   --
   --    Ownership_Error - A mutex is not owned by the caller
   --
      entry Release_All (Mutexes : Mutexes_Set);
   --
   -- Seize -- A mutex
   --
   --    Mutex - To seize
   --
   -- Seize  does  not  block when the task already owns the mutex. Each
   -- call to Seize shall be matched by a call to Release.
   --
   -- Exceptions :
   --
   --    Ownership_Error - The mutex is requested out of order
   --
      entry Seize (Mutex   : Mutex_Type);
   --
   -- Seize -- A mutex
   --
   --    Mutex[es] - To seize
   --
   -- Seize does not block  when  the  task  already  owns  all  mutexes
   -- requested.  Each  call  to  Seize  shall  be  matched by a call to
   -- Release for each of the mutexes seized.
   --
   -- Exceptions :
   --
   --    Ownership_Error - A mutex is requested out of order
   --
      entry Seize_All (Mutexes : Mutexes_Set);

   private
      pragma Inline (Get_Owner);
      pragma Inline (Is_Mine);
      pragma Inline (Is_Owned);

      procedure Acquire
                (  Mutexes : Mutexes_Set;
                   Caller  : Task_ID;
                   Success : out Boolean
                );
      function Check (Mutex : Mutex_Type; Caller : Task_ID)
         return Boolean;
      pragma Inline (Check);

      function Check (Mutexes : Mutexes_Set; Caller : Task_ID)
         return Boolean;

      function Empty_Lounges return Boolean;
      pragma Inline (Empty_Lounges);

      entry Lounge_Multiple (Boolean) (Mutexes : Mutexes_Set);
      entry Lounge_Single (Boolean) (Mutex : Mutex_Type);

      Current : Boolean       := False;
      Owner   : Task_ID_Array := (others => Null_Task_ID);
      Count   : Counts_Array  := (others => 0);
   end Mutexes_Array;
--
-- Set_Holder -- A mutex set holder  object.  This  is  a  helper  which
--               ensures  releasing  of  all mutexes even upon exception
--               propagation.
--
--    Resource - A pointer to the array mutex to seize
--    Seize    - The set of mutexes to seize
--
   type Set_Holder
        (  Resource : access Mutexes_Array;
           Seize    : access Mutexes_Set
        )  is new Ada.Finalization.Limited_Controlled with private;
--
-- Singleton_Holder -- A  mutex  holder  object.  This is a helper which
--                     ensures  releasing of a mutex even upon exception
--                     propagation.
--
--    Resource - A pointer to the array mutex to seize
--    Seize    - The mutex to seize
--
   type Singleton_Holder
        (  Resource : access Mutexes_Array;
           Seize    : Mutex_Type
        )  is new Ada.Finalization.Limited_Controlled with private;
--
-- Finalize -- Destruction
--
-- This  procedure releases the mutex. It shall be called by the derived
-- type if overridden.
--
   procedure Finalize (Object : in out Set_Holder);
   procedure Finalize (Object : in out Singleton_Holder);
--
-- Initialize -- Construction
--
-- This  procedure  seizes  the mutex. It shall be called by the derived
-- type if overridden.
--
   procedure Initialize (Object : in out Set_Holder);
   procedure Initialize (Object : in out Singleton_Holder);

private
   pragma Inline ("or", "not");

   type Set_Holder
        (  Resource : access Mutexes_Array;
           Seize    : access Mutexes_Set
        )  is new Ada.Finalization.Limited_Controlled with null record;
   type Singleton_Holder
        (  Resource : access Mutexes_Array;
           Seize    : Mutex_Type
        )  is new Ada.Finalization.Limited_Controlled with null record;

end Synchronization.Generic_Mutexes_Array;
