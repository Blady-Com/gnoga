--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Events                                      Spring, 2018       --
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

package Synchronization.Interprocess.Events is
--
-- Event -- A  plain  event.  The  event  must  be  a  component  of  an
--          environment  object.  The  event  can be signaled and reset.
--          manually.  When signaled all tasks waiting for the event are
--          released.
--
   type Event is new Abstract_Shared_Object with private;
--
-- Is_Signaled -- The current event state
--
--    Object - The event object
--
-- Returns :
--
--    True if the event is signaled
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The event is not initialized
--
   function Is_Signaled (Object : Event) return Boolean;
--
-- Reset -- The event to the non-signaled state
--
--    Object - The event object
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The event is not initialized
--
   procedure Reset (Object : in out Event);
--
-- Signal -- The event
--
--    Object - The event object
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The event is not initialized
--
   procedure Signal (Object : in out Event);
--
-- Wait -- For the event signaled
--
--    Object     - The event object
--  [ Timeout ]  - Time before Timeout_Error is propagated
--  [ Signaled ] - Set to true if the event was signaled
--
-- Exceptions :
--
--    Data_Error    - System errors
--    Status_Error  - The event is not initialized
--    Timeout_Error - Timed out (without Signaled parameter)
--
   procedure Wait
             (  Object  : in out Event;
                Timeout : Duration := Duration'Last
             );
   procedure Wait
             (  Object   : in out Event;
                Timeout  : Duration;
                Signaled : out Boolean
             );
private
   type Event_Data is record
      Futex : aliased int := 0;
   end record;
   type Event_Data_Ptr is access all Event_Data;
   type Event is new Abstract_Shared_Object with record
      Data : Event_Data_Ptr;
   end record;
   procedure Finalize (Object : in out Event);
   function Get_Size (Object : Event) return Storage_Count;
   procedure Map
             (  Object   : in out Event;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
end Synchronization.Interprocess.Events;
