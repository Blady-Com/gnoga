--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Pulse_Events                                Spring, 2018       --
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

package Synchronization.Interprocess.Pulse_Events is
--
-- Pulse_Event -- A  pulse  event.  The  event must be a component of an
--                environment object. The event can be pulsed to release
--                all tasks waiting for the event.
--
   type Pulse_Event is new Abstract_Shared_Object with private;
--
-- Pulse -- The event
--
--    Object - The event object
--
-- Exceptions :
--
--    Data_Error   - System errors
--    Status_Error - The event is not initialized
--
   procedure Pulse (Object : in out Pulse_Event);
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
             (  Object  : in out Pulse_Event;
                Timeout : Duration := Duration'Last
             );
   procedure Wait
             (  Object   : in out Pulse_Event;
                Timeout  : Duration;
                Signaled : out Boolean
             );
private
   type Pulse_Event_Data is record
      Futex : aliased int := 0;
   end record;
   type Pulse_Event_Data_Ptr is access all Pulse_Event_Data;
   type Pulse_Event is new Abstract_Shared_Object with record
      Data : Pulse_Event_Data_Ptr;
   end record;
   procedure Finalize (Object : in out Pulse_Event);
   function Get_Size (Object : Pulse_Event) return Storage_Count;
   procedure Map
             (  Object   : in out Pulse_Event;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
end Synchronization.Interprocess.Pulse_Events;
