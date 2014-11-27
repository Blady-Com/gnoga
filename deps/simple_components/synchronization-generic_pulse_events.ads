--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.                            Luebeck            --
--        Generic_Pulse_Events                     Spring, 2008       --
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
--  This package provides pulse  events  distributing  a  value.  It  is
--  basically Synchronization.Pulse_Events with the entry points  having
--  the parameter of the event value.
--
generic
   type Event_Value is private;
package Synchronization.Generic_Pulse_Events is
--
--  Pulse_Event -- Automatically reset event
--
   protected type Pulse_Event is
   --
   -- Pulse -- The event
   --
   --    Value - The value to distribute
   --
   -- This  entry releases all tasks blocked on Wait. The implementation
   -- is race condition free, so that a given task is released by  Pulse
   -- only once. Note that though this is an entry, it  does  not  block
   -- for any considerable time.
   --
      entry Pulse (Value : Event_Value);
   --
   -- Wait -- For event pulse
   --
   --    Value - Of the event
   --
      entry Wait (Value : out Event_Value);

   private
      entry Lounge (Boolean) (Value : out Event_Value);
      Current : Boolean := False;
      Data    : Event_Value;
   end Pulse_Event;

end Synchronization.Generic_Pulse_Events;
