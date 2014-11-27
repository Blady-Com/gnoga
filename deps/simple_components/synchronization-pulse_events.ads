--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Pulse_Events                Luebeck            --
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
--  This package provides pulse events. A pulse events is signaled for a
--  short  period  of  time. It is reset automatically immediately after
--  the last task awaiting the event  is  released.  The  implementation
--  provided by this package is free of race conditions. That  is,  even
--  if a  task  released  by  the  event  seized  the  processor  before
--  releasing other tasks, and then entered another wait  for  the  same
--  event, that would not be release the task again. The  implementation
--  also  ensures  that  all  tasks  entering waiting before pulsing the
--  event are released before pulsing the  event  again.  The  following
--  diagram  illustrates the constraints satisfied by the implementation
--  in order to prevent race condition state:
--
--             Pulse                       Pulse
--             ._____.                     ._____.
--             |/////| Releasing tasks     |/////|  Releasing tasks
--             |/////|_____________.       |/////|__________.
--             |/////|/////////////|       |/////|//////////|
--          ___|/////|/////////////|_______|/////|//////////|_______
--             |     |             |       |     |          |
--  Pulse:     |<-----blocked----->|       |<----blocked--->|
--   Wait: --->|<----------blocked-------->|<---------blocked--------->
--
--  Pulse awaits the state when all tasks for which it signals the event
--  are  released.  Wait  awaits  next Pulse. The ongoing Pulse does not
--  affect tasks newly enueued to Wait.
--
package Synchronization.Pulse_Events is
--
--  Pulse_Event -- Automatically reset event
--
   protected type Pulse_Event is
   --
   -- Pulse -- The event
   --
   -- This  entry releases all tasks blocked on Wait. The implementation
   -- is race condition free, so that a given task is released by  Pulse
   -- only once. Note that though this is an entry, it  does  not  block
   -- for any considerable time.
   --
      entry Pulse;
   --
   -- Wait -- For event pulse
   --
      entry Wait;

   private
      entry Lounge (Boolean);
      Current : Boolean := False;
   end Pulse_Event;

end Synchronization.Pulse_Events;
