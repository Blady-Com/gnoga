--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Interprocess_Data                      Luebeck            --
--  Helper test package                            Spring, 2018       --
--                                                                    --
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

with Synchronization.Interprocess;  use Synchronization.Interprocess;

with Synchronization.Interprocess.Events;
use  Synchronization.Interprocess.Events;

with Synchronization.Interprocess.Memory_Pools;
use  Synchronization.Interprocess.Memory_Pools;

with Synchronization.Interprocess.Mutexes;
use  Synchronization.Interprocess.Mutexes;

with Synchronization.Interprocess.Pulse_Events;
use  Synchronization.Interprocess.Pulse_Events;

with Synchronization.Interprocess.Streams;
use  Synchronization.Interprocess.Streams;

with Synchronization.Interprocess.Generic_Blackboard;
with Synchronization.Interprocess.Generic_FIFO;
with Synchronization.Interprocess.Generic_Shared_Object;

package Test_Interprocess_Data is

   package Shared_Integer is
      new Synchronization.Interprocess.
          Generic_Shared_Object (Integer);
   package Shared_Reference is
      new Synchronization.Interprocess.
          Generic_Shared_Object (Reference);

   package Shared_Integer_Queue is
      new Synchronization.Interprocess.Generic_FIFO (Integer);

   package Test_Boards is
      new Synchronization.Interprocess.Generic_Blackboard (String);

   type Shared_Data_Master is
      new Abstract_Shared_Environment with record
      Event_1         : Event;
      Event_2         : Pulse_Event;
      Mutex_1         : Mutex;
      Int_1           : Shared_Integer.Shared_Object;
      Reference_1     : Shared_Reference.Shared_Object;
      Not_Full_Event  : Event;
      Not_Empty_Event : Event;
      Queue           : Shared_Integer_Queue.FIFO_In (10);
      Stream          : aliased Output_Stream (11);
      Pool            : Interprocess_Pool (400);
      Board           : Test_Boards.Blackboard (1_000);
   end record;

   type Shared_Data_Slave is
      new Abstract_Shared_Environment with record
      Event_1         : Event;
      Event_2         : Pulse_Event;
      Mutex_1         : Mutex;
      Int_1           : Shared_Integer.Shared_Object;
      Reference_1     : Shared_Reference.Shared_Object;
      Not_Full_Event  : Event;
      Not_Empty_Event : Event;
      Queue           : Shared_Integer_Queue.FIFO_Out (10);
      Stream          : aliased Input_Stream (11);
      Pool            : Interprocess_Pool (400);
      Board           : Test_Boards.Blackboard (1_000);
   end record;

end Test_Interprocess_Data;
