--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Generic_FIFO                                Spring, 2018       --
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
--
--  FIFO is a first-in, first-out queue. The queue has two ends situated
--  in different processes. The process holding  the  in-end  can  write
--  into  the  queue.  The process holding the out-end can read from the
--  queue.  Read  and  write  are lock free unless the queue is empty or
--  full. In  the  latter  case  the  operation  is  blocked  until  the
--  counterpart supplies new items or takes items from the queue:
--
--     ._____________________.      .______________________.
--     |                     |      |                      |
--     |   Provider process  |      |  Consumer process    |
--     |           .___________________________.           |
--     |           |                           |           |
--     |   Put --->| In-end >> FIFO >> Out-end |---> Get   |
--     |           |___________________________|           |
--     |                     |      |                      |
--     |                     |      |                      |
--     |_____________________|      |______________________|
--
--  There  is  a  multiplexed  variant  of  in-end  that allows multiple
--  processes and tasks to write the queue. These ends are  interlocked,
--  only one has access to the queue at a time.
--
with Synchronization.Interprocess.Events;
use  Synchronization.Interprocess.Events;

with Synchronization.Interprocess.Mutexes;
use  Synchronization.Interprocess.Mutexes;

generic
   type Element_Type is private;
package Synchronization.Interprocess.Generic_FIFO is
--
-- FIFO -- A first-in first-out queue
--
--    Size - The maximal number of elements in the queue + 1
--
   type FIFO (Size : Positive) is abstract
      new Abstract_Shared_Object with private;
--
-- Is_Empty -- Check if empty
--
--    Queue - The queue
--
-- Returns :
--
--    True if Queue is empty
--
-- Exceptions :
--
--    Status_Error  - The FIFO is not initialized
--
   function Is_Empty (Queue : FIFO) return Boolean;
--
-- Is_Full -- Check if full
--
--    Queue - The queue
--
-- Returns :
--
--    True if Queue is full
--
-- Exceptions :
--
--    Status_Error  - The FIFO is not initialized
--
   function Is_Full (Queue : FIFO) return Boolean;
--
-- Is_Preserved -- Check if element is purged
--
--    Queue   - The queue
--    Element - From the queue
--
-- This function is called by Purge in order to determine if the element
-- to remain in the queue. The default implementation returns True.
--
-- Returns :
--
--    True if Element is to stay in the queue
--
   function Is_Preserved (Queue : FIFO; Element : Element_Type)
      return Boolean;
   type Is_Preserved_Ptr is access
      function (Element : Element_Type) return Boolean;
--
-- Wait_For_Not_Empty -- Wait for data in the queue
--
--    Queue   - The queue
--    Timeout - The timeout
--  [ Empty ] - Set to True if timeout was expired
--
-- This procedure must be interlocked with reading
--
-- Exceptions :
--
--    Status_Error  - The FIFO is not initialized
--    Timeout_Error - The timeout is expired (only if Empty is absent)
--
   procedure Wait_For_Not_Empty
             (  Queue   : in out FIFO;
                Timeout : Duration
             );
   procedure Wait_For_Not_Empty
             (  Queue   : in out FIFO;
                Timeout : Duration;
                Empty   : out Boolean
             );
--
-- Wait_For_Not_Full -- Wait for free space in the queue
--
--    Queue   - The queue
--    Timeout - The timeout
--  [ Full ]  - Set to True if timeout was expired
--
-- This procedure must be interlocked with writing
--
-- Exceptions :
--
--    Status_Error  - The FIFO is not initialized
--    Timeout_Error - The timeout is expired (only if Full is absent)
--
   procedure Wait_For_Not_Full
             (  Queue   : in out FIFO;
                Timeout : Duration
             );
   procedure Wait_For_Not_Full
             (  Queue   : in out FIFO;
                Timeout : Duration;
                Full    : out Boolean
             );
------------------------------------------------------------------------
--
-- FIFO_In -- The in-end of a first-in first-out queue
--
-- The  queue  can  be  written  from single process and task. The write
-- operation  awaits for the free place in the queue. The queue requires
-- two  events.  These  must  be placed in the shared environment object
-- before the queue. For example:
--
--     type Shared is new Abstract_Shared_Environment with record
--        Not_Full_Event  : Event;  <------ Set ----------.
--        Not_Empty_Event : Event;  <----- Reset ----------\
--        Queue           : FIFO_In; -- Uses these events _/
--     end record;
--
   type FIFO_In is new FIFO with private;
--
-- Put -- An element into the queue
--
--    Queue     - The queue
--    Element   - To put into the queue
--  [ Timeout ] - Time before Timeout_Error is propagated
--
-- Exceptions :
--
--    Data_Error    - System errors
--    Status_Error  - The FIFO is not initialized
--    Timeout_Error - Timed out
--
   procedure Put
             (  Queue   : in out FIFO_In;
                Element : Element_Type;
                Timeout : Duration := Duration'Last
             );
------------------------------------------------------------------------
--
-- FIFO_Multiplexed_In -- The in-end of a first-in first-out queue
--
-- The  queue  can  be  written  from  any  number  of  tasks. The write
-- operation  awaits for the free place in the queue. The queue requires
-- two  events  and  one  mutex.  These  must  be  placed  in the shared
-- environment object before the queue. For example:
--
--     type Shared is new Abstract_Shared_Environment with record
--        Lock            : Mutex;  <--------------------.
--        Not_Full_Event  : Event;  <------ Set ----------\
--        Not_Empty_Event : Event;  <----- Reset ----------\
--        Queue           : FIFO_In; -- Uses these events _/
--     end record;
--
   type FIFO_Multiplexed_In is new FIFO with private;
--
-- Put -- Task-safe operation
--
   procedure Put
             (  Queue   : in out FIFO_Multiplexed_In;
                Element : Element_Type;
                Timeout : Duration := Duration'Last
             );
------------------------------------------------------------------------
--
-- FIFO_Out -- The out-end of a first-in first-out queue
--
--    Size - The maximal number of elements in the queue + 1
--
-- The  queue  can  be  read  from  single  process  and  task. The read
-- operation  awaits for the free place in the queue. The queue requires
-- two  events.  These  must  be placed in the shared environment object
-- before the queue. For example:
--
--     type Shared is new Abstract_Shared_Environment with record
--        Not_Full_Event  : Event;  <------ Reset ---------.
--        Not_Empty_Event : Event;  <------- Set -----------\
--        Queue           : FIFO_Out; -- Uses these events _/
--     end record;
--
   type FIFO_Out is new FIFO with private;
--
-- Delete -- A number of elements from the queue
--
--    Queue     - The queue
--    Count     - The number of elements to delete
--  [ Timeout ] - Time before Timeout_Error is propagated
--
-- The  procedure  removes  the specified number of elements from Queue.
-- When Count is greater than the current queue length, all elements are
-- removed. The procedure does not wait for new elements, it deletes the
-- element available. Timeout_Error is propagated only if access to  the
-- queue is blocked.
--
--
-- Exceptions :
--
--    Data_Error    - System errors
--    Status_Error  - The FIFO is not initialized
--    Timeout_Error - Timed out
--
   procedure Delete
             (  Queue   : in out FIFO_Out;
                Count   : Natural := 1;
                Timeout : Duration := Duration'Last
             );
--
-- Get -- An element from the queue
--
--    Queue     - The queue
--  [ Timeout ] - Time before Timeout_Error is propagated
--
-- When the queue is empty,  the function  waits  until a new element is
-- placed into the queue.
--
-- Returns :
--
--    The element from the queue
--
-- Exceptions :
--
--    Data_Error    - System errors
--    Status_Error  - The FIFO is not initialized
--    Timeout_Error - Timed out
--
   function Get
            (  Queue   : FIFO_Out;
               Timeout : Duration := Duration'Last
            )  return Element_Type;
--
-- Peek -- An element from the queue leaving it there
--
--    Queue     - The queue
--    Element   - From the queue
--    Empty     - Set to true if there is no element to get
--  [ Timeout ] - Time before Timeout_Error is propagated
--
-- Timeout_Error is propagated only if access to the queue is blocked.
--
-- Exceptions :
--
--    Data_Error    - System errors
--    Status_Error  - The FIFO is not initialized
--    Timeout_Error - Timed out
--
   procedure Peek
             (  Queue   : FIFO_Out;
                Element : out Element_Type;
                Empty   : out Boolean;
                Timeout : Duration := Duration'Last
             );
--
-- Purge -- Delete certain elements from the queue
--
--    Queue          - The queue
--    Purged         - The number of elements removed
--  [ Is_Preserved ] - Function to use;
--  [ Timeout ]      - Time before Timeout_Error is propagated
--
-- This  procedure  removes the elements for which function Is_Preserved
-- returned False.  The  procedure  does  not  block,  Timeout_Error  is
-- propagated only if access to the queue is blocked.
--
-- Exceptions :
--
--    Data_Error    - System errors
--    Status_Error  - The FIFO is not initialized
--    Timeout_Error - Timed out
--
   procedure Purge
             (  Queue   : in out FIFO_Out;
                Purged  : out Natural;
                Timeout : Duration := Duration'Last
             );
   procedure Purge
             (  Queue        : in out FIFO_Out;
                Is_Preserved : Is_Preserved_Ptr;
                Purged       : out Natural;
                Timeout      : Duration := Duration'Last
             );
------------------------------------------------------------------------
--
--  Universal_FIFO -- A  FIFO  of  any end type. It can serve as any end
--                    depending   on   the   Mode   set.  Otherwise  all
--                    operations  are identical to the ones declared for
--                    specific end types.
--
   type FIFO_End is (In_End, Multiplexed_In_End, Out_End);
   type Universal_FIFO is new FIFO with private;
--
-- Get_Mode -- Get the FIFO mode
--
--    Queue - The queue
--
-- Returns :
--
--    THe FIFO end mode
--
   function Get_Mode (Queue : Universal_FIFO) return FIFO_End;
--
-- Set_Mode -- Set the FIFO mode
--
--    Queue - The queue
--    Mode  - The desired end mode
--
-- The mode can be set before creation or opening the environment only.
--
-- Exceptions :
--
--    Mode_Error - The FIFO is already initialized
--
   procedure Set_Mode (Queue : in out Universal_FIFO; Mode : FIFO_End);

   procedure Delete
             (  Queue   : in out Universal_FIFO;
                Count   : Natural := 1;
                Timeout : Duration := Duration'Last
             );
   function Get
            (  Queue   : Universal_FIFO;
               Timeout : Duration := Duration'Last
            )  return Element_Type;
   procedure Peek
             (  Queue   : Universal_FIFO;
                Element : out Element_Type;
                Empty   : out Boolean;
                Timeout : Duration := Duration'Last
             );
   procedure Put
             (  Queue   : in out Universal_FIFO;
                Element : Element_Type;
                Timeout : Duration := Duration'Last
             );
   procedure Purge
             (  Queue   : in out Universal_FIFO;
                Purged  : out Natural;
                Timeout : Duration := Duration'Last
             );
   procedure Purge
             (  Queue        : in out Universal_FIFO;
                Is_Preserved : Is_Preserved_Ptr;
                Purged       : out Natural;
                Timeout      : Duration := Duration'Last
             );
private
   type Element_Array is array (Positive range <>) of Element_Type;

   type Event_Ptr is access all Event'Class;
   type FIFO_Data (Size : Positive) is record
      Free        : Positive := 1;
      First       : Positive := 1;
      Has_In      : aliased short := 0; -- 1 - exlusive; 2 - mutiplexed
      Has_Out     : aliased short := 0;
      Buffer      : Element_Array (1..Size);
      pragma Atomic (Free);
      pragma Atomic (First);
      pragma Volatile (Buffer);
   end record;
   type FIFO_Data_Ptr is access all FIFO_Data;

   type Mutex_Ptr is access all Mutex'Class;
   type FIFO (Size : Positive) is abstract
      new Abstract_Shared_Object with
   record
      Not_Full  : Event_Ptr;
      Not_Empty : Event_Ptr;
      Data      : FIFO_Data_Ptr;
   end record;
   function Get_Signature (Object : FIFO) return Unsigned_16;
   function Get_Size (Queue : FIFO) return Storage_Count;
   procedure Map
             (  Queue    : in out FIFO;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
   type FIFO_In is new FIFO with null record;
   procedure Map
             (  Queue    : in out FIFO_in;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
   type FIFO_Multiplexed_In is new FIFO with record
      Lock : Mutex_Ptr;
   end record;
   procedure Map
             (  Queue    : in out FIFO_Multiplexed_In;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
   type FIFO_Out is new FIFO with null record;
   procedure Map
             (  Queue    : in out FIFO_Out;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
   type Universal_FIFO is new FIFO with record
      Mode : FIFO_End := In_End;
      Lock : Mutex_Ptr;
   end record;
   procedure Map
             (  Queue    : in out Universal_FIFO;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );

end Synchronization.Interprocess.Generic_FIFO;
