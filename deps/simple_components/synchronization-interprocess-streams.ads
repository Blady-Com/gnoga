--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Synchronization.Interprocess.               Luebeck            --
--     Streams                                     Spring, 2018       --
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
--  Interprocess stream allows the process holding  the  in-end  writing
--  the stream and the process holding the out-end  reading  it.  Stream
--  read  and write are lock free unless the stream is empty or full. In
--  the latter case the  operation  is  blocked  until  the  counterpart
--  supplies new stream elements or takes elements from it:
--
--     ._____________________.        .______________________.
--     |                     |        |                      |
--     |   Provider process  |        |  Consumer process    |
--     |           ._____________________________.           |
--     |           |                             |           |
--     | Write --->| In-end >> Stream >> Out-end |---> Read  |
--     |           |_____________________________|           |
--     |                     |        |                      |
--     |                     |        |                      |
--     |_____________________|        |______________________|
--
with Synchronization.Interprocess.Events;
use  Synchronization.Interprocess.Events;

package Synchronization.Interprocess.Streams is
--
-- Interprocess_Stream -- A stream
--
--    Size - The maximal number of elements in the stream - 1
--
   type Interprocess_Stream (Size : Stream_Element_Count) is
      abstract new Root_Stream_Type with private;
--
-- Finalize -- Destruction
--
--    Stream - The stream
--
   procedure Finalize (Stream : in out Interprocess_Stream);
--
-- Close -- The stream
--
--    Stream - The stream
--
-- When  the stream is closed  and is empty then reading from the stream
-- ends without waiting for new elements.  Writing into a closed  stream
-- propagates End_Error.
--
-- Exceptions :
--
--    Status_Error - The stream is not initialized
--
   procedure Close (Stream : in out Interprocess_Stream);
--
-- End_Of_Stream -- End of stream reached
--
--    Stream - The stream
--
-- The stream end is set by calling the procedure Close.  It is a way to
-- stop reading from  the stream at the writer  side when the reader has
-- no information when to stop.
--
-- Returns :
--
--    True of the end of stream is reached
--
-- Exceptions :
--
--    Status_Error - The stream is not initialized
--
   function End_Of_Stream (Stream : Interprocess_Stream) return Boolean;
--
-- Get_Offset -- The offset in the shared memory
--
--    Stream - The stream
--
-- Returns :
--
--    The offset to the object data in the shared memory
--
   function Get_Offset
            (  Stream : Interprocess_Stream
            )  return Storage_Offset is abstract;
--
-- Get_Timeout -- The I/O timeout
--
--    Stream - The stream
--
-- Returns :
--
--    The timeout used in the Seize
--
   function Get_Timeout (Stream : Interprocess_Stream) return Duration;
--
-- Is_Empty -- Stream buffer state
--
--    Stream - The stream
--
-- Returns :
--
--    True of the stream is empty
--
-- Exceptions :
--
--    Status_Error - The stream is not initialized
--
   function Is_Empty (Stream : Interprocess_Stream) return Boolean;
--
-- Is_Full -- Stream buffer state
--
--    Stream - The stream
--
-- Returns :
--
--    True of the stream is full
--
-- Exceptions :
--
--    Status_Error - The stream is not initialized
--
   function Is_Full (Stream : Interprocess_Stream) return Boolean;
--
-- Is_Closed -- Check if the stream is closed
--
--    Stream - The stream
--
-- Returns :
--
--    True of the stream is closed
--
-- Exceptions :
--
--    Status_Error - The stream is not initialized
--
   function Is_Closed (Stream : Interprocess_Stream) return Boolean;
--
-- Open -- The stream
--
--    Stream - The stream
--
-- Re-open the stream previously closed by Close.
--
-- Exceptions :
--
--    Status_Error - The stream is not initialized
--
   procedure Open (Stream : in out Interprocess_Stream);
--
-- Set_Timeout -- The I/O timeout
--
--    Stream  - The stream
--    Timeout - The timeout to be used in the Seize
--
   procedure Set_Timeout
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration
             );
--
-- Skip -- Elements in the stream
--
--    Stream    - The stream
--    Count     - The number of elements to skip
--  [ Timeout ] - The timeout
--
-- This procedure blocks the same way Read does.  Count is  decreased by
-- number of elements actually skipped.
--
-- Exceptions :
--
--    Status_Error - The stream is not initialized
--
   procedure Skip
             (  Stream  : in out Interprocess_Stream;
                Count   : in out Stream_Element_Count
             );
   procedure Skip
             (  Stream  : in out Interprocess_Stream;
                Count   : in out Stream_Element_Count;
                Timeout : Duration
             );
--
-- Wait_For_Not_Empty -- Wait for data in the stream
--
--    Stream  - The stream
--    Timeout - The timeout
--  [ Empty ] - Set to True if timeout was expired
--
-- This procedure must be interlocked with stream reading
--
-- Exceptions :
--
--    Status_Error  - The stream is not initialized
--    Timeout_Error - The timeout is expired (only if Empty is absent)
--
   procedure Wait_For_Not_Empty
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration
             );
   procedure Wait_For_Not_Empty
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration;
                Empty   : out Boolean
             );
--
-- Wait_For_Not_Full -- Wait for free space in the stream
--
--    Stream  - The stream
--    Timeout - The timeout
--  [ Full ]  - Set to True if timeout was expired
--
-- This procedure must be interlocked with stream writing
--
-- Exceptions :
--
--    Status_Error  - The stream is not initialized
--    Timeout_Error - The timeout is expired (only if Full is absent)
--
   procedure Wait_For_Not_Full
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration
             );
   procedure Wait_For_Not_Full
             (  Stream  : in out Interprocess_Stream;
                Timeout : Duration;
                Full    : out Boolean
             );
------------------------------------------------------------------------
--
-- Input_Stream -- Input stream
--
-- The  stream  can  be  read  by  a  single  process and task. The read
-- operation  awaits  for  complete buffer read. The stream requires two
-- events. These must be placed in the shared environment object  before
-- the stream. For example:
--
--     type Shared is new Abstract_Shared_Environment with record
--        Not_Full_Event  : Event;  <------ Set --------.
--        Not_Empty_Event : Event;  <----- Reset --------\
--        Stream          : Input_Stream; -- Uses these _/
--     end record;
--
   type Input_Stream is new Interprocess_Stream with private;
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Input_Stream
             );
   for Input_Stream'Write use Enumerate;
--
-- Write -- Into the string
--
--    Stream - The stream
--    Item   - The buffer to write
--
-- This procedure raises Mode_Error.
--
   procedure Write
             (  Stream : in out Input_Stream;
                Item   : Stream_Element_Array
             );
------------------------------------------------------------------------
--
-- Output_Stream -- Output stream
--
-- The  stream  can  be  written by a single process and task. The write
-- operation awaits for complete buffer written. The stream requires two
-- events. These must be placed in the shared environment object  before
-- the stream. For example:
--
--     type Shared is new Abstract_Shared_Environment with record
--        Not_Full_Event  : Event;  <------ Reset -------.
--        Not_Empty_Event : Event;  <------- Set ---------\
--        Stream          : Output_Stream; -- Uses these _/
--     end record;
--
   type Output_Stream is new Interprocess_Stream with private;
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Output_Stream
             );
   for Output_Stream'Write use Enumerate;
--
-- Read -- From the stream
--
--    Stream - The stream
--    Item   - The buffer to read into
--    Last   - The last written element
--
-- This procedure raises Mode_Error.
--
   procedure Read
             (  Stream : in out Output_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
--
-- Skip -- Elements in the stream
--
--    Stream    - The stream
--    Count     - The number of elements to skip
--  [ Timeout ] - The timeout
--
-- This procedure raises Mode_Error.
--
   procedure Skip
             (  Stream : in out Output_Stream;
                Count  : in out Stream_Element_Count
             );
   procedure Skip
             (  Stream  : in out Output_Stream;
                Count   : in out Stream_Element_Count;
                Timeout : Duration
             );
------------------------------------------------------------------------
--
-- Universal_Stream -- A stream that can serve both ends
--
   type Stream_End is
        (  In_End,
           Multiplexed_In_End, -- Allow many in-ends
           Out_End,
           Multiplexed_Out_End -- Allow many out-ends
        );
   type Universal_Stream is new Interprocess_Stream with private;
   procedure Enumerate
             (  Stream : access Root_Stream_Type'Class;
                Object : Universal_Stream
             );
   for Universal_Stream'Write use Enumerate;
--
-- Get_Mode -- Get the stream mode
--
--    Stream - The queue
--
-- Returns :
--
--    THe stream end mode
--
   function Get_Mode (Stream : Universal_Stream) return Stream_End;
--
-- Set_Mode -- Set the stream mode
--
--    Stream - The queue
--    Mode   - The desired end mode
--
-- The mode can be set before creation or opening the environment only.
--
-- Exceptions :
--
--    Mode_Error - The stream is already initialized
--
   procedure Set_Mode
             (  Stream : in out Universal_Stream;
                Mode   : Stream_End
             );
private
   type Event_Ptr is access all Event'Class;
   type Element_Array is array (Positive range <>) of Stream_Element;
   type Stream_Data (Size : Positive) is record
      Free    : Positive := 1;
      First   : Positive := 1;
      Closed  : Boolean  := False;
      Has_In  : aliased short := 0;
      Has_Out : aliased short := 0;
      Buffer  : Element_Array (1..Size);
      pragma Atomic (Closed);
      pragma Atomic (Free);
      pragma Atomic (First);
      pragma Volatile (Buffer);
   end record;
   type Stream_Data_Ptr is access all Stream_Data;

   type Shared_Object is abstract
      new Abstract_Shared_Object with null record;
   function Get_Signature (Object : Shared_Object) return Unsigned_16;

   type Shared_Input_Object
        (  Stream : access Input_Stream'Class
        )  is new Shared_Object with null record;
   function Get_Size (Object : Shared_Input_Object)
      return Storage_Count;
   procedure Map
             (  Object   : in out Shared_Input_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
   type Shared_Output_Object
        (  Stream : access Output_Stream'Class
        )  is new Shared_Object with null record;
   function Get_Size (Object : Shared_Output_Object)
      return Storage_Count;
   procedure Map
             (  Object   : in out Shared_Output_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );
   type Shared_Universal_Object
        (  Stream : access Universal_Stream'Class
        )  is new Shared_Object with null record;
   function Get_Size (Object : Shared_Universal_Object)
      return Storage_Count;
   procedure Map
             (  Object   : in out Shared_Universal_Object;
                Shared   : in out Abstract_Shared_Environment'Class;
                Location : System.Address;
                Size     : Storage_Count;
                Owner    : Boolean
             );

   type Interprocess_Stream (Size : Stream_Element_Count) is
      abstract new Root_Stream_Type with
   record
      Not_Full  : Event_Ptr;
      Not_Empty : Event_Ptr;
      Timeout   : Duration := 10.0;
      Data      : Stream_Data_Ptr;
   end record;
   procedure Read
             (  Stream : in out Interprocess_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Write
             (  Stream : in out Interprocess_Stream;
                Item   : Stream_Element_Array
             );

   type Input_Stream is new Interprocess_Stream with record
      Object : aliased Shared_Input_Object
                       (  Input_Stream'Unchecked_Access
                       );
   end record;
   function Get_Offset
            (  Stream : Input_Stream
            )  return Storage_Offset;

   type Output_Stream is new Interprocess_Stream with record
      Object : aliased Shared_Output_Object
                       (  Output_Stream'Unchecked_Access
                       );
   end record;
   function Get_Offset
            (  Stream : Output_Stream
            )  return Storage_Offset;

   type Universal_Stream is new Interprocess_Stream with record
      Object : aliased Shared_Universal_Object
                       (  Universal_Stream'Unchecked_Access
                       );
      Mode : Stream_End := In_End;
   end record;
   procedure Finalize (Stream : in out Universal_Stream);
   function Get_Offset
            (  Stream : Universal_Stream
            )  return Storage_Offset;
   procedure Read
             (  Stream : in out Universal_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Skip
             (  Stream : in out Universal_Stream;
                Count  : in out Stream_Element_Count
             );
   procedure Skip
             (  Stream  : in out Universal_Stream;
                Count   : in out Stream_Element_Count;
                Timeout : Duration
             );

   procedure Write
             (  Stream : in out Universal_Stream;
                Item   : Stream_Element_Array
             );

end Synchronization.Interprocess.Streams;
