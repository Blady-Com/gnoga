--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Memory_Pools.                    Luebeck            --
--        Streams                                  Winter, 2014       --
--  Interface                                                         --
--                                Last revision :  18:00 18 Aug 2022  --
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
--  This  package  provides a stream interface to reading and allocating
--  in  memory  pools  resident  in  a blocking  access  file  based  on
--  Persistent_Array.
--
with Ada.Streams;  use Ada.Streams;

with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

package Persistent.Memory_Pools.Streams is
--
-- Input_Stream -- Stream to read from memory blocks
--
--    Pool - The memory pool
--
-- The  input  stream  can  be  set to an allocated memory block written
-- using Output_Stream.  When  reading  from  the  stream  the  internal
-- pointer  moves  through the allocated memory up to the last allocated
-- byte.
--
   type Input_Stream
        (  Pool : access Persistent_Pool'Class
        )  is new Root_Stream_Type with private;
--
-- Close -- Stream
--
--    Stream - Input stream
--
-- Bring stream into the state of a newly created object.
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Close (Stream : in out Input_Stream);
--
-- Compare -- Allocated memory to a sequence of stream elements
--
--    Left  - Input stream
--    Right - The sequence of elements or text to compare with
--
-- This function compares unread stream with Item/Text.  Unopened stream
-- is considered empty.
--
-- Returns :
--
--    Comparison result
--
-- Exceptions :
--
--    I/O exceptions
--
   function Compare
            (  Left  : Input_Stream;
               Right : Stream_Element_Array
            )  return Precedence;
   function Compare
            (  Left  : Input_Stream;
               Right : String
            )  return Precedence;
--
-- End_Of -- Check if the end of stream is reached
--
--    Stream - Input stream
--
-- Returns :
--
--    True if no data left in the stream
--
-- Exceptions :
--
--    Use_Error - No memory pool file open
--    I/O exceptions
--
   function End_Of (Stream : Input_Stream) return Boolean;
--
-- Equal -- Allocated memory to a sequence of stream elements
--
--    Stream    - Input stream
--    Item/Text - The sequence of elements or text to compare with
--
-- This function compares unread stream with Item/Text.  Unopened stream
-- is considered empty.
--
-- Returns :
--
--    True if Stream contents is equal to the second parameter
--
-- Exceptions :
--
--    I/O exceptions
--
   function Equal
            (  Stream : Input_Stream;
               Item   : Stream_Element_Array
            )  return Boolean;
   function Equal
            (  Stream : Input_Stream;
               Text   : String
            )  return Boolean;
--
-- Get_First -- Index of the first byte in the memory stream
--
--    Stream - Input stream
--
-- The returned  index  can be  used  later  to open the stream with the
-- procedure Open.
--
-- Returns :
--
--    The first byte index
--
-- Exceptions :
--
--    Use_Error - No memory pool file open or no stream open
--
   function Get_First (Stream : Input_Stream) return Byte_Index;
--
-- Get_Length -- Number of stream elements in the stream
--
--    Stream - Input stream
--
-- Returns :
--
--    Number of stream elements in the stream
--
   function Get_Length (Stream : Input_Stream)
      return Stream_Element_Count;
--
-- Get_Unread -- Number of stream elements in the stream to read
--
--    Stream - Input stream
--
-- Returns :
--
--    Number of stream not yet read elements in the stream
--
   function Get_Unread (Stream : Input_Stream)
      return Stream_Element_Count;
--
-- Less -- Allocated memory to a sequence of stream elements
--
--    Stream    - Input stream
--    Item/Text - The sequence of elements or text to compare with
--
-- This function compares unread stream with Item/Text.  Unopened stream
-- is considered empty.
--
-- Returns :
--
--    True if Stream contents is less than the second parameter
--
-- Exceptions :
--
--    I/O exceptions
--
   function Less
            (  Stream : Input_Stream;
               Item   : Stream_Element_Array
            )  return Boolean;
   function Less
            (  Stream : Input_Stream;
               Text   : String
            )  return Boolean;
--
-- Open -- Existing memory stream
--
--    Stream - Input stream
--    Index  - Of the first memory block of the stream
--
-- Exceptions :
--
--    Use_Error - No memory pool file open
--    I/O exceptions
--
   procedure Open
             (  Stream : in out Input_Stream;
                Index  : Byte_Index
             );
--
-- Read -- From the allocated memory
--
--    Stream - Input stream
--    Item   - Buffer to read into
--    Last   - Element read
--
-- This procedure fills  Item with the allocated memory bytes.  When end
-- of allocated memory stream is reached Last is  set  to a value lesser
-- than Item'Last.
--
-- Exceptions :
--
--    Use_Error - No memory pool file open
--    I/O exceptions
--
   procedure Read
             (  Stream : in out Input_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
--
-- Rewind -- Move the stream to the first byte to read
--
--    Stream - Input stream
--
   procedure Rewind (Stream : in out Input_Stream);
--
-- Write -- To the allocated memory
--
--    Stream - Input stream
--    Item   - Buffer to write into
--
-- Exceptions :
--
--    Use_Error - Input stream cannot be written
--
   procedure Write
             (  Stream : in out Input_Stream;
                Item   : Stream_Element_Array
             );
--
-- Look_Ahead -- Scan unread stream contents
--
--    User_Data_Type - Of the user data to pass along
--    Visitor_Type   - Of the visitor call-back
--
-- The procedure parameters are:
--
--    Stream    - Input stream
--    Visit     - The procedure called on data chunks
--    User_Data - To pass to the call-back
--
-- The procedure visit sets Continue  to True  if it wishes  to continue
-- scanning the stream. It sets it to False if stop the process.
--
   generic
      type User_Data_Type (<>) is limited private;
      type Visitor_Type is access procedure
           (  Contents  : Byte_Array;
              User_Data : in out User_Data_Type;
              Continue  : out Boolean
           );
   procedure Look_Ahead
             (  Stream    : Input_Stream'Class;
                Visit     : Visitor_Type;
                User_Data : in out User_Data_Type
             );
------------------------------------------------------------------------
-- Output_Stream -- Stream to write to memory blocks
--
--    Pool - The memory pool
--
-- The output stream when written allocates memory blocks  as necessary.
-- The blocks written can be read from using Input_Stream.
--
   type Output_Stream
        (  Pool : access Persistent_Pool'Class
        )  is new Root_Stream_Type with private;
--
-- Append -- Open an existing memory stream to append
--
--    Stream - Output stream
--    Index  - Of the first memory block of the stream
--
-- Newly written contents will be appended
--
-- Exceptions :
--
--    Use_Error - No memory pool file open
--    I/O exceptions
--
   procedure Append
             (  Stream : in out Output_Stream;
                Index  : Byte_Index
             );
--
-- Close -- Stream
--
--    Stream - Output stream
--
-- Disconnect the stream from memory and bring it into the  state  of  a
-- newly created object. When closed unused  memory  allocated  for  the
-- stream  is  freed.  The  stream  contents  can  be  read  back  using
-- Input_Stream.
--
-- Exceptions :
--
--    I/O exceptions
--
   procedure Close (Stream : in out Output_Stream);
--
-- Get_First -- Index of the first byte in the memory stream
--
--    Stream - Output stream
--
-- The returned  index  can be  used  later  to open the stream with the
-- procedure Open.
--
-- Returns :
--
--    The first byte index
--
-- Exceptions :
--
--    Use_Error - No memory pool file open or no stream open/written
--
   function Get_First (Stream : Output_Stream) return Byte_Index;
--
-- Get_Written -- Number of stream elements written
--
--    Stream - Output stream
--
-- Returns :
--
--    Number of elements written into the stream
--
   function Get_Written (Stream : Output_Stream)
      return Stream_Element_Count;
--
-- Open -- Existing memory stream
--
--    Stream - Output stream
--    Index  - Of the first memory block of the stream
--
-- The contents will be rewritten
--
-- Exceptions :
--
--    Use_Error - No memory pool file open
--    I/O exceptions
--
   procedure Open
             (  Stream : in out Output_Stream;
                Index  : Byte_Index
             );
--
-- Read -- From the allocated memory
--
--    Stream - Output stream
--    Item   - Buffer to read into
--    Last   - Element read
--
-- This procedure fills  Item with the allocated memory bytes.  When end
-- of allocated memory stream is reached Last is  set  to a value lesser
-- than Item'Last.
--
-- Exceptions :
--
--    Use_Error - Output stream cannot be read
--
   procedure Read
             (  Stream : in out Output_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
--
-- Write -- To the allocated memory
--
--    Stream - Output stream
--    Item   - Buffer to write into
--
-- This  procedure  allocates space in the memory pool as necessary. The
-- procedure can be used write a  new  stream  after  a  new  object  is
-- initialized. Get_First  can  be  used  to  identify  the  stream  for
-- re-opening it later.
--
-- Exceptions :
--
--    Use_Error - No memory pool file open
--    I/O exceptions
--
   procedure Write
             (  Stream : in out Output_Stream;
                Item   : Stream_Element_Array
             );
--
-- Tagging_Progress -- Tagging progress indicator
--
   subtype Advance is Float range 0.0..1.0;
   type Tagging_Progress is abstract new Object.Entity with null record;
--
-- Complete -- Progress indicator callback
--
--    State    - The progress indicator
--    Progress - The progress 0.0..1.0
--
   procedure Complete
             (  State    : in out Tagging_Progress;
                Progress : Advance
             );
--
-- Start -- Starting callback
--
--    State - The progress indicator
--
-- This procedure is called at the beginning of tagging.
--
   procedure Start (State : in out Tagging_Progress);
--
-- Stop -- Stopping callback
--
--    State - The progress indicator
--
-- This procedure is called at the end of tagging.
--
   procedure Stop  (State : in out Tagging_Progress);

private
   pragma Assert (Stream_Element'Last = 255);

   Bad_Interval   : constant String := "Invalid interval of keys";
   Bad_Key_Index  : constant String := "Key with illegal item index";
   Bad_Left_Index : constant String := "Left to illegal item index";
   Bad_Next_Index : constant String := "Next with illegal item index";
   Bad_Prev_Index : constant String := "Previous to illegal item index";
   Bad_Righ_Index : constant String := "Right to illegal item index";
   Bad_Row        : constant String := "No row";
   Bad_Val_Index  : constant String := "Value with illegal item index";
   Empty_Tree     : constant String := "Waveform is empty";
   Invalid_Item   : constant String := "No item";
   Key_In_Use     : constant String := "The key is already in use";
   No_Node_Insert : constant String := "Inserting at null node";
   No_Right       : constant String := "No right point to interpolate";
   Not_Found      : constant String := "Not found";
   Not_Empty      : constant String := "Not empty";
   Null_Replace   : constant String := "Replacing an non-item";
   Reinsert_Error : constant String := "Re-inserting a key";
   Remove_Error   : constant String := "Removing an item with "        &
                                       "illegal index";
   Replace_Error  : constant String := "Replacing an item with wrong " &
                                       "index";
   Tag_Error      : constant String := "Not tagged";

   type Unchecked_Input_Stream
        (  Pool : access Persistent_Pool'Class
        )  is new Root_Stream_Type with
   record
      Start   : Byte_Index := 0; -- First memory block of the strem
      Current : Byte_Index;      -- Current memory block
      Offset  : Block_Offset;    -- Next byte to read/write in the block
      Next    : Block_Offset;    -- Next unused byte in the block
   end record;
   procedure Close (Stream : in out Unchecked_Input_Stream);
   function Count
            (  Stream : Unchecked_Input_Stream;
               Index  : Byte_Index;
               Length : Block_Offset
            )  return Stream_Element_Count;
   function End_Of (Stream : Unchecked_Input_Stream) return Boolean;
   function Get_First
            (  Stream : Unchecked_Input_Stream
            )  return Byte_Index;
   function Get_Length
            (  Stream : Unchecked_Input_Stream
            )  return Stream_Element_Count;
   function Get_Unread
            (  Stream : Unchecked_Input_Stream
            )  return Stream_Element_Count;
   procedure Open
             (  Stream : in out Unchecked_Input_Stream;
                Index  : Byte_Index
             );
   procedure Read
             (  Stream : in out Unchecked_Input_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Rewind (Stream : in out Unchecked_Input_Stream);
   procedure Write
             (  Stream : in out Unchecked_Input_Stream;
                Item   : Stream_Element_Array
             );

   type Input_Stream
        (  Pool : access Persistent_Pool'Class
        )  is new Unchecked_Input_Stream (Pool) with null record;

   type Unchecked_Output_Stream
        (  Pool : access Persistent_Pool'Class
        )  is new Root_Stream_Type with
   record
      Start   : Byte_Index := 0; -- First memory block of the strem
      Current : Byte_Index;      -- Current memory block
      Offset  : Block_Offset;    -- Next byte to read/write in the block
      Next    : Block_Offset;    -- Next unused byte in the block
   end record;
   procedure Append
             (  Stream : in out Unchecked_Output_Stream;
                Index  : Byte_Index
             );
   procedure Close (Stream : in out Unchecked_Output_Stream);
   function Count
            (  Stream : Unchecked_Output_Stream;
               Index  : Byte_Index;
               Next   : Block_Offset
            )  return Stream_Element_Count;
   function Get_First
            (  Stream : Unchecked_Output_Stream
            )  return Byte_Index;
   function Get_Written
            (  Stream : Unchecked_Output_Stream
            )  return Stream_Element_Count;
   procedure Open
             (  Stream : in out Unchecked_Output_Stream;
                Index  : Byte_Index
             );
   procedure Read
             (  Stream : in out Unchecked_Output_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Write
             (  Stream : in out Unchecked_Output_Stream;
                Item   : Stream_Element_Array
             );

   type Output_Stream
        (  Pool : access Persistent_Pool'Class
        )  is new Unchecked_Output_Stream (Pool) with null record;

end Persistent.Memory_Pools.Streams;
