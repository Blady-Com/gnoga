--                                                                    --
--  package Block_Streams           Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2011       --
--                                                                    --
--                                Last revision :  10:35 22 Oct 2011  --
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
--  Block stream is a stream formatted as a  sequence  of  blocks.  Each
--  block  has  fixed  size, a sequence number and the check sum. Blocks
--  are read and written into the transport stream. Upon the  stream  of
--  blocks the interface stream  is  defined  that  verifies  the  block
--  numbers,  their  check  sums and exposes the block contents as a new
--  stream.
--
--         ._________________________________________.
--         |                                         |
--         |            Data stream                  |
--         |_________________________________________|
--         .__________. .__________.     .___________.
--         |          | |          |     |           |
--         |  Block 0 | |  Block 1 | ... |  Block n  |
--         |__________| |__________|     |___________|
--         ._________________________________________.
--         |                                         |
--         |   Transport stream of stream elements   |
--         |_________________________________________|
--
with Ada.Streams;  use Ada.Streams;
with Interfaces;   use Interfaces;

with Ada.Finalization;

package Block_Streams is
--
-- Input_Block_Stream -- Stream  reads blocks from a stream. Blocks have
--                       sequence numbers and supplied  with  the  check
--                       sum.
--
--    Transport - The transport stream carrying the blocks
--
   type Input_Block_Stream
        (  Transport : access Root_Stream_Type'Class;
           Size      : Stream_Element_Count
        )  is new Root_Stream_Type with private;
--
-- Get_Block_No -- Current block number
--
--    Stream - The input stream
--
-- Returns :
--
--    The block number being read 1..
--
   function Get_Block_No
            (  Stream : Input_Block_Stream
            )  return Unsigned_32;
--
-- Get_Element_No -- Current stream element number
--
--    Stream - The input stream
--
-- Returns :
--
--    The number of the stream element to read (within the block) 1..
--
   function Get_Element_No
            (  Stream : Input_Block_Stream
            )  return Stream_Element_Count;
--
-- Skip -- Resynchronize the stream
--
--    Stream - To skip
--
-- This  procedure  is called to resynchronize the input stream with the
-- block of the sequence number 0.
--
-- Exceptions :
--
--    Any - I/O error
--
   procedure Skip (Stream : in out Input_Block_Stream);
--
-- Output_Block_Stream -- Stream  writes  blocks  onto  a stream. Blocks
--                        have sequence numbers and  supplied  with  the
--                        check sum.
--
--    Transport - The transport stream carrying the blocks
--
   type Output_Block_Stream
        (  Transport : access Root_Stream_Type'Class;
           Size      : Stream_Element_Count
        )  is new Root_Stream_Type with private;
--
-- Flush -- Write the last block onto the stream
--
--    Stream - To flush
--
-- This procedure is called to write the last  block  if  there  is  any
-- pending  output. After that Write can be used again starting with the
-- sequence number 0.
--
-- Exceptions :
--
--    Any - I/O error
--
   procedure Flush (Stream : in out Output_Block_Stream);
--
-- Get_Block_No -- Current block number
--
--    Stream - The output stream
--
-- Returns :
--
--    The block number being written 1..
--
   function Get_Block_No
            (  Stream : Output_Block_Stream
            )  return Unsigned_32;
--
-- Get_Element_No -- Current stream element number
--
--    Stream - The output stream
--
-- Returns :
--
--    The number of the stream element to write (within the block) 1..
--
   function Get_Element_No
            (  Stream : Output_Block_Stream
            )  return Stream_Element_Count;
--
-- Overridden operations
--
   procedure Read
             (  Stream : in out Input_Block_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Read
             (  Stream : in out Output_Block_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Write
             (  Stream : in out Input_Block_Stream;
                Item   : Stream_Element_Array
             );
   procedure Write
             (  Stream : in out Output_Block_Stream;
                Item   : Stream_Element_Array
             );
private
   type Input_Block_Stream
        (  Transport : access Root_Stream_Type'Class;
           Size      : Stream_Element_Count
        )  is new Root_Stream_Type with
   record
      No     : Unsigned_32 := 0;
      Index  : Stream_Element_Offset := Size - 6 + 1;
      Buffer : Stream_Element_Array (1..Size);
   end record;

   type Controlled
        (  Output : access Output_Block_Stream'Class
        )  is new Ada.Finalization.Limited_Controlled with null record;
   procedure Finalize (Object : in out Controlled);

   type Output_Block_Stream
        (  Transport : access Root_Stream_Type'Class;
           Size      : Stream_Element_Count
        )  is new Root_Stream_Type with
   record
      No     : Unsigned_32 := 0;
      Index  : Stream_Element_Offset := 1;
      Buffer : Stream_Element_Array (1..Size);
      Member : Controlled (Output_Block_Stream'Access);
   end record;

end Block_Streams;
