--                                                                    --
--  package Pipe_Streams            Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2022       --
--                                                                    --
--                                Last revision :  10:00 19 May 2022  --
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

with Ada.Streams;  use Ada.Streams;

package Pipe_Streams is
--
-- Pipe_Stream -- Stream acting as a pipe
--
--    Size - The buffer size
--
-- The stream can be read and written.  The reader is blocked when there
-- is no data available  to read.  Similarly the  writer is blocked when
-- there is no space available to write. Each side of the  stream can be
-- closed  to propagate  End_Error  on  an  attempt  to  read  or  write
-- correspondingly. Also the stream can set into the sink mode to accept
-- and ignoring all writing.
--
   type Pipe_Stream
        (  Size : Stream_Element_Count
        )  is new Root_Stream_Type with private;
--
-- Erase -- The stream
--
--    Stream - To erase
--
-- This procedure makes the stream empty and available to read/write.
--
   procedure Erase (Stream : in out Pipe_Stream);
--
-- Available_To_Read -- The number of elements in the stream
--
--    Stream - The stream
--
-- Returns :
--
--    How many stream items can be read without blocking
--
   function Available_To_Read (Stream : Pipe_Stream)
      return Stream_Element_Count;
--
-- Available_To_Write -- The number of free elements in the stream
--
--    Stream - The stream
--
-- Returns :
--
--    How many stream items can be written without blocking
--
   function Available_To_Write (Stream : Pipe_Stream)
      return Stream_Element_Count;
--
-- Close_Read -- Close the read end of the stream
--
--    Stream - The stream
--
-- This procedure closes  the read stream end.  Reading from  the stream
-- when empty will propagate End_Error.
--
   procedure Close_Read (Stream : in out Pipe_Stream);
--
-- Close_Write -- Close the write end of the stream
--
--    Stream - The stream
--
-- This procedure closes  the write stream end.  Writing into the stream
-- will propagate End_Error.
--
   procedure Close_Write (Stream : in out Pipe_Stream);
--
-- Is_Empty -- Empty check
--
--    Stream - The stream
--
-- Returns :
--
--    True if there is no data ready to read from the stream
--
   function Is_Empty (Stream : Pipe_Stream) return Boolean;
--
-- Is_Full -- Full check
--
--    Stream - The stream
--
-- Returns :
--
--    True if there is no space to write
--
   function Is_Full (Stream : Pipe_Stream) return Boolean;
--
-- Is_Read_Closed -- Check if reading from the stream is closed
--
--    Stream - The stream
--
-- Returns :
--
--    True if reading will propagate End_Error
--
   function Is_Read_Closed (Stream : Pipe_Stream) return Boolean;
--
-- Is_Sink -- Check if writing is in the sink mode
--
--    Stream - The stream
--
-- Returns :
--
--    True if writing is ignored
--
   function Is_Sink (Stream : Pipe_Stream) return Boolean;
--
-- Is_Write_Closed -- Check if writing into the stream is closed
--
--    Stream - The stream
--
-- Returns :
--
--    True if writing will propagate End_Error
--
   function Is_Write_Closed (Stream : Pipe_Stream) return Boolean;
--
-- Sink -- Set writing into the sink mode
--
--    Stream - The stream
--
-- This procedure  sets the stream  in the sink mode.  All newly written
-- data are accepted and ignored.
--
   procedure Sink (Stream : in out Pipe_Stream);
--
-- Overridden operations
--
   procedure Read
             (  Stream : in out Pipe_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Write
             (  Stream : in out Pipe_Stream;
                Item   : Stream_Element_Array
             );
private
   type In_End_State  is (Closed, Full, Writable, Sink);
   type Out_End_State is (Closed, Empty, Readable);

   protected type Pipe_Object (Size : Stream_Element_Count) is
      function Available_To_Read  return Stream_Element_Count;
      function Available_To_Write return Stream_Element_Count;
      procedure Close_Read;
      procedure Close_Write;
      function Get_In   return In_End_State;
      function Get_Out  return Out_End_State;
      function Is_Empty return Boolean;
      function Is_Full  return Boolean;
      procedure Erase;
      procedure Sink_Write;
      entry Read
            (  Data : in out Stream_Element_Array;
               Last : out Stream_Element_Offset
            );
      entry Write
            (  Data : Stream_Element_Array;
               Last : out Stream_Element_Offset
            );
   private
      In_End    : In_End_State          := Writable;
      Out_End   : Out_End_State         := Empty;
      Out_Index : Stream_Element_Offset := 1;
      In_Index  : Stream_Element_Offset := 1;
      Buffer    : Stream_Element_Array (1.. Size);
   end Pipe_Object;

   type Pipe_Stream
        (  Size : Stream_Element_Count
        )  is new Root_Stream_Type with
   record
      Pipe : Pipe_Object (Size);
   end record;

end Pipe_Streams;
