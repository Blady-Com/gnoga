--                                                                    --
--  package Storage_Streams         Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2011       --
--                                                                    --
--                                Last revision :  09:54 04 Feb 2017  --
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

with Ada.Finalization;
with Generic_Doubly_Linked;

package Storage_Streams is
--
-- Storage_Stream -- Stream allocated in the memory.
--
--    Block_Size - Memory block size
--
   type Storage_Stream
        (  Block_Size : Stream_Element_Count
        )  is new Root_Stream_Type with private;
--
-- Erase -- The stream
--
--    Stream - To flush
--
-- This procedure makes the stream empty.
--
   procedure Erase (Stream : in out Storage_Stream);
--
-- Get_Size -- The number of elements in the stream
--
--    Stream - The input stream
--
-- Returns :
--
--    How many stream items can be read
--
   function Get_Size (Stream : Storage_Stream)
      return Stream_Element_Count;
--
-- Overridden operations
--
   procedure Read
             (  Stream : in out Storage_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
   procedure Write
             (  Stream : in out Storage_Stream;
                Item   : Stream_Element_Array
             );
private
   package Stream_Element_Array_Lists is
      new Generic_Doubly_Linked (Stream_Element_Array);
   use Stream_Element_Array_Lists;
   use Doubly_Linked;

   type Controlled
        (  Stream : access Storage_Stream'Class
        )  is new Ada.Finalization.Limited_Controlled with null record;
   procedure Finalize (Object : in out Controlled);

   type Storage_Stream
        (  Block_Size : Stream_Element_Count
        )  is new Root_Stream_Type with
   record
      Size      : Stream_Element_Count  := 0;
      Out_Count : Stream_Element_Offset := 0;
      In_Count  : Stream_Element_Count;
      First     : List;
      Free      : List;
      Member    : Controlled (Storage_Stream'Access);
   end record;

end Storage_Streams;
