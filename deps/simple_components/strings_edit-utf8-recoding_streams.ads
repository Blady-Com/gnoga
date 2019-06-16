--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Recoding_Streams          Luebeck            --
--  Interface                                      Autumn, 2018       --
--                                                                    --
--                                Last revision :  12:27 04 Nov 2018  --
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
--  The package provides recoding streams:
--
--    .________________.               .______________.
--    |                |               |              |
--    | Encoded stream | -- Decode --> | UTF-8 stream | ---> Read
--    |                | <- Encode --  |              | <-- Write
--    |________________|               |______________|
--
-- Reading.  Reading  from the stream  causes  reading from  the encoded
-- stream.  The obtained  octets  are decoded according  to the encoding
-- method and then  recoded into an UTF-8 stream.  The result  delivered
-- to the reader.
--
-- Writing.  Written stream elements  are considered UTF-8 encoded.  The
-- corresponding code  points are encoded according to the encoding used
-- by the encoded stream and then are written into it.
--
with Ada.Streams;  use Ada.Streams;

package Strings_Edit.UTF8.Recoding_Streams is
--
-- Encoding_Type -- Encoding method
--
   type Encoding_Type is
        (  ISO_8859_1,   -- ISO 8859-1  (Latin-1)
           ISO_8859_2,   -- ISO 8859-2  (Latin-2)
           ISO_8859_3,   -- ISO 8859-3  (Latin-3)
           ISO_8859_4,   -- ISO 8859-4  (Latin-4)
           ISO_8859_5,   -- ISO 8859-5  (Cyrillic)
           ISO_8859_6,   -- ISO 8859-6  (Arabic)
           ISO_8859_7,   -- ISO 8859-7  (Greek)
           ISO_8859_8,   -- ISO 8859-8  (Hebrew)
           ISO_8859_9,   -- ISO 8859-9  (Turkish)
           ISO_8859_10,  -- ISO 8859-10 (Latin-6)
           Windows_1250, -- Windows-1250
           Windows_1251, -- Windows-1251
           Windows_1252, -- Windows-1252
           Windows_1253, -- Windows-1253
           Windows_1254, -- Windows-1254
           Windows_1255, -- Windows-1255
           Windows_1256, -- Windows-1256
           Windows_1257, -- Windows-1257
           Windows_1258, -- Windows-1258
           KOI8,         -- KOI8
           MacOS_Roman   -- Mac OS Roman
        );
--
-- Recoding_Stream -- A stream that recodes the underlying stream
--
--    Encoded          - The stream in an encoding specified by Method
--    Method           - The encoding method of the stream
--    Decoding_Default - The code point to represent invalid characters
--    Encoding_Default - To use for unsupported code points
--
   type Recoding_Stream
        (  Encoded          : access Root_Stream_Type'Class;
           Method           : Encoding_Type;
           Decoding_Default : Code_Point;
           Encoding_Default : Character
        )  is new Root_Stream_Type with private;
--
-- Read -- Overrides Ada.Streams...
--
   procedure Read
             (  Stream : in out Recoding_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
--
-- Write -- Overrides Ada.Streams...
--
-- Exceptions :
--
--    Data_Error - Invalid UTF-8 input
--
   procedure Write
             (  Stream : in out Recoding_Stream;
                Item   : Stream_Element_Array
             );

private
   type Recoding_Stream
        (  Encoded          : access Root_Stream_Type'Class;
           Method           : Encoding_Type;
           Decoding_Default : Code_Point;
           Encoding_Default : Character
        )  is new Root_Stream_Type with
   record
      Input_Data    : String (1..4);
      Output_Data   : String (1..4);
      Input_Length  : Integer := 0;
      Input_Pointer : Integer := 1;
      Output_Length : Integer := 0;
   end record;

end Strings_Edit.UTF8.Recoding_Streams;
