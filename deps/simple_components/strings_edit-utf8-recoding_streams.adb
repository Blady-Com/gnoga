--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Recoding_Streams          Luebeck            --
--  Implementation                                 Autumn, 2018       --
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
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

with Strings_Edit.UTF8.ISO_8859_2;
with Strings_Edit.UTF8.ISO_8859_3;
with Strings_Edit.UTF8.ISO_8859_4;
with Strings_Edit.UTF8.ISO_8859_5;
with Strings_Edit.UTF8.ISO_8859_6;
with Strings_Edit.UTF8.ISO_8859_7;
with Strings_Edit.UTF8.ISO_8859_8;
with Strings_Edit.UTF8.ISO_8859_9;
with Strings_Edit.UTF8.ISO_8859_10;
with Strings_Edit.UTF8.KOI8;
with Strings_Edit.UTF8.MacOS_Roman;
with Strings_Edit.UTF8.Windows_1250;
with Strings_Edit.UTF8.Windows_1251;
with Strings_Edit.UTF8.Windows_1252;
with Strings_Edit.UTF8.Windows_1253;
with Strings_Edit.UTF8.Windows_1254;
with Strings_Edit.UTF8.Windows_1255;
with Strings_Edit.UTF8.Windows_1256;
with Strings_Edit.UTF8.Windows_1257;
with Strings_Edit.UTF8.Windows_1258;

package body Strings_Edit.UTF8.Recoding_Streams is
   use UTF8.ISO_8859_2;
   use UTF8.ISO_8859_3;
   use UTF8.ISO_8859_4;
   use UTF8.ISO_8859_5;
   use UTF8.ISO_8859_6;
   use UTF8.ISO_8859_7;
   use UTF8.ISO_8859_8;
   use UTF8.ISO_8859_9;
   use UTF8.ISO_8859_10;
   use UTF8.MacOS_Roman;
   use UTF8.KOI8;
   use UTF8.Windows_1250;
   use UTF8.Windows_1251;
   use UTF8.Windows_1252;
   use UTF8.Windows_1253;
   use UTF8.Windows_1254;
   use UTF8.Windows_1255;
   use UTF8.Windows_1256;
   use UTF8.Windows_1257;
   use UTF8.Windows_1258;

--     function Image (Data : String) return String is
--        Result  : String (1..Data'Length * 3);
--        Pointer : Integer := 1;
--     begin
--        for Index in Data'Range loop
--           if Index > Data'First then
--              Result (Pointer) := ' ';
--              Pointer := Pointer + 1;
--           end if;
--           Put
--           (  Destination => Result,
--              Pointer     => Pointer,
--              Value       => Character'Pos (Data (Index)),
--              Field       => 2,
--              Fill        => '0',
--              Justify     => Right,
--              Base        => 16
--           );
--        end loop;
--        return Result (1..Pointer - 1);
--     end Image;

   procedure Read
             (  Stream : in out Recoding_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
      Data    : String  renames Stream.Input_Data;
      Pointer : Integer renames Stream.Input_Pointer;
      Length  : Integer renames Stream.Input_Length;
      Code    : Code_Point;
      This    : Character;
   begin
      Last := Item'First - 1;
      while Last < Item'Last loop
         if Pointer > Length then
            Character'Read (Stream.Encoded, This);
            case Stream.Method is
               when ISO_8859_1 =>
                  Code := Character'Pos (This);
               when ISO_8859_2 =>
                  Code :=
                     From_ISO_8859_2 (This, Stream.Decoding_Default);
               when ISO_8859_3 =>
                  Code :=
                     From_ISO_8859_3 (This, Stream.Decoding_Default);
               when ISO_8859_4 =>
                  Code :=
                     From_ISO_8859_4 (This, Stream.Decoding_Default);
               when ISO_8859_5 =>
                  Code :=
                     From_ISO_8859_5 (This, Stream.Decoding_Default);
               when ISO_8859_6 =>
                  Code :=
                     From_ISO_8859_6 (This, Stream.Decoding_Default);
               when ISO_8859_7 =>
                  Code :=
                     From_ISO_8859_7 (This, Stream.Decoding_Default);
               when ISO_8859_8 =>
                  Code :=
                     From_ISO_8859_8 (This, Stream.Decoding_Default);
               when ISO_8859_9 =>
                  Code :=
                     From_ISO_8859_9 (This, Stream.Decoding_Default);
               when ISO_8859_10 =>
                  Code :=
                     From_ISO_8859_10 (This, Stream.Decoding_Default);
               when KOI8 =>
                  Code := From_KOI8 (This);
               when MacOS_Roman =>
                  Code := From_MacOS_Roman (This);
               when Windows_1250 =>
                  Code :=
                     From_Windows_1250 (This, Stream.Decoding_Default);
               when Windows_1251 =>
                  Code :=
                     From_Windows_1251 (This, Stream.Decoding_Default);
               when Windows_1252 =>
                  Code :=
                     From_Windows_1252 (This, Stream.Decoding_Default);
               when Windows_1253 =>
                  Code :=
                     From_Windows_1253 (This, Stream.Decoding_Default);
               when Windows_1254 =>
                  Code :=
                     From_Windows_1254 (This, Stream.Decoding_Default);
               when Windows_1255 =>
                  Code :=
                     From_Windows_1255 (This, Stream.Decoding_Default);
               when Windows_1256 =>
                  Code := From_Windows_1256 (This);
               when Windows_1257 =>
                  Code :=
                     From_Windows_1257 (This, Stream.Decoding_Default);
               when Windows_1258 =>
                  Code :=
                     From_Windows_1258 (This, Stream.Decoding_Default);
            end case;
            Length := 1;
            Put (Data, Length, Code);
            Pointer := 1;
            Length  := Length - 1;
         end if;
         Last  := Last + 1;
         Item (Last) := Character'Pos (Data (Pointer));
         Pointer := Pointer + 1;
      end loop;
   end Read;

   procedure Write
             (  Stream : in out Recoding_Stream;
                Item   : Stream_Element_Array
             )  is
      Data    : String  renames Stream.Output_Data;
      Length  : Integer renames Stream.Output_Length;
      Code    : Code_Point;
   begin
      for Index in Item'Range loop
         if Length = Data'Last then
            raise Data_Error;
         end if;
         Length := Length + 1;
         Data (Length) := Character'Val (Item (Index));
         case Character'Pos (Data (Length)) is
            when 0..16#7F# | 16#80#..16#BF# =>
               declare
                  Pointer : Integer := 1;
               begin
                  Get (Data (1..Length), Pointer, Code);
                  if Pointer <= Length then
                     raise Data_Error;
                  end if;
               end;
               Length := 0;
               case Stream.Method is
                  when ISO_8859_1 =>
                     if Code <= 255 then
                        Character'Write
                        (  Stream.Encoded,
                           Character'Val (Code)
                        );
                     else
                        Character'Write
                        (  Stream.Encoded,
                           Stream.Encoding_Default
                        );
                     end if;
                  when ISO_8859_2 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_ISO_8859_2 (Code, Stream.Encoding_Default)
                     );
                  when ISO_8859_3 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_ISO_8859_3 (Code, Stream.Encoding_Default)
                     );
                  when ISO_8859_4 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_ISO_8859_4 (Code, Stream.Encoding_Default)
                     );
                  when ISO_8859_5 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_ISO_8859_5 (Code, Stream.Encoding_Default)
                     );
                  when ISO_8859_6 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_ISO_8859_6 (Code, Stream.Encoding_Default)
                     );
                  when ISO_8859_7 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_ISO_8859_7 (Code, Stream.Encoding_Default)
                     );
                  when ISO_8859_8 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_ISO_8859_8 (Code, Stream.Encoding_Default)
                     );
                  when ISO_8859_9 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_ISO_8859_9 (Code, Stream.Encoding_Default)
                     );
                  when ISO_8859_10 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_ISO_8859_10 (Code, Stream.Encoding_Default)
                     );
                  when KOI8 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_KOI8 (Code, Stream.Encoding_Default)
                     );
                  when MacOS_Roman =>
                     Character'Write
                     (  Stream.Encoded,
                        To_MacOS_Roman (Code, Stream.Encoding_Default)
                     );
                  when Windows_1250 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_Windows_1250 (Code, Stream.Encoding_Default)
                     );
                  when Windows_1251 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_Windows_1251 (Code, Stream.Encoding_Default)
                     );
                  when Windows_1252 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_Windows_1252 (Code, Stream.Encoding_Default)
                     );
                  when Windows_1253 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_Windows_1253 (Code, Stream.Encoding_Default)
                     );
                  when Windows_1254 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_Windows_1254 (Code, Stream.Encoding_Default)
                     );
                  when Windows_1255 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_Windows_1255 (Code, Stream.Encoding_Default)
                     );
                  when Windows_1256 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_Windows_1256 (Code, Stream.Encoding_Default)
                     );
                  when Windows_1257 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_Windows_1257 (Code, Stream.Encoding_Default)
                     );
                  when Windows_1258 =>
                     Character'Write
                     (  Stream.Encoded,
                        To_Windows_1258 (Code, Stream.Encoding_Default)
                     );
               end case;
            when others =>
               null;
         end case;
      end loop;
   end Write;

end Strings_Edit.UTF8.Recoding_Streams;
