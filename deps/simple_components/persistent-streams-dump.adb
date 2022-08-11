--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Persistent.Streams.Dump                    Luebeck            --
--  Implementation                                 Winter, 2021       --
--                                                                    --
--                                Last revision :  11:02 11 Apr 2021  --
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

with Strings_Edit.Integers;      use Strings_Edit.Integers;
with Strings_Edit.Quoted;        use Strings_Edit.Quoted;
with Persistent.Blocking_Files;  use Persistent.Blocking_Files;

with Interfaces;

with Persistent.Blocking_Files.Text_IO;
use  Persistent.Blocking_Files.Text_IO;

with Persistent.Blocking_Files.Transactional;
use  Persistent.Blocking_Files.Transactional;

package body Persistent.Streams.Dump is

   function Image (Value : Stream_Position) return String is
      Result : String (1..20);
      Number : Stream_Position := Value;
   begin
      for Index in reverse Result'Range loop
         Result (Index) :=
            Character'Val (Number mod 10 + Character'Pos ('0'));
         Number := Number / 10;
         if Number = 0 then
            return Result (Index..Result'Last);
         end if;
      end loop;
      return Result;
   end Image;

   type Data_Array is
      array (Block_Offset range <>) of Interfaces.Unsigned_8;

   procedure Put
             (  File   : File_Type;
                Prefix : String;
                Data   : Data_Array
             )  is
      use Strings_Edit;
      type Column is mod 4;
      Text    : String (1..80);
      Count   : Column  := 0;
      Pointer : Integer := Text'First;
   begin
      for Index in Data'Range loop
         if Pointer = Text'First then
            Put (Text, Pointer, Prefix);
            Put (Text, Pointer, Index);
            Put (Text, Pointer, ": ");
         else
            Put (Text, Pointer, ' ');
            if Count = 0 then
               Put (Text, Pointer, ' ');
            end if;
         end if;
         Count := Count + 1;
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => Integer (Data (Index)),
            Base        => 16,
            Field       => 2,
            Fill        => '0',
            Justify     => Right
         );
         if Pointer + 5 > Text'Last then
            Put_Line (File, Text (Text'First..Pointer - 1));
            Pointer := Text'First;
            Count := 0;
         end if;
      end loop;
      if Pointer > Text'First then
         Put_Line (File, Text (Text'First..Pointer - 1));
      end if;
   end Put;

   procedure Put
             (  File   : File_Type;
                Stream : Persistent_Stream'Class;
                Flags  : Dump_Flags := Dump_All
             )  is
   begin
      Put_Line (File, "File: " & Quote (Get_Name (Stream.File)));
      if 0 /= (Flags and Dump_General_Information) then
         Put_Line (File, "Persistent stream information");
         Put_Line
         (  File,
            (  "   Block size:              "
            &  Image (Integer (Block_Byte_Size))
            &  " bytes"
         )  );
         Put_Line
         (  File,
            (  "   Bits per offset:         "
            &  Image (Integer (Byte_Offset_Bits))
         )  );
         Put_Line
         (  File,
            (  "   Block offsets:           0.."
            &  Image (Block_Offset'Last)
         )  );
         Put_Line
         (  File,
            (  "   Total blocks:            "
            &  Image
               (  Get_Block_Size (Persistent_Array (Stream.File))
         )  )  );
         Put_Line
         (  File,
            (  "   Disposed blocks:         "
            &  Image (Get_Disposed_Blocks (Stream.File))
         )  );
         Put_Line
         (  File,
            (  "   Free blocks:             "
            &  Image (Get_Free_Blocks (Stream.File))
         )  );
         Put_Line
         (  File,
            (  "   Logical blocks:          "
            &  Image (Get_Block_Size (Stream.File))
         )  );
         Put_Line
         (  File,
            (  "   Writable logical blocks: "
            &  Image (Get_Block_Size (Stream.File))
         )  );
         Put_Line
         (  File,
            (  "   First index:             "
            &  Image (Get_First_Index (Stream))
            &  ", position "
            &  Image (Get_First_Position (Stream))
         )  );
         Put_Line
         (  File,
            (  "   Write index:             "
            &  Image (Get_Write_Index (Stream))
            &  ", position "
            &  Image (Get_Write_Position (Stream))
         )  );
         Put_Line
         (  File,
            (  "   Read  index:             "
            &  Image (Get_Read_Index (Stream))
            &  ", position "
            &  Image (Get_Read_Position (Stream))
         )  );
         Put_Line
         (  File,
            "   Byte size:               " & Image (Get_Size (Stream))
         );
      end if;
      if 0 /= (Flags and Dump_Free_Block_List) then
         declare
            Free : Byte_Index  := Stream.Free;
         begin
            if Free = 0 then
               Put_Line (File, "List of free blocks is empty");
            else
               Put_Line (File, "List of free blocks:");
               loop
                  Put_Line (File, "   " & Image (Free));
                  Free :=
                     Get_Next
                     (  Load (Stream.Self.File'Access, Free).all,
                        Free
                     );
                  exit when Free = 0;
               end loop;
            end if;
         end;
      end if;
      if 0 /= (Flags and Dump_Block_Contents) then
         declare
            This  : Byte_Index      := Stream.Out_Index;
            Index : Stream_Position := Stream.Out_Position;
         begin
            if This = 0 then
               Put_Line (File, "No used blocks");
            else
               Put_Line (File, "Used blocks:");
               loop
                  Put_Line
                  (  File,
                     (  "   Block "
                     &  Image (This, False)
                     &  " at "
                     &  Image (This)
                     &  ":"
                  )  );
                  declare
                     Block  : Block_Type renames
                              Load (Stream.Self.File'Access, This).all;
                     Offset : constant Block_Offset :=
                                       Get_Offset (This);
                     Size   : Block_Offset := Block'Last - Offset + 1;
                  begin
                     if (  Stream_Position (Size)
                        >= Stream.In_Position - Index
                        )  then
                        Size :=
                           Block_Offset (Stream.In_Position - Index);
                     end if;
                     Put
                     (  File,
                        "      ",
                        Data_Array (Block (Offset..Offset + Size - 1))
                     );
                     Index := Index + Stream_Position (Size);
                     exit when Index >= Stream.In_Position;
                     This := Get_Next (Block, This);
                     if This = 0 then
                        Put_Line (File, "*** broken blocks list");
                        exit;
                     end if;
                  end;
               end loop;
            end if;
         end;
      elsif 0 /= (Flags and Dump_Used_Block_List) then
         declare
            This : Byte_Index := Stream.Out_Index;
         begin
            if This = 0 then
               Put_Line (File, "No used blocks");
            else
               Put_Line (File, "List of used blocks (using offsets):");
               loop
                  Put_Line (File, "   " & Image (This, False));
                  This :=
                     Get_Next
                     (  Load (Stream.Self.File'Access, This).all,
                        This
                     );
                  exit when This = 0;
               end loop;
            end if;
         end;
      end if;
   end Put;

   procedure Put
             (  Stream : Persistent_Stream'Class;
                Flags  : Dump_Flags := Dump_All
             )  is
   begin
      Put (Standard_Output, Stream, Flags);
   end Put;

end Persistent.Streams.Dump;
