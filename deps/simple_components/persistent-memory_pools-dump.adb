--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Persistent.Memory_Pools.Dump               Luebeck            --
--  Implementation                                 Winter, 2014       --
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

with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with Interfaces;
with Persistent.Blocking_Files.Text_IO;

package body Persistent.Memory_Pools.Dump is
   use Persistent.Blocking_Files.Text_IO;

   type Data_Array is
      array (Block_Offset range <>) of Interfaces.Unsigned_8;

   function Image
            (  Value : Block_Size_Index;
               Fill  : Boolean := True
            )  return String is
   begin
      if Fill then
         declare
            Result  : String (1..4);
            Pointer : Integer := Result'First;
         begin
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Natural (Value * Min_Size),
               Field       => 4,
               Justify     => Strings_Edit.Right
            );
            return Result;
         end;
      else
         return Image (Natural (Value * Min_Size));
      end if;
   end Image;

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
             (  File  : File_Type;
                Pool  : Persistent_Pool'Class;
                Flags : Dump_Flags := Dump_All
             )  is
      procedure Dump_Block (Index : in out Byte_Index) is
         Ref : constant Block_Type_Ref := Load (Pool.File, Index);
      begin
         if Ref = null then
            Put_Line
            (  "      "
            &  Image (Index)
            &  " -- illegal byte index"
            );
         else
            declare
               Block  : Block_Type renames Ref.all;
               Offset : constant Block_Offset := Get_Offset (Index);
               Size   : constant Unsigned_16  :=
                        Get_Size (Block, Offset);
               function Status return String is
               begin
                  if Is_Free (Block, Offset) then
                     return ": Free ";
                  else
                     return ": Used ";
                  end if;
               end Status;
               function Chain return String is
               begin
                  if Is_Chained (Block, Offset) then
                     declare
                        Next : constant Byte_Index :=
                               Get (Block, Offset);
                     begin
                        return " ---> " & Image (Next);
                     end;
                  else
                     return "";
                  end if;
               end Chain;
            begin
               Put_Line
               (  "      "
               &  Image (Index)
               &  Status
               &  Image (Offset)
               &  ".."
               &  Image (Offset + Block_Offset (Size - 5))
               &  " "
               &  Image (Integer (Size))
               &  " bytes ["
               &  Image (Integer (Size - 4))
               &  " usable]"
               &  Chain
               );
               if 0 /= (Flags and Dump_Memory_Contents) then
                  Put
                  (  File,
                     "      ",
                     Data_Array
                     (  Block
                        (  Offset
                        .. Offset + Block_Offset (Size - 5)
                  )  )  );
               end if;
               if Unsigned_16 (Offset) + Size >= Block_Byte_Size then
                  -- Next block
                  Index := Get_First (Index) + Block_Byte_Size + 2;
               else -- Same block
                  Index := Index + Byte_Index (Size);
               end if;
            end;
         end if;
      end Dump_Block;

      function Dump_Free (Index : Byte_Index) return Byte_Index is
         Ref : constant Block_Type_Ref := Load (Pool.File, Index);
      begin
         if Ref = null then
            Put_Line
            (  "      "
            &  Image (Index)
            &  " -- illegal byte index"
            );
            raise Data_Error;
         else
            declare
               Block    : Block_Type renames Ref.all;
               Offset   : constant Block_Offset := Get_Offset (Index);
               Size     : constant Unsigned_16 :=
                          Get_Size (Block, Offset);
               Previous : constant Byte_Index := Get (Block, Offset);
               Next     : constant Byte_Index :=
                          Get (Block, Offset + 8);
            begin
               Put_Line
               (  "      "
               &  Image (Index)
               &  ": Free "
               &  Image (Integer (Size))
               &  " bytes "
               &  Image (Previous)
               &  " <----> "
               &  Image (Next)
               );
               return Next;
            end;
         end if;
      end Dump_Free;
   begin
      Put_Line
      (  File,
         "File:               " & Quote (Get_Name (Pool))
      );
      if 0 /= (Flags and Dump_General_Information) then
         Put_Line (File, "General pool information");
         Put_Line
         (  File,
            (  "   Block size:            "
            &  Image (Integer (Block_Byte_Size))
            &  " bytes"
         )  );
         Put_Line
         (  File,
            (  "   Bits per offset:       "
            &  Image (Integer (Byte_Offset_Bits))
         )  );
         Put_Line
         (  File,
            "   Block offsets:         0.." & Image (Block_Offset'Last)
         );
         Put_Line
         (  File,
            (  "   Max memory block size: "
            &  Image (Integer (Byte_Count'Last))
            &  " = "
            &  Image (Byte_Count'Last)
            &  " hex"
         )  );
         Put_Line
         (  File,
            (  "   First block size:      "
            &  Image (Integer (Tail_Size))
            &  " = "
            &  Image (Byte_Count (Tail_Size))
            &  " hex"
         )  );
         Put_Line
         (  File,
            (  "   Min block size:        "
            &  Image (Integer (Min_Size))
         )  );
         Put_Line
         (  File,
            (  "   Max block size:        "
            &  Image (Integer (Max_Size))
         )  );
         Put_Line
         (  File,
            (  "   File size:            "
            &  Byte_Index'Image (Get_Size (Pool.File.all))
            &  " bytes [in"
            &  Byte_Index'Image
               (  Get_Size (Pool.File.all)
               /  Block_Byte_Size
               )
            &  " block(s)]"
         )  );
         Put_Line
         (  File,
            (  "   Usable file size:     "
            &  Byte_Index'Image (Get_Space (Pool))
            &  " bytes"
         )  );
         Put_Line
         (  File,
            (  "   Blocks used:          "
            &  Byte_Index'Image (Pool.Blocks_Used)
         )  );
         Put_Line
         (  File,
            (  "   Blocks free:          "
            &  Byte_Index'Image (Pool.Blocks_Free)
         )  );
         Put_Line
         (  File,
            (  "   Bytes used:           "
            &  Byte_Index'Image (Pool.Bytes_Used)
            &  " [with block margins"
            &  Byte_Index'Image (Pool.Bytes_Used + Pool.Blocks_Used * 4)
            &  "]"
         )  );
         Put_Line
         (  File,
            (  "   Bytes free:           "
            &  Byte_Index'Image (Pool.Bytes_Free)
            &  " [with block margins"
            &  Byte_Index'Image (Pool.Bytes_Free + Pool.Blocks_Free * 4)
            &  "]"
         )  );
         Put_Line
         (  File,
            (  "   First block offset:    "
            &  Image (Byte_Count (Head_Size + 2))
         )  );
         Put_Line (File, "   First block:");
         if 0 /= (Flags and Dump_Block_Contents) then
            Put
            (  File   => File,
               Prefix => "      ",
               Data   => Data_Array (Load (Pool.File, 0).all)
            );
         end if;
      end if;
      if 0 /= (Flags and Dump_Free_List) then
         declare
            First_Empty  : Block_Size_Index;
            Chain_Length : Natural := 0;
            procedure Flush_Chain is
            begin
               case Chain_Length is
                  when 0 =>
                     null;
                  when 1 =>
                     Put_Line
                     (  File,
                        (  "   No "
                        &  Image (First_Empty, False)
                        &  " bytes blocks"
                     )  );
                     Chain_Length := 0;
                  when others =>
                     Put_Line
                     (  File,
                        (  "   No "
                        &  Image (First_Empty, False)
                        &  ".."
                        &  Image
                           (  (  First_Empty
                              +  Block_Size_Index (Chain_Length - 1)
                              ),
                              False
                           )
                        & " bytes blocks"
                     )  );
                     Chain_Length := 0;
               end case;
            end Flush_Chain;
         begin
            Put_Line (File, "Lists of free blocks");
            for Index in Block_Size_Index loop
               declare
                  Head : Byte_Index renames Pool.Free (Index);
               begin
                  if Head = 0 then
                     if Chain_Length = 0 then
                        Chain_Length := 1;
                        First_Empty := Index;
                     else
                        Chain_Length := Chain_Length + 1;
                     end if;
                  else
                     Flush_Chain;
                     Put_Line
                     (  File,
                        "   " & Image (Index, False) & " bytes blocks:"
                     );
                     declare
                        This : Byte_Index := Head;
                        Next : Byte_Index;
                     begin
                        loop
                           Next := Dump_Free (This);
                           exit when Next = Head;
                           if Next = This then
                              Put_Line (File, "   -- broken list!");
                              exit;
                           end if;
                           This := Next;
                        end loop;
                     exception
                        when Data_Error =>
                           null;
                     end;
                  end if;
               end;
            end loop;
            Flush_Chain;
         end;
      end if;
      if 0 /= (Flags and Dump_Block_Margins) then
         Put_Line (File, "Memory blocks:");
         declare
            Last  : constant Byte_Index := Get_Size (Pool.File.all);
            Old   : Byte_Index;
            Index : Byte_Index := Head_Size + 2;
         begin
            loop
               Old := Index;
               Dump_Block (Index);
               exit when Index >= Last;
               if Old = Index then
                  Put_Line (File, "*** broken memory blocks list");
                  exit;
               end if;
            end loop;
         end;
      end if;
      if 0 /= (Flags and Dump_Block_Contents) then
         Put_Line (File, "File blocks:");
         declare
            Last  : constant Byte_Index := Get_Size (Pool.File.all);
            Index : Byte_Index := 0;
         begin
            while Index < Last loop
               Put_Line
               (  File,
                  (  "   Block "
                  &  Image (Index, False)
                  &  " at "
                  &  Image (Index)
                  &  ":"
               )  );
               Put
               (  File,
                  "      ",
                  Data_Array (Load (Pool.File, Index).all)
               );
               Index := Index + Block_Byte_Size;
            end loop;
         end;
      end if;
   end Put;

   procedure Put
             (  Pool  : Persistent_Pool'Class;
                Flags : Dump_Flags := Dump_All
             )  is
   begin
      Put (Standard_Output, Pool, Flags);
   end Put;

end Persistent.Memory_Pools.Dump;
