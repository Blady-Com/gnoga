--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Blocking_Files.                  Luebeck            --
--        Transactional.Dump                       Summer, 2014       --
--  Interface                                                         --
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

with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with Persistent.Blocking_Files.Text_IO;

package body Persistent.Blocking_Files.Transactional.Dump is
   use Persistent.Blocking_Files.Text_IO;

   Margin : constant := 60;

   function Updated (Status : Boolean) return String is
   begin
      if Status then
         return "*";
      else
         return "-";
      end if;
   end Updated;

   function Yes_Image (Status : Boolean) return String is
   begin
      if Status then
         return " yes";
      else
         return " no";
      end if;
   end Yes_Image;

   procedure Put
             (  File      : Ada.Text_IO.File_Type;
                Container : Persistent_Transactional_Array'Class;
                Flags     : Dump_Flags := Dump_All
             )  is
      Text    : String (1..80);
      Pointer : Integer;

      procedure Dump (Segment : Map_Segment; Last : Map_Index) is
      begin
         Put_Line
         (  File,
            (  "      Lowest virtual block mapped: "
            &  Image (Segment.Lower)
         )  );
         Put_Line
         (  File,
            (  "      Resides at physical block:   "
            &  Image (Segment.Location)
         )  );
         Put_Line
         (  File,
            (  "      Relocated:                  "
            &  Yes_Image (Segment.Relocated)
         )  );
         Put_Line
         (  File,
            (  "      Modified:                   "
            &  Yes_Image (Segment.Modified)
         )  );
         Put_Line
         (  File,
            (  "      Used:                       "
            &  Yes_Image (Segment.Used)
         )  );
         Put_Line (File, "      Virtual blocks map:");
         Pointer := 1;
         Put (Text, Pointer, "         ");
         for Index in 0..Last loop
            exit when Segment.Map (Index) = 0;
            if Pointer > Margin then
               Put_Line (File, Text (1..Pointer - 1));
               Pointer := 1;
               Put (Text, Pointer, "         ");
            elsif Pointer > 10 then
               Put (Text, Pointer, ", ");
            end if;
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => Integer (Index),
               Justify     => Right,
               Field       => 3,
               Fill        => ' '
            );
            Put (Text, Pointer, ") ");
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => (  Segment.Lower
                              +  Block_Count (Index)
                              *  Map_Size ** Natural (Segment.Height)
            )                 );
            Put (Text, Pointer, "->");
            Put (Text, Pointer, Segment.Map (Index));
            Put (Text, Pointer, Updated (Segment.Updated (Index)));
         end loop;
         if Pointer > 1 then
            Put_Line (File, Text (1..Pointer - 1));
         end if;
      end Dump;

      procedure Put (Segment : Free_Segment) is
         Prefix  : constant := 9 + 1;
         Text    : String (1..72) := (others => ' ');
         Pointer : Integer := Prefix;
      begin
         if Segment.Length = 0 then
            Put_Line
            (  File,
               (  "      Empty block list segment at "
               &  Image (Segment.Location)
               &  ": (Next "
               &  Image (Segment.Next)
               &  ")"
            )  );
         else
            Put_Line
            (  File,
               (  "      Block list segment at "
               &  Image (Segment.Location)
               &  ": (Next "
               &  Image (Segment.Next)
               &  ")"
            )  );
            declare
               From : Block_Count := Segment.List (1);
               To   : Block_Count := From;
               procedure Flush is
                  Start : constant Integer := Pointer;
               begin
                  if Start > Prefix then
                     Put (Text, Pointer, ", ");
                  end if;
                  if From = To then
                     Put (Text, Pointer, From);
                  else
                     Put (Text, Pointer, From);
                     Put (Text, Pointer, "..");
                     Put (Text, Pointer, To);
                  end if;
               exception
                  when Layout_Error =>
                     Put_Line (File, Text (1..Start - 1));
                     Pointer := Prefix;
                     if From = To then
                        Put (Text, Pointer, From);
                     else
                        Put (Text, Pointer, From);
                        Put (Text, Pointer, "..");
                        Put (Text, Pointer, To);
                     end if;
               end;
            begin
               for Index in 2..Segment.Length loop
                  if To + 1 = Segment.List (Index) then
                     To := To + 1;
                  else
                     Flush;
                     From := Segment.List (Index);
                     To   := From;
                  end if;
               end loop;
               Flush;
               if Pointer > Prefix then
                  Put_Line (File, Text (1..Pointer - 1));
               end if;
            end;
         end if;
      end Put;

      procedure Dump (Segment : Free_Segment) is
      begin
         if Segment.Length /= 0 then
            Put (Segment);
         end if;
         declare
            Index : Block_Count := Segment.Next;
            This  : Free_Segment;
         begin
            while Index /= 0 loop
               Load (Container.Self.all, Index, This);
               Put (This);
               Index := This.Next;
            end loop;
         end;
      end Dump;
   begin
      Put_Line
      (  File,
         "File:               " & Quote (Get_Name (Container))
      );
      if 0 /= (Flags and Dump_General_Information) then
         Put_Line (File, "General file information:");
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
            (  "   File size:            "
            &  Byte_Index'Image
               (  Get_Allocated_Size (Container)
               /  Block_Byte_Size
               )
            &  " block(s)"
         )  );
         Put_Line
         (  File,
            (  "   Used file size:       "
            &  Byte_Index'Image
               (  Get_Used_Size (Container)
               /  Block_Byte_Size
               )
            &  " block(s)"
         )  );
         Put_Line
         (  File,
            (  "   Free blocks:          "
            &  Block_Count'Image (Get_Free_Blocks (Container))
            &  " block(s)"
         )  );
         Put_Line
         (  File,
            (  "   Disposed blocks:      "
            &  Block_Count'Image (Get_Disposed_Blocks (Container))
            &  " block(s)"
         )  );
         Put_Line
         (  File,
            (  "   Virtual file size:    "
            &  Block_Count'Image (Get_Block_Size (Container))
            &  " block(s)"
         )  );
         Put_Line
         (  File,
            (  "   Mapping segment size: "
            &  Integer'Image (Map_Size)
         )  );
         Put_Line
         (  File,
            (  "   Root segment size:    "
            &  Integer'Image (Root_Size)
         )  );
         Put_Line
         (  File,
            (  "   Mapping depth:        "
            &  Integer'Image (Get_Map_Depth (Container))
         )  );
         Put_Line
         (  File,
            (  "   Sequence number:      "
            &  Unsigned_64'Image (Container.Sequence)
         )  );
      end if;
      if 0 /= (Flags and Dump_Virtual_Block_Map) then
         Put_Line ("Virtual to physical block map:");
         Pointer := 1;
         Put (Text, Pointer, "   ");
         for No in 1..Get_Block_Size (Container) loop
            if Pointer > Margin then
               Put_Line (File, Text (1..Pointer - 1));
               Pointer := 1;
               Put (Text, Pointer, "   ");
            elsif Pointer > 4 then
               Put (Text, Pointer, ", ");
            end if;
            Put (Text, Pointer, No);
            Put (Text, Pointer, "->");
            Put (Text, Pointer, Get_Physical (Container, No));
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => Updated (Is_Updated (Container, No))
            );
         end loop;
         if Pointer > 1 then
            Put_Line (File, Text (1..Pointer - 1));
         end if;
      end if;
      if 0 /= (Flags and Dump_Block_Map_Segments) then
         Put_Line ("Virtual to physical block map segments:");
         Put_Line ("   Root segment:");
         Dump (Container.List (Container.List'Last), Root_Size - 1);
         for Height in reverse 0..Get_Map_Depth (Container) - 2 loop
            Put_Line
            (  "   Segments of the height"
            &  Integer'Image (Height)
            );
            declare
               No : Block_Index := 1;
            begin
               while No <= Get_Block_Size (Container) loop
                  Dump
                  (  Find (Container, No, Map_Level (Height)).all,
                     Map_Index'Last
                  );
                  No := No + Map_Size ** Natural (Height + 1);
               end loop;
            end;
         end loop;
      end if;
      if 0 /= (Flags and Dump_Free_Blocks_List) then
         Put_Line ("Free blocks:");
         Dump (Container.Free);
      end if;
      if 0 /= (Flags and Dump_Disposed_Blocks_List) then
         Put_Line
         (  "Disposed blocks (freed upon transaction commit, tail "
         &  "of the list at "
         &  Image (Container.First)
         &  "):"
         );
         Dump (Container.Disposed);
      end if;
   end Put;

   procedure Put
             (  Container : Persistent_Transactional_Array'Class;
                Flags     : Dump_Flags := Dump_All
             )  is
   begin
      Put (Standard_Output, Container, Flags);
   end Put;

end Persistent.Blocking_Files.Transactional.Dump;
