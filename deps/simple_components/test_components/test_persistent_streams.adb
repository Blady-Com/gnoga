--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Persistent_Streams                     Luebeck            --
--  Test                                           Spring, 2021       --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Directories;          use Ada.Directories;
with Persistent.Streams;       use Persistent.Streams;
with Persistent.Streams.Dump;  use Persistent.Streams.Dump;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

with Persistent.Blocking_Files;

with Persistent.Blocking_Files.Text_IO;
use  Persistent.Blocking_Files.Text_IO;

procedure Test_Persistent_Streams is
   File_Name : constant String := "test_streams.dat";
   FIFO      : aliased Persistent_Stream;
begin
   if Exists (File_Name) then
      Delete_File (File_Name);
   end if;
   Put_Line ("------------------------------------- Creating the file");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Create_Mode);
   if Get_Size (FIFO) /= 0 then
      Raise_Exception (Data_Error'Identity, "New file is not empty");
   end if;
   Close (FIFO);
   Put_Line ("------------------------------------ Reading empty file");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Mode);
   if Get_Size (FIFO) /= 0 then
      Raise_Exception (Data_Error'Identity, "The file is not empty");
   end if;
   Close (FIFO);
   Put_Line ("------------------------------------ Write 100 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Write_Mode);
   if Get_Size (FIFO) /= 0 then
      Raise_Exception (Data_Error'Identity, "The file is not empty");
   end if;
   Put (FIFO);
   for I in 1..100 loop
      Integer'Output (FIFO'Access, I);
      if Get_Size (FIFO) /= Stream_Position (I) * (Integer'Size / 8)
      then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Wrong file size"
            &  Stream_Position'Image (Get_Size (FIFO))
            &  ", expected"
            &  Stream_Position'Image
               (  Stream_Position (I) * (Integer'Size / 8)
         )  )  );
      end if;
   end loop;
   for I in 1..100 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("---------------------------------- Reading 100 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Mode);
   Put (FIFO);
   for I in 1..100 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("----------------------------------- Reading 50 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Write_Mode);
   for I in 1..50 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   Dispose (FIFO);
   Close (FIFO);
   Put_Line ("----------------------------------- Reading 50 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Mode);
   for I in 51..100 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("------------------------------- Write next 100 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Write_Mode);
   for I in 101..200 loop
      Integer'Output (FIFO'Access, I);
      if (  Get_Size (FIFO)
         /= Stream_Position (I - 50) * (Integer'Size / 8)
         )  then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Wrong file size"
            &  Stream_Position'Image (Get_Size (FIFO))
            &  ", expected"
            &  Stream_Position'Image
               (  Stream_Position (I - 50) * (Integer'Size / 8)
         )  )  );
      end if;
   end loop;
   for I in 51..200 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("------------------------------ Reading 51..200 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Mode);
   for I in 51..200 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("---------------------- Write next up to 10_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Write_Mode);
   for I in 201..10_000 loop
      Integer'Output (FIFO'Access, I);
      if (  Get_Size (FIFO)
         /= Stream_Position (I - 50) * (Integer'Size / 8)
         )  then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Wrong file size"
            &  Stream_Position'Image (Get_Size (FIFO))
            &  ", expected"
            &  Stream_Position'Image
               (  Stream_Position (I - 50) * (Integer'Size / 8)
         )  )  );
      end if;
   end loop;
   for I in 51..10_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("--------------------------- Reading 51..10_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Mode);
   declare
      use Persistent.Blocking_Files;
      Total_Blocks    : Block_Count;
      Disposed_Blocks : Block_Count;
      Free_Blocks     : Block_Count;
      Logical_Blocks  : Block_Count;
      Writable_Blocks : Block_Count;
   begin
      Get_Blocks
      (  Stream          => FIFO,
         Total_Blocks    => Total_Blocks,
         Disposed_Blocks => Disposed_Blocks,
         Free_Blocks     => Free_Blocks,
         Logical_Blocks  => Logical_Blocks,
         Writable_Blocks => Writable_Blocks
      );
      Put_Line ("  In_Index        " & Image (Get_First_Index (FIFO)));
      Put_Line ("  In_Position    " &
                     Stream_Position'Image (Get_First_Position (FIFO)));
      Put_Line ("  Out_Index       " & Image (Get_Write_Index (FIFO)));
      Put_Line ("  Out_Position   " &
                     Stream_Position'Image (Get_Write_Position (FIFO)));
      Put_Line ("  Read_Index      " & Image (Get_Read_Index (FIFO)));
      Put_Line ("  Read_Position  " &
                      Stream_Position'Image (Get_Read_Position (FIFO)));
      Put_Line ("  Total blocks   " &
                                      Block_Count'Image (Total_Blocks));
      Put_Line ("  Disposed blocks" &
                                   Block_Count'Image (Disposed_Blocks));
      Put_Line ("  Free blocks    " & Block_Count'Image (Free_Blocks));
      Put_Line ("  Logical blocks " &
                                    Block_Count'Image (Logical_Blocks));
      Put_Line ("  Writable blocks" &
                                   Block_Count'Image (Writable_Blocks));
      if Logical_Blocks /= 5 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Wrong number of logical blocks"
            &  Block_Count'Image (Logical_Blocks)
            &  ", expected 5"
         )  );
      elsif Writable_Blocks /= 0 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Wrong number of writable blocks"
            &  Block_Count'Image (Writable_Blocks)
            &  ", expected 2"
         )  );
      end if;
   end;
   for I in 51..10_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("---------------------------- Reading 51..3_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Write_Mode);
   for I in 51..3_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   Dispose (FIFO);
   Close (FIFO);
   Put_Line ("------------------------ Reading 3_001..10_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Mode);
   declare
      use Persistent.Blocking_Files;
      Total_Blocks    : Block_Count;
      Disposed_Blocks : Block_Count;
      Free_Blocks     : Block_Count;
      Logical_Blocks  : Block_Count;
      Writable_Blocks : Block_Count;
   begin
      Get_Blocks
      (  Stream          => FIFO,
         Total_Blocks    => Total_Blocks,
         Disposed_Blocks => Disposed_Blocks,
         Free_Blocks     => Free_Blocks,
         Logical_Blocks  => Logical_Blocks,
         Writable_Blocks => Writable_Blocks
      );
      Put_Line ("  In_Index        " & Image (Get_First_Index (FIFO)));
      Put_Line ("  In_Position    " &
                     Stream_Position'Image (Get_First_Position (FIFO)));
      Put_Line ("  Out_Index       " & Image (Get_Write_Index (FIFO)));
      Put_Line ("  Out_Position   " &
                     Stream_Position'Image (Get_Write_Position (FIFO)));
      Put_Line ("  Read_Index      " & Image (Get_Read_Index (FIFO)));
      Put_Line ("  Read_Position  " &
                      Stream_Position'Image (Get_Read_Position (FIFO)));
      Put_Line ("  Total blocks   " &
                                      Block_Count'Image (Total_Blocks));
      Put_Line ("  Disposed blocks" &
                                   Block_Count'Image (Disposed_Blocks));
      Put_Line ("  Free blocks    " & Block_Count'Image (Free_Blocks));
      Put_Line ("  Logical blocks " &
                                    Block_Count'Image (Logical_Blocks));
      Put_Line ("  Writable blocks" &
                                   Block_Count'Image (Writable_Blocks));
      if Logical_Blocks /= 5 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Wrong number of logical blocks"
            &  Block_Count'Image (Logical_Blocks)
            &  ", expected 5"
         )  );
      elsif Writable_Blocks /= 1 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Wrong number of writable blocks"
            &  Block_Count'Image (Writable_Blocks)
            &  ", expected 2"
         )  );
      end if;
   end;
   for I in 3_001..10_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("---------------------- Write next up to 12_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Write_Mode);
   Put (FIFO, Dump_All and not Dump_Block_Contents);
   for I in 10_001..12_000 loop
      Integer'Output (FIFO'Access, I);
   end loop;
   for I in 3_001..12_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("------------------------ Reading 3_001..12_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Mode);
   Put (FIFO, Dump_All and not Dump_Block_Contents);
   for I in 3_001..12_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("------------------------- Reading 3_001..7_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Write_Mode);
   for I in 3_001..7_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   Dispose (FIFO);
   Close (FIFO);
   Put_Line ("------------------------ Reading 7_001..12_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Mode);
   Put (FIFO, Dump_All and not Dump_Block_Contents);
   for I in 7_001..12_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
   Put_Line ("------------------------ Reading 7_001..12_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Write_Mode);
   for I in 7_001..12_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   Dispose (FIFO);
   Close (FIFO);
   Put_Line ("------------------------------ Write 1..10_000 integers");
   Open (FIFO, File_Name, Persistent.Blocking_Files.Read_Write_Mode);
   Put (FIFO, Dump_All and not Dump_Block_Contents);
   for I in 1..10_000 loop
      Integer'Output (FIFO'Access, I);
   end loop;
   for I in 1..10_000 loop
      declare
         J : constant Integer := Integer'Input (FIFO'Access);
      begin
         if I /= J then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error " & Image (J) & ", expected " & Image (I)
            );
         end if;
      end;
   end loop;
   if not End_Of (FIFO) then
      Raise_Exception (Data_Error'Identity, "End of stream expected");
   end if;
   Close (FIFO);
exception
   when Error : others =>
      Put_Line ("Fault: " & Exception_Information (Error));
end Test_Persistent_Streams;
