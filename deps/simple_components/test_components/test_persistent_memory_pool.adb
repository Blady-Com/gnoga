--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Persistent_Memory_Pool                 Luebeck            --
--  Test                                           Spring, 2014       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Streams;                   use Ada.Streams;
with Ada.Text_IO;                   use Ada.Text_IO;
with Interfaces;                    use Interfaces;
with Persistent.Memory_Pools.Dump;  use Persistent.Memory_Pools;
with Strings_Edit.Integers;         use Strings_Edit.Integers;
with Strings_Edit.Quoted;           use Strings_Edit.Quoted;

with Persistent.Blocking_Files.Text_IO;
use  Persistent.Blocking_Files.Text_IO;

with Persistent.Memory_Pools.Streams;
use  Persistent.Memory_Pools.Streams;

with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

--  with GNAT.Exception_Traces;

procedure Test_Persistent_Memory_Pool is
   use Persistent.Blocking_Files;
   use Persistent.Memory_Pools.Dump;
begin
   Put_Line ("Testing memory pools ...");
   declare
      File : aliased Persistent_Array;
      Count : Stream_Element_Count;
   begin
      Put_Line ("Blocking files test ...");
      Open (File, 10);
      declare
         Pool : aliased Persistent_Pool (File'Access);
         procedure Check (Size, Used, Free, Allocated : Natural) is
            Blocks_Free : constant Integer :=
                          Integer (Get_Blocks_Free (Pool));
            Blocks_Used : constant Integer :=
                          Integer (Get_Blocks_Used (Pool));
            Bytes_Free  : Integer := Integer (Get_Bytes_Free (Pool));
            Bytes_Used  : Integer := Integer (Get_Bytes_Used (Pool));
            Free_Space  : constant Integer :=
                          Integer (Get_Space (Pool));
         begin
            Bytes_Free := Bytes_Free + Blocks_Free * 4;
            Bytes_Used := Bytes_Used + Blocks_Used * 4;
            if Used /= Blocks_Used then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Number of used blocks "
                  &  Image (Blocks_Used)
                  &  " /= expected "
                  &  Image (Used)
               )  );
            elsif Free /= Blocks_Free then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Number of free blocks "
                  &  Image (Blocks_Free)
                  &  " /= expected "
                  &  Image (Free)
               )  );
            elsif Allocated /= Bytes_Used then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Allocated bytes "
                  &  Image (Bytes_Used)
                  &  " /= expected "
                  &  Image (Allocated)
               )  );
            elsif Free_Space - Allocated /= Bytes_Free then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Unallocated bytes "
                  &  Image (Bytes_Free)
                  &  " /= expected "
                  &  Image (Free_Space - Allocated)
               )  );
            end if;
         end Check;
      begin
   --    Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
         Check (Size => 6080, Used => 0, Free => 1, Allocated => 0);
   --    Put (Pool, not (Dump_Block_Contents));
         declare
            Ptr : array (1..10) of Byte_Index;
         begin
            Ptr (1) := Allocate (Pool, 100);
            Put_Line ("Allocated 100 bytes at " & Image (Ptr (1)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 1,
               Free => 1,
               Allocated => 128
            );
            Ptr (2) := Allocate (Pool, 10);
            Put_Line ("Allocated 10 bytes at " & Image (Ptr (2)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 2,
               Free => 1,
               Allocated => 128 + 32
            );
            Ptr (3) := Allocate (Pool, 300);
            Put_Line ("Allocated 300 bytes at " & Image (Ptr (3)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 3,
               Free => 1,
               Allocated => 128 + 32 + 320
            );
            Ptr (4) := Allocate (Pool, 4000);
            Put_Line ("Allocated 4000 bytes at " & Image (Ptr (4)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 4,
               Free => 1,
               Allocated => 128 + 32 + 320 + 4000
            );
            Ptr (5) := Allocate (Pool, 1472);
            Put_Line ("Allocated 1472 bytes at " & Image (Ptr (5)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 5,
               Free => 0,
               Allocated => 128 + 32 + 320 + 4000 + 1472
            );
            Deallocate (Pool, Ptr (2));
            Put_Line ("Freed block at " & Image (Ptr (2)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 4,
               Free => 1,
               Allocated => 128 + 320 + 4000 + 1472
            );
            Deallocate (Pool, Ptr (4));
            Put_Line ("Freed block at " & Image (Ptr (4)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 3,
               Free => 2,
               Allocated => 128 + 320 + 1472
            );
            Deallocate (Pool, Ptr (3));
            Put_Line ("Freed block at " & Image (Ptr (3)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 2,
               Free => 1,
               Allocated => 128 + 1472
            );
            Put_Line ("Truncating block at " & Image (Ptr (5)));
            Truncate (Pool, Ptr (5), 596);
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 2,
               Free => 2,
               Allocated => 128 + 608
            );
            Deallocate (Pool, Ptr (5));
            Put_Line ("Freed block at " & Image (Ptr (5)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080,
               Used => 1,
               Free => 1,
               Allocated => 128
            );
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Put_Line
            (  "Block at "
            &  Image (Ptr (1))
            &  " expanded by"
            &  Byte_Count'Image (Expand (Pool, Ptr (1)))
            &  " bytes"
            );
            Put
            (  Pool,
               not (Dump_Block_Contents or Dump_Memory_Contents)
            );
            Check
            (  Size => 6080,
               Used => 1,
               Free => 0,
               Allocated => 5952
            );
            Put (Pool, Dump_Free_List);
            Ptr (2) := Allocate (Pool, 10);
            Put_Line ("Allocated 10 bytes at " & Image (Ptr (2)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080 + 8192,
               Used => 2,
               Free => 1,
               Allocated => 5952 + 32
            );
            Ptr (3) := Fetch (Pool, 100);
            Put_Line
            (  "Allocated "
            &  Image (Integer (Get_Size (Pool, Ptr (3))))
            &  " bytes at "
            &  Image (Ptr (3))
            );
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080 + 8192,
               Used => 3,
               Free => 0,
               Allocated => 5952 + 32 + 8160
            );
            Ptr (4) := Fetch (Pool, 1000);
            Put_Line
            (  "Allocated "
            &  Image (Integer (Get_Size (Pool, Ptr (4))))
            &  " bytes at "
            &  Image (Ptr (4))
            );
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080 + 8192 + 8192,
               Used => 4,
               Free => 0,
               Allocated => 5952 + 32 + 8160 + 8192
            );
            Ptr (5) := Fetch (Pool, 1000);
            Put_Line
            (  "Allocated "
            &  Image (Integer (Get_Size (Pool, Ptr (5))))
            &  " bytes at "
            &  Image (Ptr (5))
            );
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080 + 8192 + 8192 + 8192,
               Used => 5,
               Free => 0,
               Allocated => 5952 + 32 + 8160 + 8192 + 8192
            );
            Ptr (6) := Fetch (Pool, 1000);
            Put_Line
            (  "Allocated "
            &  Image (Integer (Get_Size (Pool, Ptr (6))))
            &  " bytes at "
            &  Image (Ptr (6))
            );
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080 + 8192 + 8192 + 8192 + 8192,
               Used => 6,
               Free => 0,
               Allocated => 5952 + 32 + 8160 + 8192 + 8192 + 8192
            );
            Deallocate (Pool, Ptr (5));
            Put_Line ("Freed block at " & Image (Ptr (5)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080 + 8192 + 8192 + 8192 + 8192,
               Used => 5,
               Free => 1,
               Allocated => 5952 + 32 + 8160 + 8192 + 8192
            );
            Deallocate (Pool, Ptr (6));
            Put_Line ("Freed block at " & Image (Ptr (6)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080 + 8192 + 8192 + 8192 + 8192,
               Used => 4,
               Free => 2,
               Allocated => 5952 + 32 + 8160 + 8192
            );
            Deallocate (Pool, Ptr (4));
            Put_Line ("Freed block at " & Image (Ptr (4)));
   --       Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
            Check
            (  Size => 6080 + 8192 + 8192 + 8192 + 8192,
               Used => 3,
               Free => 3,
               Allocated => 5952 + 32 + 8160
            );
         end;
         declare
            First : Byte_Index;
            Text  : constant String := "12345678901234567890";
         begin
            --
            -- Writing string into the stream
            --
            declare
               Output : aliased Output_Stream (Pool'Access);
            begin
               String'Output (Output'Access, Text);
               First := Get_First (Output);
               Count := Get_Written (Output);
               Put_Line
               (  "Written "
               &  Quote (Text)
               &  " at "
               &  Image (First)
               &  ","
               &  Stream_Element_Count'Image (Count)
               &  " elements"
               );
      --           if Get_Written (Output) /= Text'Length + 8 then
      --              Raise_Exception
      --              (  Constraint_Error'Identity,
      --                 (  "Wrong written count"
      --                 &  Stream_Element_Count'Image (Get_Written (Output))
      --                 &  " /="
      --                 &  Stream_Element_Count'Image (Text'Length)
      --                 &  " (expected)"
      --              )  );
      --           end if;
            end;
            --
            -- Reading stream from the stream back
            --
            declare
               Input  : aliased Input_Stream (Pool'Access);
               Length : Stream_Element_Count;
            begin
               Open (Input, First);
               Length := Get_Length (Input);
               if Length /= Count then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Wrong stream length"
                     &  Stream_Element_Count'Image (Length)
                     &  " /="
                     &  Stream_Element_Count'Image (Count)
                     &  " (written)"
                  )  );
               end if;
               Length := Get_Unread (Input);
               if Length /= Count then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Wrong stream unread count"
                     &  Stream_Element_Count'Image (Length)
                     &  " /="
                     &  Stream_Element_Count'Image (Count)
                     &  " (written)"
                  )  );
               end if;
               declare
                  Data : constant String := String'Input (Input'Access);
               begin
                  if Data /= Text then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        (  "Error in reading from stream "
                        &  Quote (Data)
                        &  " /= "
                        &  Quote (Text)
                     )  );
                  end if;
               end;
               Length := Get_Unread (Input);
               if Length /= 0 then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Wrong stream unread count"
                     &  Stream_Element_Count'Image (Length)
                     &  " /= 0 (expected)"
                  )  );
               end if;
               if not End_Of (Input) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Stream end not reached"
                  );
               end if;
            end;
--              Put (Pool, not (Dump_Block_Contents or Dump_Memory_Contents));
--              GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
            --
            -- Appending source file to the stream
            --
            declare
               Output : aliased Output_Stream (Pool'Access);
               Source : File_Type;
            begin
               Append (Output, First);
               begin
                  Open
                  (  Source,
                     In_File,
                     "test_persistent_memory_pool.adb"
                  );
                  for Line_No in Positive'Range loop
                     declare
                        Line : String (1..2048);
                        Last : Integer;
                     begin
                        Get_Line (Source, Line, Last);
                        String'Output
                        (  Output'Access,
                           Line (Line'First..Last)
                        );
                     end;
                  end loop;
               exception
                  when End_Error =>
                     Count := Get_Written (Output);
                     Close (Source);
               end;
            end;
            --
            -- Reading source file to the stream back
            --
            declare
               Input  : aliased Input_Stream (Pool'Access);
               File   : File_Type;
               Length : Stream_Element_Count;
            begin
               Open (File, In_File, "test_persistent_memory_pool.adb");
               Open (Input, First);
               Length := Get_Length (Input);
               if Length /= Count then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     (  "Wrong stream length"
                     &  Stream_Element_Count'Image (Length)
                     &  " /="
                     &  Stream_Element_Count'Image (Count)
                     &  " (written)"
                  )  );
               end if;
               declare
                  Data : constant String := String'Input (Input'Access);
               begin
                  if Data /= Text then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        (  "Error in reading text from stream "
                        &  Quote (Data)
                        &  " /= "
                        &  Quote (Text)
                     )  );
                  end if;
               end;
               for Line_No in Positive'Range loop
                  declare
                     File_Line : String (1..2048);
                     Last      : Integer;
                  begin
                     Get_Line (File, File_Line, Last);
                     declare
                        Pool_Line : constant String :=
                                    String'Input (Input'Access);
                     begin
                        if File_Line (1..Last) /= Pool_Line then
                           Raise_Exception
                           (  Constraint_Error'Identity,
                              (  "Error in reading line "
                              &  Image (Line_No)
                              &  " from stream "
                              &  Quote (File_Line (1..Last))
                              &  " /= "
                              &  Quote (Pool_Line)
                           )  );
                        end if;
                     end;
                  end;
               end loop;
            exception
               when End_Error =>
                  Close (File);
                  if not End_Of (Input) then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        "Stream end not reached"
                     );
                  end if;
                  Put_Line ("Reading beyond stream end");
                  declare
                     Dummy : Integer;
                  begin
                     Dummy := Integer'Input (Input'Access);
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        "Read beyond stream end"
                     );
                  exception
                     when End_Error =>
                        null;
                  end;
            end;
            --
            -- Appending string once again
            --
            declare
               Output : aliased Output_Stream (Pool'Access);
            begin
               Append (Output, First);
               String'Output (Output'Access, Text);
            end;
            --
            -- Reading source file to the stream back
            --
            declare
               Input : aliased Input_Stream (Pool'Access);
               File  : File_Type;
            begin
               Open (File, In_File, "test_persistent_memory_pool.adb");
               Open (Input, First);
               declare
                  Data : constant String := String'Input (Input'Access);
               begin
                  if Data /= Text then
                     Raise_Exception
                     (  Constraint_Error'Identity,
                        (  "Error in reading from stream "
                        &  Quote (Data)
                        &  " /= "
                        &  Quote (Text)
                     )  );
                  end if;
               end;
               loop
                  declare
                     File_Line : String (1..2048);
                     Last      : Integer;
                  begin
                     Get_Line (File, File_Line, Last);
                     declare
                        Pool_Line : constant String :=
                                    String'Input (Input'Access);
                     begin
                        if File_Line (1..Last) /= Pool_Line then
                           Raise_Exception
                           (  Constraint_Error'Identity,
                              (  "Error in reading from stream "
                              &  Quote (File_Line (1..Last))
                              &  " /= "
                              &  Quote (Pool_Line)
                           )  );
                        end if;
                     end;
                  end;
               end loop;
            exception
               when End_Error =>
                  Close (File);
                  declare
                     Data : constant String :=
                            String'Input (Input'Access);
                  begin
                     if Data /= Text then
                        Raise_Exception
                        (  Constraint_Error'Identity,
                           (  "Error in reading from stream "
                           &  Quote (Data)
                           &  " /= "
                           &  Quote (Text)
                        )  );
                     end if;
                  end;
            end;
--              Put
--              (  Pool,
--                 not (Dump_Block_Contents or Dump_Memory_Contents)
--              );
            Put_Line ("Deleting chunked data at " & Image (First));
            Deallocate (Pool, First);
            Put
            (  Pool,
               not (Dump_Block_Contents or Dump_Memory_Contents)
            );
            --
            -- Writing string into the stream for comparison text
            --
            declare
               Output : aliased Output_Stream (Pool'Access);
            begin
               String'Write
               (  Output'Access,
                  "The quick brown fox jumps over the lazy dog"
               );
               First := Get_First (Output);
            end;
            --
            -- Doing comparison tests
            --
            declare
               Input : aliased Input_Stream (Pool'Access);
            begin
               Open (Input, First);
               if not Equal
                      (  Input,
                         "The quick brown fox jumps over the lazy dog"
                      )
               then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Wrong stream comparison (not equal)"
                  );
               elsif not Less (Input, "XYZ") then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Wrong stream comparison (not less)"
                  );
               elsif Compare (Input, "") /= Greater then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Wrong stream comparison (not greater)"
                  );
               end if;
            end;
         end;
      end;
      Close (File);
      Put_Line ("... Blocking files test done");
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Persistent_Memory_Pool;
