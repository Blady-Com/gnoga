--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Transactional_Blocking_Files           Luebeck            --
--  Test                                           Summer, 2014       --
--                                                                    --
--                                Last revision :  21:09 15 Sep 2014  --
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

with Ada.Calendar;    use Ada.Calendar;
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Text_IO;     use Ada.Text_IO;
with Interfaces;      use Interfaces;

with Persistent.Blocking_Files.Transactional.Dump;

procedure Test_Transactional_Blocking_Files is
   use Persistent.Blocking_Files;
   use Persistent.Blocking_Files.Transactional;
   use Persistent.Blocking_Files.Transactional.Dump;

   File  : aliased Persistent_Transactional_Array;
   Last  : Time;
   Count : constant := 2_000;
begin
   begin
      Open (File, "test.tmp", Create_Mode, 10);
      Put_Line
      (  "File name: "
      &  Get_Name (File)
      &  ", block size"
      &  Integer'Image (Block_Byte_Size)
      &  " bytes"
      );
      Last := Clock;
      for Index in Block_Index range 1..Count loop
         if Clock - Last > 1.0 then
            Put_Line
            (  "Writing block"
            &  Block_Count'Image (Index)
            &  ", file size"
            &  Byte_Index'Image (Get_Size (File))
            &  " bytes"
            );
            Last := Clock;
         end if;
         Get (File'Access, Compose (Index, 0)) (0) :=
            Unsigned_8 (Index mod 256);
      end loop;
      Put_Line
      (  "File "
      &  Get_Name (File)
      &  " written, file size"
      &  Byte_Index'Image (Get_Size (File))
      &  " bytes"
      );
--      Put (File);
      Put
      (  File,
         (  Dump_General_Information
         or Dump_Free_Blocks_List
         or Dump_Disposed_Blocks_List
      )  );
      Last := Clock;
      for Index in reverse Block_Index range 1..Count loop
         if Clock - Last > 1.0 then
            Put_Line ("Reading index" & Block_Count'Image (Index));
            Last := Clock;
         end if;
         if (  Load (File'Access, Compose (Index, 0)) (0)
            /= Unsigned_8 (Index mod 256)
            )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in index"
               &  Block_Count'Image (Index)
               &  " wanted"
               &  Block_Count'Image (Index mod 256)
               &  ", got"
               &  Unsigned_8'Image
                  (  Load (File'Access, Compose (Index, 0)) (0)
            )  )  );
         end if;
      end loop;
      Commit (File);
      Close (File);
   end;
   begin
      Put_Line ("Reopening the file");
      Open (File, "test.tmp", Read_Write_Mode, 10);
      Last := Clock;
      for Index in Block_Index range 1..Count loop
         if Clock - Last > 1.0 then
            Put_Line ("Reading index" & Block_Count'Image (Index));
            Last := Clock;
         end if;
         if (  Load (File'Access, Compose (Index, 0)) (0)
            /= Unsigned_8 (Index mod 256)
            )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in index"
               &  Block_Count'Image (Index)
               &  " wanted"
               &  Block_Count'Image (Index mod 256)
               &  ", got"
               &  Unsigned_8'Image
                  (  Load (File'Access, Compose (Index, 0)) (0)
            )  )  );
         end if;
      end loop;
      Close (File);
   end;
   begin
      Put_Line ("Reopening the file");
      Open (File, "test.tmp", Read_Write_Mode, 10);
      Last := Clock;
      for Index in Block_Index range 1..Count loop
         if Clock - Last > 1.0 then
            Put_Line
            (  "Writing block"
            &  Block_Count'Image (Index)
            &  ", file size"
            &  Byte_Index'Image (Get_Size (File))
            &  " bytes"
            );
            Last := Clock;
         end if;
         declare
            Block : Block_Type renames
                    Get (File'Access, Compose (Index, 0)).all;
         begin
            Block (0) := Block (0) + 1;
         end;
      end loop;
      Rollback (File); -- Dropping changes
--    Put (File);
      Put
      (  File,
         (  Dump_General_Information
         or Dump_Free_Blocks_List
         or Dump_Disposed_Blocks_List
      )  );
      Last := Clock;
      for Index in Block_Index range 1..Count loop
         if Clock - Last > 1.0 then
            Put_Line ("Reading index" & Block_Count'Image (Index));
            Last := Clock;
         end if;
         if (  Load (File'Access, Compose (Index, 0)) (0)
            /= Unsigned_8 (Index mod 256)
            )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in index"
               &  Block_Count'Image (Index)
               &  " wanted"
               &  Block_Count'Image (Index mod 256)
               &  ", got"
               &  Unsigned_8'Image
                  (  Load (File'Access, Compose (Index, 0)) (0)
            )  )  );
         end if;
      end loop;
      Last := Clock;
      for Index in Block_Index range 1..Count loop
         if Clock - Last > 1.0 then
            Put_Line
            (  "Writing block"
            &  Block_Count'Image (Index)
            &  ", file size"
            &  Byte_Index'Image (Get_Size (File))
            &  " bytes"
            );
            Last := Clock;
         end if;
         declare
            Block : Block_Type renames
                    Get (File'Access, Compose (Index, 0)).all;
         begin
            Block (0) := Block (0) + 1;
         end;
      end loop;
      Put (File, Dump_Free_Blocks_List or Dump_Disposed_Blocks_List);
      Commit (File);
      Last := Clock;
      for Index in Block_Index range 1..Count loop
         if Clock - Last > 1.0 then
            Put_Line ("Reading index" & Block_Count'Image (Index));
            Last := Clock;
         end if;
         if (  Load (File'Access, Compose (Index, 0)) (0)
            /= Unsigned_8 ((Index + 1) mod 256)
            )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in index"
               &  Block_Count'Image (Index)
               &  " wanted"
               &  Block_Count'Image ((Index + 1) mod 256)
               &  ", got"
               &  Unsigned_8'Image
                  (  Load (File'Access, Compose (Index, 0)) (0)
            )  )  );
         end if;
      end loop;
--      Put (File);
      Put
      (  File,
         (  Dump_General_Information
         or Dump_Free_Blocks_List
         or Dump_Disposed_Blocks_List
      )  );
      Close (File);
   end;
   Put_Line ("... Done");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Transactional_Blocking_Files;
