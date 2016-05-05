--                                                                    --
--  procedure Test_Blocking_Files   Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Spring, 2014       --
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

with Ada.Calendar;               use Ada.Calendar;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Text_IO;                use Ada.Text_IO;
with Interfaces;                 use Interfaces;
with Persistent.Blocking_Files;  use Persistent.Blocking_Files;

procedure Test_Blocking_Files is
   Count : constant := 100_000_000;
   File  : aliased Persistent_Array;
   Last  : Time;
begin
   Put_Line ("Testing blocking files ...");
   Open (File, 10);
   Put_Line
   (  "File name: "
   &  Get_Name (File)
   &  ", block size"
   &  Integer'Image (Block_Byte_Size)
   &  " bytes"
   );
   Last := Clock;
   for Index in Byte_Index range 0..Count loop
      if Clock - Last > 1.0 then
         Put_Line
         (  "Writing index"
         &  Byte_Index'Image (Index)
         &  ", file size"
         &  Byte_Index'Image (Get_Size (File))
         &  " bytes"
         );
         Last := Clock;
      end if;
      Get (File'Access, Index) (Get_Offset (Index)) :=
         Unsigned_8 (Index mod 256);
   end loop;
   Put_Line
   (  "File "
   &  Get_Name (File)
   &  " written, file size"
   &  Byte_Index'Image (Get_Size (File))
   &  " bytes"
   );
   Last := Clock;
   for Index in reverse Byte_Index range 0..Count loop
      if Clock - Last > 1.0 then
         Put_Line ("Reading index" & Byte_Index'Image (Index));
         Last := Clock;
      end if;
      declare
         Block : constant Block_Type_Ref := Load (File'Access, Index);
      begin
         if Block = null then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in index"
               &  Byte_Index'Image (Index)
               &  " out of file (size"
               &  Byte_Index'Image (Get_Size (File))
               &  ")"
            )  );
         elsif Block (Get_Offset (Index)) /= Unsigned_8 (Index mod 256)
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error in index"
               &  Byte_Index'Image (Index)
               &  " wanted"
               &  Byte_Index'Image (Index mod 256)
               &  ", got"
               &  Unsigned_8'Image (Block (Get_Offset (Index)))
            )  );
         end if;
      end;
   end loop;
   Close (File);
   Put_Line ("... Done");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Blocking_Files;
