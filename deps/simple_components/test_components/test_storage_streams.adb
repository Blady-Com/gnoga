--                                                                    --
--  procedure Test_Storage_Streams  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2011       --
--                                                                    --
--                                Last revision :  19:18 30 Apr 2018  --
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

with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Streams;      use Ada.Streams;
with Ada.Text_IO;      use Ada.Text_IO;
with Storage_Streams;  use Storage_Streams;

procedure Test_Storage_Streams is
begin
   Put_Line ("Testing storage streams ...");
   declare
      Stream : aliased Storage_Stream (20);
   begin
      String'Write (Stream'Access, "1");
      String'Write (Stream'Access, "23");
      String'Write (Stream'Access, "456");
      String'Write (Stream'Access, "789a");
      String'Write (Stream'Access, "12345");
      String'Write (Stream'Access, "6789b1");
      String'Write (Stream'Access, "2345678");
      String'Write (Stream'Access, "9c123456");
      String'Write (Stream'Access, "789d12345");
      String'Write (Stream'Access, "6789e12345");
      String'Write (Stream'Access, "6789f123456");
      String'Write (Stream'Access, "789g12345678");
      String'Write (Stream'Access, "9h123456789i1");
      String'Write (Stream'Access, "23456789j12345");
      String'Write (Stream'Access, "6789k123456789l");
      String'Write (Stream'Access, "123456789m123456");
      String'Write (Stream'Access, "789n123456789o123");
      String'Write (Stream'Access, "456789p123456789q1");
      String'Write (Stream'Access, "23456789r123456789s");
      String'Write (Stream'Access, "123456789t123456789u");
      String'Write (Stream'Access, "123456789v123456789w1");
      String'Write (Stream'Access, "23456789x123456789y123");
      String'Write (Stream'Access, "456789z");
      declare
         Buffer : String (1..10);
         Ends   : constant String := "abcdefghijklmnopqrstuvwxyz";
      begin
         for Index in Ends'Range loop
            String'Read (Stream'Access, Buffer);
            if Buffer /= "123456789" & Ends (Index) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Read "
                  &  Buffer
                  &  ", expected "
                  &  "123456789" & Ends (Index)
               )  );
             end if;
          end loop;
      end;
   end;
   declare
      Stream : aliased Storage_Stream (13);
      Item   : Integer;
   begin
      for Index in 1..1_000 loop
         Integer'Write (Stream'Access, Index);
      end loop;
      Put_Line
      (  "  Elements written, size"
      &  Stream_Element_Count'Image (Get_Size (Stream))
      );
      for Index in 1..1_000 loop
         Integer'Read (Stream'Access, Item);
         if Item /= Index then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Read"
               &  Integer'Image (Item)
               &  ", expected"
               &  Integer'Image (Index)
            )  );
         end if;
      end loop;
      Put_Line
      (  "  Elements read, size"
      &  Stream_Element_Count'Image (Get_Size (Stream))
      );
   end;
   declare
      Stream : aliased Storage_Stream (3);
   begin
      for Index in Integer range 1..4 loop
         Integer'Write (Stream'Access, Index);
      end loop;
      Put_Line
      (  "Stream size:"
      &  Stream_Element_Count'Image (Get_Size (Stream))
      );
      for Index in Integer range 1..4 loop
         declare
            Value : Integer;
         begin
            Integer'Read (Stream'Access, Value);
            if Index /= Value then
               Raise_Exception
               (  Data_Error'Identity,
                  "Read error for" & Integer'Image (Index)
               );
            end if;
         end;
      end loop;
      if Get_Size (Stream) /= 0 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Read error, stream is not empty,"
            &  Stream_Element_Count'Image (Get_Size (Stream))
            &  " items left"
         )  );
      end if;
      Erase (Stream);
      declare
         Data : Stream_Element_Array (1..100);
         Last : Stream_Element_Offset;
      begin
         Read (Stream, Data, Last);
         if Last /= 0 then
            Raise_Exception
            (  Data_Error'Identity,
               "Read error, read after end"
            );
         end if;
      end;
   end;
   Put_Line ("... Done");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Storage_Streams;
