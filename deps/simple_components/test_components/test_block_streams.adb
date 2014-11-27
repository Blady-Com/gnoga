--                                                                    --
--  procedure Test_Block_Streams    Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Autumn, 2011       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;
with Interfaces;            use Interfaces;
with Block_Streams;         use Block_Streams;
with Strings_Edit.Streams;  use Strings_Edit.Streams;

procedure Test_Block_Streams is
   Block_Size : constant := 153;
   Medium : aliased String_Stream (20_000);
   Input  : aliased Input_Block_Stream  (Medium'Access, Block_Size);
   Output : aliased Output_Block_Stream (Medium'Access, Block_Size);
begin
   Put_Line ("Testing block streams ...");
   for Item in Unsigned_32 range 0..1_000 loop
      Unsigned_32'Write (Output'Access, Item);
   end loop;
   Flush (Output);
   Rewind (Medium);
   for Item in Unsigned_32 range 0..1_000 loop
      declare
         Value : Unsigned_32;
      begin
         Unsigned_32'Read (Input'Access, Value);
         if Value /= Item then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error read"
               &  Unsigned_32'Image (Value)
               &  " wanted"
               &  Unsigned_32'Image (Item)
               &  ", block"
               &  Unsigned_32'Image (Get_Block_No (Output))
            )  );
         end if;
      end;
   end loop;
   Put_Line ("... Done");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Block_Streams;
