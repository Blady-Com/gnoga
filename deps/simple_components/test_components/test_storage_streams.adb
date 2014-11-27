--                                                                    --
--  procedure Test_Storage_Streams  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2011       --
--                                                                    --
--                                Last revision :  22:35 02 Dec 2011  --
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
   Stream : aliased Storage_Stream (13);
   Item   : Integer;
begin
   Put_Line ("Testing storage streams ...");
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
   Put_Line ("... Done");
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Storage_Streams;
