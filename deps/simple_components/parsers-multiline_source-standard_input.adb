--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Multiline_Source.                   Luebeck            --
--        Standard_Input                           Winter, 2004       --
--  Implementation                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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

with Ada.Text_IO;  use Ada.Text_IO;

package body Parsers.Multiline_Source.Standard_Input is

   Increment : constant Integer := 512;

   procedure Get_Line (Code : in out Source) is
      Size : Natural;
   begin
      if Code.Buffer = null then
         raise End_Error;
      end if;
      Get_Line (Code.Buffer.all, Size);
      Code.Length := Size;
      while Code.Length = Code.Buffer'Last loop
         declare
            Old_Line : String_Ptr := Code.Buffer;
         begin
            Code.Buffer := new String (1..Old_Line'Length + Increment);
            Code.Buffer (1..Old_Line'Length) := Old_Line.all;
            Free (Old_Line);
         end;
         Get_Line
         (  Code.Buffer (Code.Length + 1..Code.Buffer'Last),
            Size
         );
         Code.Length := Size;
      end loop;
   exception
      when others =>
         Free (Code.Buffer);
         raise;
   end Get_Line;

end Parsers.Multiline_Source.Standard_Input;
