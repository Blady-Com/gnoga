--                                                                    --
--  procedure Test_String_Streams   Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  17:51 10 Aug 2012  --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with Parsers.Multiline_Source;  use Parsers.Multiline_Source;
with Strings_Edit.Streams;      use Strings_Edit.Streams;

with Parsers.Multiline_Source.Stream_IO;
use  Parsers.Multiline_Source.Stream_IO;

procedure Test_String_Streams is
begin
   declare
      Text : aliased String_Stream (4);
   begin
      Set (Text, "abcd");
      declare
         Input : aliased Stream_IO.Source (Text'Access);
      begin
         if Get_Line (Input) /= "abcd" then
            Raise_Exception (Data_Error'Identity, "Wrong line read");
         end if;
      end;
   end;
   declare
      Text : aliased String_Stream (10);
   begin
      Set (Text, "abcd");
      declare
         Input : aliased Stream_IO.Source (Text'Access);
      begin
         if Get_Line (Input) /= "abcd" then
            Raise_Exception (Data_Error'Identity, "Wrong line read");
         end if;
      end;
   end;
   declare
      Text : aliased String_Stream (4);
   begin
      Set (Text, "abcd");
      declare
         Input : aliased Stream_IO.Source (Text'Access);
      begin
         if Get_Line (Input) /= "abcd" then
            Raise_Exception (Data_Error'Identity, "Wrong line read");
         end if;
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_String_Streams;
