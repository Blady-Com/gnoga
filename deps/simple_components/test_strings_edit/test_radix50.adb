--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_RADIX50                           Luebeck            --
--  Test                                           Autumn, 2018       --
--                                                                    --
--                                Last revision :  12:27 04 Nov 2018  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Strings_Edit.UTF8.RADIX50;
use  Strings_Edit.UTF8.RADIX50;

procedure Test_RADIX50 is
   Text_0 : constant String := "A";
   Text_1 : constant String := "12FTNZ%.$ Z9";
   Text_2 : constant String := "12FTNZ%.$ Z9##";
begin
   if From_RADIX50 (To_RADIX50 (Text_0)) /= "A  " then
      Raise_Exception
      (  Data_Error'Identity,
         "Encoding " & Text_0 & " error"
      );
   end if;
   if From_RADIX50 (To_RADIX50 (Text_1)) /= Text_1 then
      Raise_Exception
      (  Data_Error'Identity,
         "Encoding " & Text_1 & " error"
      );
   end if;
   if From_RADIX50 (To_RADIX50 (Text_2, '.')) /= "12FTNZ%.$ Z9.. " then
      Raise_Exception
      (  Data_Error'Identity,
         "Encoding " & Text_2 & " error"
      );
   end if;
exception
   when Error : others =>
      Put ("Error: ");
      Put_Line (Exception_Information (Error));
end Test_RADIX50;
