--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_ISO_8859_3                           Luebeck            --
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

with Strings_Edit.UTF8.ISO_8859_3;
use  Strings_Edit.UTF8.ISO_8859_3;

procedure Test_ISO_8859_3 is
begin
   for Code in Character range
               Character'Val (0)..Character'Val (255) loop
      case Character'Pos (Code) is
         when 16#80#..16#9F# |
              16#A5# |
              16#AE# |
              16#BE# |
              16#C3# |
              16#D0# |
              16#E3# |
              16#F0# =>
            if To_ISO_8859_3
               (  From_ISO_8859_3
                  (  Code,
                     Character'Pos ('?')
               )  )  /= '?'
            then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Encoding "
                  &  Image (Integer (Character'Pos (Code)))
                  &  " error"
               )  );
            end if;
         when others =>
            if To_ISO_8859_3 (From_ISO_8859_3 (Code)) /= Code then
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Encoding "
                  &  Image (Integer (Character'Pos (Code)))
                  &  " error"
               )  );
            end if;
      end case;
   end loop;
exception
   when Error : others =>
      Put ("Error: ");
      Put_Line (Exception_Information (Error));
end Test_ISO_8859_3;
