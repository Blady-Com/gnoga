--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_KOI8                                   Luebeck            --
--  Test                                           Autumn, 2018       --
--                                                                    --
--                                Last revision :  18:40 01 Aug 2019  --
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

with Strings_Edit.UTF8.KOI8;
use  Strings_Edit.UTF8.KOI8;

procedure Test_KOI8 is
begin
   for Code in Character range
               Character'Val (0)..Character'Val (255) loop
      if To_KOI8 (From_KOI8 (Code)) /= Code then
          Raise_Exception
          (  Data_Error'Identity,
             (  "Encoding "
             &  Image (Integer (Character'Pos (Code)))
             &  " error"
          )  );
      end if;
   end loop;
exception
   when Error : others =>
      Put ("Error: ");
      Put_Line (Exception_Information (Error));
end Test_KOI8;
