--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_OID                                    Luebeck            --
--  Test                                           Summer, 2019       --
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

with Strings_Edit.Object_Identifiers;
use  Strings_Edit.Object_Identifiers;

procedure Test_OID is
   procedure Check_1 (OID : Object_Identifier) is
      Other     : Object_Identifier (1..100);
      Last      : Integer;
      Text      : String (1..500);
      Pointer_1 : Integer := 1;
      Pointer_2 : Integer := 1;
   begin
      Put (Text, Pointer_1, OID);
      Get (Text (1..Pointer_1 - 1), Pointer_2, Other, Last);
      if OID /= Other (1..Last) then
         Raise_Exception
         (  Data_Error'Identity,
            (  "OID "
            &  Image (Other (1..Last))
            &  " /= "
            &  Image (OID)
            &  " (expected) "
            &  Text (1..Pointer_1)
            &  " Last="
            &  Image (Last)
         )  );
      end if;
      if Pointer_1 /= Pointer_2 then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Pointer "
            &  Image (Pointer_1)
            &  " /= "
            &  Image (Pointer_2)
            &  " (expected)"
         )  );
      end if;
   end Check_1;

begin
   Check_1 ((1, 2, 3, 4));
   Check_1 ((1 => 2));
end Test_OID;
