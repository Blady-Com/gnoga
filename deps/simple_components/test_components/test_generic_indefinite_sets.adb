--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Generic_Indefinite_Sets                Luebeck            --
--  Test program                                   Spring, 2012       --
--                                                                    --
--                                Last revision :  14:32 02 Apr 2012  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with Test_String_Sets;  use Test_String_Sets;

procedure Test_Generic_Indefinite_Sets is
begin
      -- Forward
   declare
      S, R : Set;
   begin
      for I in 1..100_000 loop
         Add (S, Integer'Image (I));
      end loop;
      if " 1" /= Get (S, 1) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error: 1 /=" & Get (S, 1)
         );
      end if;
      if " 10" /= Get (S, 2) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error: 10 /=" & Get (S, 2)
         );
      end if;
      if " 100" /= Get (S, 3) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error: 100 /=" & Get (S, 3)
         );
      end if;
      for Index in 1..Get_Size (S) loop
         if not Is_In (S, Integer'Image (Index)) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error:"
               &  Integer'Image (Index)
               &  " not found"
            )  );
         end if;
      end loop;
      -- Backward
      for I in reverse 1..100_000 loop
         Add (R, Integer'Image (I));
      end loop;
      if S /= R then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Error: inequal sets"
         );
      end if;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Generic_Indefinite_Sets;
