--                                                                    --
--  procedure Test_Generic_Sets     Copyright (c)  Dmitry A. Kazakov  --
--  Test program                                   Luebeck            --
--                                                 Autumn, 2009       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Test_Integer_Sets;
with Test_Discrete_Sets;

procedure Test_Generic_Sets is
begin
   declare
      use Test_Discrete_Sets;
      function Image (S : Set; Index : Integer := 1) return String is
      begin
         if Index > Get_Size (S) then
            return "empty";
         elsif Index = Get_Size (S) then
            return
            (  Image (From (S, Index))
            &  ".."
            &  Image (To (S, Index))
            );
         else
            return
            (  Image (From (S, Index))
            &  ".."
            &  Image (To (S, Index))
            &  ", "
            &  Image (S, Index + 1)
            );
         end if;
      end Image;
   begin
      declare
         S : Set;
      begin
         Add (S, 0, 3);
         if Is_In (S, -1) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: -1 in the set " & Image (S)
            );
         end if;
         if not Is_In (S, 0) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 0 not in the set " & Image (S)
            );
         end if;
         if not Is_In (S, 1) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 1 not in the set " & Image (S)
            );
         end if;
         if not Is_In (S, 3) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 3 not in the set " & Image (S)
            );
         end if;
         if Is_In (S, 4) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 4 in the set " & Image (S)
            );
         end if;
         Add (S, -5, -2);
         Add (S,  5, 7);
         if Get_Size (S) /= 3 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Size"
               &  Integer'Image (Get_Size (S))
               &  " /= 3, set "
               &  Image (S)
            )  );
         end if;
         Add (S, 1, 6);
         if Get_Size (S) /= 2 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Size"
               &  Integer'Image (Get_Size (S))
               &  " /= 2, set "
               &  Image (S)
            )  );
         end if;
         if S /= (Create (-5, -2) or Create (0, 7)) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Wrong set "
               &  Image (S)
            )  );
         end if;
         Remove (S, 0);
         if S /= (Create (-5, -2) or Create (1, 7)) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Wrong set "
               &  Image (S)
            )  );
         end if;
         Remove (S, -2, 0);
         if S /= (Create (-5, -3) or Create (1, 7)) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Wrong set "
               &  Image (S)
            )  );
         end if;
      end;
      declare
         S : Set;
      begin
         Add (S, 0);
         if not Is_In (S, 0) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 0 not in the set " & Image (S)
            );
         end if;
         Add (S, 1);
         if not Is_In (S, 1) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 1 not in the set " & Image (S)
            );
         end if;
         if Get_Size (S) /= 1 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Size"
               &  Integer'Image (Get_Size (S))
               &  " /= 1, set "
               &  Image (S)
            )  );
         end if;
      end;
      declare
         S : Set;
      begin
         Add (S, Integer'First);
         if not Is_In (S, Integer'First) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error:"
               &  Integer'Image (Integer'First)
               &  " not in the set "
               &  Image (S)
            )  );
         end if;
         Add (S, Integer'Last);
         if not Is_In (S, Integer'Last) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error:"
               &  Integer'Image (Integer'Last)
               &  " not in the set "
               &  Image (S)
            )  );
         end if;
         S := not S;
         if not Is_In (S, Integer'First + 1, Integer'Last - 1) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: not in the inversed set " & Image (S)
            );
         end if;
      end;
      declare
         S        : Set;
         From, To : Integer;
      begin
         for I in Integer range 1..50 loop
            Add (S, I);
         end loop;
         for I in reverse Integer range 51..100 loop
            Add (S, I);
         end loop;
         if Get_Size (S) /= 1 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Size"
               &  Integer'Image (Get_Size (S))
               &  " /= 1"
            )  );
         end if;
         Get (S, 1, From, To);
         if From /= 1 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: From"
               &  Integer'Image (From)
               &  " /= 1"
            )  );
         end if;
         if To /= 100 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: From"
               &  Integer'Image (To)
               &  " /= 100_000"
            )  );
         end if;
      end;
      declare
         S : Set;
      begin
         Add (S, Create (1) or Create (2) or Create (3));
         if S /= Create (1, 3) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong set " & Image (S)
            );
         end if;
      end;
      declare
         S : Set;
      begin
         if (not S) /= Create (Integer'First, Integer'Last) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong inverted set " & Image (S)
            );
         end if;
      end;
      declare
         S : constant Set := Create (33);
      begin
         if (not (not S)) /= S then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong inverted set " & Image (S)
            );
         end if;
      end;
      declare
         S : Set := Create (5, 6);
      begin
         Add (S, 1, 4);
         if S /= Create (1, 6) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong set " & Image (S)
            );
         end if;
      end;
      declare
         S : Set := Create (5, 8);
      begin
         Add (S, 6, 7);
         if S /= Create (5, 8) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong set " & Image (S)
            );
         end if;
      end;
      declare
         S : Set := Create (5, 8);
      begin
         Add (S, 1, 7);
         if S /= Create (1, 8) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong set " & Image (S)
            );
         end if;
      end;
      declare
         S : Set := Create (5, 8);
      begin
         Add (S, 5, 7);
         if S /= Create (5, 8) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong set " & Image (S)
            );
         end if;
      end;
      declare
         S : Set := Create (5, 8);
      begin
         Add (S, 5, 9);
         if S /= Create (5, 9) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong set " & Image (S)
            );
         end if;
      end;
      declare
         S : Set := Create (5, 8);
      begin
         Add (S, 8, 9);
         if S /= Create (5, 9) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong set " & Image (S)
            );
         end if;
      end;
      declare
         S : Set := Create (5, 8);
      begin
         Add (S, 9, 9);
         if S /= Create (5, 9) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong set " & Image (S)
            );
         end if;
      end;
   end;
   declare
      use Test_Integer_Sets;
      use Test_Integer_Sets.Integer_Sets;
   begin
         -- Forward
      declare
         S : Set;
      begin
         for I in Number range 1..100_000 loop
            Add (S, I);
         end loop;
         for Index in 1..Get_Size (S) loop
            if Number (Index) /= Get (S, Index) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Error:"
                  &  Integer'Image (Index)
                  &  " /="
                  &  Number'Image (Get (S, Index))
               )  );
            end if;
         end loop;
      end;
         -- Backward
      declare
         S : Set;
      begin
         for I in reverse Number range 1..100_000 loop
            Add (S, I);
         end loop;
         for Index in 1..Get_Size (S) loop
            if Number (Index) /= Get (S, Index) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Error:"
                  &  Integer'Image (Index)
                  &  " /="
                  &  Number'Image (Get (S, Index))
               )  );
            end if;
         end loop;
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Generic_Sets;
