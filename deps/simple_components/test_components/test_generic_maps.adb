--                                                                    --
--  procedure Test_Generic_Maps     Copyright (c)  Dmitry A. Kazakov  --
--  Test program                                   Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  14:26 27 May 2012  --
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

with Test_Discrete_Maps;

procedure Test_Generic_Maps is
begin
   declare
      use Test_Discrete_Maps;
      function "or" (Left, Right : Map) return Map is
         Result : Map := Left;
      begin
         Replace (Result, Right);
         return Result;
      end "or";

      function Image (S : Map; Index : Integer := 1) return String is
      begin
         if Index > Get_Size (S) then
            return "empty";
         elsif Index = Get_Size (S) then
            return
            (  Image (From (S, Index))
            &  ".."
            &  Image (To (S, Index))
            &  "->"
            &  Get (S, Index)
            );
         else
            return
            (  Image (From (S, Index))
            &  ".."
            &  Image (To (S, Index))
            &  "->"
            &  Get (S, Index)
            &  ", "
            &  Image (S, Index + 1)
            );
         end if;
      end Image;
   begin
      declare
         S        : Map;
         From, To : Integer;
      begin
         for I in Integer range 1..5 loop
            Add (S, I, 'a');
            if S /= Create (1, I, 'a') then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Error: Wrong Map "
                  &  Image (S)
                  &  " /= 1.." & Image (I) & "->a"
               )  );
            end if;
         end loop;
         for I in reverse Integer range 7..10 loop
            Add (S, I, 'a');
            if S /= (Create (1, 5, 'a') or Create (I, 10, 'a')) then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "Error: Wrong Map "
                  &  Image (S)
                  &  " /= "
                  &  Image ((Create (1, 5, 'a') or Create (I, 10, 'a')))
               )  );
            end if;
         end loop;
         Add (S, 6, 'a');
         if Get_Size (S) /= 1 then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Size " & Image (S)
            );
         end if;
         Get_Key (S, 1, From, To);
         if From /= 1 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: From"
               &  Integer'Image (From)
               &  " /= 1 "
               &  Image (S)
            )  );
         end if;
         if To /= 10 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: To"
               &  Integer'Image (To)
               &  " /= 10"
               &  Image (S)
            )  );
         end if;
      end;
      declare
         S : Map;
      begin
         Add (S, 0, 3, 'a');
         if Is_In (S, -1) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: -1 in the Map " & Image (S)
            );
         end if;
         if not Is_In (S, 0) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 0 not in the Map " & Image (S)
            );
         end if;
         if not Is_In (S, 1) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 1 not in the Map " & Image (S)
            );
         end if;
         if not Is_In (S, 3) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 3 not in the Map " & Image (S)
            );
         end if;
         if Is_In (S, 4) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 4 in the Map " & Image (S)
            );
         end if;
         Add (S, -5, -2, 'a');
         if S /= (Create (-5, -2, 'a') or Create (0, 3, 'a')) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Adding interval "
               &  Image (S)
               &  " /= -5..-2->a, 0..7->a"
            )  );
         end if;
         Add (S, 5, 7, 'a');
         if S /= (  Create (-5, -2, 'a')
                 or Create ( 0,  3, 'a')
                 or Create ( 5,  7, 'a')
                 )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Adding third interval "
               &  Image (S)
               &  " /= -5..-2->a, 0..3->a, 5..7->a"
            )  );
         end if;
         Add (S, 1, 6, 'a');
         if S /= (Create (-5, -2, 'a') or Create (0, 7, 'a')) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Wrong map after merging "
               &  Image (S)
               &  " /= -5..-2->a, 0..7->a"
            )  );
         end if;
         Remove (S, 0);
         if S /= (Create (-5, -2, 'a') or Create (1, 7, 'a')) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: After removing, wrong Map "
               &  Image (S)
               &  " /= -5..-2->a, 1..7->a"
            )  );
         end if;
         Remove (S, -2, 0);
         if S /= (Create (-5, -3, 'a') or Create (1, 7, 'a')) then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Wrong Map "
               &  Image (S)
               &  " /= -5..-3->a, 1..7->a"
            )  );
         end if;
      end;
      declare
         S : Map;
      begin
         Add (S, 0, 'a');
         if not Is_In (S, 0) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 0 not in the Map " & Image (S)
            );
         end if;
         Add (S, 1, 'a');
         if not Is_In (S, 1) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: 1 not in the Map " & Image (S)
            );
         end if;
         if Get_Size (S) /= 1 then
            Raise_Exception
            (  Constraint_Error'Identity,
               (  "Error: Size"
               &  Integer'Image (Get_Size (S))
               &  " /= 1, Map "
               &  Image (S)
            )  );
         end if;
      end;
      declare
         S : Map;
      begin
         Add (S, Create (1, 'a') or Create (2, 'a') or Create (3, 'a'));
         if S /= Create (1, 3, 'a') then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong Map " & Image (S)
            );
         end if;
         Replace (S, 2, 2, 'b');
         if S /= (  Create (1, 'a')
                 or Create (2, 'b')
                 or Create (3, 'a')
                 )
         then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: Wrong Map " & Image (S)
            );
         end if;
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Generic_Maps;
