--                                                                    --
--  procedure Test_Generic_Maps     Copyright (c)  Dmitry A. Kazakov  --
--  Test program                                   Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  08:25 05 May 2020  --
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

with Interfaces;
with Test_Bounded_Maps;
with Test_Discrete_Maps;

procedure Test_Generic_Maps is
begin
   declare
      use Interfaces;
      use Test_Bounded_Maps;
      function Image (S : Map; Index : Integer := 1) return String is
         Buffer  : String (1..1024 * 10);
         Pointer : Integer := 1;
      begin
         if Index > Get_Size (S) then
            return "empty";
         end if;
         for Index in 1..Get_Size (S) loop
            if Index > 1 then
               Put (Buffer, Pointer, ",");
            end if;
            Put
            (  Buffer,
               Pointer,
               (  Unsigned_32'Image (Get_Key (S, Index))
               &  "->"
               &  Image (Get (S, Index))
            )  );
         end loop;
         return Buffer (1..Pointer - 1);
      end Image;
      type Keys   is array (Positive range <>) of Unsigned_32;
      type Values is array (Positive range <>) of Integer;
      procedure Check (S : Map; K : Keys; V : Values; T : String) is
      begin
         if Get_Size (S) /= K'Length then
            Raise_Exception (Data_Error'Identity, T & " invalid size");
         end if;
         for Index in K'Range loop
            if Get_Key (S, Index) /= K (Index) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  T
                  &  " invalid key"
                  &  Unsigned_32'Image (Get_Key (S, Index))
                  &  " at "
                  &  Image (Index)
                  &  " expected"
                  &  Unsigned_32'Image (K (Index))
               )  );
            elsif Get (S, Index) /= V (Index) then
               Raise_Exception
               (  Data_Error'Identity,
                  (  T
                  &  " invalid value "
                  &  Image (Get (S, Index))
                  &  " at "
                  &  Image (Index)
                  &  " expected"
                  &  Image (V (Index))
               )  );
            end if;
         end loop;
      end Check;
   begin
      declare
         S : Map (6);
         T : Map (5);
      begin
         if Find (S, 4) /= -1 then
            Raise_Exception (Data_Error'Identity, "find 4 in empty");
         elsif not (1 < S) then
            Raise_Exception (Data_Error'Identity, "1 < empty");
         elsif not (16#FFFF_FFFF# <= S) then
            Raise_Exception (Data_Error'Identity, "max <= empty");
         elsif not (1 > S) then
            Raise_Exception (Data_Error'Identity, "1 > empty");
         elsif not (1 >= S) then
            Raise_Exception (Data_Error'Identity, "1 >= empty");
         elsif S < 1 then
            Raise_Exception (Data_Error'Identity, "empty < 1");
         elsif S <= 1 then
            Raise_Exception (Data_Error'Identity, "empty <= 1");
         elsif S > 1 then
            Raise_Exception (Data_Error'Identity, "empty > 1");
         elsif S >= 1 then
            Raise_Exception (Data_Error'Identity, "empty >= 1");
         end if;
         Add (S, 4, 4, Override_Least);
         Check (S, (1 => 4), (1 => 4), "add 4");
         if Find (S, 4) /= 1 then
            Raise_Exception (Data_Error'Identity, "find 4 in (4->4)");
         end if;
         if S <= 1 then
            Raise_Exception (Data_Error 'Identity, Image (S) & " <= 1");
         elsif not (S <= 5) then
            Raise_Exception (Data_Error 'Identity, Image (S) & " <= 5");
         elsif not (1 < S) then
            Raise_Exception (Data_Error'Identity, "1 <" & Image (S));
         elsif S < 1 then
            Raise_Exception (Data_Error'Identity, Image (S) & " < 1");
         elsif S < 4 then
            Raise_Exception (Data_Error'Identity, Image (S) & " < 4");
         elsif not (S < 5) then
            Raise_Exception (Data_Error'Identity, Image (S) & " < 5");
         elsif 16#FFFF_FFFF# <= S then
            Raise_Exception (Data_Error'Identity, "max <=" & Image (S));
         elsif 4 > S then
            Raise_Exception (Data_Error'Identity, "4 >" & Image (S));
         elsif not (4 >= S) then
            Raise_Exception (Data_Error'Identity, "4 >=" & Image (S));
         elsif not (S <= 4) then
            Raise_Exception (Data_Error 'Identity, Image (S) & " <= 4");
         elsif 1 >= S then
            Raise_Exception (Data_Error'Identity, "1 >=" & Image (S));
         elsif not (S > 1) then
            Raise_Exception (Data_Error'Identity, Image (S) & " > 1");
         elsif not (S >= 1) then
            Raise_Exception (Data_Error'Identity, Image (S) & " >= 1");
         end if;
         Add (S, 6, 6, Override_Least);
         Check (S, (4, 6), (4, 6), "add 4,6");
         if Find (S, 5) /= -2 then
            Raise_Exception (Data_Error'Identity, "find 5 in (4,6)");
         elsif S <= 1 then
            Raise_Exception (Data_Error 'Identity, Image (S) & " <= 1");
         elsif S <= 5 then
            Raise_Exception (Data_Error 'Identity, Image (S) & " <= 5");
         elsif not (S <= 6) then
            Raise_Exception (Data_Error 'Identity, Image (S) & " <= 6");
         elsif not (1 < S) then
            Raise_Exception (Data_Error'Identity, "1 <" & Image (S));
         elsif S < 1 then
            Raise_Exception (Data_Error'Identity, Image (S) & " < 1");
         elsif S < 4 then
            Raise_Exception (Data_Error'Identity, Image (S) & " < 4");
         elsif S < 5 then
            Raise_Exception (Data_Error'Identity, Image (S) & " < 5");
         elsif S < 6 then
            Raise_Exception (Data_Error'Identity, Image (S) & " < 6");
         elsif not (S < 7) then
            Raise_Exception (Data_Error'Identity, Image (S) & " < 7");
         elsif 16#FFFF_FFFF# <= S then
            Raise_Exception (Data_Error'Identity, "max <=" & Image (S));
         elsif 4 > S then
            Raise_Exception (Data_Error'Identity, "4 >" & Image (S));
         elsif 4 >= S then
            Raise_Exception (Data_Error'Identity, "4 >=" & Image (S));
         elsif not (6 >= S) then
            Raise_Exception (Data_Error'Identity, "6 >=" & Image (S));
         elsif S <= 4 then
            Raise_Exception (Data_Error 'Identity, Image (S) & " <= 4");
         elsif not (S <= 6) then
            Raise_Exception (Data_Error 'Identity, Image (S) & " <= 6");
         elsif 1 >= S then
            Raise_Exception (Data_Error'Identity, "1 >=" & Image (S));
         elsif not (S > 1) then
            Raise_Exception (Data_Error'Identity, Image (S) & " > 1");
         elsif not (S >= 1) then
            Raise_Exception (Data_Error'Identity, Image (S) & " >= 1");
         end if;
         Add (S, 5, 5, Override_Least);
         Check (S, (4, 5, 6), (4, 5, 6), "add 4,6,5");
         Add (S, 9, 9, Override_Least);
         Check (S, (4, 5, 6, 9), (4, 5, 6, 9), "add 4,6,5,9");
         Add (S, 7, 7, Override_Least);
         Check (S, (4, 5, 6, 7, 9), (4, 5, 6, 7, 9), "add 4,6,5,7,9");
         if Inf (S, 4) /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               "Inf 4 in (4,5,6,7,9)"
            );
         end if;
         if Inf (S, 3) /= 0 then
            Raise_Exception
            (  Data_Error'Identity,
               "Inf 3 in (4,5,6,7,9)"
            );
         end if;
         if Inf (S, 5) /= 2 then
            Raise_Exception
            (  Data_Error'Identity,
               "Inf 5 in (4,5,6,7,9)"
            );
         end if;
         if Inf (S, 8) /= 4 then
            Raise_Exception
            (  Data_Error'Identity,
               "Inf 8 in (4,5,6,7,9)"
            );
         end if;
         if Inf (S, 9) /= 5 then
            Raise_Exception
            (  Data_Error'Identity,
              "Inf 9 in (4,5,6,7,9)"
            );
         end if;
         if Inf (S, 89) /= 5 then
            Raise_Exception
            (  Data_Error'Identity,
               "Inf 89 in (4,5,6,7,9)"
            );
         end if;
         if Sup (S, 4) /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               "Sup 4 in (4,5,6,7,9)"
            );
         end if;
         if Sup (S, 3) /= 1 then
            Raise_Exception
            (  Data_Error'Identity,
               "Sup 3 in (4,5,6,7,9)"
            );
         end if;
         Add (S, 1, 1, Override_Least);
         Check
         (  S,
            (1, 4, 5, 6, 7, 9),
            (1, 4, 5, 6, 7, 9),
            "add 4,6,5,7,9,1"
         );
         Add (S, 0, 0, Override_Least);
         Check
         (  S,
            (0, 1, 4, 5, 6, 7),
            (0, 1, 4, 5, 6, 7),
            "add 4,6,5,7,9,1,0"
         );
         Add (S, 2, 2, Override_Least);
         Check
         (  S,
            (1, 2, 4, 5, 6, 7),
            (1, 2, 4, 5, 6, 7),
            "add 4,6,5,7,9,1,0,2"
         );
         Add (S, 3, 3, Override_Greatest);
         Check
         (  S,
            (1, 2, 3, 4, 5, 6),
            (1, 2, 3, 4, 5, 6),
            "add 4,6,5,7,9,1,0,2,3"
         );
         Add (S, 7, 7, Override_Greatest);
         Check
         (  S,
            (2, 3, 4, 5, 6, 7),
            (2, 3, 4, 5, 6, 7),
            "add 4,6,5,7,9,1,0,2,3,7"
         );
         Add (S, 8, 8, Override_Greatest);
         Check
         (  S,
            (3, 4, 5, 6, 7, 8),
            (3, 4, 5, 6, 7, 8),
            "add 4,6,5,7,9,1,0,2,3,7,8"
         );
         Replace (S, 7, -7, Override_Greatest);
         Check
         (  S,
            (3, 4, 5, 6, 7, 8),
            (3, 4, 5, 6,-7, 8),
            "add 4,6,5,7,9,1,0,2,3,7,8 replace 7"
         );
         Remove (S, Unsigned_32'(7));
         Check
         (  S,
            (3, 4, 5, 6, 8),
            (3, 4, 5, 6, 8),
            "add 4,6,5,7,9,1,0,2,3,7,8 replace 7, remove K7"
         );
         Remove (S, Integer'(1));
         Check
         (  S,
            (4, 5, 6, 8),
            (4, 5, 6, 8),
            "add 4,6,5,7,9,1,0,2,3,7,8 replace 7, remove K7,1"
         );
         Remove (S, Integer'(3));
         Check
         (  S,
            (4, 5, 8),
            (4, 5, 8),
            "add 4,6,5,7,9,1,0,2,3,7,8 replace 7, remove K7,1,3"
         );
         Add (S, 6, 6, Override_Greatest);
         Check
         (  S,
            (4, 5, 6, 8),
            (4, 5, 6, 8),
            "add 4,6,5,7,9,1,0,2,3,7,8 replace 7, remove K7,1,3 add 6"
         );
         Add (S, 7, 7, Override_Greatest);
         Check
         (  S,
            (4, 5, 6, 7, 8),
            (4, 5, 6, 7, 8),
            "add 4,6,5,7,9,1,0,2,3,7,8 replace 7, remove K7,1,3 add 6,7"
         );
         Erase (S);
         Add (S, 1, 1, Override_Least);
         Check
         (  S,
            (1 => 1),
            (1 => 1),
            "add 1"
         );
         Add (S, 2, 2, Override_Least);
         Check
         (  S,
            (1, 2),
            (1, 2),
            "add 1,2"
         );
         Add (S, 3, 3, Override_Least);
         Check
         (  S,
            (1, 2, 3),
            (1, 2, 3),
            "add 1,2,3"
         );
         Add (S, 4, 4, Override_Least);
         Check
         (  S,
            (1, 2, 3, 4),
            (1, 2, 3, 4),
            "add 1,2,3,4"
         );
         Add (S, 5, 5, Override_Least);
         Check
         (  S,
            (1, 2, 3, 4, 5),
            (1, 2, 3, 4, 5),
            "add 1,2,3,4,5"
         );
         Add (S, 6, 6, Override_Least);
         Check
         (  S,
            (1, 2, 3, 4, 5, 6),
            (1, 2, 3, 4, 5, 6),
            "add 1,2,3,4,5,6"
         );
         Add (S, 7, 7, Override_Least);
         Check
         (  S,
            (2, 3, 4, 5, 6, 7),
            (2, 3, 4, 5, 6, 7),
            "add 1,2,3,4,5,6,7"
         );
         Remove (S, Positive'(1));
         Check
         (  S,
            (3, 4, 5, 6, 7),
            (3, 4, 5, 6, 7),
            "add 1,2,3,4,5,6,7 remove 2"
         );
         Add (S, 8, 8, Override_Least);
         Check
         (  S,
            (3, 4, 5, 6, 7, 8),
            (3, 4, 5, 6, 7, 8),
            "add 1,2,3,4,5,6,7, remove 2, add 8"
         );
         Remove (S, Positive'(1));
         Check
         (  S,
            (4, 5, 6, 7, 8),
            (4, 5, 6, 7, 8),
            "add 1,2,3,4,5,6,7, remove 2, add 8, remove 3"
         );
         Add (S, 9, 9, Override_Least);
         Check
         (  S,
            (4, 5, 6, 7, 8, 9),
            (4, 5, 6, 7, 8, 9),
            "add 1,2,3,4,5,6,7, remove 2, add 8, remove 3, add 9"
         );
         Erase (S);
         Add (S,  1,  1, Override_Least);
         Add (S,  2,  2, Override_Least);
         Add (S,  3,  3, Override_Least);
         Add (S,  4,  4, Override_Least);
         Add (S,  5,  5, Override_Least);
         Add (S,  6,  6, Override_Least);
         Add (S,  7,  7, Override_Least);  -- First = 2
         Add (S,  8,  8, Override_Least);  --       = 3
         Add (S,  9,  9, Override_Least);  --       = 4
         Add (S, 10, 10, Override_Least);  --       = 5
         Add (S, 11, 11, Override_Least);  --       = 6
         Check
         (  S,
            (6, 7, 8, 9, 10, 11),
            (6, 7, 8, 9, 10, 11),
            "add 1..11"
         );
         Add (S, 12, 12, Override_Least);  --       = 1
         Check
         (  S,
            (7, 8, 9, 10, 11, 12),
            (7, 8, 9, 10, 11, 12),
            "add 1..11, 12"
         );
         Add (S, 13, 13, Override_Least);  --       = 2
         Check
         (  S,
            (8, 9, 10, 11, 12, 13),
            (8, 9, 10, 11, 12, 13),
            "add 1..11, 12, 13"
         );
         Add (T, 1, 1, Override_Least);
         Add (T, 2, 2, Override_Least);
         Add (T, 3, 3, Override_Least);
         Add (T, 4, 4, Override_Least);
         Add (T, 5, 5, Override_Least);
         Add (T, 6, 6, Override_Least);
         Check
         (  T,
            (2, 3, 4, 5, 6),
            (2, 3, 4, 5, 6),
            "add 1..6"
         );
         Remove (T, Positive'(1));
         Check
         (  T,
            (3, 4, 5, 6),
            (3, 4, 5, 6),
            "add 1..6, remove 1"
         );
         Add (T, 7, 7, Override_Least);
         Check
         (  T,
            (3, 4, 5, 6, 7),
            (3, 4, 5, 6, 7),
            "add 1..6, remove 1, add 7"
         );
         Remove (T, Positive'(1));
         Check
         (  T,
            (4, 5, 6, 7),
            (4, 5, 6, 7),
            "add 1..6, remove 1, add 7, remove 1"
         );
         Add (T, 8, 8, Override_Least);
         Check
         (  T,
            (4, 5, 6, 7, 8),
            (4, 5, 6, 7, 8),
            "add 1..6, remove 1, add 7, remove 1, add 8"
         );
         Remove (T, Positive'(1));
         Check
         (  T,
            (5, 6, 7, 8),
            (5, 6, 7, 8),
            "add 1..6, remove 1, add 7, remove 1, add 8, remove 1"
         );
         Add (T, 9, 9, Override_Least);
         Check
         (  T,
            (5, 6, 7, 8, 9),
            (5, 6, 7, 8, 9),
            "add/remove 1..9"
         );
         Remove (T, Positive'(1));
         Check
         (  T,
            (6, 7, 8, 9),
            (6, 7, 8, 9),
            "add/remove 1..9, remove 1"
         );
         Add (T, 10, 10, Override_Least);
         Check
         (  T,
            (6, 7, 8, 9, 10),
            (6, 7, 8, 9, 10),
            "add/remove 1..10"
         );
         Remove (T, Positive'(1));
         Check
         (  T,
            (7, 8, 9, 10),
            (7, 8, 9, 10),
            "add/remove 1..10, remove 1"
         );
         Add (T, 11, 11, Override_Least);
         Check
         (  T,
            (7, 8, 9, 10, 11),
            (7, 8, 9, 10, 11),
            "add/remove 1..11"
         );
      end;
   end;
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
