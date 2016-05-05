--                                                                    --
--  package Test_Tables             Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  22:44 07 Apr 2016  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Test_Tables_Table;  use Test_Tables_Table;
with Test_Tables_Names;  use Test_Tables_Names;
with Text_IO;            use Text_IO;

procedure Test_Tables is
   Data    : Integer;
   Pointer : Integer;

   procedure Print (Folder : Table) is
   begin
      Put ("---------------- T A B L E ----------------");
      New_Line;
      Put ("Total tokens:" & Integer'Image (GetSize (Folder)));
      New_Line;
      for Token in 1..GetSize (Folder) loop
         Put (GetName (Folder, Token));
         New_Line;
      end loop;
      Put ("-------------------------------------------");
      New_Line;
   end Print;

   procedure Error (Text : String) is
   begin
      Put
      (  "Error in table operations - " & Text
      &  " Data:" & Integer'Image (Data)
      &  " Pointer:" & Integer'Image (Pointer)
      );
      New_Line;
      raise Data_Error;
   end Error;

   Copy    : Table;
   Text    : constant String := "123AA123";

begin
   Put_Line ("Testing tables ...");
   declare
      Folder  : Table;
   begin
      Add (Folder, "**", 1);
      Add (Folder, "*", 2);
      Add (Folder, "Very_very_very_long_name", 3);
      Add (Folder, "Name with space", 4);
      Add (Folder, "AAAAAA", 5);
      Add (Folder, "AAAA", 6);
      Add (Folder, "A", 7);
      Add (Folder, "AA", 8);
      if 8 /= Find (Folder, "AA") then
         Error ("Cannot find item");
      end if;
      Delete (Folder, "AAAA");
      Delete (Folder, "AAAAAA");
      Add (Folder, "AAAAAA", 5);
      Delete (Folder, "Name with space");
      Add (Folder, "AAB", 9);
      Add (Folder, "AAAAB", 10);
      if 8 /= Find (Folder, "AA") then
         Error ("Cannot find item");
      end if;
      Pointer := Text'First + 3;
      Get (Text, Pointer, Folder, Data);
      if Data /= 8 or Pointer /= Text'First + 5 then
         Error ("Cannot parse string");
      end if;
      Pointer := 1;
      Get ("AAB  ", Pointer, Folder, Data);
      if Data /= 9 or Pointer /= 4 then
         Error ("Cannot parse item AAB");
      end if;
      Pointer := 1;
      Get ("AAAAB", Pointer, Folder, Data);
      if Data /= 10 or Pointer /= 6 then
         Error ("Cannot parse item AAAAB");
      end if;
      Pointer := 1;
      Get ("AA]  ", Pointer, Folder, Data);
      if Data /= Find (Folder, "AA") or Pointer /= 3 then
         Error ("Cannot parse item AA]");
      end if;
      Pointer := 1;
      Get ("A]   ", Pointer, Folder, Data);
      if Data /= Find (Folder, "A") or Pointer /= 2 then
         Error ("Cannot parse item A]");
      end if;
      Pointer := 1;
      Get ("AA   ", Pointer, Folder, Data);
      if Data /= Find (Folder, "AA") or Pointer /= 3 then
         Error ("Cannot parse item AA<space>");
      end if;
      Pointer := 1;
      Get ("A   ", Pointer, Folder, Data);
      if Data /= Find (Folder, "A") or Pointer /= 2 then
         Error ("Cannot parse item A<space>");
      end if;
      Copy := Folder;
      for Index in 1..GetSize (Folder) loop
         if (  GetName (Folder, Index) /= GetName (Copy, Index)
            or GetTag  (Folder, Index) /= GetTag  (Copy, Index)
            )
         then
            Error ("Error in assignement");
         end if;
      end loop;
      Print (Folder);
      Erase (Folder);
      Add (Folder, "A", 1);
      Add (Folder, "A B", 2);
      Add (Folder, "AAC", 3);
      Pointer := 1;
      Get ("AB", Pointer, Folder, Data);
      if Data /= Find (Folder, "A") or Pointer /= 2 then
         Error ("Cannot parse item AB");
      end if;
      Add (Folder, "AACC", 4);
      Pointer := 1;
      Get ("AB", Pointer, Folder, Data);
      if Data /= Find (Folder, "A") or Pointer /= 2 then
         Error ("Cannot parse item AB");
      end if;
      Print (Folder);
   end;
--
-- Testing case-insensitive tables
--
   declare
      Folder : Dictionary;
   begin
      Add (Folder, "Test", 1);
      if GetName (Folder, 1) /= "Test" then
         Error ("Cannot get in a dictionary item Test");
      end if;
      if Find (Folder, "TEST") /= 1 then
         Error ("Cannot find in a dictionary item Test");
      end if;
      begin
         Add (Folder, "tEst", 1);
         Error ("Error in Add for item tEst");
      exception
         when Name_Error =>
            null;
      end;
      begin
         Add (Folder, "?tEst", 1);
         Error ("Error in Add for item ?tEst");
      exception
         when Constraint_Error =>
            null;
      end;
      Add (Folder, "Test1", 2);
      Add (Folder, "Test1_2", 3);
      Add (Folder, "Test 1", 4);
      Pointer := 1;
      Get ("TesT_1", Pointer, Folder, Data);
      if Data /= Find (Folder, "test") or Pointer /= 5 then
         Error ("Cannot parse item test");
      end if;
      Pointer := 2;
      Get (" TesT    1  ", Pointer, Folder, Data);
      if Data /= 4 or Pointer /= 11 then
         Error ("Cannot parse item Test 1");
      end if;
   end;
   Put_Line ("... Done");
exception
   when Fault : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Fault));
end Test_Tables;
