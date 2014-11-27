--                                                                    --
--  procedure Test_UTF8_Tables      Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Spring, 2008       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2010  --
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

with Ada.Exceptions;  use Ada.Exceptions;
with Text_IO;         use Text_IO;

with Ada.IO_Exceptions;
with Test_UTF8_Tables_Table;

procedure Test_UTF8_Tables is
   use Test_UTF8_Tables_Table.Identifier_Tables;

   End_Error    : exception renames Ada.IO_Exceptions.End_Error;
   Layout_Error : exception renames Ada.IO_Exceptions.Layout_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;

   Folder  : Dictionary;
   Text    : String := "123AA123";
   Pointer : Integer;
   Data    : Integer;

   procedure Print (Folder : Dictionary) is
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
begin
   Put_Line ("Testing tables ...");
--
-- Testing case-insensitive tables
--
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
   Pointer := 1;
   Get ("TesT.1", Pointer, Folder, Data);
   if Data /= Find (Folder, "test") or Pointer /= 5 then
      Error ("Cannot parse item test");
   end if;
   Add (Folder, "Test with spaces", 6);
   Pointer := 1;
   Get ("TesT    with   Spaces  ", Pointer, Folder, Data);
   if Data /= 6 then
      Error ("Cannot parse item with spaces");
   end if;
   Put_Line ("... Done");
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
      Print (Folder);
end Test_UTF8_Tables;
