--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Single_File_Persistence                Luebeck            --
--  Implementation                                 Autumn, 2014       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Deposit_Handles;         use Deposit_Handles;
with Persistent.Directory;    use Persistent.Directory;
with Persistent.Handle;       use Persistent.Handle;
with Persistent.Single_File;  use Persistent.Single_File;
with Test_Persistent_Object;  use Test_Persistent_Object;
with Test_Persistent_Tree;    use Test_Persistent_Tree;

with Persistent.Single_File.Text_IO;
--with GNAT.Exception_Traces;

procedure Test_Single_File_Persistence is
   use Persistent.Single_File.Text_IO;
   File_Name   : constant String := "test.db";
   Object_Name : constant String := "The tree";
   Folder_Name : constant String := "Folder";
begin
--   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   Put_Line ("Session 1");
   declare
      DB   : Storage_Handle := Create (File_Name, True);
      Root : Handle;
   begin
      Root :=
         Create_Node
         (  1,
            Create_Node (2),
            Create_Node
            (  3,
               Create_Node
               (  4,
                  Create_Node (5)
               ),
               Create_Node (6)
         )  );
      Print (Root);
      Put (DB, Root, Object_Name);
   exception
      when Error : others =>
         Put_Line ("Session 1 fault: " & Exception_Information (Error));
         raise;
   end;
   Put_Line ("Session 2");
   declare
      DB   : constant Storage_Handle := Create (File_Name);
      Root : Handle;
   begin
      Root := Get (DB, Object_Name);
      Print (Root);
   exception
      when Error : others =>
         Put_Line ("Session 2 fault: " & Exception_Information (Error));
         raise;
   end;
   Put_Line ("Session 3 - Renaming");
   declare
      DB   : Storage_Handle := Create (File_Name);
      Dir  : Handle;
      Root : Handle;
   begin
      Create (DB, Dir, Folder_Name);
      Root := Get (DB, Object_Name);
      Rename (DB, Root, Get_Name (DB, Root), Dir);
   exception
      when Error : others =>
         Put_Line ("Session 3 fault: " & Exception_Information (Error));
         raise;
   end;
   Put_Line ("Session 4");
   declare
      DB   : constant Storage_Handle := Create (File_Name);
      Dir  : Handle;
      Root : Handle;
   begin
      Dir  := Get (DB, Folder_Name);
      Root := Get (DB, Object_Name, Dir);
      Print (Root);
   exception
      when Error : others =>
         Put_Line ("Session 4 fault: " & Exception_Information (Error));
         raise;
   end;
   Put_Line ("Session 5");
   declare
      DB : Storage_Handle := Create (File_Name);
      A  : Handle := Create_Term ("A");
      B  : Handle := Create_Term ("B");
      C  : Handle := Create_Term ("C");
      D  : Handle := Create_Term ("D");
      L1 : Handle := Create_List_Of_Terms ("L1");
   begin
      Add (L1, A);
      Add (L1, B);
      Add (L1, C);
      Add (L1, D);
      Put (DB, A, String'("A"));
      Put (DB, B, String'("B"));
      Put (DB, C, String'("C"));
      Put (DB, D, String'("D"));
      Put (DB, L1, String'("L1"));
   exception
      when Error : others =>
         Put_Line ("Session 5 fault: " & Exception_Information (Error));
         raise;
   end;
   Put_Line ("Session 6 - C is unnamed, referenced by resident L1");
   declare
      DB : Storage_Handle := Create (File_Name);
      L1 : Handle;
   begin
      L1 := Get (DB, String'("L1"));
      Put_Line (Image (L1));
      Unname (DB, String'("C"));
      Put_Line (Image (L1));
   exception
      when Error : others =>
         Put_Line ("Session 6 fault: " & Exception_Information (Error));
         raise;
   end;
   Put_Line ("Session 7 - B is unnamed, referenced by non-resident L1");
   declare
      DB : Storage_Handle := Create (File_Name);
      B  : Handle := Create_Term ("B");
   begin
      Unname (DB, String'("B"));
   exception
      when Error : others =>
         Put_Line ("Session 7 fault:" & Exception_Information (Error));
         raise;
   end;
   Put_Line ("Session 8 - A is unnamed when non-resident");
   declare
      DB : Storage_Handle := Create (File_Name);
   begin
      Unname (DB, String'("A"));
   exception
      when Error : others =>
         Put_Line ("Session 8 fault: " & Exception_Information (Error));
         raise;
   end;
   Put_Line ("Session 9 - D is unnamed when L1 is resident");
   declare
      DB : Storage_Handle := Create (File_Name);
      L1 : Handle;
   begin
      L1 := Get (DB, String'("L1"));
      Unname (DB, String'("D"));
   exception
      when Error : others =>
         Put_Line ("Session 9 fault: " & Exception_Information (Error));
         raise;
   end;
exception
   when Error : others =>
      Put_Line ("Fault: " & Exception_Information (Error));
end Test_Single_File_Persistence;
