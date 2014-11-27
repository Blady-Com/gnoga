--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_APQ_Persistence                        Luebeck            --
--  Implementation                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  16:19 28 Mar 2009  --
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

with Ada.Text_IO;             use Ada.Text_IO;
with Deposit_Handles;         use Deposit_Handles;
with Persistent.Directory;    use Persistent.Directory;
with Persistent.Handle;       use Persistent.Handle;
with Test_Persistent_Object;  use Test_Persistent_Object;
with Test_Persistent_Tree;    use Test_Persistent_Tree;
with Test_APQ_Session;        use Test_APQ_Session;

procedure Test_APQ_Persistence is
   Object_Name : constant String := "The tree";
   Folder_Name : constant String := "Folder";
begin
   Put_Line ("Session 1");
   declare
      DB   : Storage_Handle := Open;
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
   end;
   Put_Line ("Session 2");
   declare
      DB   : Storage_Handle := Open;
      Root : Handle;
   begin
      Root := Get (DB, Object_Name);
      Print (Root);
   end;
   Put_Line ("Session 3 - Renaming");
   declare
      DB   : Storage_Handle := Open;
      Dir  : Handle;
      Root : Handle;
   begin
      Create (DB, Dir, Folder_Name);
      Root := Get (DB, Object_Name);
      Rename (DB, Root, Get_Name (DB, Root), Dir);
   end;
   Put_Line ("Session 4");
   declare
      DB   : Storage_Handle := Open;
      Dir  : Handle;
      Root : Handle;
   begin
      Dir  := Get (DB, Folder_Name);
      Root := Get (DB, Object_Name, Dir);
      Print (Root);
   end;
   Put_Line ("Session 5");
   declare
      DB : Storage_Handle := Open;
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
   end;
   Put_Line ("Session 6 - C is unnamed, referenced by resident L1");
   declare
      DB : Storage_Handle := Open;
      L1 : Handle;
   begin
      L1 := Get (DB, String'("L1"));
      Put_Line (Image (L1));
      Unname (DB, String'("C"));
      Put_Line (Image (L1));
   end;
   Put_Line ("Session 7 - B is unnamed, referenced by non-resident L1");
   declare
      DB : Storage_Handle := Open;
      B  : Handle := Create_Term ("B");
   begin
      Unname (DB, String'("B"));
   end;
   Put_Line ("Session 8 - A is unnamed when non-resident");
   declare
      DB : Storage_Handle := Open;
   begin
      Unname (DB, String'("A"));
   end;
   Put_Line ("Session 9 - D is unnamed when L1 is resident");
   declare
      DB : Storage_Handle := Open;
      L1 : Handle;
   begin
      L1 := Get (DB, String'("L1"));
      Unname (DB, String'("D"));
   end;
end Test_APQ_Persistence;
