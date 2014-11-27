--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Persistent_Storage                     Luebeck            --
--  Test                                           Autumn, 2004       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

with Ada.Text_IO;                   use Ada.Text_IO;
with Test_Persistent_File_Storage;  use Test_Persistent_File_Storage;
with Test_Persistent_Tree;          use Test_Persistent_Tree;
with Deposit_Handles;               use Deposit_Handles;

procedure Test_Persistent_Storage is
   Root_Key : Key;
begin
   Clean_Up;
   Put_Line ("Session 1");
   declare
      DB   : aliased File_Storage;
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
      Root_Key := Store (DB'Access, Root);
   end;
   Put_Line ("Session 2");
   declare
      DB   : aliased File_Storage;
      Root : Handle;
   begin
      Root := Restore (DB'Access, Root_Key);
      Print (Root);
   end;
end Test_Persistent_Storage;
