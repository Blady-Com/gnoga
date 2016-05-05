--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Suffix_Tree                            Luebeck            --
--  Sample implementation                          Winter, 2009       --
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

with Ada.Text_IO;  use Ada.Text_IO;

package body Test_Suffix_Tree is
   use Character_Weighted_Graphs;

   function Equal (Left, Right : access Character) return Boolean is
   begin
      return Left.all = Right.all;
   end Equal;

   function Less (Left, Right : access Character) return Boolean is
   begin
      return Left.all < Right.all;
   end Less;

   function Build (Text : String) return Suffix_Tree is
      Root  : constant Node := new Node_Type;
      Focus : Node;
      Lower : Natural;
      Upper : Natural;
   begin
      for Index in Text'Range loop
         Focus := Root;
         for Current in Index..Text'Last loop
            Classify (Focus, Text (Current), Lower, Upper);
            if Lower = Upper then
               Focus := Get_Child (Focus, Lower);
            else
               declare
                  Branch : constant Node := new Node_Type;
               begin
                  Connect (Focus, Branch, Text (Current));
                  Focus := Branch;
               end;
            end if;
         end loop;
      end loop;
      return Root;
   end Build;

   procedure Print (Tree : Suffix_Tree; Prefix : String := "") is
   begin
      for Index in 1..Get_Children_Number (Tree) loop
         if Index > 1 then
            Put (Prefix);
         end if;
         Put (Get_Weight (Tree, Index));
         Print (Get_Child (Tree, Index), Prefix & ' ');
      end loop;
      if Get_Children_Number (Tree) = 0 then
         New_Line;
      end if;
   end Print;

end Test_Suffix_Tree;
