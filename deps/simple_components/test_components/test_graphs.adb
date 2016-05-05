--                                                                    --
--  procedure Test_Graphs           Copyright (c)  Dmitry A. Kazakov  --
--  Test program                                   Luebeck            --
--                                                 Winter, 2009       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.Text_IO;        use Ada.Text_IO;

with Test_String_Graph;
with Test_Suffix_Tree;

procedure Test_Graphs is
begin
   declare
      use Test_Suffix_Tree;
      use Character_Weighted_Graphs;
      Tree : Suffix_Tree := Build ("mississippi");
   begin
      Print (Tree);
      Delete (Tree);
   end;
   declare
      use Test_String_Graph.Instance;
      procedure Print (Vertex : Node; Prefix : String := "") is
      begin
         Put_Line (Prefix & Vertex.all);
         for Child in 1..Get_Children_Number (Vertex) loop
            Print (Get_Child (Vertex, Child), Prefix & "  ");
         end loop;
      end Print;
   begin
      declare
         A : Node := new String'("A");
      begin
         Free (A); -- Free an unconnected node
      end;
      declare
         A : Node := new String'("A");
      begin
         Connect (A, new String'("B"));
         Connect (Get_Child (A, 1), new String'("C"));
         Connect (A, new String'("D"));
         if not Is_Ancestor (A, Get_Child (A, 1)) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Ancestor"
            );
         end if;
         if not Is_Descendant (Get_Child (A, 1), A) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Descendant"
            );
         end if;
         Print (A);
         Put_Line ("----------");
         Delete (A, Any); -- Clean up
      end;
      declare
         A : Node := new String'("A");
         B : constant Node := new String'("B");
         C : Node := new String'("C");
         D : constant Node := new String'("D");
         E : Node := new String'("E");
      begin
         Connect (A, C);  -- A --> C --> D
         Connect (B, C);  -- B -->   --> E
         Connect (C, D);
         Connect (C, E);
         if not Is_Ancestor (A, E) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Ancestor"
            );
         end if;
         if not Is_Sibling (D, E) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Sibling"
            );
         end if;
         Print (A);
         Put_Line ("----------");
         Print (B);
         Put_Line ("----------");
         Remove (E);
         if Is_Ancestor (A, E) then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Error: in Is_Ancestor"
            );
         end if;              -- A --> D
         Free (E);            -- B -->
         Print (A);
         Put_Line ("----------");
         Remove (C);
         Free (C);
         Print (A);
         Put_Line ("----------");
         Delete (A, Any);
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Graphs;
