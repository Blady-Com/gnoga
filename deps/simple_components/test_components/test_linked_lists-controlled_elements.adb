--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Controlled_Elements                         Luebeck            --
--  Separate body                                  Autumn, 2006       --
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

with Ada.Finalization;               use Ada.Finalization;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Test_Linked_Lists_Of_Elements;  use Test_Linked_Lists_Of_Elements;

with Test_Linked_Lists_Of_Elements.Strings;

separate (Test_Linked_Lists) procedure Controlled_Elements is
   use Test_Linked_Lists_Of_Elements.Element_Lists;
   use Test_Linked_Lists_Of_Elements.Strings;
   use A, B, C;

   function Image
            (  Brand     : List_Type;
               Container : Web
            )  return String is
      This   : Node := Node (Container);
      Result : Unbounded_String;
   begin
      if This = null then
         Append (Result, "<>");
      else
         loop
            Append (Result, Image (This));
            This := Next (Brand, This);
            exit when This = Node (Container);
            Append (Result, "-");
         end loop;
      end if;
      return To_String (Result);
   end Image;

   procedure Check
             (  Name      : String;
                Brand     : List_Type;
                Container : Web;
                Expected  : STring
             )  is
   begin
      if Image (Brand, Container) /= Expected then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Name
            &  ". Got "
            &  Image (Brand, Container)
            &  ", expect "
            &  Expected
         )  );
      end if;
   end Check;

   List_1 : A.List;
   List_2 : B.List;
   List_3 : C.List;
   List_4 : A.List;
begin
   Append (List_1, new String_Element'(Controlled with 1, "A"));
      -- 1 = A
   Append (List_2, B.Item (List_1));
      -- 2 = A
   Insert (A.Item (List_1), new String_Element'(Controlled with 1, "B"));
      -- 1 = A-B
   Insert (Next (List_1), new String_Element'(Controlled with 1, "C"));
      -- 1 = A-B-C
   Put_Line ("Controlled dope size:" & Integer'Image (Integer (Dope_Size)));
   declare
      B_N : constant B.Item := B.Next (List_2);
      N   : constant Node := Next (List_B, Node (List_2));
   begin
      if B_N /= B.Item (List_2) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Failed to insert"
         );
      end if;
      if N = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong universal pointer"
         );
      end if;
   end;
   Append (List_4, new String_Element'(Controlled with 1, "D"));
   Append (List_4, new String_Element'(Controlled with 1, "E"));
   Append (List_4, new String_Element'(Controlled with 1, "F"));
      -- 1 = D-E-F
      Check ("1", List_A, Web (List_1), "A-B-C");
      Check ("2", List_B, Web (List_2), "A");
      Check ("3", List_C, Web (List_3), "<>");
      Check ("4", List_A, Web (List_4), "D-E-F");
   Append (List_1, Previous (List_4), List_4);
      Check ("1", List_A, Web (List_1), "A-B-C-F");
      Check ("4", List_A, Web (List_4), "D-E");
   Append (List_1, A.Item (List_4), List_4);
      Check ("1", List_A, Web (List_1), "A-B-C-F-D");
      Check ("4", List_A, Web (List_4), "E");
   declare
      Temp : A.List;
   begin
      Prepend (List_1, Previous (List_1), Temp);
         Check ("1", List_A, Web (List_1), "D-A-B-C-F"); 
   end;
   Remove (List_1, A.Item (List_1));
      Check ("1", List_A, Web (List_1), "A-B-C-F");
   declare
      Head : A.Item;
   begin
      while List_1 /= null loop
         Head := A.Item (List_1);
         Delete (List_1, Head);
      end loop;
   end;
   Check ("1", List_A, Web (List_1), "<>");
   Check ("2", List_B, Web (List_2), "A");
   Check ("3", List_C, Web (List_3), "<>");
   Check ("4", List_A, Web (List_4), "E");
end Controlled_Elements;
