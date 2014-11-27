--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Linked_Lists_Of_Strings                Luebeck            --
--  Instantiation package                          Autumn, 2006       --
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

with Generic_Doubly_Linked_Web.Generic_List;

package Test_Linked_Lists_Of_Strings is
--
-- Three lists in which an integer may participate
--
   type List_Type is (List_A, List_B, List_C);

   type Default is access Boolean;
--
-- String_Lists -- Lists  of  Strings,  each  string  may participate in
--                 three lists, enumerated by the values of of List.
--
   package String_Lists is
      new Generic_Doubly_Linked_Web
          (  List_Type,
             String,
             Default'Storage_Pool
          );
--
-- List-specific packages
--
   package A is new String_Lists.Generic_List (List_A);
   package B is new String_Lists.Generic_List (List_B);
   package C is new String_Lists.Generic_List (List_C);

end Test_Linked_Lists_Of_Strings;
