--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Linked_Web.                         Luebeck            --
--        Generic_List                             Autumn, 2006       --
--  Implementation                                                    --
--                                Last revision :  10:35 22 Oct 2011  --
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

package body Generic_Doubly_Linked_Web.Generic_List is

   procedure Append (Container : in out List; Element : Item) is
   begin
      Append (Brand, Web (Container), Element);
   end Append;

   procedure Append
             (  Container : in out List;
                Element   : Item;
                Source    : in out List
             )  is
   begin
      Append (Brand, Web (Container), Node (Element), Web (Source));
   end Append;

   procedure Append
             (  Container : in out List;
                Element   : Node;
                Source    : in out List
             )  is
   begin
      Append (Brand, Web (Container), Element, Web (Source));
   end Append;

   procedure Delete
             (  Container : in out List;
                Element   : in out Item
             )  is
   begin
      Delete (Brand, Container, Node (Element));
   end Delete;

   procedure Delete
             (  Container : in out List;
                Element   : in out Node
             )  is
   begin
      Delete (Brand, Web (Container), Element);
   end Delete;

   procedure Erase (Container : in out List) is
   begin
      Erase (Brand, Web (Container));
   end Erase;

   procedure Insert (Position : Item; Element : Item) is
   begin
      Insert (Brand, Node (Position), Node (Element));
   end Insert;

   procedure Insert (Position : Node; Element : Item) is
   begin
      Insert (Brand, Position, Node (Element));
   end Insert;

   procedure Insert
             (  Position : Item;
                Element  : Item;
                Source   : in out List
             )  is
   begin
      Insert (Brand, Node (Position), Node (Element), Web (Source));
   end Insert;

   procedure Insert
             (  Position : Item;
                Element  : Node;
                Source   : in out List
             )  is
   begin
      Insert (Brand, Node (Position), Element, Web (Source));
   end Insert;

   procedure Insert
             (  Position : Node;
                Element  : Item;
                Source   : in out List
             )  is
   begin
      Insert (Brand, Position, Node (Element), Web (Source));
   end Insert;

   procedure Insert
             (  Position : Node;
                Element  : Node;
                Source   : in out List
             )  is
   begin
      Insert (Brand, Position, Element, Web (Source));
   end Insert;

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container = null;
   end Is_Empty;

   function Is_In (Element : Item) return Boolean is
   begin
      return Is_In (Brand, Element);
   end Is_In;

   procedure Merge (Head : in out List; Tail : in out List) is
   begin
      Merge (Brand, Head, Tail);
   end Merge;

   function Next (Element : Item) return Item is
   begin
      return Item (Next (Brand, Element));
   end Next;

   function Next (Container : List) return Item is
   begin
      return Item (Next (Brand, Node (Container)));
   end Next;

   procedure Prepend (Container : in out List; Element : Item) is
   begin
      Prepend (Brand, Web (Container), Element);
   end Prepend;

   procedure Prepend
             (  Container : in out List;
                Element   : Item;
                Source    : in out List
             )  is
   begin
      Prepend (Brand, Web (Container), Node (Element), Web (Source));
   end Prepend;

   procedure Prepend
             (  Container : in out List;
                Element   : Node;
                Source    : in out List
             )  is
   begin
      Prepend (Brand, Web (Container), Element, Web (Source));
   end Prepend;

   function Previous (Element : Item) return Item is
   begin
      return Item (Previous (Brand, Element));
   end Previous;

   function Previous (Container : List) return Item is
   begin
      return Item (Previous (Brand, Node (Container)));
   end Previous;

   procedure Remove
             (  Container : in out List;
                Element   : Item
             ) is
   begin
      Remove (Brand, Web (Container), Element);
   end Remove;

   procedure Remove
             (  Container : in out List;
                Element   : Node
             ) is
   begin
      Remove (Brand, Web (Container), Element);
   end Remove;

   procedure Take
             (  Container : in out List;
                Element   : out Item
             )  is
   begin
      Take (Brand, Web (Container), Element);
   end Take;

   procedure Take
             (  Container : in out List;
                Element   : out Node
             )  is
   begin
      Take (Brand, Web (Container), Element);
   end Take;

end Generic_Doubly_Linked_Web.Generic_List;
