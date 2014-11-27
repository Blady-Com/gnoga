--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Doubly_Linked_Web.                  Luebeck            --
--        Generic_List                             Autumn, 2006       --
--  Interface                                                         --
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

generic
   Brand : List_Identification_Type;
package Generic_Doubly_Linked_Web.Generic_List is
   type Item is new Node;
   type List is new Web;
--
-- Append -- Item to a list
--
--    Container - The list
--    Element   - To insert
--
-- Exceptions :
--
--    Constraint_Error - Element is in a list or null
--
   procedure Append (Container : in out List; Element : Item);
--
-- Append -- Item to one list taking it from another
--
--    Container - The list to insert into
--    Element   - To move
--    Source    - The list to remove from
--
-- Exceptions :
--
--    Constraint_Error - Element is null
--
   procedure Append
             (  Container : in out List;
                Element   : Item;
                Source    : in out List
             );
   procedure Append
             (  Container : in out List;
                Element   : Node;
                Source    : in out List
             );
--
-- Delete -- Item from the list
--
--    Container - The list
--    Element   - To remove and possibly free
--
   procedure Delete
             (  Container : in out List;
                Element   : in out Item
             );
   procedure Delete
             (  Container : in out List;
                Element   : in out Node
             );
--
-- Erase -- Item from the list
--
--    Container - The list
--
-- This procedure removes all elements from Container. When  an  element
-- after its removal does not belong to any list its  target  object  is
-- finalized, and freed.
--
   procedure Erase (Container : in out List);
--
-- Insert -- Item into a list
--
--    Position - An item in the list
--    Element  - To inserted
--
-- Exceptions :
--
--    Constraint_Error - Element  is in a list or null, Position is null
--                       or not in any list
--
   procedure Insert (Position : Item; Element : Item);
   procedure Insert (Position : Node; Element : Item);
--
-- Insert -- Item taking it from another list
--
--    Position - An item in the list
--    Element  - To move to Position
--    Source   - The list to remove Element from
--
-- This procedure is an equivalent to Remove followed by Insert.
--
-- Exceptions :
--
--    Constraint_Error - Element is null, Position is null or not in any
--                       list
--
   procedure Insert
             (  Position : Item;
                Element  : Item;
                Source   : in out List
             );
   procedure Insert
             (  Position : Item;
                Element  : Node;
                Source   : in out List
             );
   procedure Insert
             (  Position : Node;
                Element  : Item;
                Source   : in out List
             );
   procedure Insert
             (  Position : Node;
                Element  : Node;
                Source   : in out List
             );
--
-- Is_Empty -- Check if Container is empty
--
--    Container - A list
--
-- Returns :
--
--    True if Container is null
--
   function Is_Empty (Container : List) return Boolean;
--
-- Is_In -- Check if element is not in a container
--
--    Element - An element
--
-- Returns :
--
--    False if Element is null or not in any list
--
   function Is_In (Element : Item) return Boolean;
--
-- Merge -- Two lists
--
--    Head  - The list to append
--    Tail  - The list to prepend
--
-- Either list can be empty. When Head = Tail the operation is void.
--
   procedure Merge (Head : in out List; Tail : in out List);
--
-- Next -- The next item
--
--    Container / Element - A list or an it item
--
-- Returns :
--
--    The item following Element
--
-- Exceptions :
--
--    Constraint_Error - Element is not in a list or null
--
   function Next (Element   : Item) return Item;
   function Next (Container : List) return Item;
--
-- Prepend -- Item to a list
--
--    Container - The list
--    Element   - To insert
--
-- Exceptions :
--
--    Constraint_Error - Element is in a list or null
--
   procedure Prepend (Container : in out List; Element : Item);
--
-- Prepend -- Item to one list taking it from another
--
--    Container - The list to insert into
--    Element   - To move
--    Source    - The list to remove from
--
-- Exceptions :
--
--    Constraint_Error - Element is null
--
   procedure Prepend
             (  Container : in out List;
                Element   : Item;
                Source    : in out List
             );
   procedure Prepend
             (  Container : in out List;
                Element   : Node;
                Source    : in out List
             );
--
-- Previous -- The previous item
--
--    Container / Element - A list or an it item
--
-- Returns :
--
--    The previous item
--
-- Exceptions :
--
--    Constraint_Error - Element is not in a list or null
--
   function Previous (Element   : Item) return Item;
   function Previous (Container : List) return Item;
--
-- Remove -- Item from the list
--
--    Container - The list
--    Element   - To remove
--
   procedure Remove
             (  Container : in out List;
                Element   : Item
             );
   procedure Remove
             (  Container : in out List;
                Element   : Node
             );
--
-- Take -- The first item from the list
--
--    Container - The list
--    Element   - The first item (removed from Container)
--
   procedure Take
             (  Container : in out List;
                Element   : out Item
             );
   procedure Take
             (  Container : in out List;
                Element   : out Node
             );
private
   pragma Inline (Append);
   pragma Inline (Delete);
   pragma Inline (Erase);
   pragma Inline (Insert);
   pragma Inline (Is_Empty);
   pragma Inline (Is_In);
   pragma Inline (Merge);
   pragma Inline (Next);
   pragma Inline (Prepend);
   pragma Inline (Previous);
   pragma Inline (Remove);
   pragma Inline (Take);

end Generic_Doubly_Linked_Web.Generic_List;
