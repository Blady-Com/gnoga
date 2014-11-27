--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Doubly_Linked_Web                   Luebeck            --
--  Interface                                      Autumn, 2006       --
--                                                                    --
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
--
--  This generic package provides double-linked lists  of  items.  Items
--  can be of any type. The type of the items is the formal parameter
--  List_Item_Type.  The  items  of  the  list  are  never  copied  when
--  inserted,  moved or removed from a list. All operations on the lists
--  and  their  items  are referential.  An item may participate several
--  lists  of  different  types.  The  set of types is determined by the
--  formal parameter List_Identification_Type, which is a discrete type.
--  For  each  value  of  this type each item becomes a pair of pointers
--  (links).  So  the  number  of  values  of   List_Identification_Type
--  determines the  number  of  lists  an  item  can  be  simultaneously
--  situated  in. For a standard doubly-linked list where an item can be
--  in  only  one list, the parameter List_Identification_Type could be,
--  for example:
--
--     type List_Identification is (The_List);
--
--  The  items are allocated in the storage pool specified by the formal
--  parameter  Pool.  The  package  provides  the  access  type Node for
--  referencing  the items and the access type Web to reference the head
--  items of lists. Note that each list is circular, so any of its items
--  can be considered as a head. A distinct  type  was  chosen  only  to
--  separate pointers  to  items  and  lists,  which  is  important  for
--  aliasing prevention. All list operations are  defined  in  terms  of
--  Node  and Web. Naturally Web and Node are freely convertible to each
--  other.
--
--  A  list  item  is  created  by using the allocator new. The obtained
--  pointer  is  passed  to  Add or Insert as appropriate. The procedure
--  Append is used for creating a new list. The list is specified  by  a
--  pointer to its head. So it can be create like:
--
--        Head : Web;  -- Empty list
--     begin
--        Append (The_List, Head, new Object);
--
--  Now  Head points to the newly allocated item in the list. Subsequent
--  items can be created as:
--
--        Append (The_List, Head, new Object);
--
--  The first parameter of all list operations is the type of the  list.
--  If, there were several types of lists, we could place the same  item
--  into different lists. For example:
--
--     type List_Identification is (Alarm, Log);
--     ...
--     Signal := new Error_Message;
--     Append (Alarm, Notifications_List, Signal);
--     Append (Log,   System_Trace_List,  Signal);
--
--  Both  Append  (Prepend)  and  Insert  are intended for placing newly
--  allocated items or items removed from their lists before. To move an
--  item from one list to another of the same type,  Append  and  Insert
--  take an additional parameter of the source list head. The difference
--  between Append and Insert, is that  for  Append  the  list  head  is
--  specified  and  thus the list can empty. For Insert the list head is
--  not identified and the list cannot be empty.
--
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with System.Storage_Pools;     use System.Storage_Pools;

generic
   type List_Identification_Type is (<>);
   type List_Item_Type (<>) is limited private;
   Pool : in out Root_Storage_Pool'Class;
package Generic_Doubly_Linked_Web is
--
-- Items_Storage_Pool -- The type of a proxy pool that keeps  the  items
--                       of all lists. The memory for the items is taken
--                       from  another  pool,  which is specified as the
--                       discriminant.
--
--    Host - The pool to take the memory from
--
   type Items_Storage_Pool (Host : access Root_Storage_Pool'Class) is
      new Root_Storage_Pool with null record;
--
-- Allocate -- Overrides System.Storage_Pools...
--
   procedure Allocate
             (  Pool            : in out Items_Storage_Pool;
                Storage_Address : out Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             );
--
-- Deallocate -- Overrides System.Storage_Pools...
--
   procedure Deallocate
             (  Pool            : in out Items_Storage_Pool;
                Storage_Address : in Address;
                Size            : Storage_Count;
                Alignment       : Storage_Count
             );
--
-- Storage_Size -- Overrides System.Storage_Pools...
--
   function Storage_Size (Pool : Items_Storage_Pool)
      return Storage_Count;
--
-- Items_Pool -- The pool of the list items
--
   Items_Pool : Items_Storage_Pool (Pool'Access);
--
-- Node -- Pointers to the list items. Each item is a node of a  network
--         of  lists,  because an item can participate in many lists The
--         T'Size representation clause is used to prevent the  compiler
--         from making it a fat pointer, which would not fit a word.
--
   type Node is access List_Item_Type;
   for Node'Storage_Pool use Items_Pool;
   for Node'Size use Integer_Address'Size;
--
-- Web -- The  type  of  lists  rooted in an item. Both Node and Web are
--        pointers to an item
--
   type Web is new Node;
--
-- Append -- Item to a list
--
--    Brand     - Of the list
--    Container - The list
--    Element   - To be inserted
--
-- This procedure inserts Element at the end of  Container.  Element  is
-- either  a  newly  allocated  item or an item outside any lists of the
-- type Brand. Constraint_Error is propagated when Element already is in
-- a list. This includes Container. Container can be empty before a call
-- to the procedure, in which case Element becomes the head of it.
--
-- Exceptions :
--
--    Constraint_Error - Element is in a list or null
--
   procedure Append
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node
             );
--
-- Append -- Item to a list taking it from another
--
--    Brand     - Of the list
--    Container - The list to insert into
--    Element   - To move
--    Source    - The list to remove from
--
-- This procedure is an equivalent to Remove followed  by  Append.  When
-- Source and Container is  the  same  list  Container  parameter  takes
-- advantage. The operation is void when Container = Element.
--
-- Exceptions :
--
--    Constraint_Error - Element is null
--
   procedure Append
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node;
                Source    : in out Web
             );
--
-- Delete -- Item from the list
--
--    Brand     - Of the list
--    Container - The list
--    Element   - To remove and free
--
-- This  procedure  removes  Element  from Container. The item following
-- Element becomes the new list head. The operation is void when Element
-- is  null.  Container  is  ignored  when  null. When Element after its
-- removal does not belong to any list its target object  is  finalized,
-- freed, and then Element is set to null.
--
   procedure Delete
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : in out Node
             );
--
-- Erase -- Item from the list
--
--    Brand     - Of the list
--    Container - The list
--
-- This procedure removes all elements from Container. When  an  element
-- after its removal does not belong to any list its  target  object  is
-- finalized, and freed.
--
   procedure Erase
             (  Brand     : List_Identification_Type;
                Container : in out Web
             );
--
-- Dope_Size -- The size of the List_Item_Type dope
--
-- Returns :
--
--    Size in storage elements
--
-- This function returns the number of  storage  elements  the  compiler
-- places  in  front of an item. The value is estimated and is available
-- only after first call to a list operation.
--
-- Exception :
--
--    Constraint_Error - The size is yet not determined
--
   function Dope_Size return Storage_Offset;
--
-- Insert -- Item into a list
--
--    Brand    - Of the list
--    Position - An item in the list
--    Element  - The item to insert
--
-- Insert is similar to Append, with the difference  that  an  arbitrary
-- list item is used to indicate the insertion position. For this reason
-- Add can deal with empty lists, while Insert  requires  at  least  one
-- item in.  Element  is  inserted  after  the  item  specified  by  the
-- parameter  Position. Element may not be in any list. Constraint_Error
-- is  propagated otherwise or when Element is null. Constraint_Error is
-- also  propagated  when  Position  is null or not in a list. To insert
-- Element before Position use:
--
--    Insert (Brand, Previous (Brand, Position), Element);
--
-- Exceptions :
--
--    Constraint_Error - Element  is in a list or null, Position is null
--                       or not in any list
--
   procedure Insert
             (  Brand    : List_Identification_Type;
                Position : Node;
                Element  : Node
             );
--
-- Insert -- Item taking it from a list
--
--    Brand    - Of the list
--    Position - An item in the list
--    Element  - To move to Position
--    Source   - The list to remove Element from
--
-- This procedure is an equivalent to Remove followed by Insert.  It  is
-- void when Position = Element.
--
-- Exceptions :
--
--    Constraint_Error - Element is null, Position is null or not in any
--                       list
--
   procedure Insert
             (  Brand    : List_Identification_Type;
                Position : Node;
                Element  : Node;
                Source   : in out Web
             );
--
-- Is_Empty -- Check if Container is empty
--
--    Brand     - Of the list
--    Container - A list
--
-- Returns :
--
--    True if Container is null
--
   function Is_Empty
            (  Brand     : List_Identification_Type;
               Container : Web
            )  return Boolean;
--
-- Is_In -- Check if Element is in a list
--
--    Brand   - Of the list
--    Element - An element
--
-- Returns :
--
--    False if Element is null or not in any list of Brand type
--
   function Is_In
            (  Brand   : List_Identification_Type;
               Element : Node
            )  return Boolean;
--
-- Is_In -- Check if Element is in any list
--
--    Element - An element
--
-- Returns :
--
--    False if Element is null or not in any list
--
   function Is_In (Element : Node) return Boolean;
--
-- Merge -- Two lists
--
--    Brand - Of the list
--    Head  - The list to append
--    Tail  - The list to prepend
--
-- Either list can be empty. When Head = Tail the operation is void.
--
   procedure Merge
             (  Brand : List_Identification_Type;
                Head  : in out Web;
                Tail  : in out Web
             );
--
-- Next -- The next item
--
--    Brand               - Of the list
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
   function Next
            (  Brand   : List_Identification_Type;
               Element : Node
            )  return Node;
   function Next
            (  Brand     : List_Identification_Type;
               Container : Web
            )  return Node;
--
-- Prepend -- Item to a list
--
--    Brand     - Of the list
--    Container - The list
--    Element   - To be inserted
--
-- This  procedure  inserts  Element  in  front of Container. Element is
-- either  a  newly  allocated  item or an item outside any lists of the
-- type Brand. Constraint_Error is propagated when Element already is in
-- a list. This includes Container. Container can be empty before a call
-- to the procedure.
--
-- Exceptions :
--
--    Constraint_Error - Element is in a list or null
--
   procedure Prepend
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node
             );
--
-- Prepend -- Item to a list taking it from another
--
--    Brand     - Of the list
--    Container - The list to insert into
--    Element   - To move
--    Source    - The list to remove from
--
-- This  procedure  is an equivalent to Remove followed by Prepend. When
-- Source and Container is  the  same  list  Container  parameter  takes
-- advantage. It is void when Container = Element.
--
-- Exceptions :
--
--    Constraint_Error - Element is null
--
   procedure Prepend
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node;
                Source    : in out Web
             );
--
-- Previous -- The previous item
--
--    Brand               - Of the list
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
   function Previous
            (  Brand   : List_Identification_Type;
               Element : Node
            )  return Node;
   function Previous
            (  Brand     : List_Identification_Type;
               Container : Web
            )  return Node;
--
-- Remove -- Item from the list
--
--    Brand     - Of the list
--    Container - The list
--    Element   - To removed
--
-- This  procedure  removes  Element  from Container. The item following
-- Item becomes the new head. Otherwise, when Element is the  last  item
-- of the list, Container becomes empty. The operation is void when Item
-- is not in a list or null. Container is ignored when null.
--
   procedure Remove
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : Node
             );
--
-- Take -- The first item from
--
--    Container - The list
--    Element   - Removed from
--
-- This procedure removes the first element from Container.  The  result
-- is returned in the parameter Element. When Container is null, Element
-- is  also  set  to  null.  When Is_In (Element) returns false, then it
-- becomes  the  caller's responsibility to delete it, or else to return
-- in a list.
--
   procedure Take
             (  Brand     : List_Identification_Type;
                Container : in out Web;
                Element   : out Node
             );

private
   pragma Inline (Append);
   pragma Inline (Delete);
   pragma Inline (Insert);
   pragma Inline (Insert);
   pragma Inline (Is_Empty);
   pragma Inline (Is_In);
   pragma Inline (Next);
   pragma Inline (Prepend);
   pragma Inline (Previous);
   pragma Inline (Remove);
   pragma Inline (Take);

end Generic_Doubly_Linked_Web;
