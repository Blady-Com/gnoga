--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Generic_Doubly_Linked                       Luebeck            --
--  Interface                                      Autumn, 2006       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
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
--  This package provides doubly-linked lists of items of any nature. It
--  is  a  specialization  of Generic_Doubly_Linked_Web for the standard
--  pool and items that can participate in only  one  list.  The  formal
--  parameter  List_Item_Type  is  the  type  of list items. The package
--  instantiates  Generic_Doubly_Linked_Web.Generic_List  under the name
--  Doubly_Linked. This package provides the types Item, List and a  set
--  of operations over them to use. For example:
--
--      type My_Item is ...;
--      package My_Lists is new Generic_Doubly_Linked (My_Item);
--      use My_Lists.Doubly_Linked;
--
with Generic_Doubly_Linked_Web.Generic_List;

generic
   type List_Item_Type (<>) is limited private;
package Generic_Doubly_Linked is
   type Singleton is (The_List);
   type Default_Pointer is access List_Item_Type;
   package Doubly_Linked_Webs is
      new Generic_Doubly_Linked_Web
          (  Singleton,
             List_Item_Type,
             Default_Pointer'Storage_Pool
          );
   package Doubly_Linked is
      new Doubly_Linked_Webs.Generic_List (The_List);
end Generic_Doubly_Linked;
