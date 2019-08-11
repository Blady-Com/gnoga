--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sets.Generic_Reference                 Summer, 2019       --
--  Interface                                                         --
--                                Last revision :  18:41 01 Aug 2019  --
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
   type Value_Type (<>) is private;
   type Element_Type is new Abstract_ASN1_Data_Item with private;
   with function Get (Element : Element_Type) return Value_Type is <>;
   with procedure Set
                  (  Element : in out Element_Type;
                     Value   : Value_Type
                  )  is <>;
package GNAT.Sockets.Connection_State_Machine.ASN1.Sets.
        Generic_Reference is
   type Element_Type_Ptr is access all Element_Type;
--
-- Reference_To -- Reference  to  an  element.  The element  is  created
--                 dynamically in an external buffer.
--
   type Reference_To is new Reference_Data_Item with null record;
--
-- Get -- Get target of the reference
--
--    Item     - The reference
--    Allocate - Create target if it does not exist
--
-- Returns :
--
--    The element's value
--
-- Exceptions :
--
--    Constraint_Error - The target does not exist
--    Storage_Error    - No space in the external buffer
--    Use_Error        - The reference is not initialized
--
   function Get
            (  Item     : Reference_To;
               Allocate : Boolean := False
            )  return Value_Type;
--
-- Get -- Get target of the reference
--
--    Item     - The reference
--    Allocate - Create target if it does not exist
--
-- Returns :
--
--    A pointer to the element
--
-- Exceptions :
--
--    Storage_Error - No space in the external buffer
--    Use_Error     - The reference is not initialized
--
   function Get
            (  Item     : Reference_To;
               Allocate : Boolean := False
            )  return Element_Type_Ptr;
--
-- Set -- Set target of the reference
--
--    Item     - The Reference
--    Value    - To set
--    Allocate - Create target if it does not exist
--
-- Exceptions :
--
--    Constraint_Error - The target does not exist
--    Storage_Error    - No space in the external buffer
--    Use_Error        - The Reference is not initialized
--
   procedure Set
             (  Item     : in out Reference_To;
                Value    : Value_Type;
                Allocate : Boolean := False
             );
private
   function Create
            (  Item : access Reference_To
            )  return Abstract_ASN1_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Generic_Reference;
