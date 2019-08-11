--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Sets.Generic_Set_Of                    Summer, 2019       --
--  Interface                                                         --
--                                Last revision :  13:37 03 Aug 2019  --
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

with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Explicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Explicit;

with GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;
use  GNAT.Sockets.Connection_State_Machine.ASN1.Sets.Implicit;

generic
   type Value_Type (<>) is private;
   type Element_Type is new Abstract_ASN1_Data_Item with private;
   with function Get (Element : Element_Type) return Value_Type is <>;
   with procedure Set
                  (  Element : in out Element_Type;
                     Value   : Value_Type
                  )  is <>;
package GNAT.Sockets.Connection_State_Machine.ASN1.Sets.
        Generic_Set_Of is
--
-- [Implicit_]Set_Of -- Set of values
--
   type Set_Of is
      new External_Set_Of_Data_Item with null record;
   type Implicit_Set_Of is
      new Implicit_External_Set_Of_Data_Item with null record;
   type Element_Type_Ptr is access all Element_Type;
--
-- Append -- Append a new element to the set
--
--    Item  - The set
--    Value - Of the new element
--
-- Exceptions :
--
--    Storage_Error - No space in the external buffer
--    Use_Error     - The set is not initialized
--
   procedure Append
             (  Item  : in out Implicit_Set_Of;
                Value : Value_Type
             );
   procedure Append
             (  Item  : in out Set_Of;
                Value : Value_Type
             );
--
-- Get -- Get value of the set element
--
--    Item  - The set
--    Index - Of the element 1..Get_Length
--
-- Returns :
--
--    The element's value
--
-- Exceptions :
--
--    Constraint_Error - Invalid index
--    Use_Error        - The set is not initialized
--
   function Get
            (  Item  : Implicit_Set_Of;
               Index : Positive
            )  return Value_Type;
   function Get
            (  Item  : Set_Of;
               Index : Positive
            )  return Value_Type;
--
-- Get -- Get element of the set
--
--    Item  - The set
--    Index - Of the element 1..Get_Length
--
-- Returns :
--
--    A pointer to the element
--
-- Exceptions :
--
--    Constraint_Error - Invalid index
--    Use_Error        - The set is not initialized
--
   function Get
            (  Item  : Implicit_Set_Of;
               Index : Positive
            )  return Element_Type_Ptr;
   function Get
            (  Item  : Set_Of;
               Index : Positive
            )  return Element_Type_Ptr;
--
-- Set -- Set value of the set element
--
--    Item  - The set
--    Index - Of the element 1..Get_Length
--    Value - To set
--
-- Exceptions :
--
--    Constraint_Error - Invalid index
--    Storage_Error    - No space in the external buffer
--    Use_Error        - The set is not initialized
--
   procedure Set
             (  Item  : in out Implicit_Set_Of;
                Index : Positive;
                Value : Value_Type
             );
   procedure Set
             (  Item  : in out Set_Of;
                Index : Positive;
                Value : Value_Type
             );
private
   function Create
            (  Item : access Implicit_Set_Of
            )  return Abstract_ASN1_Data_Item_Ptr;
   function Create
            (  Item : access Set_Of
            )  return Abstract_ASN1_Data_Item_Ptr;

end GNAT.Sockets.Connection_State_Machine.ASN1.Sets.
    Generic_Set_Of;
