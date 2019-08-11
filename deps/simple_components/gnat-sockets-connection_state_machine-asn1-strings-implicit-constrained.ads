--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Strings.Implicit.Constrained           Spring, 2019       --
--  Interface                                                         --
--                                Last revision :  21:39 03 Aug 2019  --
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

package GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit.
        Constrained is
--
-- Implicit_Constrained_String_Data_Item -- ASN.1  encoded  string.  The
--                                          string   is constrained to a
--                                          specific type
--
--    String_Type - The string type constraint
--    Size        - The maximum string length
--
   type Implicit_Constrained_String_Data_Item
        (  String_Type : ASN1_Type;
           Size        : Natural
        )  is new Implicit_String_Data_Item (Size) with null record;
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Implicit_Constrained_String_Data_Item
            )  return ASN1_Type;
--
-- Set_Value -- Set the string value
--
--    Item  - The item
--    Value - The new value to set
--
-- Exceptions :
--
--    Constraint_Error - Invalid value
--    Storage_Error    - The value is too large to store
--    Use_Error        - The object was not initialized
--
   procedure Set_Value
             (  Item  : in out
                        Implicit_Constrained_String_Data_Item;
                Value : String
             );
------------------------------------------------------------------------
--
-- Implicit_Constrained_External_String_Data_Item -- ASN.1   constrained
--                                                   string
--    String_Type - The string type constraint
--
   type Implicit_Constrained_External_String_Data_Item
        (  String_Type : ASN1_Type
        )  is new Implicit_External_String_Data_Item with null record;
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Implicit_Constrained_External_String_Data_Item
            )  return ASN1_Type;

--
-- Set_Value -- Set the string value
--
--    Item  - The item
--    Value - The new value to set
--
-- Exceptions :
--
--    Constraint_Error - Invalid value
--    Storage_Error    - The value is too large to store
--    Use_Error        - The object was not initialized
--
   procedure Set_Value
             (  Item  : in out
                        Implicit_Constrained_External_String_Data_Item;
                Value : String
             );
private
   procedure Encode
             (  Item    : Implicit_Constrained_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Encode
             (  Item : Implicit_Constrained_External_String_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Initialize
             (  Item : in out Implicit_Constrained_String_Data_Item
             );
   procedure Initialize
             (  Item : in out
                       Implicit_Constrained_External_String_Data_Item
             );

end GNAT.Sockets.Connection_State_Machine.ASN1.Strings.Implicit.
        Constrained;
