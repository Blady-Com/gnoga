--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Bit_Strings                            Spring, 2019       --
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
--
--  ASN.1 encoding of bit strings.  The implementation  of Feed supports
--  constructed bit strings.
--
package GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings is
--
-- Public_Time_Data_Item -- ASN.1 encoded bit string, public view
--
   type Boolean_Array is array (Integer range <>) of Boolean;
   type Public_Bit_String_Data_Item (Size : Natural) is
      abstract new ASN1_Data_Item with
   record
      Length : Natural := 0; -- The actual length of the value
      Value  : Boolean_Array (1..Size);
   end record;
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Public_Bit_String_Data_Item
            )  return ASN1_Type;
--
-- Get_Length -- The current string value length
--
--    Item - The item
--
-- Returns :
--
--    The current value length
--
   function Get_Length
            (  Item : Public_Bit_String_Data_Item
            )  return Natural;
--
-- Get_Value -- The current string value
--
--    Item - The item
--
-- Returns :
--
--    The current value
--
   function Get_Value
            (  Item : Public_Bit_String_Data_Item
            )  return Boolean_Array;
--
-- Set_Value -- Set the string value
--
--    Item  - The item
--    Value - The value to set
--
-- Exceptions :
--
--    Constraint_Error - The value is too large
--
   procedure Set_Value
             (  Item  : in out Public_Bit_String_Data_Item;
                Value : Boolean_Array
             );
------------------------------------------------------------------------
--
-- Get -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Value   - The result
--
-- Note that the data type is  not included.  The parameter  Pointer  is
-- advanced beyond the value obtained
--
-- Exceptions :
--
--    Data_Error   - Invalid encoding
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Boolean_Array
             );
--
-- Put -- Into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer is advanced beyond the value output.
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Boolean_Array
             );
end GNAT.Sockets.Connection_State_Machine.ASN1.Bit_Strings;
