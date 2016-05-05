--                                                                    --
--  package GNAT.Sockets.           Copyright (c)  Dmitry A. Kazakov  --
--     Connection_State_Machine.Little_Endian.     Luebeck            --
--     Generic_Single_Precision_IEEE_754           Winter, 2012       --
--  Interface                                                         --
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

with IEEE_754.Generic_Single_Precision;

generic
   with package IEEE_Single_Precision is
      new IEEE_754.Generic_Single_Precision (<>);
package GNAT.Sockets.Connection_State_Machine.Little_Endian.
        Generic_Single_Precision_IEEE_754 is
   use IEEE_Single_Precision;
--
-- IEEE_754_Data_Item -- Little endian-encoded IEEE 754 number
--
   type IEEE_754_Data_Item is new Data_Item with record
      Value : Float_32;
   end record;
--
-- Feed -- Implementation of the feed operation
--
   procedure Feed
             (  Item    : in out IEEE_754_Data_Item;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
--
-- Get -- Number from stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Value   - The result
--
-- The parameter Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    End_Error    - Not enough data
--    Layout_Error - Pointer is outside bounds
--                   When Ada numberic type is the result:
--    Not_A_Number_Error      - Not a number
--    Positive_Overflow_Error - Positive infinity or too big positive
--    Negative_Overflow_Error - Negative infinity or too big negative
--
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Float_32
             );
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out Number
             );
--
-- Put -- Number into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer is advanced beyond the value output
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Float_32
             );
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : Number
             );
--
-- Value -- Numeric value
--
--    Item - The string data item
--
-- Returns :
--
--    The value contained by Item
--
-- Exceptions :
--
--    Not_A_Number_Error      - Not a number
--    Positive_Overflow_Error - Positive infinity or too big positive
--    Negative_Overflow_Error - Negative infinity or too big negative
--
   function Get_Value (Item : IEEE_754_Data_Item) return Number;

private
   pragma Assert (Stream_Element'Size = 8);
   pragma Inline (Get_Value);

end GNAT.Sockets.Connection_State_Machine.Little_Endian.
    Generic_Single_Precision_IEEE_754;
