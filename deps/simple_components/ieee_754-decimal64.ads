--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     IEEE_754.Decimal64                          Luebeck            --
--  Interface                                      Summer, 2020       --
--                                                                    --
--                                Last revision :  20:46 27 Aug 2020  --
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

package IEEE_754.Decimal64 is
   pragma Pure (IEEE_754.Decimal64);
--
-- Decimal_64 -- 64-bit IEEE 754 decimal64 float.  The  memory layout is
--               big endian,  i.e. the byte containing the number's sign
-- and the most significant  bits  of  the exponent  is the  first array
-- element.  The  byte  containing  the least  significant  bits of  the
-- mantissa is the last array element.
--
   type Decimal_64 is array (1..8) of Byte;
   type Decimal_64_Mantissa is range -9_999_999_999_999_999
                                    ..9_999_999_999_999_999;
   type Decimal_64_Exponent is range -398..369;
--
-- From_IEEE -- Conversion from decimal64 IEEE 754 float
--
--    Value    - The argument
--    Mantissa - The mantissa
--    Exponent - The exponent
--    Encoding - The decimal encoding method
--
-- Exceptions :
--
--    Not_A_Number_Error      - Not a number
--    Positive_Overflow_Error - Positive infinity
--    Negative_Overflow_Error - Negative infinity
--
   procedure From_IEEE
             (  Value    : Decimal_64;
                Mantissa : out Decimal_64_Mantissa;
                Exponent : out Decimal_64_Exponent;
                Encoding : Decimal_Encoding := Binary_Integer
             );
--
-- Is_NaN -- NaN test
--
--    Value - The argument
--
-- Returns :
--
--    True if Value is an IEEE NaN
--
   function Is_NaN (Value : Decimal_64) return Boolean;
--
-- Is_Negative -- IEEE sign test
--
--    Value - The argument
--
-- Returns :
--
--    True if Value has an IEEE sign
--
   function Is_Negative (Value : Decimal_64) return Boolean;
--
-- Is_Real -- Value test
--
--    Value - The argument
--
-- This function tests if Value represents a real number. Infinities and
-- NaN are not numbers.
--
-- Returns :
--
--    True if Value represents a real number
--
   function Is_Real (Value : Decimal_64) return Boolean;
--
-- To_IEEE -- Conversion to decimal64 IEEE 754 float
--
--    Mantissa - The mantissa
--    Exponent - The exponent
--    Encoding - The decimal encoding method
--
-- Returns :
--
--    The corresponding IEEE 754 representation
--
-- Exceptions :
--
--    Constraint_Error - The number is out of range
--
   function To_IEEE
            (  Mantissa : Decimal_64_Mantissa;
               Exponent : Decimal_64_Exponent;
               Encoding : Decimal_Encoding := Binary_Integer
            )  return Decimal_64;

private
   pragma Inline (Is_NaN);
   pragma Inline (Is_Negative);
   pragma Inline (Is_Real);

end IEEE_754.Decimal64;
