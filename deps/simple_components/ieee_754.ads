--                                                                    --
--  package IEEE_754                Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2008       --
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

with Interfaces;  use Interfaces;

package IEEE_754 is
   pragma Pure (IEEE_754);

   subtype Byte is Interfaces.Unsigned_8;

   Not_A_Number_Error      : exception;
   Positive_Overflow_Error : exception;
   Negative_Overflow_Error : exception;
--
-- Decimal_Encoding -- Method to encode decimal numbers
--
--    Binary_Integer - BID (binary integer decimal)
--    Densely_Packed - DPD (densely packed decimal)
--
   type Decimal_Encoding is (Binary_Integer, Densely_Packed);
--
-- Densely packed decimals (IEEE 754-2008)
--
   subtype Unpacked_Decimal is Integer range 0..999;
   type Packed_Decimal   is mod 2**10;
--
-- Pack -- Three decimal digits into 10 bits
--
--    Value - The value to pack
--
-- Returns :
--
--    Densely packed result
--
   function Pack (Value : Unpacked_Decimal) return Packed_Decimal;
--
-- Unpack -- 10 bits into three decimal digits
--
--    Value - The value to unpack
--
-- Returns :
--
--    The result
--
   function Unpack (Value : Packed_Decimal) return Unpacked_Decimal;
--
-- Integer_128 -- Most platforms  do not support large  128-bit numbers.
--                This  is  a substitute  type,  it is  encoded  as  2's
--                complement in big-endian 32-bit words.
--
   type Integer_128 is array (1..4) of Unsigned_32;
   Integer_128_Zero  : constant Integer_128;
   Integer_128_First : constant Integer_128;
   Integer_128_Last  : constant Integer_128;

   function Is_Zero     (Left : Integer_128) return Boolean;
   function Is_Positive (Left : Integer_128) return Boolean;
   function Is_Negative (Left : Integer_128) return Boolean;

   function "<"  (Left, Right : Integer_128) return Boolean;
   function "<=" (Left, Right : Integer_128) return Boolean;
   function ">"  (Left, Right : Integer_128) return Boolean;
   function ">=" (Left, Right : Integer_128) return Boolean;

   function "abs" (Left : Integer_128) return Integer_128;
   function "+"   (Left : Integer_128) return Integer_128;
   function "-"   (Left : Integer_128) return Integer_128;

   function "+"   (Left, Right : Integer_128) return Integer_128;
   function "-"   (Left, Right : Integer_128) return Integer_128;

   function "*"   (Left, Right : Integer_128) return Integer_128;
   function "/"   (Left, Right : Integer_128) return Integer_128;
   function "mod" (Left, Right : Integer_128) return Integer_128;
   function "rem" (Left, Right : Integer_128) return Integer_128;

   function "**" (Left : Integer_128; Right : Natural)
      return Integer_128;

   function From_Integer_64  (Value : Integer_64 ) return Integer_128;
   function From_Unsigned_64 (Value : Unsigned_64) return Integer_128;

   function To_Integer_64  (Value : Integer_128) return Integer_64;
   function To_Unsigned_64 (Value : Integer_128) return Unsigned_64;

private
   Integer_128_Zero  : constant Integer_128 := (    0, 0, 0, 0);
   Integer_128_First : constant Integer_128 := (2**31, 0, 0, 0);
   Integer_128_Last  : constant Integer_128 := (2**31 - 1, not 0,
                                                    not 0, not 0
                                               );
   procedure Add_32
             (  Summand : in out Integer_128;
                Addend  : Unsigned_32
             );
   procedure Div_32
             (  Dividend  : in out Integer_128;
                Divisor   : Unsigned_32;
                Remainder : out Unsigned_32
             );
   procedure Mul_32
             (  Multiplicand : in out Integer_128;
                Multiplier   : Unsigned_32
             );
   procedure Neg (Value : in out Integer_128);
   pragma Inline (Neg);

end IEEE_754;
