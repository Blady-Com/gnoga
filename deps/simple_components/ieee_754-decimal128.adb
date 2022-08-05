--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     IEEE_754.Generic_Decimal128                  Luebeck            --
--  Implementation                                 Summer, 2008       --
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

package body IEEE_754.Decimal128 is

   Exponent_Bias : constant := 6176;

   function Pack (Value : Integer_128) return Integer_128;
   pragma Inline (Pack);

   function Unpack (Value : Integer_128) return Integer_128;
   pragma Inline (Unpack);

   procedure From_IEEE
             (  Value    : Decimal_128;
                Mantissa : out Integer_128;
                Exponent : out Decimal_128_Exponent;
                Encoding : Decimal_Encoding := Binary_Integer
             )  is
      Fraction : Integer_128;
      Power    : Unsigned_32;
   begin
      Fraction (1) :=
          (  Shift_Left (Unsigned_32 (Value (1)), 24)
          or Shift_Left (Unsigned_32 (Value (2)), 16)
          or Shift_Left (Unsigned_32 (Value (3)), 8)
          or             Unsigned_32 (Value (4))
          );
      case Value (1) is
         when 2#0000_0000#..2#0101_1111# | -- 00..10
              2#1000_0000#..2#1101_1111# =>
            case Encoding is
               when Binary_Integer =>
                  --  123 4567 8901 234
                  -- SEEE EEEE EEEE EEEM MMMM MMMM
                  Power :=
                     (  Shift_Right (Fraction (1), 17)
                     and
                        2#11_1111_1111_1111#
                     );
                  Fraction (1) :=
                     (  Fraction (1)
                     and
                        2#0000_0000_0000_0001_1111_1111_1111_1111#
                     );
               when Densely_Packed =>
                  --  12    34 5678 9012 34
                  -- SEEM MMEE EEEE EEEE EEMM MMMM
                  Power :=
                     (  (  Shift_Right (Fraction (1), 14)
                        and
                           2#00_1111_1111_1111#
                        )
                     or (  Shift_Right (Fraction (1), 17)
                        and
                           2#11_0000_0000_0000#
                     )  );
                  Fraction (1) :=
                     (  (  Fraction (1)
                        and
                           2#0000_0000_0000_0000_0011_1111_1111_1111#
                        )
                     or (  Shift_Right (Fraction (1), 12)
                        and
                           2#0000_0000_0000_0001_1100_0000_0000_0000#
                     )  );
            end case;
         when 2#0110_0000#..2#0111_0111# | -- 1100..1110
              2#1110_0000#..2#1111_0111# =>
            case Encoding is
               when Binary_Integer =>
                  --    1 2345 6789 0123 4
                  -- S11E EEEE EEEE EEEE EMMM MMMM
                  Power :=
                     (  Shift_Right (Fraction (1), 15)
                     and
                        2#11_1111_1111_1111#
                     );
                  Fraction (1) :=
                     (  (  Fraction (1)
                        and
                           2#0000_0000_0000_0000_0011_1111_1111_1111#
                        ) -- SEEE EEEE EEEE EEEM MMMM MMMM MMMM MMMM
                     or    2#0000_0000_0000_0010_0000_0000_0000_0000#
                     );
               when Densely_Packed =>
                  --    1 2 34 5678 9012 34
                  -- S11E EMEE EEEE EEEE EEMM MMMM
                  Power :=
                     (  (  Shift_Right (Fraction (1), 14)
                        and
                           2#0000_1111_1111_1111#
                        )
                     or (  Shift_Right (Fraction (1), 15)
                        and
                           2#0011_0000_0000_0000#
                     )  );
                  Fraction (1) :=
                     (  (  (  Fraction (1)
                           and
                              2#0000_0000_0000_0000_0011_1111_1111_1111#
                           )
                        or (  Shift_Right (Fraction (1), 12)
                           and
                              2#0000_0000_0000_0000_0100_0000_0000_0000#
                        )  ) -- SEEE EEEE EEEE EEEM MMMM MMMM MMMM MMMM
                     or       2#0000_0000_0000_0010_0000_0000_0000_0000#
                     );
            end case;
         when 2#0111_1000#..2#0111_1011# => -- +Infinity
            raise Positive_Overflow_Error;
         when 2#1111_1000#..2#1111_1011# => -- -Infinity
            raise Negative_Overflow_Error;
         when 2#0111_1100#..2#0111_1111# | -- NaN
              2#1111_1100#..2#1111_1111# =>
            raise Not_A_Number_Error;
      end case;
      Exponent :=
         Decimal_128_Exponent (Integer (Power) - Exponent_Bias);
      Fraction (2) :=
          (  Shift_Left (Unsigned_32 (Value ( 5)), 24)
          or Shift_Left (Unsigned_32 (Value ( 6)), 16)
          or Shift_Left (Unsigned_32 (Value ( 7)), 8)
          or             Unsigned_32 (Value ( 8))
          );
      Fraction (3) :=
          (  Shift_Left (Unsigned_32 (Value ( 9)), 24)
          or Shift_Left (Unsigned_32 (Value (10)), 16)
          or Shift_Left (Unsigned_32 (Value (11)), 8)
          or             Unsigned_32 (Value (12))
          );
      Fraction (4) :=
          (  Shift_Left (Unsigned_32 (Value (13)), 24)
          or Shift_Left (Unsigned_32 (Value (14)), 16)
          or Shift_Left (Unsigned_32 (Value (15)), 8)
          or             Unsigned_32 (Value (16))
          );
      case Encoding is
         when Binary_Integer =>
            Mantissa := Fraction;
         when Densely_Packed =>
            Mantissa := Unpack (Fraction);
      end case;
      if Value (1) > 2**7 then
         Neg (Mantissa);
      end if;
   exception
      when Constraint_Error =>
         if Value (1) > 2**7 then
            raise Negative_Overflow_Error;
         else
            raise Positive_Overflow_Error;
         end if;
   end From_IEEE;

   function Is_NaN (Value : Decimal_128) return Boolean is
   begin
      return (Value (1) and 2#0111_1100#) = 2#0111_1100#;
   end Is_NaN;

   function Is_Negative (Value : Decimal_128) return Boolean is
   begin
      return Value (1) > 127;
   end Is_Negative;

   function Is_Real (Value : Decimal_128) return Boolean is
   begin
      return (Value (1) and 2#0111_1000#) < 2#0111_1000#;
   end Is_Real;

   function Pack (Value : Integer_128) return Integer_128 is
      Accumulator : Integer_128 := Value;
      Result      : Integer_128 := (others => 0);
      Triade      : array (1..12) of Unsigned_32;
   begin
      for Index in reverse Triade'Range loop
         Div_32 (Accumulator, 1000, Triade (Index));
      end loop;
      Add_32
      (  Result,
         Unsigned_32 (Pack (Unpacked_Decimal (Triade (1))))
      );
      for Index in 2..Triade'Last loop
         Mul_32 (Result, 2**10);
         Add_32
         (  Result,
            Unsigned_32 (Pack (Unpacked_Decimal (Triade (Index))))
         );
      end loop;
      return Result;
   end Pack;

   function To_IEEE
            (  Mantissa : Integer_128;
               Exponent : Decimal_128_Exponent;
               Encoding : Decimal_Encoding := Binary_Integer
            )  return Decimal_128 is
      Result   : Decimal_128;
      Power    : Unsigned_32;
      Fraction : Integer_128 := Mantissa;
      Sign     : Byte := 0;
   begin
      if Fraction (1) > 2**31 then
         Sign := 2#1000_0000#;
         Neg (Fraction);
      end if;
      case Encoding is
         when Binary_Integer =>
            null;
         when Densely_Packed =>
            Fraction := Pack (Fraction);
      end case;
      if Fraction (1) > 2#1_0011_1111_1111_11111# then
         raise Constraint_Error;
      end if;
      -- low-order bits of mantissa
      Result (16) := Byte (             Fraction (4)      and 16#FF#);
      Result (15) := Byte (Shift_Right (Fraction (4),  8) and 16#FF#);
      Result (14) := Byte (Shift_Right (Fraction (4), 16) and 16#FF#);
      Result (13) := Byte (Shift_Right (Fraction (4), 24) and 16#FF#);
      Result (12) := Byte (             Fraction (3)      and 16#FF#);
      Result (11) := Byte (Shift_Right (Fraction (3),  8) and 16#FF#);
      Result (10) := Byte (Shift_Right (Fraction (3), 16) and 16#FF#);
      Result ( 9) := Byte (Shift_Right (Fraction (3), 24) and 16#FF#);
      Result ( 8) := Byte (             Fraction (2)      and 16#FF#);
      Result ( 7) := Byte (Shift_Right (Fraction (2),  8) and 16#FF#);
      Result ( 6) := Byte (Shift_Right (Fraction (2), 16) and 16#FF#);
      Result ( 5) := Byte (Shift_Right (Fraction (2), 24) and 16#FF#);
      Result ( 4) := Byte (             Fraction (1)      and 16#FF#);
      Result ( 3) := Byte (Shift_Right (Fraction (1),  8) and 16#FF#);
      Result ( 2) := Byte (Shift_Right (Fraction (1), 16) and 16#FF#);
      Power := Unsigned_32 (Exponent + Exponent_Bias);
      if Result (2) >= 2#0000_0010# then
         Result (2) := Result (2) and 2#0000_0111#;
         case Encoding is
            when Binary_Integer =>
               --    1 2345 6789 0123 4
               -- S11E EEEE EEEE EEEE EMMM MMMM
               Result (1) :=
                  (  Sign
                  or 2#0110_0000#
                  or Byte (Shift_Right (Power, 9))
                  );
               Result (2) :=
                  Byte (Shift_Right (Power, 1) and 2#1111_1111#);
               Result (3) :=
                  (  Result (3)
                  or Byte (Shift_Left (Power, 7) and 16#FF#)
                  );
            when Densely_Packed =>
               --    1 2 34 5678 9012 34
               -- S11E EMEE EEEE EEEE EEMM MMMM
               Result (1) :=
                  (  Sign
                  or 2#0110_0000#
                  or Shift_Right (Result (3) and 2#0100_0000#, 4)
                  or Byte (Shift_Right (Power,  9) and 2#001_1000#)
                  or Byte (Shift_Right (Power, 10) and 2#000_0011#)
                  );
               Result (2) :=
                  Byte (Shift_Right (Power, 2) and 16#FF#);
               Result (3) := -- higher-order bits of mantissa
                  (  (Result (3) and 2#0011_1111#)
                  or Byte (Shift_Left (Power, 6) and 16#FF#)
                  );
         end case;
      else
         if Power >= 2#11_0000_0000_0000# then -- Clamping
            raise Constraint_Error;
         end if;
         case Encoding is
            when Binary_Integer =>
               --  123 4567 8901 234
               -- SEEE EEEE EEEE EEEM MMMM MMMM
               Result (1) :=
                  Sign or Byte (Shift_Right (Power, 7) and 16#FF#);
               Result (2) :=
                  Result (2) or Byte (Shift_Left (Power, 1) and 16#FF#);
            when Densely_Packed =>
               --  12    34 5678 9012 34
               -- SEEM MMEE EEEE EEEE EEMM MMMM
               Result (1) :=
                  (  Sign
                  or Byte (Shift_Right (Fraction (1), 12) and 2#1_1100#)
                  or Byte (Shift_Right (Power,  7) and 2#0110_0000#)
                  or Byte (Shift_Right (Power, 10) and 2#0000_0011#)
                  );
               Result (2) :=
                  Byte (Shift_Right (Power, 2) and 16#FF#);
               Result (3) :=
                  (  (Result (3) and 2#0011_1111#)
                  or Byte (Shift_Left (Power, 6) and 16#FF#)
                  );
         end case;
      end if;
      return Result;
   end To_IEEE;

   function Unpack (Value : Integer_128) return Integer_128 is
      Accumulator : Integer_128 := Value;
      Result      : Integer_128 := (others => 0);
      Triade      : array (1..12) of Unsigned_32;
   begin
      for Index in reverse Triade'Range loop
         Div_32 (Accumulator, 2**10, Triade (Index));
      end loop;
      Add_32
      (  Result,
         Unsigned_32 (Unpack (Packed_Decimal (Triade (1))))
      );
      for Index in 2..Triade'Last loop
         Mul_32 (Result, 1000);
         Add_32
         (  Result,
            Unsigned_32 (Unpack (Packed_Decimal (Triade (Index))))
         );
      end loop;
      return Result;
   end Unpack;

end IEEE_754.Decimal128;
