--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     IEEE_754.Generic_Decimal32                  Luebeck            --
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

package body IEEE_754.Decimal32 is
   Exponent_Bias : constant := 101;

   function Pack (Value : Unsigned_32) return Unsigned_32 is
   begin
      return
      (  Unsigned_32 (Pack (Unpacked_Decimal (Value mod 1000)))
      or Shift_Left
         (  Unsigned_32
            (  Pack (Unpacked_Decimal ((Value / 10**3) mod 1000))
            ),
            10
         )
      or Shift_Left
         (  Unsigned_32
            (  Pack (Unpacked_Decimal ((Value / 10**6) mod 1000))
            ),
            20
      )  );
   end Pack;

   function Unpack (Value : Unsigned_32) return Unsigned_32 is
   begin
      return
      (  Unsigned_32
         (  Unpack
            (  Packed_Decimal (Value         and 2#11_1111_1111#)
         )  )
      +  (  Unsigned_32
            (  Unpack
               (  Packed_Decimal
                  (  Shift_Right (Value, 10) and 2#11_1111_1111#
            )  )  )
         *  10**3
         )
      +  (  Unsigned_32
            (  Unpack
               (  Packed_Decimal
                  (  Shift_Right (Value, 20) and 2#11_1111_1111#
            )  )  )
         *  10**6
      )  );
   end Unpack;

   procedure From_IEEE
             (  Value    : Decimal_32;
                Mantissa : out Decimal_32_Mantissa;
                Exponent : out Decimal_32_Exponent;
                Encoding : Decimal_Encoding := Binary_Integer
             )  is
      Fraction : Unsigned_32;
      Power    : Unsigned_32;
   begin
      case Value (1) is
         when 2#0000_0000#..2#0101_1111# | -- 00..10
              2#1000_0000#..2#1101_1111# =>
            case Encoding is
               when Binary_Integer =>
                  --  123 4567 8
                  -- SEEE EEEE EMMM MMMM
                  Power :=
                     (  Shift_Left
                        (  Unsigned_32 (Value (1) and 2#0111_1111#),
                           1
                        )
                     or Unsigned_32 (Shift_Right (Value (2), 7))
                     );
                  Fraction := Unsigned_32 (Value (2) and 2#0111_1111#);
               when Densely_Packed =>
                  --  12    34 5678
                  -- SEEM MMEE EEEE MMMM
                  Power :=
                     (  Shift_Left
                        (  Unsigned_32 (Value (1) and 2#0110_0000#),
                           1
                        )
                     or Shift_Left
                        (  Unsigned_32 (Value (1) and 2#0000_0011#),
                           4
                        )
                     or Unsigned_32 (Shift_Right (Value (2), 4))
                     );
                  Fraction :=
                     Unsigned_32
                     (  Shift_Left (Value (1) and 2#0001_1100#, 2)
                     or (           Value (2) and 2#0000_1111#)
                     );
            end case;
         when 2#0110_0000#..2#0111_0111# | -- 1100..1110
              2#1110_0000#..2#1111_0111# =>
            case Encoding is
               when Binary_Integer =>
                  --    1 2345 678
                  -- S11E EEEE EEEM MMMM
                  Power :=
                     (  Shift_Left
                        (  Unsigned_32 (Value (1) and 2#0001_1111#),
                           3
                        )
                     or Unsigned_32 (Shift_Right (Value (2), 5))
                     );
                  Fraction :=
                     Unsigned_32
                     (  (Value (2) and 2#0001_1111#)
                     or 2#1000_0000#
                     );
               when Densely_Packed =>
                  --   1  2 34 5678
                  -- S11E EMEE EEEE MMMM
                  Power :=
                     (  Shift_Left
                        (  Unsigned_32 (Value (1) and 2#0001_1000#),
                           3
                        )
                     or Shift_Left
                        (  Unsigned_32 (Value (1) and 2#0000_0011#),
                           4
                        )
                     or Unsigned_32 (Shift_Right (Value (2), 4))
                     );
                  Fraction :=
                     Unsigned_32
                     (  Shift_Left (Value (1) and 2#0000_0100#, 2)
                     or (Value (2) and 2#0000_1111#)
                     or 2#1000_0000#
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
      Exponent := Decimal_32_Exponent (Integer (Power) - Exponent_Bias);
      Fraction :=
          (  Shift_Left (Fraction,               16)
          or Shift_Left (Unsigned_32 (Value (3)), 8)
          or             Unsigned_32 (Value (4))
          );
      case Encoding is
         when Binary_Integer =>
            Mantissa := Decimal_32_Mantissa (Fraction);
         when Densely_Packed =>
            Mantissa := Decimal_32_Mantissa (Unpack (Fraction));
      end case;
      if Value (1) > 127 then
         Mantissa := -Mantissa;
      end if;
   exception
      when Constraint_Error =>
         if Value (1) > 127 then
            raise Negative_Overflow_Error;
         else
            raise Positive_Overflow_Error;
         end if;
   end From_IEEE;

   function Is_NaN (Value : Decimal_32) return Boolean is
   begin
      return (Value (1) and 2#0111_1100#) = 2#0111_1100#;
   end Is_NaN;

   function Is_Negative (Value : Decimal_32) return Boolean is
   begin
      return Value (1) > 127;
   end Is_Negative;

   function Is_Real (Value : Decimal_32) return Boolean is
   begin
      return (Value (1) and 2#0111_1000#) < 2#0111_1000#;
   end Is_Real;

   function To_IEEE
            (  Mantissa : Decimal_32_Mantissa;
               Exponent : Decimal_32_Exponent;
               Encoding : Decimal_Encoding := Binary_Integer
            )  return Decimal_32 is
      Result   : Decimal_32;
      Power    : Unsigned_32;
      Fraction : Unsigned_32;
      Sign     : Byte := 0;
   begin
      if Mantissa < 0 then
         Fraction := Unsigned_32 (-Mantissa);
         Sign := 2#1000_0000#;
      else
         Fraction := Unsigned_32 (Mantissa);
      end if;
      case Encoding is
         when Binary_Integer =>
            null;
         when Densely_Packed =>
            Fraction := Pack (Fraction);
      end case;
      -- low-order bits of mantissa
      Result (4) := Byte (             Fraction      and 16#FF#);
      Result (3) := Byte (Shift_Right (Fraction,  8) and 16#FF#);
      Result (2) := Byte (Shift_Right (Fraction, 16) and 16#FF#);
      Power := Unsigned_32 (Exponent + Exponent_Bias);
      if Result (2) >= 2#0010_0000# then
         Result (2) := Result (2) and 2#0001_1111#;
         case Encoding is
            when Binary_Integer =>
               --    1 2345 678
               -- S11E EEEE EEEM MMMM
               Result (1) :=
                  (  Sign
                  or 2#0110_0000#
                  or Byte (Shift_Right (Power, 3))
                  );
               Result (2) := -- higher-order bits of mantissa
                  (  Result (2)
                  or Byte (Shift_Left (Power, 5) and 2#1110_0000#)
                  );
            when Densely_Packed =>
               --   1  2 34 5678
               -- S11E EMEE EEEE MMMM
               Result (1) :=
                  (  Sign
                  or 2#0110_0000#
                  or Shift_Right (Result (2) and 2#0001_0000#, 2)
                  or Byte (Shift_Right (Power, 4) and 2#000_0011#)
                  or Byte (Shift_Right (Power, 3) and 2#001_1000#)
                  );
               Result (2) := -- higher-order bits of mantissa
                  (  (Result (2) and 2#0000_1111#)
                  or Byte (Shift_Left (Power, 4) and 2#1111_0000#)
                  );
         end case;
      else
         if Power >= 2#1100_0000# then -- Clamping
            raise Constraint_Error;
         end if;
         case Encoding is
            when Binary_Integer =>
               --  123 4567 8
               -- SEEE EEEE EMMM MMMM
               Result (1) :=
                  Sign or Byte (Shift_Right (Power, 1));
               Result (2) :=
                  (  Result (2) -- higher-order bits of mantissa
                  or Byte (Shift_Left (Power, 7) and 2#1000_0000#)
                  );
            when Densely_Packed =>
               --  12    34 5678
               -- SEEM MMEE EEEE MMMM
               Result (1) :=
                  (  Sign
                  or Shift_Right (Result (2)      and 2#0111_0000#, 2)
                  or Byte (Shift_Right (Power, 4) and 2#0000_0011#)
                  or Byte (Shift_Right (Power, 1) and 2#0110_0000#)
                  );
               Result (2) :=
                  (  (Result (2) and 2#0000_1111#)
                  or Byte (Shift_Left (Power, 4) and 2#1111_1111#)
                  );
         end case;
      end if;
      return Result;
   end To_IEEE;

end IEEE_754.Decimal32;
