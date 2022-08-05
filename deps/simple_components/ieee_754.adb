--                                                                    --
--  package IEEE_754                Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2020       --
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

package body IEEE_754 is

   procedure Add (Left : in out Integer_128; Right : Integer_128);
   pragma Inline (Add);

   function Invert (Value : Integer_128) return Integer_128;
   pragma Inline (Invert);

   function Less (Left, Right : Integer_128) return Boolean;
   pragma Inline (Less);

   function Pow
            (  Left  : Integer_128;
               Right : Natural
            )  return Integer_128;
   pragma Inline (Pow);

   procedure Shift_Left (Value : in out Integer_128);
   pragma Inline (Shift_Left);

   procedure Shift_Right (Value : in out Integer_128);
   pragma Inline (Shift_Right);

   procedure Shift_Right_32 (Value : in out Integer_128);
   pragma Inline (Shift_Right_32);

   procedure Set (Value : in out Integer_128; Init : Unsigned_64);
   pragma Inline (Set);

   procedure Sub (Left : in out Integer_128; Right : Integer_128);
   pragma Inline (Sub);

   procedure Add (Left : in out Integer_128; Right : Integer_128) is
      This : Unsigned_64;
   begin
      This := Unsigned_64 (Left (4)) + Unsigned_64 (Right (4));
      Left (4) := Unsigned_32 (This and 16#FFFF_FFFF#);
      This := Shift_Right (This, 32)
            + Unsigned_64 (Left  (3))
            + Unsigned_64 (Right (3));
      Left (3) := Unsigned_32 (This and 16#FFFF_FFFF#);
      This := Shift_Right (This, 32)
            + Unsigned_64 (Left  (2))
            + Unsigned_64 (Right (2));
      Left (2) := Unsigned_32 (This and 16#FFFF_FFFF#);
      This := Shift_Right (This, 32)
            + Unsigned_64 (Left  (1))
            + Unsigned_64 (Right (1));
      Left (1) := Unsigned_32 (This and 16#FFFF_FFFF#);
   end Add;

   procedure Add_32
             (  Summand : in out Integer_128;
                Addend  : Unsigned_32
             )  is
      Accumulator : Unsigned_64;
   begin
      Accumulator := Unsigned_64 (Addend) + Unsigned_64 (Summand (4));
      Summand (4) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
      Accumulator := Shift_Right (Accumulator, 32);
      if Accumulator = 0 then
         return;
      end if;
      Accumulator := Accumulator + Unsigned_64 (Summand (3));
      Summand (3) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
      Accumulator := Shift_Right (Accumulator, 32);
      if Accumulator = 0 then
         return;
      end if;
      Accumulator := Accumulator + Unsigned_64 (Summand (2));
      Summand (2) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
      Accumulator := Shift_Right (Accumulator, 32);
      if Accumulator = 0 then
         return;
      end if;
      Accumulator := Accumulator + Unsigned_64 (Summand (1));
      Summand (1) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
      Accumulator := Shift_Right (Accumulator, 32);
      if Accumulator = 0 then
         return;
      else
         raise Constraint_Error;
      end if;
   end Add_32;

   procedure Add_64
             (  Summand : in out Integer_128;
                Addend  : Unsigned_64
             )  is
      Accumulator : Unsigned_64;
   begin
      Accumulator := (Addend and 16#FFFF_FFFF#)
                   + Unsigned_64 (Summand (4));
      Summand (4) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
      Accumulator := Shift_Right (Accumulator, 32);

      Accumulator := Shift_Right (Addend, 32)
                   + Accumulator
                   + Unsigned_64 (Summand (3));
      Summand (3) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
      Accumulator := Shift_Right (Accumulator, 32);
      if Accumulator = 0 then
         return;
      end if;
      Accumulator := Accumulator + Unsigned_64 (Summand (2));
      Summand (2) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
      Accumulator := Shift_Right (Accumulator, 32);
      if Accumulator = 0 then
         return;
      end if;
      Accumulator := Accumulator + Unsigned_64 (Summand (1));
      Summand (1) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
      Accumulator := Shift_Right (Accumulator, 32);
      if Accumulator = 0 then
         return;
      else
         raise Constraint_Error;
      end if;
   end Add_64;

   procedure Div
             (  Dividend  : Integer_128;
                Divisor   : Integer_128;
                Result    : out Integer_128;
                Remainder : out Integer_128
             )  is
   begin
      Result := (0,0,0,0);
      if Less (Dividend, Divisor) then
         Remainder := Divisor;
         return;
      end if;
      declare
         Subtrahend : Integer_128 := Divisor;
         Power      : Integer_128 := (0,0,0,1);
         Shift      : Integer     := 1;
      begin
         Remainder := Dividend;
         loop
            Shift_Left (Subtrahend);
            if Less (Remainder, Subtrahend) then
               Shift_Right (Subtrahend);
               exit;
            end if;
            Shift_Left (Power);
            Shift := Shift + 1;
         end loop;
         Sub (Remainder, Subtrahend);
         Add (Result, Power);
         while Shift > 1 loop
            Shift := Shift - 1;
            Shift_Right (Subtrahend);
            Shift_Right (Power);
            if not Less (Remainder, Subtrahend) then
               Sub (Remainder, Subtrahend);
               Add (Result, Power);
            end if;
         end loop;
      end;
   end Div;

   procedure Div_32
             (  Dividend  : in out Integer_128;
                Divisor   : Unsigned_32;
                Remainder : out Unsigned_32
             )  is
      Denominator : constant Unsigned_64 := Unsigned_64 (Divisor);
      Accumulator : Unsigned_64 := 0;
   begin
      for Index in Dividend'Range loop
         Accumulator := Shift_Left (Accumulator, 32)
                      + Unsigned_64 (Dividend (Index));
         Dividend (Index) := Unsigned_32 (Accumulator / Denominator);
         Accumulator      := Accumulator mod Denominator;
      end loop;
      Remainder := Unsigned_32 (Accumulator);
   end Div_32;

   function From_Integer_64 (Value : Integer_64) return Integer_128 is
   begin
      if Value >= 0 then
         return
         (  0,
            0,
            Unsigned_32 (Value / 2**32),
            Unsigned_32 (Value mod 2**32)
         );
      else
         declare
            Left : constant Unsigned_64 :=
                         not Unsigned_64 (-(Value + 1));
         begin
            return
            (  16#FFFF_FFFF#,
               16#FFFF_FFFF#,
               Unsigned_32 (Shift_Right (Left, 32)),
               Unsigned_32 (Left and 16#FFFF_FFFF#)
            );
         end;
      end if;
   end From_Integer_64;

   function From_Unsigned_64 (Value : Unsigned_64) return Integer_128 is
   begin
      return
      (  0,
         0,
         Unsigned_32 (Shift_Right (Value, 32)),
         Unsigned_32 (Value and 16#FFFF_FFFF#)
      );
   end From_Unsigned_64;

   function Invert (Value : Integer_128) return Integer_128 is
      This   : Unsigned_64;
      Result : Integer_128;
   begin
      This := 1                      + Unsigned_64 (not Value (4));
      Result (4) := Unsigned_32 (This and 16#FFFF_FFFF#);
      This := Shift_Right (This, 32) + Unsigned_64 (not Value (3));
      Result (3) := Unsigned_32 (This and 16#FFFF_FFFF#);
      This := Shift_Right (This, 32) + Unsigned_64 (not Value (2));
      Result (2) := Unsigned_32 (This and 16#FFFF_FFFF#);
      This := Shift_Right (This, 32) + Unsigned_64 (not Value (1));
      Result (1) := Unsigned_32 (This and 16#FFFF_FFFF#);
      return Result;
   end Invert;

   function Is_Negative (Left : Integer_128) return Boolean is
   begin
      return Left (1) >= 2**31;
   end Is_Negative;

   function Is_Positive (Left : Integer_128) return Boolean is
   begin
      return (  Left (1) < 2**31
             and then
                (  Left (1) /= 0
                or else
                   Left (2) /= 0
                or else
                   Left (3) /= 0
                or else
                   Left (4) /= 0
             )  );
   end Is_Positive;

   function Is_Zero (Left : Integer_128) return Boolean is
   begin
      return Left (1) = 0 and then Left (2) = 0 and then
             Left (3) = 0 and then Left (4) = 0;
   end Is_Zero;

   function Less (Left, Right : Integer_128) return Boolean is
   begin
      for Index in Left'Range loop
         if Left (Index) /= Right (Index) then
            return Left (Index) < Right (Index);
         end if;
      end loop;
      return False;
   end Less;

   function Mul (Left, Right : Integer_128) return Integer_128 is
      Result      : Integer_128;
      Accumulator : Integer_128;
      I0 : constant := 4;
      I1 : constant := 3;
      I2 : constant := 2;
      I3 : constant := 1;
   begin
      if (  (Left (I2) /= 0 and then Right (I2) /= 0) -- 2 + 2    > 3
         or else                                      -- 3 + 1..3 > 3
            (  Left (I3) /= 0
            and then
               (  Right (I1) /= 0
               or else
                  Right (I2) /= 0
               or else
                  Right (I3) /= 0
            )  )
         or else
            (  Right (I3) /= 0                       -- 1..3 + 3 > 3
            and then
               (  Left (I1) /= 0
               or else
                  Left (I2) /= 0
         )  )  )  then
         raise Constraint_Error;
      end if;
      -- 0 + 0 = 0  Powers
      Set
      (  Accumulator,
         Unsigned_64 (Left (I0)) *  Unsigned_64 (Right (I0))
      );
      Result (I0) := Accumulator (4);
      Shift_Right_32 (Accumulator);
      --
      -- 0 + 1 = 1 + 0 = 1  Powers
      --
      Add_64
      (  Accumulator,
         Unsigned_64 (Left (I0)) * Unsigned_64 (Right (I1))
      );
      Add_64
      (  Accumulator,
         Unsigned_64 (Left (I1)) * Unsigned_64 (Right (I0))
      );
      Result (I1) := Accumulator (4);
      Shift_Right_32 (Accumulator);
      --
      -- 1 + 1 = 0 + 2 = 2 + 0 = 2  Powers
      --
      Add_64
      (  Accumulator,
         Unsigned_64 (Left (I1)) * Unsigned_64 (Right (I1))
      );
      Add_64
      (  Accumulator,
         Unsigned_64 (Left (I0)) * Unsigned_64 (Right (I2))
      );
      Add_64
      (  Accumulator,
         Unsigned_64 (Left (I2)) * Unsigned_64 (Right (I0))
      );
      Result (I2) := Accumulator (4);
      Shift_Right_32 (Accumulator);
      --
      -- 0 + 3 = 3 + 0 = 1 + 2 = 2 + 1 = 3
      --
      Add_64
      (  Accumulator,
         Unsigned_64 (Left (I0)) * Unsigned_64 (Right (I3))
      );
      Add_64
      (  Accumulator,
         Unsigned_64 (Left (I3)) * Unsigned_64 (Right (I0))
      );
      Add_64
      (  Accumulator,
         Unsigned_64 (Left (I1)) * Unsigned_64 (Right (I2))
      );
      Add_64
      (  Accumulator,
         Unsigned_64 (Left (I2)) * Unsigned_64 (Right (I1))
      );
      Result (I3) := Accumulator (4);
      Shift_Right_32 (Accumulator);
      if Accumulator (4) = 0 then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end Mul;

   procedure Mul_32
             (  Multiplicand : in out Integer_128;
                Multiplier   : Unsigned_32
             )  is
      Factor      : constant Unsigned_64 := Unsigned_64 (Multiplier);
      Accumulator : Unsigned_64 := 0;
   begin
      for Index in reverse Multiplicand'Range loop
         Accumulator :=
            Accumulator + Unsigned_64 (Multiplicand (Index)) * Factor;
         Multiplicand (Index) :=
            Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
         Accumulator  := Shift_Right (Accumulator, 32);
      end loop;
      if Accumulator /= 0 then
         raise Constraint_Error;
      end if;
   end Mul_32;

   procedure Neg (Value : in out Integer_128) is
      High : constant Unsigned_32 := Value (1);
      This : Unsigned_64;
   begin
      This := 1                      + Unsigned_64 (not Value (4));
      Value (4) := Unsigned_32 (This and 16#FFFF_FFFF#);
      This := Shift_Right (This, 32) + Unsigned_64 (not Value (3));
      Value (3) := Unsigned_32 (This and 16#FFFF_FFFF#);
      This := Shift_Right (This, 32) + Unsigned_64 (not Value (2));
      Value (2) := Unsigned_32 (This and 16#FFFF_FFFF#);
      This := Shift_Right (This, 32) + Unsigned_64 (not Value (1));
      Value (1) := Unsigned_32 (This and 16#FFFF_FFFF#);
      if 0 = ((High xor Value (1)) and 2**31) then
         raise Constraint_Error; -- No sign change
      end if;
   end Neg;

   function Pack (Value : Unpacked_Decimal) return Packed_Decimal is
      High   : constant Unsigned_16 :=
                        Unsigned_16 (Value / 100);
      Middle : constant Unsigned_16 :=
                        Unsigned_16 ((Value / 10) mod 10);
      Low    : constant Unsigned_16 :=
                        Unsigned_16 (Value mod 10);
   begin
      if High < 8 then
         if Middle < 8 then
            if Low < 8 then   -- 0..7 0..7 0..7
               return Packed_Decimal
                      (  Shift_Left (High,   7)
                      or Shift_Left (Middle, 4)
                      or             Low
                      );
            else              -- 0..7 0..7 8..9
               return Packed_Decimal
                      (  Shift_Left (High,   7)
                      or Shift_Left (Middle, 4)
                      or            (Low and 1)
                      or 2#00_0000_1000#
                      );
            end if;
         else
            if Low < 8 then  -- 0..7 8..9 0..7
               return Packed_Decimal
                      (  Shift_Left (High, 7)
                      or Shift_Left (Middle and 1,      4)
                      or Shift_Left (Low    and 2#110#, 4)
                      or            (Low    and 1        )
                      or 2#00_0000_1010#
                      );
            else              -- 0..7 8..9 8..9
               return Packed_Decimal
                      (  Shift_Left (High, 7)
                      or Shift_Left (Middle and 1, 4)
                      or            (Low    and 1   )
                      or 2#00_0100_1110#
                      );
            end if;
         end if;
      else
         if Middle < 8 then
            if Low < 8 then  -- 8..9 0..7 0..7
               return Packed_Decimal
                      (  Shift_Left (High   and 1,      7)
                      or Shift_Left (Middle,            4)
                      or Shift_Left (Low    and 2#110#, 7)
                      or            (Low    and 1        )
                      or 2#00_0000_1100#
                      );
            else             -- 8..9 0..7 8..9
               return Packed_Decimal
                      (  Shift_Left (High   and 1,      7)
                      or Shift_Left (Middle and 2#110#, 7)
                      or Shift_Left (Middle and 1,      4)
                      or            (Low    and 1        )
                      or 2#00_0010_1110#
                      );
            end if;
         else
            if Low < 8 then  -- 8..9 8..9 0..7
               return Packed_Decimal
                      (  Shift_Left (High   and 1,      7)
                      or Shift_Left (Middle and 1,      4)
                      or Shift_Left (Low    and 2#110#, 7)
                      or            (Low    and 1        )
                      or 2#00_0000_1110#
                      );
            else             -- 8..9 8..9 8..9
               return Packed_Decimal
                      (  Shift_Left (High   and 1, 7)
                      or Shift_Left (Middle and 1, 4)
                      or            (Low    and 1   )
                      or 2#00_0110_1110#
                      );
            end if;
         end if;
      end if;
   end Pack;

   function Pow
            (  Left  : Integer_128;
               Right : Natural
            )  return Integer_128 is
      Exponent     : Unsigned_32 := Unsigned_32 (Right);
      Multiplicand : Integer_128 := Left;
      Result       : Integer_128;
   begin
      if 0 = (Exponent and 1) then
         Result := (0, 0, 0, 1);
      else
         Result := Multiplicand;
      end if;
      loop
         Exponent := Shift_Right (Exponent, 1);
         exit when Exponent = 0;
         Multiplicand := Mul (Multiplicand, Multiplicand);
         if 0 /= (Exponent and 1) then
            Result := Mul (Result, Multiplicand);
         end if;
      end loop;
      return Result;
   end Pow;

   procedure Set (Value : in out Integer_128; Init : Unsigned_64) is
   begin
      Value (4) := Unsigned_32 (Init and 16#FFFF_FFFF#);
      Value (3) := Unsigned_32 (Shift_Right (Init, 32));
      Value (2) := 0;
      Value (1) := 0;
   end Set;

   procedure Shift_Left (Value : in out Integer_128) is
   begin
      Value (1) :=
         Shift_Left (Value (1), 1) or Shift_Right (Value (2), 31);
      Value (2) :=
         Shift_Left (Value (2), 1) or Shift_Right (Value (3), 31);
      Value (3) :=
         Shift_Left (Value (3), 1) or Shift_Right (Value (4), 31);
      Value (4) :=
         Shift_Left (Value (4), 1);
   end Shift_Left;

   procedure Shift_Right (Value : in out Integer_128) is
   begin
      Value (4) :=
         Shift_Right (Value (4), 1) or Shift_Left (Value (3), 31);
      Value (3) :=
         Shift_Right (Value (3), 1) or Shift_Left (Value (2), 31);
      Value (2) :=
         Shift_Right (Value (2), 1) or Shift_Left (Value (1), 31);
      Value (1) :=
         Shift_Right (Value (2), 1);
   end Shift_Right;

   procedure Shift_Right_32 (Value : in out Integer_128) is
   begin
      Value (4) := Value (3);
      Value (3) := Value (2);
      Value (2) := Value (1);
      Value (1) := 0;
   end Shift_Right_32;

   procedure Sub (Left : in out Integer_128; Right : Integer_128) is
      Borrow : Unsigned_64;
   begin
      Borrow := Unsigned_64 (Left (4)) - Unsigned_64 (Right (4));
      Left (4) := Unsigned_32 (Borrow and 16#FFFF_FFFF#);
      Borrow := (  Unsigned_64 (Left (3))
                -  Unsigned_64 (Right (3))
                -  (Shift_Right (Borrow, 32) and 1)
                );
      Left (3) := Unsigned_32 (Borrow and 16#FFFF_FFFF#);
      Borrow := (  Unsigned_64 (Left (2))
                -  Unsigned_64 (Right (2))
                -  (Shift_Right (Borrow, 32) and 1)
                );
      Left (2) := Unsigned_32 (Borrow and 16#FFFF_FFFF#);
      Borrow := (  Unsigned_64 (Left (1))
                -  Unsigned_64 (Right (1))
                -  (Shift_Right (Borrow, 32) and 1)
                );
      Left (1) := Unsigned_32 (Borrow and 16#FFFF_FFFF#);
   end Sub;

   function To_Integer_64 (Value : Integer_128) return Integer_64 is
   begin
      if Value (1) >= 2**31 then
         if (  Value (1) = 16#FFFF_FFFF#
            or else
               Value (2) = 16#FFFF_FFFF#
            or else
               Value (3) >= 2**31
            )  then
            return -  Integer_64
                      (  not
                         (  Unsigned_64 (Value (4))
                         or Shift_Left (Unsigned_64 (Value (3)), 32)
                      )  )
                   -  1;
         end if;
      else
         if (  Value (1) = 0
            or else
               Value (2) = 0
            or else
               Value (3) < 2**31
            )  then
            return Integer_64 (Value (4))
                 + Integer_64 (Value (3)) * 2**32;
         end if;
      end if;
      raise Constraint_Error;
   end To_Integer_64;

   function To_Unsigned_64 (Value : Integer_128) return Unsigned_64 is
   begin
      if (  Value (1) < 2**31
         and then
            (  Value (1) = 0
            or else
               Value (2) = 0
         )  )  then
         return
         (  Unsigned_64 (Value (4))
         or Shift_Left (Unsigned_64 (Value (3)), 32)
         );
      else
         raise Constraint_Error;
      end if;
   end To_Unsigned_64;

   function Unpack (Value : Packed_Decimal) return Unpacked_Decimal is
      Data   : constant Unsigned_16 := Unsigned_16 (Value);
      High   : Unsigned_16 := Shift_Right (Data, 7);
      Middle : Unsigned_16 := Shift_Right (Data, 4) and 2#111#;
      Low    : Unsigned_16 :=              Data     and 2#111#;
   begin
      case Shift_Right (Data, 1) and 2#111# is
         when 2#000#..2#011# =>
            null;
         when 2#100# =>
            Low := (Low and 1) or 8;
         when 2#101# =>
            Low    := (Low and 1) or (Middle and 2#110#);
            Middle := (Middle and 1) or 8;
         when 2#110# =>
            Low    := (Low and 1) or (High and 2#110#);
            High   := (High and 1) or 8;
         when others =>
            case Shift_Right (Data, 5) and 2#11# is
               when 2#00# =>
                  Low    := (Low    and 1) or (High and 2#110#);
                  Middle := (Middle and 1) or 8;
                  High   := (High   and 1) or 8;
               when 2#01# =>
                  Middle := (Middle and 1) or (High and 2#110#);
                  High   := (High   and 1) or 8;
                  Low    := (Low    and 1) or 8;
               when 2#10# =>
                  Middle := (Middle and 1) or 8;
                  Low    := (Low    and 1) or 8;
               when others =>
                  High   := (High   and 1) or 8;
                  Middle := (Middle and 1) or 8;
                  Low    := (Low    and 1) or 8;
            end case;
      end case;
      return Unpacked_Decimal (High * 100 + Middle * 10 + Low);
   end Unpack;

   function "<" (Left, Right : Integer_128) return Boolean is
   begin
      if Left (1) >= 2**31 then -- Negative
         if Right (1) < 2**31 then -- Not negative
            return True;
         end if;
      else
         if Right (1) >= 2**31 then -- Negative
            return False;
         end if;
      end if;
      for Index in Left'Range loop
         if Left (Index) /= Right (Index) then
            return Left (Index) < Right (Index);
         end if;
      end loop;
      return False;
   end "<";

   function "<=" (Left, Right : Integer_128) return Boolean is
   begin
      if Left (1) >= 2**31 then -- Negative
         if Right (1) < 2**31 then -- Not negative
            return True;
         end if;
      else
         if Right (1) >= 2**31 then -- Negative
            return False;
         end if;
      end if;
      for Index in Left'Range loop
         if Left (Index) /= Right (Index) then
            return Left (Index) < Right (Index);
         end if;
      end loop;
      return True;
   end "<=";

   function ">" (Left, Right : Integer_128) return Boolean is
   begin
      return not (Left <= Right);
   end ">";

   function ">=" (Left, Right : Integer_128) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function "+" (Left : Integer_128) return Integer_128 is
   begin
      return Left;
   end "+";

   function "+" (Left, Right : Integer_128) return Integer_128 is
      Result      : Integer_128;
      Accumulator : Unsigned_64 := 0;
   begin
      for Index in reverse Left'Range loop
         Accumulator :=
            (  Accumulator
            +  Unsigned_64 (Left (Index))
            +  Unsigned_64 (Right (Index))
            );
         Result (Index) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
         Accumulator    := Shift_Right (Accumulator, 32);
      end loop;
      if Left (1) >= 2**31 then
         if Right (1) < 2**31 or else Result (1) >= 2**31 then
            return Result;
         end if;
      else
         if Right (1) >= 2**31 or else Result (1) < 2**31 then
            return Result;
         end if;
      end if;
      raise Constraint_Error;
   end "+";

   function "-" (Left : Integer_128) return Integer_128 is
      Result : Integer_128 := Left;
   begin
      Neg (Result);
      return Result;
   end "-";

   function "-" (Left, Right : Integer_128) return Integer_128 is
      Result      : Integer_128;
      Accumulator : Unsigned_64 := 1;
   begin
      for Index in reverse Left'Range loop
         Accumulator :=
            (  Accumulator
            +  Unsigned_64 (Left (Index))
            +  Unsigned_64 (not Right (Index))
            );
         Result (Index) := Unsigned_32 (Accumulator and 16#FFFF_FFFF#);
         Accumulator    := Shift_Right (Accumulator, 32);
      end loop;
      if Left (1) >= 2**31 then
         if Right (1) >= 2**31 or else Result (1) >= 2**31 then
            return Result;
         end if;
      else
         if Right (1) < 2**31 or else Result (1) < 2**31 then
            return Result;
         end if;
      end if;
      raise Constraint_Error;
   end "-";

   function "*" (Left, Right : Integer_128) return Integer_128 is
      Result : Integer_128;
   begin
      if Left (1) >= 2**31 then
         if Right (1) >= 2**31 then
            Result := Mul (Invert (Left), Invert (Right));
            if Result (1) >= 2**31 then
               raise Constraint_Error;
            end if;
         else
            Result := Invert (Mul (Invert (Left), Right));
            if Result (1) < 2**31 then
               raise Constraint_Error;
            end if;
         end if;
      else
         if Right (1) >= 2**31 then
            Result := Invert (Mul (Left, Invert (Right)));
            if Result (1) < 2**31 then
               raise Constraint_Error;
            end if;
         else
            Result := Mul (Left, Right);
            if Result (1) >= 2**31 then
               raise Constraint_Error;
            end if;
         end if;
      end if;
      return Result;
   end "*";

   function "**" (Left : Integer_128; Right : Natural)
      return Integer_128 is
      Result : Integer_128;
   begin
      if Right = 0 then
         return (0,0,0,1);
      elsif Right = 1 then
         return Left;
      elsif Left (1) >= 2**31 then
         Result := Pow (Invert (Left), Right);
         if (Right mod 2) = 1 then
            Neg (Result);
            return Result;
         end if;
      else
         Result := Pow (Left, Right);
      end if;
      if Result (1) >= 2**31 then
         raise Constraint_Error;
      end if;
      return Result;
   end "**";

   function "/" (Left, Right : Integer_128) return Integer_128 is
      Result    : Integer_128;
      Remainder : Integer_128;
   begin
      if Left (1) >= 2**31 then
         if Right (1) >= 2**31 then
            Div (Invert (Left), Invert (Right), Result, Remainder);
            return Result;
         else
            Div (Invert (Left), Right, Result, Remainder);
            return Invert (Result);
         end if;
      else
         if Right (1) >= 2**31 then
            Div (Left, Invert (Right), Result, Remainder);
            return Invert (Result);
         else
            Div (Left, Right, Result, Remainder);
            return Result;
         end if;
      end if;
   end "/";

   function "abs" (Left : Integer_128) return Integer_128 is
   begin
      if Left (1) >= 2**31 then
         return -Left;
      else
         return Left;
      end if;
   end "abs";

   function "mod" (Left, Right : Integer_128) return Integer_128 is
      Result    : Integer_128;
      Remainder : Integer_128;
   begin
      if Left (1) >= 2**31 then
         if Right (1) >= 2**31 then
            Div (Invert (Left), Invert (Right), Result, Remainder);
            return Invert (Remainder);
         else
            Div (Invert (Left), Right, Result, Remainder);
            if Is_Zero (Remainder) then
               return Remainder;
            else
               return Right - Remainder;
            end if;
         end if;
      else
         if Right (1) >= 2**31 then
            Div (Left, Invert (Right), Result, Remainder);
            if Is_Zero (Remainder) then
               return Remainder;
            else
               return Right + Remainder;
            end if;
         else
            Div (Left, Right, Result, Remainder);
            return Remainder;
         end if;
      end if;
   end "mod";

   function "rem" (Left, Right : Integer_128) return Integer_128 is
      Result    : Integer_128;
      Remainder : Integer_128;
   begin
      if Left (1) >= 2**31 then
         if Right (1) >= 2**31 then
            Div (Invert (Left), Invert (Right), Result, Remainder);
            return Invert (Remainder);
         else
            Div (Invert (Left), Right, Result, Remainder);
            return Invert (Remainder);
         end if;
      else
         if Right (1) >= 2**31 then
            Div (Left, Invert (Right), Result, Remainder);
            return Remainder;
         else
            Div (Left, Right, Result, Remainder);
            return Remainder;
         end if;
      end if;
   end "rem";

end IEEE_754;
