--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

with ZanyBlue.Text.Format_Parser;
with ZanyBlue.Text.Generic_Buffer;

package body ZanyBlue.Text.Generic_Floats is

   use ZanyBlue.Text.Format_Parser;

   package Integer_Buffer is new ZanyBlue.Text.Generic_Buffer
     (Integer_Type => Integer);
   use Integer_Buffer;
   --  Used to accumulate integer values (format integers to strings).

   subtype Positive_Float_Type is Float_Type range 0.0 .. Float_Type'Last;

   Extra_Digits      : constant          := 1;
   Default_Precision : constant Positive := Float_Type'Digits - 1;
   Number_Of_Digits  : constant Positive := Float_Type'Digits + Extra_Digits;

   subtype Digit_Type is Natural range 0 .. 9;
   type Digits_List_Type is array (Positive range <>) of Digit_Type;

   procedure Decompose
     (Value         :     Positive_Float_Type;
      Number_Digits : out Digits_List_Type;
      Exponent      : out Integer);
   --  Decompose a floating point number into a sequence of decimal digits
   --  (number of digits defined by the Float_TYpe'Digits attribute) and
   --  an exponent, e.g., 1.234567e+10 => {1, 2, 3, 4, 5, 6, 7}, 10

   procedure Emit_Digits
     (Buffer         : in out Buffer_Type;
      Digit_Map      :        String;
      Number_Digits  :        Digits_List_Type;
      Point_Position :        Positive;
      Length         :        Positive;
      Digits_Start   :        Positive;
      Locale         :        Locale_Type);
   --  Write 1 .. Length digits to the output buffer.  The digits are taken
   --  from the Number_Digits vector for positions Digits_Start to length of
   --  the Number_Digits list.  The Zero digit is write for digits not in the
   --  range.  A decimal point character written before outputing position
   --  Point_Position.

   procedure Emit_Sign
     (Buffer      : in out Buffer_Type;
      Is_Negative :        Boolean;
      Sign_Format :        Sign_Type;
      Locale      :        Locale_Type);
   --  Write a sign string to the output buffer.  If the value is negative
   --  the localized '-' is written, otherwise the Sign_Format determines
   --  what is written: ' ', localized '+' or nothing.

   function Sign_String
     (Is_Negative : Boolean;
      Sign_Format : Sign_Type;
      Locale      : Locale_Type)
      return String;
   --  Return the string for the sign based on whether the value is negative
   --  or if positive, a localized '+' or space character.

   procedure Format_E
     (Buffer        : in out Buffer_Type;
      Number_Digits : in out Digits_List_Type;
      Exponent      : in out Integer;
      Formatting    :        Format_Type;
      Digit_Map     :        String;
      Locale        :        Locale_Type);
   --  Implement E formatting, e.g., "1.23456E+10"

   procedure Format_F
     (Buffer        : in out Buffer_Type;
      Number_Digits : in out Digits_List_Type;
      Exponent      : in out Integer;
      Formatting    :        Format_Type;
      Digit_Map     :        String;
      Locale        :        Locale_Type);
   --  Implement F formatting, e.g., "123456E0000.0"

   procedure Format_G
     (Buffer        : in out Buffer_Type;
      Number_Digits : in out Digits_List_Type;
      Exponent      : in out Integer;
      Formatting    :        Format_Type;
      Digit_Map     :        String;
      Locale        :        Locale_Type);
   --  Implement G formatting, the shorter of E or F.

   procedure Format_Infinity
     (Buffer     : in out Buffer_Type;
      Formatting :        Format_Type;
      Negative   :        Boolean;
      Locale     :        Locale_Type);
   --  Format the infinity value, e.g., "Inf", etc.

   procedure Format_Value
     (Buffer     : in out Buffer_Type;
      Fill       :    out Unicode_Character;
      Sign       :        String;
      Formatting :        Format_Type;
      Value      :        Float_Type;
      Locale     :        Locale_Type);
   --  Format a normal (not Infinity or NaN) float type value.

   procedure Format_NaN
     (Buffer : in out Buffer_Type;
      Locale :        Locale_Type);
   --  Format the "Not a Number" value, e.g., "Nan", etc.

   function Is_Infinity
     (Value : Float_Type)
      return Boolean;
   --  Determine if the value is the representation for Infinity.

   function Is_Not_A_Number
     (Value : Float_Type)
      return Boolean;
   --  Determine if the value is the representation for NaN.

   procedure Round
     (Buffer   : in out Digits_List_Type;
      Exponent : in out Integer;
      Overflow :    out Boolean;
      Position :        Positive);
   --  Round the sequence of digits at the given position.  Since this might
   --  cause the an overflow of the buffer, e.g., rounding "9999999" at
   --  position 2 gives "0099999" with an overflow, the exponent might need
   --  to be adjusted, e.g., { "9999999", 2, e } => { "1000999", e + 1 }

   ------------
   -- Create --
   ------------

   function Create
     (Float_Value : Float_Type)
      return Float_Argument_Type
   is
   begin
      return Float_Argument_Type'(Data => Float_Value);
   end Create;

   ---------------
   -- Decompose --
   ---------------
   --
   --  The expressions used to calculate the exponent here are taken from
   --  David M. Gay's paper "Correctly Rounded Binary-Decimal and
   --  Decimal-Binary Conversions",
   --
   --  Floating operations are used to calculate the individual digits rather
   --  "big integers".
   --
   --  Denormalized values are not handled and are simply treated as zero.
   --

   procedure Decompose
     (Value         :     Positive_Float_Type;
      Number_Digits : out Digits_List_Type;
      Exponent      : out Integer)
   is

      C1 : constant := 0.3010_2999_5663_981;    --  log_{10} 2
      C2 : constant := 0.2895_2965_4602_168;    --  1/(1.5*log_{2} 10)
      C3 : constant := 0.1760_9125_9055_8;      --  log_{10} 1.5
      --  These C* constants are taken from Gay's and are used to determine the
      --  base 10 exponent for a floating point value.  The estimate of this
      --  value (k, in Gay's paper) might be off by 1.  It is adjusted during
      --  formatting.

      X       : Float_Type := Float_Type'Fraction (Value);
      l       : Integer    := Float_Type'Exponent (Value);
      Is_Zero : Boolean    := True;
      Scaled  : Float_Type;

   begin
      --  Scale up "l" based on the machine radix (the compiler should optimize
      --  this case statement away.
      case Float_Type'Machine_Radix is
         when 2 =>
            null;
         when 4 =>
            l := l * 2;
         when 8 =>
            l := l * 3;
         when 16 =>
            l := l * 4;
         when others =>
            raise Unsupported_Radix_Error
              with Integer'Image (Float_Type'Machine_Radix);
      end case;

      while X > 2.0 loop
         X := X / 2.0;
         l := l + 1;
      end loop;
      Exponent :=
        Integer (Float_Type'Floor (Float_Type (l) * C1 + (X - 1.5) * C2 + C3));
      Scaled := Value * 10.0**(-Exponent);
      for I in 1 .. Number_Digits'Last loop
         Number_Digits (I) := Integer (Float_Type'Truncation (Scaled));
         Is_Zero           := Is_Zero and then Number_Digits (I) = 0;
         Scaled            := (Scaled - Float_Type (Number_Digits (I))) * 10.0;
      end loop;
      if Is_Zero then
         Exponent := 0;
      elsif Number_Digits (1) = 0 then
         --  Exponent off by one, adjust
         Number_Digits (1 .. Number_Digits'Last - 1) :=
           Number_Digits (2 .. Number_Digits'Last);
         Number_Digits (Number_Digits'Last) :=
           Integer (Float_Type'Truncation (Scaled));
         Exponent := Exponent - 1;
      end if;
   exception
      when Constraint_Error =>
      --  Underflow?  Just set to zero
         Number_Digits := (others => 0);
         Exponent      := 0;
   end Decompose;

   -----------------
   -- Emit_Digits --
   -----------------

   procedure Emit_Digits
     (Buffer         : in out Buffer_Type;
      Digit_Map      :        String;
      Number_Digits  :        Digits_List_Type;
      Point_Position :        Positive;
      Length         :        Positive;
      Digits_Start   :        Positive;
      Locale         :        Locale_Type)
   is

      Zero_Index  : constant Positive          := Digit_Map.First;
      Zero        : constant Unicode_Character := Digit_Map (Zero_Index);
      Digit_Index : Integer;
      Digit       : Unicode_Character;

   begin
      for I in 1 .. Length loop
         if I = Point_Position then
            Add (Buffer, Numeric_Item (Locale, Decimal_Point_Character));
         end if;
         Digit_Index := I - Digits_Start + 1;
         if Digit_Index in Number_Digits'Range then
            Digit := Digit_Map (Zero_Index + Number_Digits (Digit_Index));
         else
            Digit := Zero;
         end if;
         Add (Buffer, Digit);
      end loop;
   end Emit_Digits;

   ---------------
   -- Emit_Sign --
   ---------------

   procedure Emit_Sign
     (Buffer      : in out Buffer_Type;
      Is_Negative :        Boolean;
      Sign_Format :        Sign_Type;
      Locale      :        Locale_Type)
   is
   begin
      Add (Buffer, Sign_String (Is_Negative, Sign_Format, Locale));
   end Emit_Sign;

   ------------
   -- Format --
   ------------

   overriding function Format
     (Value     : Float_Argument_Type;
      Type_Name : String;
      Template  : String;
      Locale    : Locale_Type)
      return String
   is

      pragma Unreferenced (Type_Name);

      Formatting  : constant Format_Type := Parse (Template, Locale);
      Is_Negative : constant Boolean     := Value.Data < 0.0;
      Sign        : constant String      :=
        Sign_String (Is_Negative, Formatting.Sign, Locale);
      Fill   : Unicode_Character := Formatting.Fill;
      Buffer : Buffer_Type;

   begin
      if Is_Not_A_Number (Value.Data) then
         Format_NaN (Buffer, Locale);
      elsif Is_Infinity (Value.Data) then
         Format_Infinity (Buffer, Formatting, Is_Negative, Locale);
      else
         Format_Value (Buffer, Fill, Sign, Formatting, Value.Data, Locale);
      end if;
      if Sign.Length > 0 and then Formatting.Align = Numeric then
         return
           Sign &
           Align
             (To_String (Buffer), Fill,
              Natural'Max (Formatting.Width - Sign.Length, 1),
              Formatting.Align);
      else
         return
           Align
             (To_String (Buffer), Fill, Formatting.Width, Formatting.Align);
      end if;
   end Format;

   --------------
   -- Format_E --
   --------------

   procedure Format_E
     (Buffer        : in out Buffer_Type;
      Number_Digits : in out Digits_List_Type;
      Exponent      : in out Integer;
      Formatting    :        Format_Type;
      Digit_Map     :        String;
      Locale        :        Locale_Type)
   is

      Rounding_Precision  : Natural := Default_Precision;
      Requested_Precision : Natural := Default_Precision;
      Overflow            : Boolean;

   begin
      if Formatting.Precision_Defined then
         Rounding_Precision :=
           Natural'Min (Formatting.Precision, Default_Precision);
         Requested_Precision := Formatting.Precision;
      end if;
      Round (Number_Digits, Exponent, Overflow, Rounding_Precision + 1);
      Emit_Digits
        (Buffer, Digit_Map, Number_Digits (1 .. Float_Type'Digits),
         Point_Position => 2, Length => Requested_Precision + 1,
         Digits_Start   => 1, Locale => Locale);
      Add (Buffer, Numeric_Item (Locale, Exponent_Character));
      Emit_Sign (Buffer, Exponent < 0, Plus, Locale);
      Accumulate (Buffer, abs Exponent, Locale, Width => 2);
   end Format_E;

   --------------
   -- Format_F --
   --------------
   --
   --  F formatting is simple floating point (no E+nn).  It is simple a
   --  long sequence of zeros with the available digits and a decimal point
   --  embedded.  The location of the decimal point in this string is located
   --  at max (exp + 1, 1).  The location of the start of the decimal digits
   --  is given by max (-exp + 1, 1).  The location of the last decimal
   --  digit is start plus number of digits - 1.
   --

   procedure Format_F
     (Buffer        : in out Buffer_Type;
      Number_Digits : in out Digits_List_Type;
      Exponent      : in out Integer;
      Formatting    :        Format_Type;
      Digit_Map     :        String;
      Locale        :        Locale_Type)
   is

      procedure Set_Format_F_Params
        (Exponent       :     Integer;
         Point_Position : out Positive;
         Digits_Start   : out Positive;
         Length         : out Positive);
      --  Helper routine to set the parameters needed for formatting.

      procedure Set_Format_F_Params
        (Exponent       :     Integer;
         Point_Position : out Positive;
         Digits_Start   : out Positive;
         Length         : out Positive)
      is
      begin
         Point_Position := Integer'Max (Exponent + 1, 1) + 1;
         Digits_Start   := Integer'Max (-Exponent + 1, 1);
         Length         :=
           Integer'Max
             (Point_Position, Digits_Start + Number_Digits'Length - 1);
         if Formatting.Precision_Defined then
            Length := Point_Position + Formatting.Precision - 1;
         end if;
      end Set_Format_F_Params;

      Rounding_Precision : Natural := Default_Precision;
      Point_Position     : Positive;
      Digits_Start       : Positive;
      Overflow           : Boolean;
      Length             : Positive;

   begin
      Set_Format_F_Params (Exponent, Point_Position, Digits_Start, Length);
      if Digits_Start <= Length then
         Rounding_Precision :=
           Integer'Min (Default_Precision, Length - Digits_Start);
      elsif Digits_Start - 1 = Length then
         --  Possible rounding might overflow to allow display
         Rounding_Precision := 1;
      end if;
      Round (Number_Digits, Exponent, Overflow, Rounding_Precision + 1);
      if Overflow then
         --  If the rounding causes and overflow, then the exponent has been
         --  incremented and the format parameters should be recalculated.
         Set_Format_F_Params (Exponent, Point_Position, Digits_Start, Length);
      end if;
      Emit_Digits
        (Buffer, Digit_Map, Number_Digits (1 .. Number_Digits'Length),
         Point_Position => Point_Position, Length => Length,
         Digits_Start   => Digits_Start, Locale => Locale);
   end Format_F;

   --------------
   -- Format_G --
   --------------

   procedure Format_G
     (Buffer        : in out Buffer_Type;
      Number_Digits : in out Digits_List_Type;
      Exponent      : in out Integer;
      Formatting    :        Format_Type;
      Digit_Map     :        String;
      Locale        :        Locale_Type)
   is
   begin
      if Exponent < 0 or else Exponent > Number_Digits'Length - 1 then
         Format_E
           (Buffer, Number_Digits, Exponent, Formatting, Digit_Map, Locale);
      else
         Format_F
           (Buffer, Number_Digits, Exponent, Formatting, Digit_Map, Locale);
      end if;
   end Format_G;

   ---------------------
   -- Format_Infinity --
   ---------------------

   procedure Format_Infinity
     (Buffer     : in out Buffer_Type;
      Formatting :        Format_Type;
      Negative   :        Boolean;
      Locale     :        Locale_Type)
   is
   begin
      Emit_Sign (Buffer, Negative, Formatting.Sign, Locale);
      Add (Buffer, Numeric_Item (Locale, Infinity_Character));
   end Format_Infinity;

   ----------------
   -- Format_NaN --
   ----------------

   procedure Format_NaN
     (Buffer : in out Buffer_Type;
      Locale :        Locale_Type)
   is
   begin
      Add (Buffer, Numeric_Item (Locale, Nan_Character));
   end Format_NaN;

   ------------------
   -- Format_Value --
   ------------------

   procedure Format_Value
     (Buffer     : in out Buffer_Type;
      Fill       :    out Unicode_Character;
      Sign       :        String;
      Formatting :        Format_Type;
      Value      :        Float_Type;
      Locale     :        Locale_Type)
   is

      Digit_Map : constant String :=
        Numeric_Item (Locale, Decimal_Digits_String);
      Default_Fill  : Unicode_Character := ' ';
      Number_Digits : Digits_List_Type (1 .. Number_Of_Digits);
      Exponent      : Integer;

   begin
      Decompose (abs Value, Number_Digits, Exponent);
      if Formatting.Align = Numeric then
         Default_Fill := Digit_Map (Digit_Map.First);
      else
         Add (Buffer, Sign);
      end if;
      if Formatting.Fill_Defined then
         Fill := Formatting.Fill;
      else
         Fill := Default_Fill;
      end if;
      case Formatting.Data is
         when 'f' | 'F' =>
            Format_F
              (Buffer, Number_Digits, Exponent, Formatting, Digit_Map, Locale);
         when 'g' | 'G' =>
            Format_G
              (Buffer, Number_Digits, Exponent, Formatting, Digit_Map, Locale);
         when others =>
            Format_E
              (Buffer, Number_Digits, Exponent, Formatting, Digit_Map, Locale);
      end case;
   end Format_Value;

   -----------------
   -- Is_Infinity --
   -----------------
   --
   --  A floating point value is a representation of Infinity if it's
   --  not valid.  This appears to work on x86, at least.
   --

   function Is_Infinity
     (Value : Float_Type)
      return Boolean
   is
   begin
      return not Value'Valid;
   end Is_Infinity;

   ---------------------
   -- Is_Not_A_Number --
   ---------------------
   --
   --  A floating point value is a NaN if it's not equal to itself.  All
   --  comparsions involving NaN's return False.
   --

   function Is_Not_A_Number
     (Value : Float_Type)
      return Boolean
   is
   begin
      return Value /= Value;
   end Is_Not_A_Number;

   -----------
   -- Round --
   -----------

   procedure Round
     (Buffer   : in out Digits_List_Type;
      Exponent : in out Integer;
      Overflow :    out Boolean;
      Position :        Positive)
   is

      Index     : Integer := Position;
      New_Value : Positive range 1 .. 10;

   begin
      Overflow := False;
      if Position < Buffer'Last then
         Overflow := Buffer (Position + 1) >= 5;
         while Overflow and then Index > 0 loop
            New_Value      := Buffer (Index) + 1;
            Overflow       := New_Value > 9;
            Buffer (Index) := New_Value mod 10;
            Index          := Index - 1;
         end loop;
      end if;
      if Overflow then
         --  OK, an overflow, increment the exponent, push the digits down
         --  and set the first digit to 1.
         Exponent := Exponent + 1;
         for I in reverse Buffer'First .. Buffer'Last - 1 loop
            Buffer (I + 1) := Buffer (I);
         end loop;
         Buffer (Buffer'First) := 1;
      end if;
      --  Zero out the digits after the requested precision
      for I in Position + 1 .. Buffer'Last loop
         Buffer (I) := 0;
      end loop;
   end Round;

   -----------------
   -- Sign_String --
   -----------------

   function Sign_String
     (Is_Negative : Boolean;
      Sign_Format : Sign_Type;
      Locale      : Locale_Type)
      return String
   is
   begin
      if Is_Negative then
         return Numeric_Item (Locale, Minus_Character);
      else
         --  Positive add '+' or ' ', depending on the Sign_Format
         case Sign_Format is
            when None | Minus =>
               return "";
            when Plus =>
               return Numeric_Item (Locale, Plus_Character);
            when Space =>
               return " ";
         end case;
      end if;
   end Sign_String;

end ZanyBlue.Text.Generic_Floats;
