--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
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

--
--  Format string handling for ZanyBlue.  The format syntax is based on the
--  syntax defined for Python format strings.  The Python description of this
--  syntax is
--
--   "Format specifications" are used within replacement fields
--   contained within a format string to define how individual values
--   are presented (see Format String Syntax.) They can also be passed
--   directly to the builtin format() function. Each formattable type
--   may define how the format specification is to be interpreted.
--
--   Most built-in types implement the following options for format
--   specifications, although some of the formatting options are only
--   supported by the numeric types.
--
--   A general convention is that an empty format string ("") produces
--   the same result as if you had called str() on the value.
--
--   The general form of a standard format specifier is:
--
--   format_spec ::=  [[fill]align][sign][#][0][width][.precision][type][*]
--   fill        ::=  <a character other than '}'>
--   align       ::=  "<" | ">" | "=" | "^"
--   sign        ::=  "+" | "-" | " "
--   width       ::=  integer
--   precision   ::=  integer
--   type        ::=  "b" | "c" | "d" | "e" | "E" | "f" | "F"
--                  | "g" | "G" | "n" | "o" | "x" | "X" | "%"
--
--   The fill character can be any character other than '}' (which
--   signifies the end of the field). The presence of a fill character is
--   signaled by the next character, which must be one of the alignment
--   options. If the second character of format_spec is not a valid
--   alignment option, then it is assumed that both the fill character
--   and the alignment option are absent.
--
--   The meaning of the various alignment options is as follows:
--
--   Option     Meaning
--   '<'        Forces the field to be left-aligned within the available
--              space (This is the default.)
--   '>'        Forces the field to be right-aligned within the available
--              space.
--   '='        Forces the padding to be placed after the sign (if any)
--              but before the digits. This is used for printing fields
--              in the form '+000000120'. This alignment option is
--              only valid for numeric types.
--   '^'        Forces the field to be centered within the available
--              space.
--
--   Note that unless a minimum field width is defined, the field width
--   will always be the same size as the data to fill it, so that the
--   alignment option has no meaning in this case.
--
--   The sign option is only valid for number types, and can be one of
--   the following:
--
--   Option     Meaning
--   '+'        indicates that a sign should be used for both positive
--              as well as negative numbers.
--   '-'        indicates that a sign should be used only for negative
--              numbers (this is the default behavior).
--   space      indicates that a leading space should be used on positive
--              numbers, and a minus sign on negative numbers.
--
--   The '#' option is only valid for integers, and only for binary,
--   octal, or hexadecimal output. If present, it specifies that the
--   output will be prefixed by '0b', '0o', or '0x', respectively.
--
--   width is a decimal integer defining the minimum field width. If not
--   specified, then the field width will be determined by the content.
--
--   If the width field is preceded by a zero ('0') character, this
--   enables zero-padding. This is equivalent to an alignment type of
--   '=' and a fill character of '0'.
--
--   The precision is a decimal number indicating how many digits
--   should be displayed after the decimal point for a floating point
--   value formatted with 'f' and 'F', or before and after the decimal
--   point for a floating point value formatted with 'g' or 'G'. For
--   non-number types the field indicates the maximum field size -
--   in other words, how many characters will be used from the field
--   content. The precision is not allowed for integer values.
--
--   Finally, the type determines how the data should be presented.
--
--   The available integer presentation types are:
--
--   Type       Meaning
--   'b'        Binary format. Outputs the number in base 2.
--   'c'        Character. Converts the integer to the corresponding
--              unicode character before printing.
--   'd'        Decimal Integer. Outputs the number in base 10.
--   'o'        Octal format. Outputs the number in base 8.
--   'x'        Hex format. Outputs the number in base 16, using lower-
--              case letters for the digits above 9.
--   'X'        Hex format. Outputs the number in base 16, using upper-
--              case letters for the digits above 9.
--   'n'        Number. This is the same as 'd', except that it uses the
--              current locale setting to insert the appropriate number
--              separator characters.
--   None       The same as 'd'.
--
--   The available presentation types for floating point and decimal
--   values are:
--
--   Type       Meaning
--   'e'        Exponent notation. Prints the number in scientific
--              notation using the letter ‘e’ to indicate the
--              exponent.
--   'E'        Exponent notation. Same as 'e' except it uses an upper
--              case 'E' as the separator character.
--   'f'        Fixed point. Displays the number as a fixed-point number.
--   'F'        Fixed point. Same as 'f'.
--   'g'        General format. This prints the number as a fixed-point
--              number, unless the number is too large, in which case
--              it switches to 'e' exponent notation. Infinity and NaN
--              values are formatted as inf, -inf and nan, respectively.
--   'G'        General format. Same as 'g' except switches to 'E'
--              if the number gets to large. The representations of
--              infinity and NaN are uppercased, too.
--   'n'        Number. This is the same as 'g', except that it uses the
--              current locale setting to insert the appropriate number
--              separator characters.
--   '%'        Percentage. Multiplies the number by 100 and displays
--              in fixed ('f') format, followed by a percent sign. (Not
--              implemented).
--   None       The same as 'g'.
--

with ZanyBlue.Text.Locales;

package ZanyBlue.Text.Format_Parser is

   use ZanyBlue.Text.Locales;

   type Align_Type is (None, Left, Right, Numeric, Center);
   type Sign_Type is (None, Plus, Minus, Space);
   type Data_Type is (None, 'a', 'b', 'c', 'd', 'e', 'E', 'f', 'F',
                      'g', 'G', 'n', 'o', 'x', 'X', '%');
   type Format_Type is
      record
         Fill_Defined      : Boolean := False;
         Fill              : Wide_Character := ' ';
         Align             : Align_Type := None;
         Sign              : Sign_Type := None;
         Include_Base      : Boolean := False;
         Width             : Natural := 0;
         Precision_Defined : Boolean := False;
         Precision         : Natural := 0;
         Data              : Data_Type := None;
      end record;

   Invalid_Format       : exception;
   Internal_Error       : exception;
   Field_Too_Wide_Error : exception;

   function Align (Value     : Wide_String;
                   Fill      : Wide_Character;
                   Width     : Natural;
                   Alignment : Align_Type;
                   Prefix    : Wide_String := "") return Wide_String;
   --  Format a string value within a field width with the given
   --  alignment, e.g., center "XYZ" in 20 spaces padding with spaces.
   --  The Prefix argument is used for numeric formatting allowing the
   --  the padding to occur after the sign, e.g., "+*******20" to format
   --  20 in width of 10, numeric justified.

   function Parse (Format            : Wide_String;
                   Locale            : Locale_Type) return Format_Type;
   --  Parse a format string to the individual field values.

   function To_String (Format : Format_Type) return Wide_String;
   --  Generate a string representation for formatting information.
   --  Used for debugging purposes.

   function Maximum_Field_Width return Positive;
   --  Return the current maximum allowed field value (limit imposed to
   --  to prevent possible overflows (malicious localizations).

   procedure Maximum_Field_Width (Value : Positive);
   --  Set the maximum allowed field value.

end ZanyBlue.Text.Format_Parser;
