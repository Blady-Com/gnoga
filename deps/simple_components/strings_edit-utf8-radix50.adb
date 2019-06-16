--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.RADIX50                   Luebeck            --
--  Implementation                                 Autumn, 2018       --
--                                                                    --
--                                Last revision :  12:27 04 Nov 2018  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Strings_Edit.UTF8.RADIX50 is

   type Character_Count is mod 3;
   type RADIX50_Digit is mod 8#50#;
   type RADIX50_Code  is mod 8#50#**3;

   function Encode (Value : Character) return RADIX50_Code is
   begin
      case Value is
         when ' ' => return 8#00#;
         when 'A' => return 8#01#;
         when 'B' => return 8#02#;
         when 'C' => return 8#03#;
         when 'D' => return 8#04#;
         when 'E' => return 8#05#;
         when 'F' => return 8#06#;
         when 'G' => return 8#07#;
         when 'H' => return 8#10#;
         when 'I' => return 8#11#;
         when 'J' => return 8#12#;
         when 'K' => return 8#13#;
         when 'L' => return 8#14#;
         when 'M' => return 8#15#;
         when 'N' => return 8#16#;
         when 'O' => return 8#17#;
         when 'P' => return 8#20#;
         when 'Q' => return 8#21#;
         when 'R' => return 8#22#;
         when 'S' => return 8#23#;
         when 'T' => return 8#24#;
         when 'U' => return 8#25#;
         when 'V' => return 8#26#;
         when 'W' => return 8#27#;
         when 'X' => return 8#30#;
         when 'Y' => return 8#31#;
         when 'Z' => return 8#32#;
         when '$' => return 8#33#;
         when '.' => return 8#34#;
         when '%' => return 8#35#;
         when '0' => return 8#36#;
         when '1' => return 8#37#;
         when '2' => return 8#40#;
         when '3' => return 8#41#;
         when '4' => return 8#42#;
         when '5' => return 8#43#;
         when '6' => return 8#44#;
         when '7' => return 8#45#;
         when '8' => return 8#46#;
         when '9' => return 8#47#;
         when others =>
            return 8#50#;
      end case;
   end Encode;

   function Decode (Code : RADIX50_Digit) return Character is
   begin
      case Code is
         when 8#00# => return ' ';
         when 8#01# => return 'A';
         when 8#02# => return 'B';
         when 8#03# => return 'C';
         when 8#04# => return 'D';
         when 8#05# => return 'E';
         when 8#06# => return 'F';
         when 8#07# => return 'G';
         when 8#10# => return 'H';
         when 8#11# => return 'I';
         when 8#12# => return 'J';
         when 8#13# => return 'K';
         when 8#14# => return 'L';
         when 8#15# => return 'M';
         when 8#16# => return 'N';
         when 8#17# => return 'O';
         when 8#20# => return 'P';
         when 8#21# => return 'Q';
         when 8#22# => return 'R';
         when 8#23# => return 'S';
         when 8#24# => return 'T';
         when 8#25# => return 'U';
         when 8#26# => return 'V';
         when 8#27# => return 'W';
         when 8#30# => return 'X';
         when 8#31# => return 'Y';
         when 8#32# => return 'Z';
         when 8#33# => return '$';
         when 8#34# => return '.';
         when 8#35# => return '%';
         when 8#36# => return '0';
         when 8#37# => return '1';
         when 8#40# => return '2';
         when 8#41# => return '3';
         when 8#42# => return '4';
         when 8#43# => return '5';
         when 8#44# => return '6';
         when 8#45# => return '7';
         when 8#46# => return '8';
         when 8#47# => return '9';
      end case;
   end Decode;

   function From_RADIX50 (Value : Wide_String) return String is
      Result  : String (1..Value'Length * 3);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         if Wide_Character'Pos (Value (Index)) >= 40**3 then
            raise Data_Error;
         end if;
         declare
            Code : constant RADIX50_Code :=
                            Wide_Character'Pos (Value (Index));
         begin
            Result (Pointer) :=
               Decode (RADIX50_Digit (Code / 40**2));
            Result (Pointer + 1) :=
               Decode (RADIX50_Digit ((Code / 40) mod 40));
            Result (Pointer + 2) :=
               Decode (RADIX50_Digit (Code mod 40));
            Pointer := Pointer + 3;
         end;
      end loop;
      return Result;
   end From_RADIX50;

   function To_RADIX50 (Value : String) return Wide_String is
      Result  : Wide_String (1..(Value'Length + 2) / 3);
      Pointer : Integer         := Value'First;
      Count   : Character_Count := 0;
      Index   : Integer         := 1;
      Accum   : RADIX50_Code;
      Code    : Code_Point;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         if Code > 255 then
            raise Constraint_Error;
         end if;
         declare
            Digit : constant RADIX50_Code :=
                             Encode (Character'Val (Code));
         begin
            if Digit >= 8#50# then
               raise Constraint_Error;
            end if;
            case Count is
               when 0 =>
                  Accum := Digit;
                  Count := 1;
               when 1 =>
                  Accum := Accum * 8#50# + Digit;
                  Count := 2;
               when 2 =>
                  Result (Index) :=
                     Wide_Character'Val (Accum * 8#50# + Digit);
                  Index := Index + 1;
                  Count := 0;
            end case;
         end;
      end loop;
      case Count is
         when 0 =>
            null;
         when 1 =>
            Result (Index) := Wide_Character'Val (Accum * 8#50#**2);
            Index := Index + 1;
         when 2 =>
            Result (Index) := Wide_Character'Val (Accum * 8#50#);
            Index := Index + 1;
      end case;      
      return Result (1..Index - 1);
   end To_RADIX50;

   function To_RADIX50
            (  Value      : String;
               Substitute : Character
            )  return Wide_String is
      Result  : Wide_String (1..(Value'Length + 2) / 3);
      Default : RADIX50_Code;
      Digit   : RADIX50_Code;
      Pointer : Integer         := Value'First;
      Count   : Character_Count := 0;
      Index   : Integer         := 1;
      Accum   : RADIX50_Code;
      Code    : Code_Point;
   begin
      Default := Encode (Substitute);
      if Default >= 8#50# then
         raise Use_Error;
      end if;
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         if Code > 255 then
            Digit := Default;
         else
            Digit := Encode (Character'Val (Code));
            if Digit >= 8#50# then
               Digit := Default;
            end if;
         end if;
         case Count is
            when 0 =>
               Accum := Digit;
               Count := 1;
            when 1 =>
               Accum := Accum * 8#50# + Digit;
               Count := 2;
            when 2 =>
               Result (Index) :=
                  Wide_Character'Val (Accum * 8#50# + Digit);
               Index := Index + 1;
               Count := 0;
         end case;
      end loop;
      case Count is
         when 0 =>
            null;
         when 1 =>
            Result (Index) := Wide_Character'Val (Accum * 8#50#**2);
            Index := Index + 1;
         when 2 =>
            Result (Index) := Wide_Character'Val (Accum * 8#50#);
            Index := Index + 1;
      end case;      
      return Result (1..Index - 1);
   end To_RADIX50;

end Strings_Edit.UTF8.RADIX50;
