--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Windows_1256              Luebeck            --
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

with Ada.Streams;  use Ada.Streams;

package body Strings_Edit.UTF8.Windows_1256 is

   function Convert (Value : Character) return Code_Point is
   begin
      case Stream_Element (Character'Pos (Value)) is
         when 16#00#..16#7F# =>
            return Character'Pos (Value);
         when 16#80# => return 16#20AC#;
         when 16#81# => return 16#067E#;
         when 16#82# => return 16#201A#;
         when 16#83# => return 16#0192#;
         when 16#84# => return 16#201E#;
         when 16#85# => return 16#2026#;
         when 16#86# => return 16#2020#;
         when 16#87# => return 16#2021#;
         when 16#88# => return 16#02C6#;
         when 16#89# => return 16#2030#;
         when 16#8A# => return 16#0679#;
         when 16#8B# => return 16#2039#;
         when 16#8C# => return 16#0152#;
         when 16#8D# => return 16#0686#;
         when 16#8E# => return 16#0698#;
         when 16#8F# => return 16#0688#;
         when 16#90# => return 16#06AF#;
         when 16#91# => return 16#2018#;
         when 16#92# => return 16#2019#;
         when 16#93# => return 16#201C#;
         when 16#94# => return 16#201D#;
         when 16#95# => return 16#2022#;
         when 16#96# => return 16#2013#;
         when 16#97# => return 16#2014#;
         when 16#98# => return 16#06A9#;
         when 16#99# => return 16#2122#;
         when 16#9A# => return 16#0691#;
         when 16#9B# => return 16#203A#;
         when 16#9C# => return 16#0153#;
         when 16#9D# => return 16#200C#;
         when 16#9E# => return 16#200D#;
         when 16#9F# => return 16#06BA#;
         when 16#A0# => return 16#00A0#;
         when 16#A1# => return 16#060C#;
         when 16#A2# => return 16#00A2#;
         when 16#A3# => return 16#00A3#;
         when 16#A4# => return 16#00A4#;
         when 16#A5# => return 16#00A5#;
         when 16#A6# => return 16#00A6#;
         when 16#A7# => return 16#00A7#;
         when 16#A8# => return 16#00A8#;
         when 16#A9# => return 16#00A9#;
         when 16#AA# => return 16#06BE#;
         when 16#AB# => return 16#00AB#;
         when 16#AC# => return 16#00AC#;
         when 16#AD# => return 16#00AD#;
         when 16#AE# => return 16#00AE#;
         when 16#AF# => return 16#00AF#;
         when 16#B0# => return 16#00B0#;
         when 16#B1# => return 16#00B1#;
         when 16#B2# => return 16#00B2#;
         when 16#B3# => return 16#00B3#;
         when 16#B4# => return 16#00B4#;
         when 16#B5# => return 16#00B5#;
         when 16#B6# => return 16#00B6#;
         when 16#B7# => return 16#00B7#;
         when 16#B8# => return 16#00B8#;
         when 16#B9# => return 16#00B9#;
         when 16#BA# => return 16#061B#;
         when 16#BB# => return 16#00BB#;
         when 16#BC# => return 16#00BC#;
         when 16#BD# => return 16#00BD#;
         when 16#BE# => return 16#00BE#;
         when 16#BF# => return 16#061F#;
         when 16#C0# => return 16#06C1#;
         when 16#C1# => return 16#0621#;
         when 16#C2# => return 16#0622#;
         when 16#C3# => return 16#0623#;
         when 16#C4# => return 16#0624#;
         when 16#C5# => return 16#0625#;
         when 16#C6# => return 16#0626#;
         when 16#C7# => return 16#0627#;
         when 16#C8# => return 16#0628#;
         when 16#C9# => return 16#0629#;
         when 16#CA# => return 16#062A#;
         when 16#CB# => return 16#062B#;
         when 16#CC# => return 16#062C#;
         when 16#CD# => return 16#062D#;
         when 16#CE# => return 16#062E#;
         when 16#CF# => return 16#062F#;
         when 16#D0# => return 16#0630#;
         when 16#D1# => return 16#0631#;
         when 16#D2# => return 16#0632#;
         when 16#D3# => return 16#0633#;
         when 16#D4# => return 16#0634#;
         when 16#D5# => return 16#0635#;
         when 16#D6# => return 16#0636#;
         when 16#D7# => return 16#00D7#;
         when 16#D8# => return 16#0637#;
         when 16#D9# => return 16#0638#;
         when 16#DA# => return 16#0639#;
         when 16#DB# => return 16#063A#;
         when 16#DC# => return 16#0640#;
         when 16#DD# => return 16#0641#;
         when 16#DE# => return 16#0642#;
         when 16#DF# => return 16#0643#;
         when 16#E0# => return 16#00E0#;
         when 16#E1# => return 16#0644#;
         when 16#E2# => return 16#00E2#;
         when 16#E3# => return 16#0645#;
         when 16#E4# => return 16#0646#;
         when 16#E5# => return 16#0647#;
         when 16#E6# => return 16#0648#;
         when 16#E7# => return 16#00E7#;
         when 16#E8# => return 16#00E8#;
         when 16#E9# => return 16#00E9#;
         when 16#EA# => return 16#00EA#;
         when 16#EB# => return 16#00EB#;
         when 16#EC# => return 16#0649#;
         when 16#ED# => return 16#064A#;
         when 16#EE# => return 16#00EE#;
         when 16#EF# => return 16#00EF#;
         when 16#F0# => return 16#064B#;
         when 16#F1# => return 16#064C#;
         when 16#F2# => return 16#064D#;
         when 16#F3# => return 16#064E#;
         when 16#F4# => return 16#00F4#;
         when 16#F5# => return 16#064F#;
         when 16#F6# => return 16#0650#;
         when 16#F7# => return 16#00F7#;
         when 16#F8# => return 16#0651#;
         when 16#F9# => return 16#00F9#;
         when 16#FA# => return 16#0652#;
         when 16#FB# => return 16#00FB#;
         when 16#FC# => return 16#00FC#;
         when 16#FD# => return 16#200E#;
         when 16#FE# => return 16#200F#;
         when 16#FF# => return 16#06D2#;
      end case;
   end Convert;

   procedure Convert
             (  Code  : Code_Point;
                Value : out Character;
                Valid : out Boolean
             )  is
   begin
      Valid := True;
      case Code is
         when 16#00#..16#7F# => Value := Character'Val (Code);
         when 16#20AC# => Value := Character'Val (16#80#);
         when 16#067E# => Value := Character'Val (16#81#);
         when 16#201A# => Value := Character'Val (16#82#);
         when 16#0192# => Value := Character'Val (16#83#);
         when 16#201E# => Value := Character'Val (16#84#);
         when 16#2026# => Value := Character'Val (16#85#);
         when 16#2020# => Value := Character'Val (16#86#);
         when 16#2021# => Value := Character'Val (16#87#);
         when 16#02C6# => Value := Character'Val (16#88#);
         when 16#2030# => Value := Character'Val (16#89#);
         when 16#0679# => Value := Character'Val (16#8A#);
         when 16#2039# => Value := Character'Val (16#8B#);
         when 16#0152# => Value := Character'Val (16#8C#);
         when 16#0686# => Value := Character'Val (16#8D#);
         when 16#0698# => Value := Character'Val (16#8E#);
         when 16#0688# => Value := Character'Val (16#8F#);
         when 16#06AF# => Value := Character'Val (16#90#);
         when 16#2018# => Value := Character'Val (16#91#);
         when 16#2019# => Value := Character'Val (16#92#);
         when 16#201C# => Value := Character'Val (16#93#);
         when 16#201D# => Value := Character'Val (16#94#);
         when 16#2022# => Value := Character'Val (16#95#);
         when 16#2013# => Value := Character'Val (16#96#);
         when 16#2014# => Value := Character'Val (16#97#);
         when 16#06A9# => Value := Character'Val (16#98#);
         when 16#2122# => Value := Character'Val (16#99#);
         when 16#0691# => Value := Character'Val (16#9A#);
         when 16#203A# => Value := Character'Val (16#9B#);
         when 16#0153# => Value := Character'Val (16#9C#);
         when 16#200C# => Value := Character'Val (16#9D#);
         when 16#200D# => Value := Character'Val (16#9E#);
         when 16#06BA# => Value := Character'Val (16#9F#);
         when 16#00A0# => Value := Character'Val (16#A0#);
         when 16#060C# => Value := Character'Val (16#A1#);
         when 16#00A2# => Value := Character'Val (16#A2#);
         when 16#00A3# => Value := Character'Val (16#A3#);
         when 16#00A4# => Value := Character'Val (16#A4#);
         when 16#00A5# => Value := Character'Val (16#A5#);
         when 16#00A6# => Value := Character'Val (16#A6#);
         when 16#00A7# => Value := Character'Val (16#A7#);
         when 16#00A8# => Value := Character'Val (16#A8#);
         when 16#00A9# => Value := Character'Val (16#A9#);
         when 16#06BE# => Value := Character'Val (16#AA#);
         when 16#00AB# => Value := Character'Val (16#AB#);
         when 16#00AC# => Value := Character'Val (16#AC#);
         when 16#00AD# => Value := Character'Val (16#AD#);
         when 16#00AE# => Value := Character'Val (16#AE#);
         when 16#00AF# => Value := Character'Val (16#AF#);
         when 16#00B0# => Value := Character'Val (16#B0#);
         when 16#00B1# => Value := Character'Val (16#B1#);
         when 16#00B2# => Value := Character'Val (16#B2#);
         when 16#00B3# => Value := Character'Val (16#B3#);
         when 16#00B4# => Value := Character'Val (16#B4#);
         when 16#00B5# => Value := Character'Val (16#B5#);
         when 16#00B6# => Value := Character'Val (16#B6#);
         when 16#00B7# => Value := Character'Val (16#B7#);
         when 16#00B8# => Value := Character'Val (16#B8#);
         when 16#00B9# => Value := Character'Val (16#B9#);
         when 16#061B# => Value := Character'Val (16#BA#);
         when 16#00BB# => Value := Character'Val (16#BB#);
         when 16#00BC# => Value := Character'Val (16#BC#);
         when 16#00BD# => Value := Character'Val (16#BD#);
         when 16#00BE# => Value := Character'Val (16#BE#);
         when 16#061F# => Value := Character'Val (16#BF#);
         when 16#06C1# => Value := Character'Val (16#C0#);
         when 16#0621# => Value := Character'Val (16#C1#);
         when 16#0622# => Value := Character'Val (16#C2#);
         when 16#0623# => Value := Character'Val (16#C3#);
         when 16#0624# => Value := Character'Val (16#C4#);
         when 16#0625# => Value := Character'Val (16#C5#);
         when 16#0626# => Value := Character'Val (16#C6#);
         when 16#0627# => Value := Character'Val (16#C7#);
         when 16#0628# => Value := Character'Val (16#C8#);
         when 16#0629# => Value := Character'Val (16#C9#);
         when 16#062A# => Value := Character'Val (16#CA#);
         when 16#062B# => Value := Character'Val (16#CB#);
         when 16#062C# => Value := Character'Val (16#CC#);
         when 16#062D# => Value := Character'Val (16#CD#);
         when 16#062E# => Value := Character'Val (16#CE#);
         when 16#062F# => Value := Character'Val (16#CF#);
         when 16#0630# => Value := Character'Val (16#D0#);
         when 16#0631# => Value := Character'Val (16#D1#);
         when 16#0632# => Value := Character'Val (16#D2#);
         when 16#0633# => Value := Character'Val (16#D3#);
         when 16#0634# => Value := Character'Val (16#D4#);
         when 16#0635# => Value := Character'Val (16#D5#);
         when 16#0636# => Value := Character'Val (16#D6#);
         when 16#00D7# => Value := Character'Val (16#D7#);
         when 16#0637# => Value := Character'Val (16#D8#);
         when 16#0638# => Value := Character'Val (16#D9#);
         when 16#0639# => Value := Character'Val (16#DA#);
         when 16#063A# => Value := Character'Val (16#DB#);
         when 16#0640# => Value := Character'Val (16#DC#);
         when 16#0641# => Value := Character'Val (16#DD#);
         when 16#0642# => Value := Character'Val (16#DE#);
         when 16#0643# => Value := Character'Val (16#DF#);
         when 16#00E0# => Value := Character'Val (16#E0#);
         when 16#0644# => Value := Character'Val (16#E1#);
         when 16#00E2# => Value := Character'Val (16#E2#);
         when 16#0645# => Value := Character'Val (16#E3#);
         when 16#0646# => Value := Character'Val (16#E4#);
         when 16#0647# => Value := Character'Val (16#E5#);
         when 16#0648# => Value := Character'Val (16#E6#);
         when 16#00E7# => Value := Character'Val (16#E7#);
         when 16#00E8# => Value := Character'Val (16#E8#);
         when 16#00E9# => Value := Character'Val (16#E9#);
         when 16#00EA# => Value := Character'Val (16#EA#);
         when 16#00EB# => Value := Character'Val (16#EB#);
         when 16#0649# => Value := Character'Val (16#EC#);
         when 16#064A# => Value := Character'Val (16#ED#);
         when 16#00EE# => Value := Character'Val (16#EE#);
         when 16#00EF# => Value := Character'Val (16#EF#);
         when 16#064B# => Value := Character'Val (16#F0#);
         when 16#064C# => Value := Character'Val (16#F1#);
         when 16#064D# => Value := Character'Val (16#F2#);
         when 16#064E# => Value := Character'Val (16#F3#);
         when 16#00F4# => Value := Character'Val (16#F4#);
         when 16#064F# => Value := Character'Val (16#F5#);
         when 16#0650# => Value := Character'Val (16#F6#);
         when 16#00F7# => Value := Character'Val (16#F7#);
         when 16#0651# => Value := Character'Val (16#F8#);
         when 16#00F9# => Value := Character'Val (16#F9#);
         when 16#0652# => Value := Character'Val (16#FA#);
         when 16#00FB# => Value := Character'Val (16#FB#);
         when 16#00FC# => Value := Character'Val (16#FC#);
         when 16#200E# => Value := Character'Val (16#FD#);
         when 16#200F# => Value := Character'Val (16#FE#);
         when 16#06D2# => Value := Character'Val (16#FF#);
         when others   => Valid := False;
      end case;
   end Convert;

   function From_Windows_1256 (Value : Character) return Code_Point is
   begin
      return Convert (Value);
   end From_Windows_1256;

   function From_Windows_1256 (Value : String) return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_Windows_1256 (Value));
      end loop;
      return Result (1..Pointer - 1);
   end From_Windows_1256;

   function From_Windows_1256 (Value : String) return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Result (Pointer) :=
            Wide_Character'Val (From_Windows_1256 (Value (Index)));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_Windows_1256;

   function To_Windows_1256 (Value : Code_Point) return Character is
      Result : Character;
      Valid  : Boolean;
   begin
      Convert (Value, Result, Valid);
      if Valid then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end To_Windows_1256;

   function To_Windows_1256
            (  Value      : Code_Point;
               Substitute : Character
            )  return Character is
      Result : Character;
      Valid  : Boolean;
   begin
      Convert (Value, Result, Valid);
      if Valid then
         return Result;
      else
         return Substitute;
      end if;
   end To_Windows_1256;

   function To_Windows_1256 (Value : String) return String is
      Result  : String (1..Value'Length);
      Index   : Integer := 1;
      Code    : Code_Point;
      Pointer : Integer := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         Result (Index) := To_Windows_1256 (Code);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_Windows_1256;

   function To_Windows_1256
            (  Value      : String;
               Substitute : Character
            )  return String is
      Result  : String (1..Value'Length);
      Index   : Integer := 1;
      Code    : Code_Point;
      Pointer : Integer := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         Result (Index) := To_Windows_1256 (Code, Substitute);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_Windows_1256;

   function To_Windows_1256 (Value : Wide_String) return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_Windows_1256 (Wide_Character'Pos (Value (Pointer)));
         Index := Index + 1;
      end loop;
      return Result;
   end To_Windows_1256;

   function To_Windows_1256
            (  Value      : Wide_String;
               Substitute : Character
            )  return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_Windows_1256
            (  Wide_Character'Pos (Value (Pointer)),
               Substitute
            );
         Index := Index + 1;
      end loop;
      return Result;
   end To_Windows_1256;

end Strings_Edit.UTF8.Windows_1256;
