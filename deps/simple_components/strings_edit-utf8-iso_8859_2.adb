--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.ISO_8859_2                Luebeck            --
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

package body Strings_Edit.UTF8.ISO_8859_2 is

   function Convert (Value : Character) return Code_Point is
   begin
      case Stream_Element (Character'Pos (Value)) is
         when 16#80#..16#9F# => return 16#80#; 
         when 16#00#..16#7F# => return Character'Pos (Value);
         when 16#A0# => return 16#00A0#;
         when 16#A1# => return 16#0104#;
         when 16#A2# => return 16#02D8#;
         when 16#A3# => return 16#0141#;
         when 16#A4# => return 16#00A4#;
         when 16#A5# => return 16#013D#;
         when 16#A6# => return 16#015A#;
         when 16#A7# => return 16#00A7#;
         when 16#A8# => return 16#00A8#;
         when 16#A9# => return 16#0160#;
         when 16#AA# => return 16#015E#;
         when 16#AB# => return 16#0164#;
         when 16#AC# => return 16#0179#;
         when 16#AD# => return 16#00AD#;
         when 16#AE# => return 16#017D#;
         when 16#AF# => return 16#017B#;
         when 16#B0# => return 16#00B0#;
         when 16#B1# => return 16#0105#;
         when 16#B2# => return 16#02DB#;
         when 16#B3# => return 16#0142#;
         when 16#B4# => return 16#00B4#;
         when 16#B5# => return 16#013E#;
         when 16#B6# => return 16#015B#;
         when 16#B7# => return 16#02C7#;
         when 16#B8# => return 16#00B8#;
         when 16#B9# => return 16#0161#;
         when 16#BA# => return 16#015F#;
         when 16#BB# => return 16#0165#;
         when 16#BC# => return 16#017A#;
         when 16#BD# => return 16#02DD#;
         when 16#BE# => return 16#017E#;
         when 16#BF# => return 16#017C#;
         when 16#C0# => return 16#0154#;
         when 16#C1# => return 16#00C1#;
         when 16#C2# => return 16#00C2#;
         when 16#C3# => return 16#0102#;
         when 16#C4# => return 16#00C4#;
         when 16#C5# => return 16#0139#;
         when 16#C6# => return 16#0106#;
         when 16#C7# => return 16#00C7#;
         when 16#C8# => return 16#010C#;
         when 16#C9# => return 16#00C9#;
         when 16#CA# => return 16#0118#;
         when 16#CB# => return 16#00CB#;
         when 16#CC# => return 16#011A#;
         when 16#CD# => return 16#00CD#;
         when 16#CE# => return 16#00CE#;
         when 16#CF# => return 16#010E#;
         when 16#D0# => return 16#0110#;
         when 16#D1# => return 16#0143#;
         when 16#D2# => return 16#0147#;
         when 16#D3# => return 16#00D3#;
         when 16#D4# => return 16#00D4#;
         when 16#D5# => return 16#0150#;
         when 16#D6# => return 16#00D6#;
         when 16#D7# => return 16#00D7#;
         when 16#D8# => return 16#0158#;
         when 16#D9# => return 16#016E#;
         when 16#DA# => return 16#00DA#;
         when 16#DB# => return 16#0170#;
         when 16#DC# => return 16#00DC#;
         when 16#DD# => return 16#00DD#;
         when 16#DE# => return 16#0162#;
         when 16#DF# => return 16#00DF#;
         when 16#E0# => return 16#0155#;
         when 16#E1# => return 16#00E1#;
         when 16#E2# => return 16#00E2#;
         when 16#E3# => return 16#0103#;
         when 16#E4# => return 16#00E4#;
         when 16#E5# => return 16#013A#;
         when 16#E6# => return 16#0107#;
         when 16#E7# => return 16#00E7#;
         when 16#E8# => return 16#010D#;
         when 16#E9# => return 16#00E9#;
         when 16#EA# => return 16#0119#;
         when 16#EB# => return 16#00EB#;
         when 16#EC# => return 16#011B#;
         when 16#ED# => return 16#00ED#;
         when 16#EE# => return 16#00EE#;
         when 16#EF# => return 16#010F#;
         when 16#F0# => return 16#0111#;
         when 16#F1# => return 16#0144#;
         when 16#F2# => return 16#0148#;
         when 16#F3# => return 16#00F3#;
         when 16#F4# => return 16#00F4#;
         when 16#F5# => return 16#0151#;
         when 16#F6# => return 16#00F6#;
         when 16#F7# => return 16#00F7#;
         when 16#F8# => return 16#0159#;
         when 16#F9# => return 16#016F#;
         when 16#FA# => return 16#00FA#;
         when 16#FB# => return 16#0171#;
         when 16#FC# => return 16#00FC#;
         when 16#FD# => return 16#00FD#;
         when 16#FE# => return 16#0163#;
         when 16#FF# => return 16#02D9#;
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
         when 16#00A0# => Value := Character'Val (16#A0#);
         when 16#0104# => Value := Character'Val (16#A1#);
         when 16#02D8# => Value := Character'Val (16#A2#);
         when 16#0141# => Value := Character'Val (16#A3#);
         when 16#00A4# => Value := Character'Val (16#A4#);
         when 16#013D# => Value := Character'Val (16#A5#);
         when 16#015A# => Value := Character'Val (16#A6#);
         when 16#00A7# => Value := Character'Val (16#A7#);
         when 16#00A8# => Value := Character'Val (16#A8#);
         when 16#0160# => Value := Character'Val (16#A9#);
         when 16#015E# => Value := Character'Val (16#AA#);
         when 16#0164# => Value := Character'Val (16#AB#);
         when 16#0179# => Value := Character'Val (16#AC#);
         when 16#00AD# => Value := Character'Val (16#AD#);
         when 16#017D# => Value := Character'Val (16#AE#);
         when 16#017B# => Value := Character'Val (16#AF#);
         when 16#00B0# => Value := Character'Val (16#B0#);
         when 16#0105# => Value := Character'Val (16#B1#);
         when 16#02DB# => Value := Character'Val (16#B2#);
         when 16#0142# => Value := Character'Val (16#B3#);
         when 16#00B4# => Value := Character'Val (16#B4#);
         when 16#013E# => Value := Character'Val (16#B5#);
         when 16#015B# => Value := Character'Val (16#B6#);
         when 16#02C7# => Value := Character'Val (16#B7#);
         when 16#00B8# => Value := Character'Val (16#B8#);
         when 16#0161# => Value := Character'Val (16#B9#);
         when 16#015F# => Value := Character'Val (16#BA#);
         when 16#0165# => Value := Character'Val (16#BB#);
         when 16#017A# => Value := Character'Val (16#BC#);
         when 16#02DD# => Value := Character'Val (16#BD#);
         when 16#017E# => Value := Character'Val (16#BE#);
         when 16#017C# => Value := Character'Val (16#BF#);
         when 16#0154# => Value := Character'Val (16#C0#);
         when 16#00C1# => Value := Character'Val (16#C1#);
         when 16#00C2# => Value := Character'Val (16#C2#);
         when 16#0102# => Value := Character'Val (16#C3#);
         when 16#00C4# => Value := Character'Val (16#C4#);
         when 16#0139# => Value := Character'Val (16#C5#);
         when 16#0106# => Value := Character'Val (16#C6#);
         when 16#00C7# => Value := Character'Val (16#C7#);
         when 16#010C# => Value := Character'Val (16#C8#);
         when 16#00C9# => Value := Character'Val (16#C9#);
         when 16#0118# => Value := Character'Val (16#CA#);
         when 16#00CB# => Value := Character'Val (16#CB#);
         when 16#011A# => Value := Character'Val (16#CC#);
         when 16#00CD# => Value := Character'Val (16#CD#);
         when 16#00CE# => Value := Character'Val (16#CE#);
         when 16#010E# => Value := Character'Val (16#CF#);
         when 16#0110# => Value := Character'Val (16#D0#);
         when 16#0143# => Value := Character'Val (16#D1#);
         when 16#0147# => Value := Character'Val (16#D2#);
         when 16#00D3# => Value := Character'Val (16#D3#);
         when 16#00D4# => Value := Character'Val (16#D4#);
         when 16#0150# => Value := Character'Val (16#D5#);
         when 16#00D6# => Value := Character'Val (16#D6#);
         when 16#00D7# => Value := Character'Val (16#D7#);
         when 16#0158# => Value := Character'Val (16#D8#);
         when 16#016E# => Value := Character'Val (16#D9#);
         when 16#00DA# => Value := Character'Val (16#DA#);
         when 16#0170# => Value := Character'Val (16#DB#);
         when 16#00DC# => Value := Character'Val (16#DC#);
         when 16#00DD# => Value := Character'Val (16#DD#);
         when 16#0162# => Value := Character'Val (16#DE#);
         when 16#00DF# => Value := Character'Val (16#DF#);
         when 16#0155# => Value := Character'Val (16#E0#);
         when 16#00E1# => Value := Character'Val (16#E1#);
         when 16#00E2# => Value := Character'Val (16#E2#);
         when 16#0103# => Value := Character'Val (16#E3#);
         when 16#00E4# => Value := Character'Val (16#E4#);
         when 16#013A# => Value := Character'Val (16#E5#);
         when 16#0107# => Value := Character'Val (16#E6#);
         when 16#00E7# => Value := Character'Val (16#E7#);
         when 16#010D# => Value := Character'Val (16#E8#);
         when 16#00E9# => Value := Character'Val (16#E9#);
         when 16#0119# => Value := Character'Val (16#EA#);
         when 16#00EB# => Value := Character'Val (16#EB#);
         when 16#011B# => Value := Character'Val (16#EC#);
         when 16#00ED# => Value := Character'Val (16#ED#);
         when 16#00EE# => Value := Character'Val (16#EE#);
         when 16#010F# => Value := Character'Val (16#EF#);
         when 16#0111# => Value := Character'Val (16#F0#);
         when 16#0144# => Value := Character'Val (16#F1#);
         when 16#0148# => Value := Character'Val (16#F2#);
         when 16#00F3# => Value := Character'Val (16#F3#);
         when 16#00F4# => Value := Character'Val (16#F4#);
         when 16#0151# => Value := Character'Val (16#F5#);
         when 16#00F6# => Value := Character'Val (16#F6#);
         when 16#00F7# => Value := Character'Val (16#F7#);
         when 16#0159# => Value := Character'Val (16#F8#);
         when 16#016F# => Value := Character'Val (16#F9#);
         when 16#00FA# => Value := Character'Val (16#FA#);
         when 16#0171# => Value := Character'Val (16#FB#);
         when 16#00FC# => Value := Character'Val (16#FC#);
         when 16#00FD# => Value := Character'Val (16#FD#);
         when 16#0163# => Value := Character'Val (16#FE#);
         when 16#02D9# => Value := Character'Val (16#FF#);
         when others   => Valid := False;
      end case;
   end Convert;

   function From_ISO_8859_2 (Value : Character) return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#80# then
         raise Constraint_Error;
      else
         return Result;
      end if;
   end From_ISO_8859_2;

   function From_ISO_8859_2
            (  Value      : Character;
               Substitute : Code_Point
            )  return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#80# then
         return Substitute;
      else
         return Result;
      end if;
   end From_ISO_8859_2;

   function From_ISO_8859_2 (Value : String) return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_ISO_8859_2 (Value));
      end loop;
      return Result (1..Pointer - 1);
   end From_ISO_8859_2;

   function From_ISO_8859_2
            (  Value      : String;
               Substitute : Code_Point
            )  return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_ISO_8859_2 (Value, Substitute));
      end loop;
      return Result (1..Pointer - 1);
   end From_ISO_8859_2;

   function From_ISO_8859_2 (Value : String) return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Result (Pointer) :=
            Wide_Character'Val (From_ISO_8859_2 (Value (Index)));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_ISO_8859_2;

   function From_ISO_8859_2
            (  Value      : String;
               Substitute : Wide_Character
            )  return Wide_String is
      Default : constant Code_Point := Wide_Character'Pos (Substitute);
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Result (Pointer) :=
            Wide_Character'Val
            (  From_ISO_8859_2 (Value (Index), Default)
            );
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_ISO_8859_2;

   function To_ISO_8859_2 (Value : Code_Point) return Character is
      Result : Character;
      Valid  : Boolean;
   begin
      Convert (Value, Result, Valid);
      if Valid then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end To_ISO_8859_2;

   function To_ISO_8859_2
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
   end To_ISO_8859_2;

   function To_ISO_8859_2 (Value : String) return String is
      Result  : String (1..Value'Length);
      Index   : Integer := 1;
      Code    : Code_Point;
      Pointer : Integer := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         Result (Index) := To_ISO_8859_2 (Code);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_ISO_8859_2;

   function To_ISO_8859_2
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
         Result (Index) := To_ISO_8859_2 (Code, Substitute);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_ISO_8859_2;

   function To_ISO_8859_2 (Value : Wide_String) return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_ISO_8859_2 (Wide_Character'Pos (Value (Pointer)));
         Index := Index + 1;
      end loop;
      return Result;
   end To_ISO_8859_2;

   function To_ISO_8859_2
            (  Value      : Wide_String;
               Substitute : Character
            )  return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_ISO_8859_2
            (  Wide_Character'Pos (Value (Pointer)),
               Substitute
            );
         Index := Index + 1;
      end loop;
      return Result;
   end To_ISO_8859_2;

end Strings_Edit.UTF8.ISO_8859_2;
