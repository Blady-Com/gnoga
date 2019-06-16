--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.ISO_8859_6                Luebeck            --
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

package body Strings_Edit.UTF8.ISO_8859_6 is

   function Convert (Value : Character) return Code_Point is
   begin
      case Stream_Element (Character'Pos (Value)) is
         when 16#80#..16#9F# |
              16#A1#..16#A3# |
              16#A5#..16#AB# |
              16#AE#..16#BA# |
              16#BC#..16#BE# |
              16#C0#         | 
              16#DB#..16#DF# |
              16#F3#..16#FF# => return 16#80#; 
         when 16#00#..16#7F# => return Character'Pos (Value);
         when 16#A0# => return 16#00A0#;
         when 16#A4# => return 16#00A4#;
         when 16#AC# => return 16#060C#;
         when 16#AD# => return 16#00AD#;
         when 16#BB# => return 16#061B#;
         when 16#BF# => return 16#061F#;
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
         when 16#D7# => return 16#0637#;
         when 16#D8# => return 16#0638#;
         when 16#D9# => return 16#0639#;
         when 16#DA# => return 16#063A#;
         when 16#E0# => return 16#0640#;
         when 16#E1# => return 16#0641#;
         when 16#E2# => return 16#0642#;
         when 16#E3# => return 16#0643#;
         when 16#E4# => return 16#0644#;
         when 16#E5# => return 16#0645#;
         when 16#E6# => return 16#0646#;
         when 16#E7# => return 16#0647#;
         when 16#E8# => return 16#0648#;
         when 16#E9# => return 16#0649#;
         when 16#EA# => return 16#064A#;
         when 16#EB# => return 16#064B#;
         when 16#EC# => return 16#064C#;
         when 16#ED# => return 16#064D#;
         when 16#EE# => return 16#064E#;
         when 16#EF# => return 16#064F#;
         when 16#F0# => return 16#0650#;
         when 16#F1# => return 16#0651#;
         when 16#F2# => return 16#0652#;
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
         when 16#00A4# => Value := Character'Val (16#A4#);
         when 16#060C# => Value := Character'Val (16#AC#);
         when 16#00AD# => Value := Character'Val (16#AD#);
         when 16#061B# => Value := Character'Val (16#BB#);
         when 16#061F# => Value := Character'Val (16#BF#);
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
         when 16#0637# => Value := Character'Val (16#D7#);
         when 16#0638# => Value := Character'Val (16#D8#);
         when 16#0639# => Value := Character'Val (16#D9#);
         when 16#063A# => Value := Character'Val (16#DA#);
         when 16#0640# => Value := Character'Val (16#E0#);
         when 16#0641# => Value := Character'Val (16#E1#);
         when 16#0642# => Value := Character'Val (16#E2#);
         when 16#0643# => Value := Character'Val (16#E3#);
         when 16#0644# => Value := Character'Val (16#E4#);
         when 16#0645# => Value := Character'Val (16#E5#);
         when 16#0646# => Value := Character'Val (16#E6#);
         when 16#0647# => Value := Character'Val (16#E7#);
         when 16#0648# => Value := Character'Val (16#E8#);
         when 16#0649# => Value := Character'Val (16#E9#);
         when 16#064A# => Value := Character'Val (16#EA#);
         when 16#064B# => Value := Character'Val (16#EB#);
         when 16#064C# => Value := Character'Val (16#EC#);
         when 16#064D# => Value := Character'Val (16#ED#);
         when 16#064E# => Value := Character'Val (16#EE#);
         when 16#064F# => Value := Character'Val (16#EF#);
         when 16#0650# => Value := Character'Val (16#F0#);
         when 16#0651# => Value := Character'Val (16#F1#);
         when 16#0652# => Value := Character'Val (16#F2#);
         when others   => Valid := False;
      end case;
   end Convert;

   function From_ISO_8859_6 (Value : Character) return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#80# then
         raise Constraint_Error;
      else
         return Result;
      end if;
   end From_ISO_8859_6;

   function From_ISO_8859_6
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
   end From_ISO_8859_6;

   function From_ISO_8859_6 (Value : String) return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_ISO_8859_6 (Value));
      end loop;
      return Result (1..Pointer - 1);
   end From_ISO_8859_6;

   function From_ISO_8859_6
            (  Value      : String;
               Substitute : Code_Point
            )  return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_ISO_8859_6 (Value, Substitute));
      end loop;
      return Result (1..Pointer - 1);
   end From_ISO_8859_6;

   function From_ISO_8859_6 (Value : String) return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Result (Pointer) :=
            Wide_Character'Val (From_ISO_8859_6 (Value (Index)));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_ISO_8859_6;

   function From_ISO_8859_6
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
            (  From_ISO_8859_6 (Value (Index), Default)
            );
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_ISO_8859_6;

   function To_ISO_8859_6 (Value : Code_Point) return Character is
      Result : Character;
      Valid  : Boolean;
   begin
      Convert (Value, Result, Valid);
      if Valid then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end To_ISO_8859_6;

   function To_ISO_8859_6
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
   end To_ISO_8859_6;

   function To_ISO_8859_6 (Value : String) return String is
      Result  : String (1..Value'Length);
      Index   : Integer := 1;
      Code    : Code_Point;
      Pointer : Integer := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         Result (Index) := To_ISO_8859_6 (Code);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_ISO_8859_6;

   function To_ISO_8859_6
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
         Result (Index) := To_ISO_8859_6 (Code, Substitute);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_ISO_8859_6;

   function To_ISO_8859_6 (Value : Wide_String) return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_ISO_8859_6 (Wide_Character'Pos (Value (Pointer)));
         Index := Index + 1;
      end loop;
      return Result;
   end To_ISO_8859_6;

   function To_ISO_8859_6
            (  Value      : Wide_String;
               Substitute : Character
            )  return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_ISO_8859_6
            (  Wide_Character'Pos (Value (Pointer)),
               Substitute
            );
         Index := Index + 1;
      end loop;
      return Result;
   end To_ISO_8859_6;

end Strings_Edit.UTF8.ISO_8859_6;
