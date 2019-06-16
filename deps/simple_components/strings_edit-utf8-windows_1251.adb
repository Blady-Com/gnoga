--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Windows_1251              Luebeck            --
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

package body Strings_Edit.UTF8.Windows_1251 is

   function Convert (Value : Character) return Code_Point is
   begin
      case Stream_Element (Character'Pos (Value)) is
         when 16#80# => return 16#0402#; 
         when 16#81# => return 16#0403#; 
         when 16#82# => return 16#201A#; 
         when 16#83# => return 16#0453#; 
         when 16#84# => return 16#201E#; 
         when 16#85# => return 16#2026#; 
         when 16#86# => return 16#2020#; 
         when 16#87# => return 16#2021#; 
         when 16#88# => return 16#20AC#; 
         when 16#89# => return 16#2030#; 
         when 16#8A# => return 16#0409#; 
         when 16#8B# => return 16#2039#; 
         when 16#8C# => return 16#040A#; 
         when 16#8D# => return 16#040C#; 
         when 16#8E# => return 16#040B#; 
         when 16#8F# => return 16#040F#; 
         when 16#90# => return 16#0452#; 
         when 16#91# => return 16#2018#; 
         when 16#92# => return 16#2019#; 
         when 16#93# => return 16#201C#; 
         when 16#94# => return 16#201D#; 
         when 16#95# => return 16#2022#; 
         when 16#96# => return 16#2013#; 
         when 16#97# => return 16#2014#; 
         when 16#99# => return 16#2122#; 
         when 16#9A# => return 16#0459#; 
         when 16#9B# => return 16#203A#; 
         when 16#9C# => return 16#045A#; 
         when 16#9D# => return 16#045C#; 
         when 16#9E# => return 16#045B#; 
         when 16#9F# => return 16#045F#; 
         when 16#A0# => return 16#00A0#; 
         when 16#A1# => return 16#040E#; 
         when 16#A2# => return 16#045E#; 
         when 16#A3# => return 16#0408#; 
         when 16#A4# => return 16#00A4#; 
         when 16#A5# => return 16#0490#; 
         when 16#A6# => return 16#00A6#; 
         when 16#A7# => return 16#00A7#; 
         when 16#A8# => return 16#0401#; 
         when 16#A9# => return 16#00A9#; 
         when 16#AA# => return 16#0404#; 
         when 16#AB# => return 16#00AB#; 
         when 16#AC# => return 16#00AC#; 
         when 16#AD# => return 16#00AD#; 
         when 16#AE# => return 16#00AE#; 
         when 16#AF# => return 16#0407#; 
         when 16#B0# => return 16#00B0#; 
         when 16#B1# => return 16#00B1#; 
         when 16#B2# => return 16#0406#; 
         when 16#B3# => return 16#0456#; 
         when 16#B4# => return 16#0491#; 
         when 16#B5# => return 16#00B5#; 
         when 16#B6# => return 16#00B6#; 
         when 16#B7# => return 16#00B7#; 
         when 16#B8# => return 16#0451#; 
         when 16#B9# => return 16#2116#; 
         when 16#BA# => return 16#0454#; 
         when 16#BB# => return 16#00BB#; 
         when 16#BC# => return 16#0458#; 
         when 16#BD# => return 16#0405#; 
         when 16#BE# => return 16#0455#; 
         when 16#BF# => return 16#0457#; 
         when 16#C0# => return 16#0410#; 
         when 16#C1# => return 16#0411#; 
         when 16#C2# => return 16#0412#; 
         when 16#C3# => return 16#0413#; 
         when 16#C4# => return 16#0414#; 
         when 16#C5# => return 16#0415#; 
         when 16#C6# => return 16#0416#; 
         when 16#C7# => return 16#0417#; 
         when 16#C8# => return 16#0418#; 
         when 16#C9# => return 16#0419#; 
         when 16#CA# => return 16#041A#; 
         when 16#CB# => return 16#041B#; 
         when 16#CC# => return 16#041C#; 
         when 16#CD# => return 16#041D#; 
         when 16#CE# => return 16#041E#; 
         when 16#CF# => return 16#041F#; 
         when 16#D0# => return 16#0420#; 
         when 16#D1# => return 16#0421#; 
         when 16#D2# => return 16#0422#; 
         when 16#D3# => return 16#0423#; 
         when 16#D4# => return 16#0424#; 
         when 16#D5# => return 16#0425#; 
         when 16#D6# => return 16#0426#; 
         when 16#D7# => return 16#0427#; 
         when 16#D8# => return 16#0428#; 
         when 16#D9# => return 16#0429#; 
         when 16#DA# => return 16#042A#; 
         when 16#DB# => return 16#042B#; 
         when 16#DC# => return 16#042C#; 
         when 16#DD# => return 16#042D#; 
         when 16#DE# => return 16#042E#; 
         when 16#DF# => return 16#042F#; 
         when 16#E0# => return 16#0430#; 
         when 16#E1# => return 16#0431#; 
         when 16#E2# => return 16#0432#; 
         when 16#E3# => return 16#0433#; 
         when 16#E4# => return 16#0434#; 
         when 16#E5# => return 16#0435#; 
         when 16#E6# => return 16#0436#; 
         when 16#E7# => return 16#0437#; 
         when 16#E8# => return 16#0438#; 
         when 16#E9# => return 16#0439#; 
         when 16#EA# => return 16#043A#; 
         when 16#EB# => return 16#043B#; 
         when 16#EC# => return 16#043C#; 
         when 16#ED# => return 16#043D#; 
         when 16#EE# => return 16#043E#; 
         when 16#EF# => return 16#043F#; 
         when 16#F0# => return 16#0440#; 
         when 16#F1# => return 16#0441#; 
         when 16#F2# => return 16#0442#; 
         when 16#F3# => return 16#0443#; 
         when 16#F4# => return 16#0444#; 
         when 16#F5# => return 16#0445#; 
         when 16#F6# => return 16#0446#; 
         when 16#F7# => return 16#0447#; 
         when 16#F8# => return 16#0448#; 
         when 16#F9# => return 16#0449#; 
         when 16#FA# => return 16#044A#; 
         when 16#FB# => return 16#044B#; 
         when 16#FC# => return 16#044C#; 
         when 16#FD# => return 16#044D#; 
         when 16#FE# => return 16#044E#; 
         when 16#FF# => return 16#044F#;
         when 0..127 => return Character'Pos (Value);
         when 152    => return 152; -- Unused
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
         when 16#0402# => Value := Character'Val (16#80#);
         when 16#0403# => Value := Character'Val (16#81#); 
         when 16#201A# => Value := Character'Val (16#82#); 
         when 16#0453# => Value := Character'Val (16#83#); 
         when 16#201E# => Value := Character'Val (16#84#); 
         when 16#2026# => Value := Character'Val (16#85#); 
         when 16#2020# => Value := Character'Val (16#86#); 
         when 16#2021# => Value := Character'Val (16#87#); 
         when 16#20AC# => Value := Character'Val (16#88#); 
         when 16#2030# => Value := Character'Val (16#89#); 
         when 16#0409# => Value := Character'Val (16#8A#); 
         when 16#2039# => Value := Character'Val (16#8B#); 
         when 16#040A# => Value := Character'Val (16#8C#); 
         when 16#040C# => Value := Character'Val (16#8D#); 
         when 16#040B# => Value := Character'Val (16#8E#); 
         when 16#040F# => Value := Character'Val (16#8F#); 
         when 16#0452# => Value := Character'Val (16#90#); 
         when 16#2018# => Value := Character'Val (16#91#); 
         when 16#2019# => Value := Character'Val (16#92#); 
         when 16#201C# => Value := Character'Val (16#93#); 
         when 16#201D# => Value := Character'Val (16#94#); 
         when 16#2022# => Value := Character'Val (16#95#); 
         when 16#2013# => Value := Character'Val (16#96#); 
         when 16#2014# => Value := Character'Val (16#97#); 
         when 16#2122# => Value := Character'Val (16#99#); 
         when 16#0459# => Value := Character'Val (16#9A#); 
         when 16#203A# => Value := Character'Val (16#9B#); 
         when 16#045A# => Value := Character'Val (16#9C#); 
         when 16#045C# => Value := Character'Val (16#9D#); 
         when 16#045B# => Value := Character'Val (16#9E#); 
         when 16#045F# => Value := Character'Val (16#9F#); 
         when 16#00A0# => Value := Character'Val (16#A0#); 
         when 16#040E# => Value := Character'Val (16#A1#); 
         when 16#045E# => Value := Character'Val (16#A2#); 
         when 16#0408# => Value := Character'Val (16#A3#); 
         when 16#00A4# => Value := Character'Val (16#A4#); 
         when 16#0490# => Value := Character'Val (16#A5#); 
         when 16#00A6# => Value := Character'Val (16#A6#); 
         when 16#00A7# => Value := Character'Val (16#A7#); 
         when 16#0401# => Value := Character'Val (16#A8#); 
         when 16#00A9# => Value := Character'Val (16#A9#); 
         when 16#0404# => Value := Character'Val (16#AA#); 
         when 16#00AB# => Value := Character'Val (16#AB#); 
         when 16#00AC# => Value := Character'Val (16#AC#); 
         when 16#00AD# => Value := Character'Val (16#AD#); 
         when 16#00AE# => Value := Character'Val (16#AE#); 
         when 16#0407# => Value := Character'Val (16#AF#); 
         when 16#00B0# => Value := Character'Val (16#B0#); 
         when 16#00B1# => Value := Character'Val (16#B1#); 
         when 16#0406# => Value := Character'Val (16#B2#); 
         when 16#0456# => Value := Character'Val (16#B3#); 
         when 16#0491# => Value := Character'Val (16#B4#); 
         when 16#00B5# => Value := Character'Val (16#B5#); 
         when 16#00B6# => Value := Character'Val (16#B6#); 
         when 16#00B7# => Value := Character'Val (16#B7#); 
         when 16#0451# => Value := Character'Val (16#B8#); 
         when 16#2116# => Value := Character'Val (16#B9#); 
         when 16#0454# => Value := Character'Val (16#BA#); 
         when 16#00BB# => Value := Character'Val (16#BB#); 
         when 16#0458# => Value := Character'Val (16#BC#); 
         when 16#0405# => Value := Character'Val (16#BD#); 
         when 16#0455# => Value := Character'Val (16#BE#); 
         when 16#0457# => Value := Character'Val (16#BF#); 
         when 16#0410# => Value := Character'Val (16#C0#); 
         when 16#0411# => Value := Character'Val (16#C1#); 
         when 16#0412# => Value := Character'Val (16#C2#); 
         when 16#0413# => Value := Character'Val (16#C3#); 
         when 16#0414# => Value := Character'Val (16#C4#); 
         when 16#0415# => Value := Character'Val (16#C5#); 
         when 16#0416# => Value := Character'Val (16#C6#); 
         when 16#0417# => Value := Character'Val (16#C7#); 
         when 16#0418# => Value := Character'Val (16#C8#); 
         when 16#0419# => Value := Character'Val (16#C9#); 
         when 16#041A# => Value := Character'Val (16#CA#); 
         when 16#041B# => Value := Character'Val (16#CB#); 
         when 16#041C# => Value := Character'Val (16#CC#); 
         when 16#041D# => Value := Character'Val (16#CD#); 
         when 16#041E# => Value := Character'Val (16#CE#); 
         when 16#041F# => Value := Character'Val (16#CF#); 
         when 16#0420# => Value := Character'Val (16#D0#); 
         when 16#0421# => Value := Character'Val (16#D1#); 
         when 16#0422# => Value := Character'Val (16#D2#); 
         when 16#0423# => Value := Character'Val (16#D3#); 
         when 16#0424# => Value := Character'Val (16#D4#); 
         when 16#0425# => Value := Character'Val (16#D5#); 
         when 16#0426# => Value := Character'Val (16#D6#); 
         when 16#0427# => Value := Character'Val (16#D7#); 
         when 16#0428# => Value := Character'Val (16#D8#); 
         when 16#0429# => Value := Character'Val (16#D9#); 
         when 16#042A# => Value := Character'Val (16#DA#); 
         when 16#042B# => Value := Character'Val (16#DB#); 
         when 16#042C# => Value := Character'Val (16#DC#); 
         when 16#042D# => Value := Character'Val (16#DD#); 
         when 16#042E# => Value := Character'Val (16#DE#); 
         when 16#042F# => Value := Character'Val (16#DF#); 
         when 16#0430# => Value := Character'Val (16#E0#); 
         when 16#0431# => Value := Character'Val (16#E1#); 
         when 16#0432# => Value := Character'Val (16#E2#); 
         when 16#0433# => Value := Character'Val (16#E3#); 
         when 16#0434# => Value := Character'Val (16#E4#); 
         when 16#0435# => Value := Character'Val (16#E5#); 
         when 16#0436# => Value := Character'Val (16#E6#); 
         when 16#0437# => Value := Character'Val (16#E7#); 
         when 16#0438# => Value := Character'Val (16#E8#); 
         when 16#0439# => Value := Character'Val (16#E9#); 
         when 16#043A# => Value := Character'Val (16#EA#); 
         when 16#043B# => Value := Character'Val (16#EB#); 
         when 16#043C# => Value := Character'Val (16#EC#); 
         when 16#043D# => Value := Character'Val (16#ED#); 
         when 16#043E# => Value := Character'Val (16#EE#); 
         when 16#043F# => Value := Character'Val (16#EF#); 
         when 16#0440# => Value := Character'Val (16#F0#); 
         when 16#0441# => Value := Character'Val (16#F1#); 
         when 16#0442# => Value := Character'Val (16#F2#); 
         when 16#0443# => Value := Character'Val (16#F3#); 
         when 16#0444# => Value := Character'Val (16#F4#); 
         when 16#0445# => Value := Character'Val (16#F5#); 
         when 16#0446# => Value := Character'Val (16#F6#); 
         when 16#0447# => Value := Character'Val (16#F7#); 
         when 16#0448# => Value := Character'Val (16#F8#); 
         when 16#0449# => Value := Character'Val (16#F9#); 
         when 16#044A# => Value := Character'Val (16#FA#); 
         when 16#044B# => Value := Character'Val (16#FB#); 
         when 16#044C# => Value := Character'Val (16#FC#); 
         when 16#044D# => Value := Character'Val (16#FD#); 
         when 16#044E# => Value := Character'Val (16#FE#); 
         when 16#044F# => Value := Character'Val (16#FF#);
         when 0..127   => Value := Character'Val (Code);
         when others   => Valid := False;
      end case;
   end Convert;

   function From_Windows_1251 (Value : Character) return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#98# then
         raise Constraint_Error;
      else
         return Result;
      end if;
   end From_Windows_1251;

   function From_Windows_1251
            (  Value      : Character;
               Substitute : Code_Point
            )  return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#98# then
         return Substitute;
      else
         return Result;
      end if;
   end From_Windows_1251;

   function From_Windows_1251 (Value : String) return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_Windows_1251 (Value));
      end loop;
      return Result (1..Pointer - 1);
   end From_Windows_1251;

   function From_Windows_1251
            (  Value      : String;
               Substitute : Code_Point
            )  return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_Windows_1251 (Value, Substitute));
      end loop;
      return Result (1..Pointer - 1);
   end From_Windows_1251;

   function From_Windows_1251 (Value : String) return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Result (Pointer) :=
            Wide_Character'Val (From_Windows_1251 (Value (Index)));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_Windows_1251;

   function From_Windows_1251
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
            (  From_Windows_1251 (Value (Index), Default)
            );
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_Windows_1251;

   function To_Windows_1251 (Value : Code_Point) return Character is
      Result : Character;
      Valid  : Boolean;
   begin
      Convert (Value, Result, Valid);
      if Valid then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end To_Windows_1251;

   function To_Windows_1251
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
   end To_Windows_1251;

   function To_Windows_1251 (Value : String) return String is
      Result  : String (1..Value'Length);
      Index   : Integer := 1;
      Code    : Code_Point;
      Pointer : Integer := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         Result (Index) := To_Windows_1251 (Code);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_Windows_1251;

   function To_Windows_1251
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
         Result (Index) := To_Windows_1251 (Code, Substitute);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_Windows_1251;

   function To_Windows_1251 (Value : Wide_String) return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_Windows_1251 (Wide_Character'Pos (Value (Pointer)));
         Index := Index + 1;
      end loop;
      return Result;
   end To_Windows_1251;

   function To_Windows_1251
            (  Value      : Wide_String;
               Substitute : Character
            )  return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_Windows_1251
            (  Wide_Character'Pos (Value (Pointer)),
               Substitute
            );
         Index := Index + 1;
      end loop;
      return Result;
   end To_Windows_1251;

end Strings_Edit.UTF8.Windows_1251;
