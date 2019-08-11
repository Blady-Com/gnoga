--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.ITU_T61                   Luebeck            --
--  Implementation                                 Spring, 2019       --
--                                                                    --
--                                Last revision :  13:36 23 Jun 2019  --
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

package body Strings_Edit.UTF8.ITU_T61 is

   function Convert (Value : Character) return Code_Point is
   begin
      case Stream_Element (Character'Pos (Value)) is
         when 16#23#         |
              16#24#         |
              16#5C#         |
              16#5E#         |
              16#60#         |
              16#7B#         |
              16#7D#..16#7E# |
              16#A9#..16#AA# |
              16#AC#..16#AF# |
              16#B9#..16#BA# |
              16#C0#         |
              16#C9#         |
              16#D0#..16#DF# |
              16#E5#         |
              16#FF# =>
            return 16#0060#; 
         when 16#00#..16#22# |
              16#25#..16#5B# |
              16#5D#         |
              16#5F#         |
              16#61#..16#7A# |
              16#7C#         |
              16#7F#..16#A3# |
              16#A5#         |
              16#A7#         |
              16#AB#         |
              16#B0#..16#B3# |
              16#B5#..16#B7# |
              16#BB#..16#BF# =>
            return Character'Pos (Value);
         when 16#A4# => return 16#0024#;
         when 16#A6# => return 16#0023#;
         when 16#A8# => return 16#00A4#;
         when 16#B4# => return 16#00D7#;
         when 16#B8# => return 16#00F7#;
         when 16#C1# => return 16#0300#;
         when 16#C2# => return 16#0301#;
         when 16#C3# => return 16#0302#;
         when 16#C4# => return 16#0303#;
         when 16#C5# => return 16#0304#;
         when 16#C6# => return 16#0306#;
         when 16#C7# => return 16#0307#;
         when 16#C8# => return 16#0308#;
         when 16#CA# => return 16#030A#;
         when 16#CB# => return 16#0327#;
         when 16#CC# => return 16#0332#;
         when 16#CD# => return 16#030B#;
         when 16#CE# => return 16#0328#;
         when 16#CF# => return 16#030C#;
         when 16#E0# => return 16#2126#;
         when 16#E1# => return 16#00C6#;
         when 16#E2# => return 16#00D0#;
         when 16#E3# => return 16#00AA#;
         when 16#E4# => return 16#0126#;
         when 16#E6# => return 16#0132#;
         when 16#E7# => return 16#013F#;
         when 16#E8# => return 16#0141#;
         when 16#E9# => return 16#00D8#;
         when 16#EA# => return 16#0152#;
         when 16#EB# => return 16#00BA#;
         when 16#EC# => return 16#00DE#;
         when 16#ED# => return 16#0166#;
         when 16#EE# => return 16#014A#;
         when 16#EF# => return 16#0149#;
         when 16#F0# => return 16#0138#;
         when 16#F1# => return 16#00E6#;
         when 16#F2# => return 16#0111#;
         when 16#F3# => return 16#00F0#;
         when 16#F4# => return 16#0127#;
         when 16#F5# => return 16#0131#;
         when 16#F6# => return 16#0133#;
         when 16#F7# => return 16#0140#;
         when 16#F8# => return 16#0142#;
         when 16#F9# => return 16#00F8#;
         when 16#FA# => return 16#0153#;
         when 16#FB# => return 16#00DF#;
         when 16#FC# => return 16#00FE#;
         when 16#FD# => return 16#0167#;
         when 16#FE# => return 16#014B#;
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
         when 16#00#..16#22# |
              16#25#..16#5B# |
              16#5D#         |
              16#5F#         |
              16#61#..16#7A# |
              16#7C#         |
              16#7F#..16#A3# |
              16#A5#         |
              16#A7#         |
              16#AB#         |
              16#B0#..16#B3# |
              16#B5#..16#B7# |
              16#BB#..16#BF# =>
            Value := Character'Val (Code);
         when 16#0024# => Value := Character'Val (16#A4#);
         when 16#0023# => Value := Character'Val (16#A6#);
         when 16#00A4# => Value := Character'Val (16#A8#);
         when 16#00D7# => Value := Character'Val (16#B4#);
         when 16#00F7# => Value := Character'Val (16#B8#);
         when 16#0300# => Value := Character'Val (16#C1#);
         when 16#0301# => Value := Character'Val (16#C2#);
         when 16#0302# => Value := Character'Val (16#C3#);
         when 16#0303# => Value := Character'Val (16#C4#);
         when 16#0304# => Value := Character'Val (16#C5#);
         when 16#0306# => Value := Character'Val (16#C6#);
         when 16#0307# => Value := Character'Val (16#C7#);
         when 16#0308# => Value := Character'Val (16#C8#);
         when 16#030A# => Value := Character'Val (16#CA#);
         when 16#0327# => Value := Character'Val (16#CB#);
         when 16#0332# => Value := Character'Val (16#CC#);
         when 16#030B# => Value := Character'Val (16#CD#);
         when 16#0328# => Value := Character'Val (16#CE#);
         when 16#030C# => Value := Character'Val (16#CF#);
         when 16#2126# => Value := Character'Val (16#E0#);
         when 16#00C6# => Value := Character'Val (16#E1#);
         when 16#00D0# => Value := Character'Val (16#E2#);
         when 16#00AA# => Value := Character'Val (16#E3#);
         when 16#0126# => Value := Character'Val (16#E4#);
         when 16#0132# => Value := Character'Val (16#E6#);
         when 16#013F# => Value := Character'Val (16#E7#);
         when 16#0141# => Value := Character'Val (16#E8#);
         when 16#00D8# => Value := Character'Val (16#E9#);
         when 16#0152# => Value := Character'Val (16#EA#);
         when 16#00BA# => Value := Character'Val (16#EB#);
         when 16#00DE# => Value := Character'Val (16#EC#);
         when 16#0166# => Value := Character'Val (16#ED#);
         when 16#014A# => Value := Character'Val (16#EE#);
         when 16#0149# => Value := Character'Val (16#EF#);
         when 16#0138# => Value := Character'Val (16#F0#);
         when 16#00E6# => Value := Character'Val (16#F1#);
         when 16#0111# => Value := Character'Val (16#F2#);
         when 16#00F0# => Value := Character'Val (16#F3#);
         when 16#0127# => Value := Character'Val (16#F4#);
         when 16#0131# => Value := Character'Val (16#F5#);
         when 16#0133# => Value := Character'Val (16#F6#);
         when 16#0140# => Value := Character'Val (16#F7#);
         when 16#0142# => Value := Character'Val (16#F8#);
         when 16#00F8# => Value := Character'Val (16#F9#);
         when 16#0153# => Value := Character'Val (16#FA#);
         when 16#00DF# => Value := Character'Val (16#FB#);
         when 16#00FE# => Value := Character'Val (16#FC#);
         when 16#0167# => Value := Character'Val (16#FD#);
         when 16#014B# => Value := Character'Val (16#FE#);
         when others   => Valid := False;
      end case;
   end Convert;

   function From_ITU_T61 (Value : Character) return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#60# then
         raise Constraint_Error;
      else
         return Result;
      end if;
   end From_ITU_T61;

   function From_ITU_T61
            (  Value      : Character;
               Substitute : Code_Point
            )  return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#60# then
         return Substitute;
      else
         return Result;
      end if;
   end From_ITU_T61;

   function From_ITU_T61 (Value : String) return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_ITU_T61 (Value));
      end loop;
      return Result (1..Pointer - 1);
   end From_ITU_T61;

   function From_ITU_T61
            (  Value      : String;
               Substitute : Code_Point
            )  return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_ITU_T61 (Value, Substitute));
      end loop;
      return Result (1..Pointer - 1);
   end From_ITU_T61;

   function From_ITU_T61 (Value : String) return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Result (Pointer) :=
            Wide_Character'Val (From_ITU_T61 (Value (Index)));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_ITU_T61;

   function From_ITU_T61
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
            (  From_ITU_T61 (Value (Index), Default)
            );
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_ITU_T61;

   function To_ITU_T61 (Value : Code_Point) return Character is
      Result : Character;
      Valid  : Boolean;
   begin
      Convert (Value, Result, Valid);
      if Valid then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end To_ITU_T61;

   function To_ITU_T61
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
   end To_ITU_T61;

   function To_ITU_T61 (Value : String) return String is
      Result  : String (1..Value'Length);
      Index   : Integer := 1;
      Code    : Code_Point;
      Pointer : Integer := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         Result (Index) := To_ITU_T61 (Code);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_ITU_T61;

   function To_ITU_T61
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
         Result (Index) := To_ITU_T61 (Code, Substitute);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_ITU_T61;

   function To_ITU_T61 (Value : Wide_String) return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_ITU_T61 (Wide_Character'Pos (Value (Pointer)));
         Index := Index + 1;
      end loop;
      return Result;
   end To_ITU_T61;

   function To_ITU_T61
            (  Value      : Wide_String;
               Substitute : Character
            )  return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_ITU_T61
            (  Wide_Character'Pos (Value (Pointer)),
               Substitute
            );
         Index := Index + 1;
      end loop;
      return Result;
   end To_ITU_T61;

end Strings_Edit.UTF8.ITU_T61;
