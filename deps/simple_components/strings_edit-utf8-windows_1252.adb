--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Windows_1252              Luebeck            --
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

package body Strings_Edit.UTF8.Windows_1252 is

   function Convert (Value : Character) return Code_Point is
   begin
      case Stream_Element (Character'Pos (Value)) is
         when 16#81# | 16#8D# | 16#8F# | 16#90# | 16#9D# =>
            return 16#0081#; 
         when 16#80# => return 16#20AC#; 
         when 16#82# => return 16#201A#; 
         when 16#83# => return 16#0192#; 
         when 16#84# => return 16#201E#; 
         when 16#85# => return 16#2026#; 
         when 16#86# => return 16#2020#; 
         when 16#87# => return 16#2021#; 
         when 16#88# => return 16#02C6#; 
         when 16#89# => return 16#2030#; 
         when 16#8A# => return 16#0160#; 
         when 16#8B# => return 16#2039#; 
         when 16#8C# => return 16#0152#; 
         when 16#8E# => return 16#017D#; 
         when 16#91# => return 16#2018#; 
         when 16#92# => return 16#2019#; 
         when 16#93# => return 16#201C#; 
         when 16#94# => return 16#201D#; 
         when 16#95# => return 16#2022#; 
         when 16#96# => return 16#2013#; 
         when 16#97# => return 16#2014#; 
         when 16#98# => return 16#02DC#;
         when 16#99# => return 16#2122#; 
         when 16#9A# => return 16#0161#; 
         when 16#9B# => return 16#203A#; 
         when 16#9C# => return 16#0153#; 
         when 16#9E# => return 16#017E#; 
         when 16#9F# => return 16#0178#;
         when 16#00#..16#7F# | 16#A0#..16#FF# =>
            return Character'Pos (Value);
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
         when 16#20AC# => Value := Character'Val (16#80#);
         when 16#201A# => Value := Character'Val (16#82#); 
         when 16#0192# => Value := Character'Val (16#83#); 
         when 16#201E# => Value := Character'Val (16#84#); 
         when 16#2026# => Value := Character'Val (16#85#); 
         when 16#2020# => Value := Character'Val (16#86#); 
         when 16#2021# => Value := Character'Val (16#87#); 
         when 16#02C6# => Value := Character'Val (16#88#); 
         when 16#2030# => Value := Character'Val (16#89#); 
         when 16#0160# => Value := Character'Val (16#8A#); 
         when 16#2039# => Value := Character'Val (16#8B#); 
         when 16#0152# => Value := Character'Val (16#8C#); 
         when 16#017D# => Value := Character'Val (16#8E#); 
         when 16#2018# => Value := Character'Val (16#91#); 
         when 16#2019# => Value := Character'Val (16#92#); 
         when 16#201C# => Value := Character'Val (16#93#); 
         when 16#201D# => Value := Character'Val (16#94#); 
         when 16#2022# => Value := Character'Val (16#95#); 
         when 16#2013# => Value := Character'Val (16#96#); 
         when 16#2014# => Value := Character'Val (16#97#); 
         when 16#02DC# => Value := Character'Val (16#98#); 
         when 16#2122# => Value := Character'Val (16#99#); 
         when 16#0161# => Value := Character'Val (16#9A#); 
         when 16#203A# => Value := Character'Val (16#9B#); 
         when 16#0153# => Value := Character'Val (16#9C#); 
         when 16#017E# => Value := Character'Val (16#9E#); 
         when 16#0178# => Value := Character'Val (16#9F#); 
         when 0..127 | 16#A0#..16#FF# => Value := Character'Val (Code);
         when others   => Valid := False;
      end case;
   end Convert;

   function From_Windows_1252 (Value : Character) return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#81# then
         raise Constraint_Error;
      else
         return Result;
      end if;
   end From_Windows_1252;

   function From_Windows_1252
            (  Value      : Character;
               Substitute : Code_Point
            )  return Code_Point is
      Result : constant Code_Point := Convert (Value);
   begin
      if Result = 16#81# then
         return Substitute;
      else
         return Result;
      end if;
   end From_Windows_1252;

   function From_Windows_1252 (Value : String) return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_Windows_1252 (Value));
      end loop;
      return Result (1..Pointer - 1);
   end From_Windows_1252;

   function From_Windows_1252
            (  Value      : String;
               Substitute : Code_Point
            )  return String is
      Result  : String (1..Value'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Put (Result, Pointer, From_Windows_1252 (Value, Substitute));
      end loop;
      return Result (1..Pointer - 1);
   end From_Windows_1252;

   function From_Windows_1252 (Value : String) return Wide_String is
      Result  : Wide_String (1..Value'Length);
      Pointer : Integer := Result'First;
   begin
      for Index in Value'Range loop
         Result (Pointer) :=
            Wide_Character'Val (From_Windows_1252 (Value (Index)));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_Windows_1252;

   function From_Windows_1252
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
            (  From_Windows_1252 (Value (Index), Default)
            );
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_Windows_1252;

   function To_Windows_1252 (Value : Code_Point) return Character is
      Result : Character;
      Valid  : Boolean;
   begin
      Convert (Value, Result, Valid);
      if Valid then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end To_Windows_1252;

   function To_Windows_1252
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
   end To_Windows_1252;

   function To_Windows_1252 (Value : String) return String is
      Result  : String (1..Value'Length);
      Index   : Integer := 1;
      Code    : Code_Point;
      Pointer : Integer := Value'First;
   begin
      while Pointer <= Value'Last loop
         Get (Value, Pointer, Code);
         Result (Index) := To_Windows_1252 (Code);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_Windows_1252;

   function To_Windows_1252
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
         Result (Index) := To_Windows_1252 (Code, Substitute);
         Index := Index + 1;
      end loop;
      return Result (1..Index - 1);
   end To_Windows_1252;

   function To_Windows_1252 (Value : Wide_String) return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_Windows_1252 (Wide_Character'Pos (Value (Pointer)));
         Index := Index + 1;
      end loop;
      return Result;
   end To_Windows_1252;

   function To_Windows_1252
            (  Value      : Wide_String;
               Substitute : Character
            )  return String is
      Result : String (1..Value'Length);
      Index  : Integer := 1;
   begin
      for Pointer in Value'Range loop
         Result (Index) :=
            To_Windows_1252
            (  Wide_Character'Pos (Value (Pointer)),
               Substitute
            );
         Index := Index + 1;
      end loop;
      return Result;
   end To_Windows_1252;

end Strings_Edit.UTF8.Windows_1252;
