--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Symmetric_Serialization        Luebeck            --
--  Implementation                                 Winter, 2008       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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
with Interfaces;         use Interfaces;

with Ada.Numerics.Discrete_Random;
with Generic_Random_Sequence;

package body Strings_Edit.Symmetric_Serialization is

   type Octet is new Interfaces.Unsigned_8;
--
-- Random_Octet -- Pseudo-random generator of blocks
--
   package Random_Octet is
      new Ada.Numerics.Discrete_Random (Octet);
   use Random_Octet;
--
-- Octet_Sequence - A sequence of octets
--
   package Octet_Sequence is new Generic_Random_Sequence (Octet);
   use Octet_Sequence;

   Seed_Init  : constant := 71;
   Carry_Init : constant := 37;
   Bit_Mask   : constant array (0..8) of Unsigned_16 :=
                (  2#0000_0000#, 2#0000_0001#, 2#0000_0011#,
                   2#0000_0111#, 2#0000_1111#, 2#0001_1111#,
                   2#0011_1111#, 2#0111_1111#, 2#1111_1111#
                );
   Encoding   : constant array (Unsigned_16 range 0..63) of Character :=
                (  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                &  "abcdefghijklmnopqrstuvwxyz"
                &  "0123456789_~"
                );
   Decoding   : constant array (Character) of Octet :=
                (  'A'=> 0, 'B'=> 1, 'C'=> 2, 'D'=> 3, 'E'=> 4,
                   'F'=> 5, 'G'=> 6, 'H'=> 7, 'I'=> 8, 'J'=> 9,
                   'K'=>10, 'L'=>11, 'M'=>12, 'N'=>13, 'O'=>14,
                   'P'=>15, 'Q'=>16, 'R'=>17, 'S'=>18, 'T'=>19,
                   'U'=>20, 'V'=>21, 'W'=>22, 'X'=>23, 'Y'=>24,
                   'Z'=>25, 'a'=>26, 'b'=>27, 'c'=>28, 'd'=>29,
                   'e'=>30, 'f'=>31, 'g'=>32, 'h'=>33, 'i'=>34,
                   'j'=>35, 'k'=>36, 'l'=>37, 'm'=>38, 'n'=>39,
                   'o'=>40, 'p'=>41, 'q'=>42, 'r'=>43, 's'=>44,
                   't'=>45, 'u'=>46, 'v'=>47, 'w'=>48, 'x'=>49,
                   'y'=>50, 'z'=>51, '0'=>52, '1'=>53, '2'=>54,
                   '3'=>55, '4'=>56, '5'=>57, '6'=>58, '7'=>59,
                   '8'=>60, '9'=>61, '_'=>62, '~'=>63,
                   others =>64
                );

   procedure Adjust
             (  Dice    : Generator;
                Key     : String;
                Pointer : in out Integer;
                Seed    : in out Integer
             )  is
   begin
      if Pointer > Key'First then
         Pointer := Pointer - 1;
      else
         Pointer := Key'Last;
      end if;
      Seed := Seed + Character'Pos (Key (Pointer));
      Reset (Dice, Seed);
   end Adjust;

   function Decode (Data : Encoded_String; Key : String)
      return String is
      Sequencer : Sequence;
      Dice      : Generator;
      Result    : String (1..256);
      Value     : Octet       := Carry_Init;
      Accum     : Unsigned_16 := 0;
      Bits      : Natural     := 0;
      Position  : Integer     := 0;
      Key_Index : Integer     := Key'First;
      Seed      : Integer     := Seed_Init;
   begin
      for Pointer in Data'Range loop
         declare
            Code : constant Octet := Decoding (Data (Pointer));
         begin
            if Code > 63 then
               raise Data_Error;
            end if;
            Accum := Shift_Left (Accum, 6) + Unsigned_16 (Code);
         end;
         Bits := Bits + 6;
         if Bits >= 8 then
            Adjust (Dice, Key, Key_Index, Seed);
            Next (Sequencer, Random (Dice) xor Value, Octet (Position));
            Position := Position + 1;
            Bits  := Bits - 8;
            Value := Octet (Shift_Right (Accum, Bits));
            Result (Position) := Character'Val (Value xor Random (Dice));
            Accum := Accum and Bit_Mask (Bits);
            if Length (Sequencer) = 0 then
               if Pointer /= Data'Last then
                  raise Data_Error;
               end if;
               return Result;
            end if;
         end if;
      end loop;
      raise Data_Error;
   end Decode;

   function Encode (Text : String; Key : String)
      return Encoded_String is
      Sequencer : Sequence;
      Dice      : Generator;
      Result    : Encoded_String;
      Data      : array (Octet) of Octet;
      Value     : Octet       := Carry_Init;
      Accum     : Unsigned_16 := 0;
      Bits      : Natural     := 0;
      Pointer   : Integer     := Result'First;
      Key_Index : Integer     := Key'First;
      Seed      : Integer     := Seed_Init;
   begin
      if Text'Length > 256 then
         raise Constraint_Error;
      end if;
      -- Copying text
      for Index in Text'Range loop
         Data (Octet (Index - Text'First)) :=
            Character'Pos (Text (Index));
      end loop;
      if Text'Length < Data'Length then
         -- Padding to block size with some random stuff
         for Index in Text'Length..Data'Last loop
             Data (Index) := Random (Dice);
         end loop;
      end if;
      for Index in Data'Range loop
         Adjust (Dice, Key, Key_Index, Seed);
         Value := Data (Next (Sequencer, Random (Dice) xor Value));
         Value := Value xor Random (Dice);
         Accum := Shift_Left (Accum, 8) + Unsigned_16 (Value);
         Bits  := Bits + 8;
         if Bits >= 6 then
            if Bits >= 12 then
               Bits := Bits - 6;
               Result (Pointer) := Encoding (Shift_Right (Accum, Bits));
               Pointer := Pointer + 1;
               Bits := Bits - 6;
               Result (Pointer) :=
                  Encoding (Shift_Right (Accum, Bits) and 2#11_1111#);
            else
               Bits := Bits - 6;
               Result (Pointer) := Encoding (Shift_Right (Accum, Bits));
            end if;
            Pointer := Pointer + 1;
            Accum   := Accum and Bit_Mask (Bits);
         end if;
      end loop;
      if Bits > 0 then
         Result (Pointer) := Encoding (Shift_Left (Accum, 6 - Bits));
         Pointer := Pointer + 1;
      end if;
      return Result (1..Pointer - 1);
   end Encode;

end Strings_Edit.Symmetric_Serialization;
