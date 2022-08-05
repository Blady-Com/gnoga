--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Universally_Unique_Identifiers              Luebeck            --
--  Implementation                                 Winter, 2021       --
--                                                                    --
--                                Last revision :  13:12 05 Jan 2021  --
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

with Interfaces;  use Interfaces;

with Ada.Calendar.Formatting;
with Ada.Numerics.Discrete_Random;

package body Universally_Unique_Identifiers is
   package Random_Numbers is
      new Ada.Numerics.Discrete_Random (Unsigned_64);
   use Random_Numbers;

   Dice   : Generator;
   Epoch  : constant Time := Formatting.Time_Of (2000, 1, 1);
   --
   -- 100 nanosecods since October 15, 1582 to January 1, 2000
   --
   Shift  : constant := (12_2192_9280_0000 + 9466_8480_0000) * 1_000_0;
   Count  : Unsigned_16 := 0;

   function Create return UUID_Value is
      H : constant Unsigned_64 := Random (Dice);
      L : constant Unsigned_64 := Random (Dice);
   begin
      return
      (   1 => Byte (Shift_Right (H, 8*7) and 16#FF#),
          2 => Byte (Shift_Right (H, 8*6) and 16#FF#),
          3 => Byte (Shift_Right (H, 8*5) and 16#FF#),
          4 => Byte (Shift_Right (H, 8*4) and 16#FF#),

          5 => Byte (Shift_Right (H, 8*3) and 16#FF#),
          6 => Byte (Shift_Right (H, 8*2) and 16#FF#),

          7 => Byte (Shift_Right (H, 8*1) and 16#FF#),
          8 => Byte (             H       and 16#0F#) or 16#40#,

          9 => Byte (Shift_Right (L, 8*7) and 16#3F#) or 16#40#,
         10 => Byte (Shift_Right (L, 8*6) and 16#FF#),

         11 => Byte (Shift_Right (L, 8*5) and 16#FF#),
         12 => Byte (Shift_Right (L, 8*4) and 16#FF#),
         13 => Byte (Shift_Right (L, 8*3) and 16#FF#),
         14 => Byte (Shift_Right (L, 8*2) and 16#FF#),
         15 => Byte (Shift_Right (L, 8*1) and 16#FF#),
         16 => Byte (             L       and 16#FF#)
      );
   end Create;

   function Create
            (  ID    : Node_ID;
               Stamp : Time := Clock
            )  return UUID_Value is
      Ticks  : constant Unsigned_64 :=
                        (  Unsigned_64
                           (  Long_Float (Stamp - Epoch)
                           *  1_000_000_0.0
                           )
                        +  Shift
                        );
   begin
      Count := Count + 1;
      return
      (   1 => Byte (Shift_Right (Ticks, 8*7) and 16#FF#),
          2 => Byte (Shift_Right (Ticks, 8*6) and 16#FF#),
          3 => Byte (Shift_Right (Ticks, 8*5) and 16#FF#),
          4 => Byte (Shift_Right (Ticks, 8*4) and 16#FF#),

          5 => Byte (Shift_Right (Ticks, 8*3) and 16#FF#),
          6 => Byte (Shift_Right (Ticks, 8*2) and 16#FF#),

          7 => Byte (Shift_Right (Ticks, 8*1) and 16#FF#),
          8 => Byte (             Ticks       and 16#0F#) or 16#10#,

          9 => Byte (Shift_Right (Count, 8*1) and 16#3F#) or 16#40#,
         10 => Byte (             Count       and 16#0F#),

         11 => Byte (Character'Pos (ID (1))),
         12 => Byte (Character'Pos (ID (2))),
         13 => Byte (Character'Pos (ID (3))),
         14 => Byte (Character'Pos (ID (4))),
         15 => Byte (Character'Pos (ID (5))),
         16 => Byte (Character'Pos (ID (6)))
      );
   end Create;

   function "<" (Left, Right : UUID_Value) return Boolean is
   begin
      for Index in Left'Range loop
         if Left (Index) /= Right (Index) then
            return Left (Index) < Right (Index);
         end if;
      end loop;
      return False;
   end "<";

   function "<=" (Left, Right : UUID_Value) return Boolean is
   begin
      for Index in Left'Range loop
         if Left (Index) /= Right (Index) then
            return Left (Index) < Right (Index);
         end if;
      end loop;
      return True;
   end "<=";

   function ">=" (Left, Right : UUID_Value) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function ">" (Left, Right : UUID_Value) return Boolean is
   begin
      return not (Left <= Right);
   end ">";

begin
   Reset (Dice);
end Universally_Unique_Identifiers;
