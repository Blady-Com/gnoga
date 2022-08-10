--                                                                    --
--  package IEEE_754.Edit           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2020       --
--                                                                    --
--                                Last revision :  20:46 27 Aug 2020  --
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

package body IEEE_754.Edit is

   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Integer_128;
                Base    : NumberBase := 10
             )  is
      Radix    : constant Unsigned_32 := Unsigned_32 (Base);
      Result   : Integer_128 := (others => 0);
      Index    : Integer := Pointer;
      Has_Sign : Boolean := False;
      Negative : Boolean := False;
      Digit    : Unsigned_32;
      Start    : Integer;
   begin
      if Index < Source'First then
         raise Layout_Error;
      end if;
      if Index > Source'Last then
         if Index - 1 > Source'Last then
            raise Layout_Error;
         else
            raise End_Error;
         end if;
      end if;
      case Source (Index) is
         when '+' =>
            Index    := Index + 1;
            Has_Sign := True;
            Get (Source, Index);
         when '-' =>
            Negative := True;
            Has_Sign := True;
            Index    := Index + 1;
            Get (Source, Index);
         when others =>
            null;
      end case;
      Start := Index;
      while Index <= Source'Last loop
         case Source (Index) is
            when '0'..'9' =>
               Digit := Character'Pos (Source (Index))
                      - Character'Pos ('0');
            when 'A'..'F' =>
               Digit := Character'Pos (Source (Index))
                      - Character'Pos ('A')
                      + 10;
            when 'a'..'f' =>
               Digit := Character'Pos (Source (Index))
                      - Character'Pos ('a')
                      + 10;
            when others =>
               exit;
         end case;
         exit when Digit >= Radix;
         Index := Index + 1;
         Mul_32 (Result, Radix);
         Add_32 (Result, Digit);
      end loop;
      if Start = Index then
         if Has_Sign then
            raise Data_Error;
         else
            raise End_Error;
         end if;
      end if;
      if Result (1) >= 2**31 then -- Overflow or minimal number
         if not Negative or else Result /= Integer_128_First then
            raise Constraint_Error;
         end if;
      else
         if Negative then
            Neg (Result);
         end if;
      end if;
      Value   := Result;
      Pointer := Index;
   end Get;

   function Image
            (  Value    : Integer_128;
               Base     : NumberBase := 10;
               Put_Plus : Boolean := False
            )  return String is
      Text    : String (1..80);
      Pointer : Integer := Text'First;
   begin
      Put (Text, Pointer, Value, Base, Put_Plus);
      return Text (Text'First..Pointer - 1);
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Integer_128;
                Base        : NumberBase := 10;
                Put_Plus    : Boolean    := False;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             )  is
      Radix       : constant Unsigned_32 := Unsigned_32 (Base);
      Result      : String (1..256);
      Accumulator : Integer_128 := Value;
      Index       : Positive := Result'Last;
      Negative    : constant Boolean := Accumulator (1) >= 2**31;
      Digit       : Unsigned_32;
   begin
      if Negative and then Accumulator /= Integer_128_First then
         Neg (Accumulator);
      end if;
      loop
         Div_32 (Accumulator, Radix, Digit);
         if Digit < 10 then
            Result (Index) :=
               Character'Val (Character'Pos ('0') + Digit);
         else
            Result (Index) :=
               Character'Val (Character'Pos ('A') + Digit - 10);
         end if;
         Index := Index - 1;
         exit when Accumulator = Integer_128_Zero;
      end loop;
      if Negative then
         Result (Index) := '-';
         Index := Index - 1;
      elsif Put_Plus then
         Result (Index) := '+';
         Index := Index - 1;
      end if;
      Put
      (  Destination,
         Pointer,
         Result (Index + 1..Result'Last),
         Field,
         Justify,
         Fill
      );
   end Put;

   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Integer_128 is
      Result  : Integer_128;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, SpaceAndTab);
      Get (Source, Pointer, Result, Base);
      Get (Source, Pointer, SpaceAndTab);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

end IEEE_754.Edit;
