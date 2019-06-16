--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.ChaCha20.Poly1305              Luebeck            --
--  Implementation                                 Summer, 2018       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Strings_Edit.ChaCha20.Poly1305 is

--     function Image (Value : Unsigned_32) return String is
--        package Edit is new Strings_Edit.Integer_Edit (Integer_64);
--        use Edit;
--        Result  : String (1..8);
--        Pointer : Integer :=1;
--     begin
--        Put
--        (  Destination => Result,
--           Pointer     => Pointer,
--           Value       => Integer_64 (Value),
--           Base        => 16,
--           Field       => 8,
--           Justify     => Strings_Edit.Right,
--           Fill        => '0'
--        );
--        return Result;
--     end Image;
--
--     function Image (Value : Unsigned) return String is
--        Result : constant String :=
--                 (  Image (Value (1))
--                 &  Image (Value (2))
--                 &  Image (Value (3))
--                 &  Image (Value (4))
--                 &  Image (Value (5))
--                 &  Image (Value (6))
--                 &  Image (Value (7))
--                 &  Image (Value (8))
--                 &  Image (Value (9))
--                 );
--     begin
--        for Index in Result'Range loop
--           if Result (Index) /= '0' then
--              return Result (Index..Result'Last);
--           end if;
--        end loop;
--        return "0";
--     end Image;

--
-- Add -- Two long unsigned numbers
--
--    Value     - The accumulator
--    Increment - The value to add
--
   procedure Add
             (  Value     : in out Unsigned;
                Increment : Unsigned
             );
--
-- Bit -- Get bit value
--
--    Value - The long number
--    Index - Of the bit, 0 is the least significant bit
--
   function Bit
            (  Value : Unsigned;
               Index : Bit_Offset
            )  return Boolean;
--
-- Double -- Multiply long number by 2
--
--    Value - The long number
--
   procedure Double (Value : in out Unsigned);
--
-- Sub -- Subtract a long number if less or equal to
--
--    Value     - The long number to subtract from
--    Decrement - The long number to subtract
--
   procedure Sub
             (  Value     : in out Unsigned;
                Decrement : Unsigned
             );
--
-- * -- Multiply two long number
--
--    Left  - The long number to multiply
--    Right - The long number to multiply
--
-- Returns :
--
--    The result
--
   function "*" (Left, Right : Unsigned) return Unsigned;
--
-- mod -- Remainder
--
--    Left  - The long number divident
--    Right - The long number divisor
--
-- Returns :
--
--    The result
--
   function "mod" (Left, Right : Unsigned) return Unsigned;

   pragma Inline (Add, Bit, Double, Sub, "*", "mod");

   procedure Add
             (  Value     : in out Unsigned;
                Increment : Unsigned
             )  is
      Carry  : Unsigned_64 := 0;
   begin
      for Index in reverse Value'Range loop
         Carry := (  Carry
                  +  Unsigned_64 (Value     (Index))
                  +  Unsigned_64 (Increment (Index))
                  );
         Value (Index) := Unsigned_32 (Carry and 16#FFFF_FFFF#);
         Carry := Shift_Right (Carry, 32);
      end loop;
   end Add;
--  --
--  -- Add -- Short unsigned number to a long unsigned number
--  --
--  --    Value     - The accumulator
--  --    Increment - The value to add
--  --    Shift     - Index  where start adding Unsigned'Last corresponds to
--  --                the power 0.
--  --
--     procedure Add
--               (  Value     : in out Unsigned;
--                  Increment : Unsigned_32;
--                  Shift     : Integer
--               )  is
--     begin
--        if Increment /= 0 then
--           declare
--              Carry : Unsigned_64 := Unsigned_64 (Value (Shift)) +
--                                     Unsigned_64 (Increment);
--           begin
--              Value (Shift) := Unsigned_32 (Carry and 16#FFFF_FFFF#);
--              for Index in reverse 1..Shift - 1 loop
--                 Carry := Shift_Right (Carry, 32);
--                 exit when Carry = 0;
--                 Carry := Unsigned_64 (Value (Index)) + Carry;
--                 Value (Index) := Unsigned_32 (Carry and 16#FFFF_FFFF#);
--              end loop;
--           end;
--        end if;
--     end Add;

   function Bit
            (  Value : Unsigned;
               Index : Bit_Offset
            )  return Boolean is
      Offset : constant Bit_Offset := Bit_Offset'Last - Index;
   begin
      return 0 /= (Value (Offset / 32 + 1) and 2 ** (Index mod 32));
   end Bit;

   procedure Double (Value : in out Unsigned) is
      Carry  : Unsigned_64 := 0;
   begin
      for Index in reverse Value'Range loop
         Carry := Shift_Left (Unsigned_64 (Value (Index)), 1) or Carry;
         Value (Index) := Unsigned_32 (Carry and 16#FFFF_FFFF#);
         Carry := Shift_Right (Carry, 32);
      end loop;
   end Double;

   procedure Sub
             (  Value     : in out Unsigned;
                Decrement : Unsigned
             )  is
      Result : Unsigned;
      Carry  : Unsigned_64 := 1;
   begin
      for Index in reverse Value'Range loop
         Carry := (  Carry
                  +  Unsigned_64 (Value (Index))
                  +  Unsigned_64 (not Decrement (Index))
                  );
         Result (Index) := Unsigned_32 (Carry and 16#FFFF_FFFF#);
         Carry := Shift_Right (Carry, 32);
      end loop;
      if Carry /= 0 then
         Value := Result;
      end if;
   end Sub;

--     function "*" (Left, Right : Unsigned) return Unsigned is
--        Result : Unsigned := (others => 0);
--     begin
--        for I in reverse Left'Range loop
--           if Left (I) /= 0 then
--              for J in reverse Right'Range loop
--                if Right (J) /= 0 then
--                    declare
--                       Block : constant Unsigned_64 :=
--                                        Unsigned_64 (Left  (I)) *
--                                        Unsigned_64 (Right (J));
--                       Shift : constant Integer := I + J - Unsigned'Last;
--                    begin
--                       Add
--                       (  Result,
--                          Unsigned_32 (Block and 16#FFFF_FFFF#),
--                          Shift
--                       );
--                       Add
--                       (  Result,
--                          Unsigned_32 (Shift_Right (Block, 32)),
--                          Shift - 1
--                       );
--                    end;
--                 end if;
--              end loop;
--           end if;
--        end loop;
--        return Result;
--     end "*";

   function "*" (Left, Right : Unsigned) return Unsigned is
      Result : Unsigned;
      Accum  : Unsigned_64;
      Carry  : Unsigned_64 := 0;
   begin
      for Shift in reverse Result'Range loop
         -- For all I, J | I + J = Shift + Unsigned'Last
         Accum := Carry and 16#FFFF_FFFF#;
         Carry := Shift_Right (Carry, 32);
         for I in Integer'Max (1, Shift)
               .. Integer'Min (Unsigned'Last, Shift + Unsigned'Last - 1)
         loop
            declare
               Mul : constant Unsigned_64 :=
                     (  Unsigned_64 (Left  (I))
                     *  Unsigned_64 (Right (Shift + Unsigned'Last - I))
                     );
            begin
               Accum := Accum + (Mul and 16#FFFF_FFFF#);
               Carry := Carry + Shift_Right (Mul, 32);
            end;
         end loop;
         Result (Shift) := Unsigned_32 (Accum and 16#FFFF_FFFF#);
         Carry := Carry + Shift_Right (Accum, 32);
      end loop;
      return Result;
   end "*";

   function "mod" (Left, Right : Unsigned) return Unsigned is
      Result : Unsigned := (others => 0);
      Carry  : Unsigned_64 := 0;
   begin
      for Index in reverse Bit_Offset'Range loop
         Double (Result);
         if Bit (Left, Index) then
            Result (Result'Last) := Result (Result'Last) or 1;
         end if;
         Sub (Result, Right); -- If greater
      end loop;
      return Result;
   end "mod";
--
-- From_Number -- Number to little-endian stream array
--
--    Value - The number to convert
--
-- Returns :
--
--    The result
--
   function From_Number (Value : Unsigned_32)
      return Stream_Element_Array is
   begin
      return
      (  1 => Stream_Element (Value           mod 2**8),
         2 => Stream_Element ((Value / 2**8 ) mod 2**8),
         3 => Stream_Element ((Value / 2**16) mod 2**8),
         4 => Stream_Element ((Value / 2**24) mod 2**8)
      );
   end From_Number;

   P : constant Unsigned :=
                (  0, 0, 0, 0,
                   16#0000_0003#,
                   16#FFFF_FFFF#,
                   16#FFFF_FFFF#,
                   16#FFFF_FFFF#,
                   16#FFFF_FFFB#
                );

   function Digest
            (  Message : Stream_Element_Array;
               Key     : ChaCha20_Key
            )  return ChaCha20_Tag is
      Stream : Poly1305_Stream;
      Result : ChaCha20_Tag;
   begin
      Start (Stream, Key);
      Write (Stream, Message);
      Stop  (Stream, Result);
      return Result;
   end Digest;

   function Digest
            (  Message : String;
               Key     : ChaCha20_Key
            )  return ChaCha20_Tag is
      Stream : aliased Poly1305_Stream;
      Result : ChaCha20_Tag;
   begin
      Start (Stream, Key);
      String'Write (Stream'Access, Message);
      Stop  (Stream, Result);
      return Result;
   end Digest;

   function Digest
            (  Message : Stream_Element_Array;
               Key     : ChaCha20_Key;
               Nonce   : ChaCha20_Nonce
            )  return ChaCha20_Tag is
      Stream : Poly1305_Stream;
      Result : ChaCha20_Tag;
   begin
      Start (Stream, Key, Nonce);
      Write (Stream, Message);
      Stop  (Stream, Result);
      return Result;
   end Digest;

   function Digest
            (  Message : String;
               Key     : ChaCha20_Key;
               Nonce   : ChaCha20_Nonce
            )  return ChaCha20_Tag is
      Stream : aliased Poly1305_Stream;
      Result : ChaCha20_Tag;
   begin
      Start (Stream, Key, Nonce);
      String'Write (Stream'Access, Message);
      Stop  (Stream, Result);
      return Result;
   end Digest;

   procedure Read
             (  Stream : in out Poly1305_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             )  is
   begin
      Raise_Exception
      (  End_Error'Identity,
         "Poly1305 stream cannot be read"
      );
   end Read;

   procedure Start
             (  Stream : in out Poly1305_Stream;
                Key    : ChaCha20_Key
             )  is
   begin
      Stream.Accum := (others => 0);
      Stream.R :=
          (  0, 0, 0, 0, 0,
             16#0FFFFFFC# and +Key (13..16),
             16#0FFFFFFC# and +Key ( 9..12),
             16#0FFFFFFC# and +Key ( 5.. 8),
             16#0FFFFFFF# and +Key ( 1.. 4)
          );
      Stream.S :=
          (  0, 0, 0, 0, 0,
             +Key (29..32),
             +Key (25..28),
             +Key (21..24),
             +Key (17..20)
          );
      Stream.Count := 0;
   end Start;

   procedure Start
             (  Stream : in out Poly1305_Stream;
                Key    : ChaCha20_Key;
                Nonce  : ChaCha20_Nonce
             )  is
      Cipher : aliased ChaCha20_Cipher;
   begin
      Set_Key (Cipher, Key, Nonce);
      declare
         Key : constant Stream_Element_Array :=
                        Get_Key_Stream (Cipher'Access);
      begin
         Start (Stream, Key (Key'First..Key'First + 31));
      end;
   end Start;

   procedure Stop
             (  Stream : in out Poly1305_Stream;
                Digest : out ChaCha20_Tag
             )  is
      Accum : Unsigned renames Stream.Accum;
      Count : Stream_Element_Count renames Stream.Count;
      Data  : Stream_Element_Array renames Stream.Data;
   begin
      if Count > 0 then
         Data (Count + 1..Data'Last) := (others => 0);
         declare
            Next : constant Stream_Element_Offset := Count + 1;
         begin
            if Next <= Data'Last then
               Data (Next) := 1;
               Add
               (  Accum,
                  (  0, 0, 0, 0, 0,
                    +Data (13..16),
                    +Data ( 9..12),
                    +Data ( 5.. 8),
                    +Data ( 1.. 4)
               )  );
            else
               Add
               (  Accum,
                  (  0, 0, 0, 0, 1,
                    +Data (13..16),
                    +Data ( 9..12),
                    +Data ( 5.. 8),
                    +Data ( 1.. 4)
               )  );
            end if;
         end;
         Accum := (Stream.R * Accum) mod P;
         Count := 0;
      end if;
      Add (Accum, Stream.S);
      Digest := (  From_Number (Accum (Accum'Last))
                &  From_Number (Accum (Accum'Last - 1))
                &  From_Number (Accum (Accum'Last - 2))
                &  From_Number (Accum (Accum'Last - 3))
                );
      Accum := (others => 0);
   end Stop;

   procedure Write
             (  Stream : in out Poly1305_Stream;
                Item   : Stream_Element_Array
             )  is
      Accum   : Unsigned renames Stream.Accum;
      Count   : Stream_Element_Count renames Stream.Count;
      Data    : Stream_Element_Array renames Stream.Data;
      Pointer : Stream_Element_Offset := Item'First;
   begin
      while Pointer <= Item'Last loop
         Count := Count + 1;
         Data (Count) := Item (Pointer);
         Pointer := Pointer + 1;
         if Count = Data'Last then
            Add
            (  Accum,
               (  0, 0, 0, 0, 1,
                  +Data (13..16),
                  +Data ( 9..12),
                  +Data ( 5.. 8),
                  +Data ( 1.. 4)
            )  );
            Accum := (Stream.R * Accum) mod P;
            Count := 0;
         end if;
      end loop;
   end Write;

end Strings_Edit.ChaCha20.Poly1305;
