--                                                                    --
--  procedure Test_IEEE_754         Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Summer, 2008       --
--                                                                    --
--                                Last revision :  13:32 28 Jan 2022  --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.Text_IO;            use Ada.Text_IO;
with Interfaces;             use Interfaces;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with IEEE_754.Edit;
with IEEE_754.Floats;
with IEEE_754.Long_Floats;
with IEEE_754.Decimal32;
with IEEE_754.Decimal64;
with IEEE_754.Decimal128;
with Universally_Unique_Identifiers.Edit;

procedure Test_IEEE_754 is
begin
   declare
      use IEEE_754.Floats;
      function Image (X : Float_32) return String is
         Text    : String (1..80);
         Pointer : Integer := 1;
      begin
         for I in X'Range loop
            Put
            (  Text,
               Pointer,
               Integer (X (I)),
               Field   => 2,
               Fill    => '0',
               Justify => Right,
               Base    => 16
            );
            Put (Text, Pointer, ' ');
         end loop;
         return Text (1..Pointer - 2);
      end Image;
   begin
      Put_Line ("Float'First " & Image (To_IEEE (Float'First)));
      Put_Line ("Float'Last  " & Image (To_IEEE (Float'Last)));
      Put_Line ("Float'Small " & Image (To_IEEE (Float'Small)));
      for Power in -10..10 loop
         declare
            N : constant Float := Float'Scaling (1.0, Power);
         begin
            if From_IEEE (To_IEEE (N)) /= N then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "N=" & Image (Power)
                  &  " IEEE=" & Image (To_IEEE (N))
                  &  " Back=" & Float'Image (N)
               )  );
            end if;
         end;
      end loop;
      if Float_32'((0, 0, 0, 1)) /= To_IEEE (1.401298464324817e-45) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Image (To_IEEE (1.401298464324817e-45))
            &  " /= "
            &  Image (Float_32'((0, 0, 0, 1)))
         )  );
      end if;
      if From_IEEE ((0, 0, 0, 1)) /= 1.401298464324817e-45 then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1.401298464324817e-45 /="
            &  Float'Image (From_IEEE ((0, 0, 0, 1)))
         )  );
      end if;
      if (  From_IEEE ((16#00#, 16#7F#, 16#FF#, 16#FF#))
         /= 1.1754942106924411e-38
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1.1754942106924411e-38 /="
            &  Float'Image
               (  From_IEEE ((16#00#, 16#7F#, 16#FF#, 16#FF#))
         )  )  );
      end if;
      if (  Float_32'(16#00#, 16#7F#, 16#FF#, 16#FF#)
         /= To_IEEE (1.1754942106924411e-38)
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Image (To_IEEE (1.1754942106924411e-38))
            &  " /= "
            &  Image (Float_32'(16#00#, 16#7F#, 16#FF#, 16#FF#))
         )  );
      end if;
      if From_IEEE ((16#49#, 16#96#, 16#BB#, 16#38#)) /= 1234791.0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1234791.0 /="
            &  Float'Image
               (  From_IEEE ((16#49#, 16#96#, 16#BB#, 16#38#))
         )  )  );
      end if;
      if From_IEEE ((16#3F#, 16#80#, 0, 0)) /= 1.0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1.0 /="
            &  Float'Image (From_IEEE ((16#3F#, 16#80#, 0, 0)))
         )  );
      end if;
      if To_IEEE (1.0) /= (16#3F#, 16#80#, 0, 0) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Image (To_IEEE (1.0))
            &  " /= "
            &  Image ((16#3F#, 16#80#, 0, 0))
         )  );
      end if;
      if From_IEEE (Positive_Zero) /= 0.0 then
         Raise_Exception (Constraint_Error'Identity, "Error in +0");
      end if;
      if From_IEEE (Negative_Zero) /= 0.0 then
         Raise_Exception (Constraint_Error'Identity, "Error in -0");
      end if;
   exception
      when Error : Constraint_Error =>
         Put_Line
         (  "Error (note some tests may fail on some machines "
         &  "depending on the range of the standard type Float): "
         &  Exception_Message (Error)
         );
   end;

   declare
      use IEEE_754.Long_Floats;
      function Image (X : Float_64) return String is
         Text    : String (1..80);
         Pointer : Integer := 1;
      begin
         for I in X'Range loop
            Put
            (  Text,
               Pointer,
               Integer (X (I)),
               Field   => 2,
               Fill    => '0',
               Justify => Right,
               Base    => 16
            );
            Put (Text, Pointer, ' ');
         end loop;
         return Text (1..Pointer - 2);
      end Image;
   begin
      Put_Line
      (  "Long_Float'First "
      &  Image (To_IEEE (Long_Float'First))
      );
      Put_Line
      (  "Long_Float'Last "
      &  Image (To_IEEE (Long_Float'Last))
      );
      Put_Line
      (  "Long_Float'Small "
      &  Image (To_IEEE (Long_Float'Small))
      );
      if To_IEEE (1.0) /= Float_64'(16#3F#, 16#F0#, others => 0) then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in "
            &  Image (To_IEEE (1.0))
            &  " /= "
            &  Image ((16#3F#, 16#F0#, others => 0))
         )  );
      end if;
      if From_IEEE ((16#3F#, 16#F0#, others => 0)) /= 1.0 then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 1.0 /="
            &  Long_Float'Image
               (  From_IEEE ((16#3F#, 16#F0#, others => 0))
         )  )  );
      end if;
      for Power in -20..20 loop
         declare
            N : constant Long_Float := Long_Float'Scaling (1.0, Power);
         begin
            if From_IEEE (To_IEEE (N)) /= N then
               Raise_Exception
               (  Constraint_Error'Identity,
                  (  "N=" & Image (Power)
                  &  " IEEE=" & Image (To_IEEE (N))
                  &  " Back=" & Long_Float'Image (N)
               )  );
            end if;
         end;
      end loop;
      if (  From_IEEE
            (( 16#42#, 16#DC#, 16#13#, 16#6E#,
               16#FB#, 16#C2#, 16#DA#, 16#40#
            ))
         /= 123479167798121.0
         )
      then
         Raise_Exception
         (  Constraint_Error'Identity,
            (  "Error in 123479167798121.0 /="
            &  Long_Float'Image
               (  From_IEEE
                  (( 16#42#, 16#DC#, 16#13#, 16#6E#,
                     16#FB#, 16#C2#, 16#DA#, 16#40#
         )  )  )  ));
      end if;
      if From_IEEE (Positive_Zero) /= 0.0 then
         Raise_Exception (Constraint_Error'Identity, "Error in +0");
      end if;
      if From_IEEE (Negative_Zero) /= 0.0 then
         Raise_Exception (Constraint_Error'Identity, "Error in -0");
      end if;
   exception
      when Error : Constraint_Error =>
         Put_Line
         (  "Error (note some tests may fail on some machines "
         &  "depending on the range of the standard type Long_Float): "
         &  Exception_Message (Error)
         );
   end;

   declare
      use IEEE_754;
      procedure Check (X : Unpacked_Decimal; Y : Packed_Decimal) is
      begin
         if Pack (X) /= Y then
            Raise_Exception
            (  Data_Error'Identity,
               (  "DPD packing"
               &  Unpacked_Decimal'Image (X)
               &  " error "
               &  Packed_Decimal'Image (Pack (X))
               &  ", expected "
               &  Packed_Decimal'Image (Y)
            )  );
         end if;
         if Unpack (Y) /= X then
            Raise_Exception
            (  Data_Error'Identity,
               (  "DPD unpacking"
               &  Packed_Decimal'Image (Y)
               &  " error "
               &  Unpacked_Decimal'Image (Unpack (Y))
               &  ", expected "
               &  Unpacked_Decimal'Image (X)
            )  );
         end if;
      end Check;
   begin
      Check (001, 2#000_000_0001#);
      Check (234, 2#010_011_0100#);
      Check (005, 2#000_000_0101#);
      Check (009, 2#000_000_1001#);
      Check (055, 2#000_101_0101#);
      Check (079, 2#000_111_1001#);
      Check (080, 2#000_000_1010#);
      Check (099, 2#000_101_1111#);
      Check (555, 2#101_101_0101#);
      Check (999, 2#001_111_1111#);
   end;

   declare
      use IEEE_754;
      use IEEE_754.Decimal64;

      function "+" (Value : Unsigned_64) return Decimal_64 is
      begin
         return
         (  Byte (Shift_Right (Value, 56)),
            Byte (Shift_Right (Value, 48) and 16#FF#),
            Byte (Shift_Right (Value, 40) and 16#FF#),
            Byte (Shift_Right (Value, 32) and 16#FF#),
            Byte (Shift_Right (Value, 24) and 16#FF#),
            Byte (Shift_Right (Value, 16) and 16#FF#),
            Byte (Shift_Right (Value,  8) and 16#FF#),
            Byte (Value and 16#FF#)
         );
      end "+";

      function Image (X : Decimal_64) return String is
         Text    : String (1..80);
         Pointer : Integer := 1;
      begin
         for I in X'Range loop
            Put
            (  Text,
               Pointer,
               Integer (X (I)),
               Field   => 2,
               Fill    => '0',
               Justify => Right,
               Base    => 16
            );
            Put (Text, Pointer, ' ');
         end loop;
         return Text (1..Pointer - 2);
      end Image;

      function Image
               (  M : Decimal_64_Mantissa;
                  D : Decimal_64_Exponent
               )  return String is
      begin
         return Decimal_64_Mantissa'Image (M) &
                " * 10 **"                      &
                Decimal_64_Exponent'Image (D);
      end Image;

      procedure Check
                (  M : Decimal_64_Mantissa;
                   D : Decimal_64_Exponent;
                   E : Unsigned_64
                )  is
         Encoded  : constant Decimal_64 := +E;
         Mantissa : Decimal_64_Mantissa;
         Exponent : Decimal_64_Exponent;
      begin
         if To_IEEE (M, D) /= Encoded then
            Raise_Exception
            (  Data_Error'Identity,
               "Encode BID " & Image (M, D) &
               " = "         & Image (To_IEEE (M, D))  &
               " (expected " & Image (Encoded) & ")"
            );
         end if;
         From_IEEE (Encoded, Mantissa, Exponent);
         if M /= Mantissa or else D /= Exponent then
            Raise_Exception
            (  Data_Error'Identity,
               "Decode BID " & Image (Encoded)            &
               " = "         & Image (Mantissa, Exponent) &
               " (expected " & Image (M, D) & ")"
            );
         end if;
      end Check;

      procedure Check_DPD
                (  M : Decimal_64_Mantissa;
                   D : Decimal_64_Exponent;
                   E : Unsigned_64
                )  is
         Encoded  : constant Decimal_64 := +E;
         Mantissa : Decimal_64_Mantissa;
         Exponent : Decimal_64_Exponent;
      begin
         if To_IEEE (M, D, Densely_Packed) /= Encoded then
            Raise_Exception
            (  Data_Error'Identity,
               "Encode DPD " & Image (M, D) &
               " = "         & Image (To_IEEE (M, D, Densely_Packed)) &
               " (expected " & Image (Encoded) & ")"
            );
         end if;
         From_IEEE (Encoded, Mantissa, Exponent, Densely_Packed);
         if M /= Mantissa or else D /= Exponent then
            Raise_Exception
            (  Data_Error'Identity,
               "Decode DPD " & Image (Encoded)              &
               " = "         & Image (Mantissa, Exponent)   &
               " (expected " & Image (M, D) & ")"
            );
         end if;
      end Check_DPD;
   begin
      Check ( 9_999_999_999_999_999, 250, 16#7443_86F2_6FC0_FFFF#);
      Check (                   -50, 101, 16#BE60_0000_0000_0032#);
      Check (                    -5, 102, 16#BE80_0000_0000_0005#);
      Check (-9_830_000_000_000_001,   5, 16#EC9A_EC55_3A24_6001#);
      Check (                   123,  -4, 16#3140_0000_0000_007B#);

      Check_DPD (              123,   -4, 16#2228_0000_0000_00A3#);
      Check_DPD (             -750,   -2, 16#A230_0000_0000_03D0#);
      Check_DPD (             -750,   -3, 16#A22c_0000_0000_03D0#);
      Check_DPD (             -750,   -4, 16#A228_0000_0000_03D0#);
      Check_DPD (             -750,   -9, 16#A214_0000_0000_03D0#);
      Check_DPD (              123,    0, 16#2238_0000_0000_00a3#);
      Check_DPD (             1234,    0, 16#2238_0000_0000_0534#);
      Check_DPD (            12345,    0, 16#2238_0000_0000_49c5#);
      Check_DPD ( 1234567890123456,    0, 16#2639_34b9_c1e2_8e56#);
      Check_DPD (-1234567890123456,    0, 16#a639_34b9_c1e2_8e56#);
      Check_DPD ( 1234567890123456,  -12, 16#2609_34b9_c1e2_8e56#);
      Check_DPD ( 1111111111111111,    0, 16#2638_9124_4912_4491#);
      Check_DPD ( 9999999999999999,  369, 16#77fc_ff3f_cff3_fcff#);
      Check_DPD ( 9999999999999999,    0, 16#6e38_ff3f_cff3_fcff#);
      Check_DPD ( 1234567890123456,  369, 16#47fd_34b9_c1e2_8e56#);
      Check_DPD (                1,    0, 16#2238_0000_0000_0001#);
      Check_DPD (               -1, -383, 16#803c_0000_0000_0001#);
      Check_DPD (-9999999999999999,  369, 16#f7f_cff3f_cff3_fcff#);
   end;

   declare
      use IEEE_754;
      use IEEE_754.Decimal32;

      function "+" (Value : Unsigned_32) return Decimal_32 is
      begin
         return
         (  Byte (Shift_Right (Value, 24)),
            Byte (Shift_Right (Value, 16) and 16#FF#),
            Byte (Shift_Right (Value,  8) and 16#FF#),
            Byte (Value and 16#FF#)
         );
      end "+";

      function Image (X : Decimal_32) return String is
         Text    : String (1..80);
         Pointer : Integer := 1;
      begin
         for I in X'Range loop
            Put
            (  Text,
               Pointer,
               Integer (X (I)),
               Field   => 2,
               Fill    => '0',
               Justify => Right,
               Base    => 16
            );
            Put (Text, Pointer, ' ');
         end loop;
         return Text (1..Pointer - 2);
      end Image;

      function Image
               (  M : Decimal_32_Mantissa;
                  D : Decimal_32_Exponent
               )  return String is
      begin
         return Decimal_32_Mantissa'Image (M) &
                " * 10 **"                      &
                Decimal_32_Exponent'Image (D);
      end Image;

      procedure Check
                (  M : Decimal_32_Mantissa;
                   D : Decimal_32_Exponent;
                   E : Unsigned_32
                )  is
         Encoded  : constant Decimal_32 := +E;
         Mantissa : Decimal_32_Mantissa;
         Exponent : Decimal_32_Exponent;
      begin
         if To_IEEE (M, D) /= Encoded then
            Raise_Exception
            (  Data_Error'Identity,
               "Encode BID " & Image (M, D) &
               " = "         & Image (To_IEEE (M, D))  &
               " (expected " & Image (Encoded) & ")"
            );
         end if;
         From_IEEE (Encoded, Mantissa, Exponent);
         if M /= Mantissa or else D /= Exponent then
            Raise_Exception
            (  Data_Error'Identity,
               "Decode BID " & Image (Encoded)            &
               " = "         & Image (Mantissa, Exponent) &
               " (expected " & Image (M, D) & ")"
            );
         end if;
      end Check;

      procedure Check_DPD
                (  M : Decimal_32_Mantissa;
                   D : Decimal_32_Exponent;
                   E : Unsigned_32
                )  is
         Encoded  : constant Decimal_32 := +E;
         Mantissa : Decimal_32_Mantissa;
         Exponent : Decimal_32_Exponent;
      begin
         if To_IEEE (M, D, Densely_Packed) /= Encoded then
            Raise_Exception
            (  Data_Error'Identity,
               "Encode DPD " & Image (M, D) &
               " = "         & Image (To_IEEE (M, D, Densely_Packed)) &
               " (expected " & Image (Encoded) & ")"
            );
         end if;
         From_IEEE (Encoded, Mantissa, Exponent, Densely_Packed);
         if M /= Mantissa or else D /= Exponent then
            Raise_Exception
            (  Data_Error'Identity,
               "Decode DPD " & Image (Encoded)              &
               " = "         & Image (Mantissa, Exponent)   &
               " (expected " & Image (M, D) & ")"
            );
         end if;
      end Check_DPD;
   begin
      Check (      0,  0, 16#3280_0000#);
      Check (      1,  0, 16#3280_0001#);
      Check (     -1,  0, 16#B280_0001#);
      Check (9999999,  0, 16#6CB8_967F#);
      Check (9999999, 80, 16#76B8_967F#);
      Check (1919495,  0, 16#329D_4A07#);

      Check_DPD (    -750,   -2, 16#A230_03D0#);
      Check_DPD (    -750,    1, 16#A260_03D0#);
      Check_DPD (    -750,   -1, 16#A240_03D0#);
      Check_DPD (    -750,   -3, 16#A220_03D0#);
      Check_DPD (    -750,   -6, 16#A1f0_03D0#);
      Check_DPD (    -750,   -8, 16#A1d0_03D0#);
      Check_DPD (    -750,   -9, 16#A1c0_03D0#);
      Check_DPD (-1234567,    0, 16#a654_d2e7#);
      Check_DPD ( 1234567,    0, 16#2654_d2e7#);
      Check_DPD ( 1111111,    0, 16#2652_4491#);
      Check_DPD ( 9999999,   90, 16#77f3_fcff#);
      Check_DPD ( 1234567,   90, 16#47f4_d2e7#);
      Check_DPD ( 1000000, -101, 16#0400_0000#);
   end;

   declare
      use IEEE_754;
      use IEEE_754.Edit;
      use IEEE_754.Decimal128;

      function "&" (L, R : Unsigned_64) return Decimal_128 is
      begin
         return
         (  Byte (Shift_Right (L, 56) and 16#FF#),
            Byte (Shift_Right (L, 48) and 16#FF#),
            Byte (Shift_Right (L, 40) and 16#FF#),
            Byte (Shift_Right (L, 32) and 16#FF#),
            Byte (Shift_Right (L, 24) and 16#FF#),
            Byte (Shift_Right (L, 16) and 16#FF#),
            Byte (Shift_Right (L,  8) and 16#FF#),
            Byte (             L      and 16#FF#),
            Byte (Shift_Right (R, 56) and 16#FF#),
            Byte (Shift_Right (R, 48) and 16#FF#),
            Byte (Shift_Right (R, 40) and 16#FF#),
            Byte (Shift_Right (R, 32) and 16#FF#),
            Byte (Shift_Right (R, 24) and 16#FF#),
            Byte (Shift_Right (R, 16) and 16#FF#),
            Byte (Shift_Right (R,  8) and 16#FF#),
            Byte (             R      and 16#FF#)
         );
      end "&";

      function Image (X : Decimal_128) return String is
         Text    : String (1..80);
         Pointer : Integer := 1;
      begin
         for I in X'Range loop
            Put
            (  Text,
               Pointer,
               Integer (X (I)),
               Field   => 2,
               Fill    => '0',
               Justify => Right,
               Base    => 16
            );
            Put (Text, Pointer, ' ');
         end loop;
         return Text (1..Pointer - 2);
      end Image;

      function Image
               (  M : String;
                  D : Decimal_128_Exponent
               )  return String is
      begin
         return M & " * 10 **" &  Decimal_128_Exponent'Image (D);
      end Image;

      procedure Check (X, Y : String; LT, LE, GT, GE : Boolean) is
         Left  : constant IEEE_754.Integer_128 := Value (X);
         Right : constant IEEE_754.Integer_128 := Value (Y);
      begin
         if (Left < Right) /= LT then
            Raise_Exception
            (  Data_Error'Identity,
               X & " < " & Y & " /= " & Boolean'Image (LT)
            );
         end if;
         if (Left <= Right) /= LE then
            Raise_Exception
            (  Data_Error'Identity,
               X & " <= " & Y & " /= " & Boolean'Image (LE)
            );
         end if;
         if (Left > Right) /= GT then
            Raise_Exception
            (  Data_Error'Identity,
               X & " > " & Y & " /= " & Boolean'Image (GT)
            );
         end if;
         if (Left >= Right) /= GE then
            Raise_Exception
            (  Data_Error'Identity,
               X & " >= " & Y & " /= " & Boolean'Image (GE)
            );
         end if;
      end Check;

      procedure Check (X, Y : IEEE_754.Integer_128; Error : String) is
      begin
         if X /= Y then
            Raise_Exception
            (  Data_Error'Identity,
               Error & " error:" & Image (X) & "/=" & Image (Y)
            );
         end if;
      end Check;

      procedure Check
                (  M  : String;
                   D  : Decimal_128_Exponent;
                   E1 : Unsigned_64;
                   E2 : Unsigned_64
                )  is
         Encoded  : constant Decimal_128 := E1 & E2;
         Mantissa : IEEE_754.Integer_128;
         Exponent : Decimal_128_Exponent;
      begin
         if To_IEEE (Value (M), D) /= Encoded then
            Raise_Exception
            (  Data_Error'Identity,
               "Encode BID " & Image (M, D)                   &
               " = "         & Image (To_IEEE (Value (M), D)) &
               " (expected " & Image (Encoded) & ")"
            );
         end if;
         From_IEEE (Encoded, Mantissa, Exponent);
         if Value (M) /= Mantissa or else D /= Exponent then
            Raise_Exception
            (  Data_Error'Identity,
               "Decode BID " & Image (Encoded)                    &
               " = "         & Image (Image (Mantissa), Exponent) &
               " (expected " & Image (M, D) & ")"
            );
         end if;
      end Check;

      procedure Check_DPD
                (  M  : String;
                   D  : Decimal_128_Exponent;
                   E1 : Unsigned_64;
                   E2 : Unsigned_64
                )  is
         Encoded  : constant Decimal_128 := E1 & E2;
         Mantissa : IEEE_754.Integer_128;
         Exponent : Decimal_128_Exponent;
      begin
         if To_IEEE (Value (M), D, Densely_Packed) /= Encoded then
            Raise_Exception
            (  Data_Error'Identity,
               "Encode DPD " & Image (M, D) &
               " = "         & Image
                               (  To_IEEE (Value (M),
                                  D,
                                  Densely_Packed)
                               )  &
               " (expected " & Image (Encoded) & ")"
            );
         end if;
         From_IEEE (Encoded, Mantissa, Exponent, Densely_Packed);
         if Value (M) /= Mantissa or else D /= Exponent then
            Raise_Exception
            (  Data_Error'Identity,
               "Decode DPD " & Image (Encoded)                    &
               " = "         & Image (Image (Mantissa), Exponent) &
               " (expected " & Image (M, D) & ")"
            );
         end if;
      end Check_DPD;

      procedure Verify (X : IEEE_754.Integer_128; S : String)  is
         S1 : constant String := Image (X);
         X1 : IEEE_754.Integer_128;
      begin
         if S1 /= S then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Image error"
               &  Unsigned_32'Image (X (1))
               &  Unsigned_32'Image (X (2))
               &  Unsigned_32'Image (X (3))
               &  Unsigned_32'Image (X (4))
               &  " got "
               &  S1
               &  ", expected "
               &  S
            )  );
         end if;
         X1 := Value (S);
         if X1 /= X then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Value error"
               &  Unsigned_32'Image (X1 (1))
               &  Unsigned_32'Image (X1 (2))
               &  Unsigned_32'Image (X1 (3))
               &  Unsigned_32'Image (X1 (4))
               &  ", expected"
               &  Unsigned_32'Image (X (1))
               &  Unsigned_32'Image (X (2))
               &  Unsigned_32'Image (X (3))
               &  Unsigned_32'Image (X (4))
            )  );
         end if;
      end Verify;
   begin
      Verify ((0,0,           0, 16#FFFF_FFFF#),          "4294967295");
      Verify ((0,0,           0,      16#FFFF#),               "65535");
      Verify ((0,0,    16#FFFF#, 16#FFFF_FFFF#),     "281474976710655");
      Verify ((0,0,16#FFF_FFFF#, 16#FFFF_FFFF#), "1152921504606846975");

      Check
      (  Value ("1000100010001", 16) * Value ("1000100010001", 16),
         value ("79230580421244924985813893121"),
         "16#1000100010001# * 16#1000100010001#"
      );
      Check
      (  Value ("12345678901") * Value ("12345678901"),
         value ("152415787526596567801"),
         "12345678901 * 12345678901"
      );
      Check
      (  Value ("123456789") * Value ("123456789"),
         value ("15241578750190521"),
         "123456789 * 123456789"
      );

      Check (Value ("2") ** Value ("3"),   Value ("8"),    "2**3");
      Check (Value ("2") ** Value ("2"),   Value ("4"),    "2**2");
      Check (Value ("2") ** Value ("0"),   Value ("1"),    "2**0");
      Check (Value ("2") ** Value ("1"),   Value ("2"),    "2**1");
      Check (Value ("2") ** Value ("10"),  Value ("1024"), "2**10");

      declare
         X : IEEE_754.Integer_128;
      begin
         X := Integer_128_Last - Value ("-1");
         Raise_Exception
         (  Data_Error'Identity,
            "Overflow check error"
         );
      exception
         when Constraint_Error =>
            null;
      end;
      declare
         X : IEEE_754.Integer_128;
      begin
         X := Integer_128_First - Value ("1");
         Raise_Exception
         (  Data_Error'Identity,
            "Overflow check error"
         );
      exception
         when Constraint_Error =>
            null;
      end;

      Check
      (  Value ("92345678901234565") mod Value ("10000000000000000"),
         Value ( "2345678901234565"),
         "92345678901234565 mod 10000000000000000"
      );
      Check
      (  Value ("92345678901234565") / Value ("10000000000000000"),
         Value ( "9"),
         "92345678901234565 / 10000000000000000"
      );
      Check
      (  Integer_128_Last / (Integer_128_Last - From_Integer_64 (1)),
         From_Integer_64 (1),
         "Last / (Last - 1)"
      );
      Check
      (  From_Integer_64 (-10) rem From_Integer_64 (-5),
         From_Integer_64 (0),
         "-10 rem -5"
      );
      Check
      (  From_Integer_64 (-11) rem From_Integer_64 (-5),
         From_Integer_64 (-1),
         "-11 rem -5"
      );
      Check
      (  From_Integer_64 (-12) rem From_Integer_64 (-5),
         From_Integer_64 (-2),
         "-12 rem -5"
      );
      Check
      (  From_Integer_64 (-13) rem From_Integer_64 (-5),
         From_Integer_64 (-3),
         "-13 rem -5"
      );
      Check
      (  From_Integer_64 (-14) rem From_Integer_64 (-5),
         From_Integer_64 (-4),
         "-14 rem -5"
      );
      Check
      (  From_Integer_64 (-10) rem From_Integer_64 (5),
         From_Integer_64 (0),
         "-10 rem 5"
      );
      Check
      (  From_Integer_64 (-11) rem From_Integer_64 (5),
         From_Integer_64 (-1),
         "-11 rem 5"
      );
      Check
      (  From_Integer_64 (-12) rem From_Integer_64 (5),
         From_Integer_64 (-2),
         "-12 rem 5"
      );
      Check
      (  From_Integer_64 (-13) rem From_Integer_64 (5),
         From_Integer_64 (-3),
         "-13 rem 5"
      );
      Check
      (  From_Integer_64 (-14) rem From_Integer_64 (5),
         From_Integer_64 (-4),
         "-14 rem 5"
      );
      Check
      (  From_Integer_64 (10) rem From_Integer_64 (-5),
         From_Integer_64 (0),
         "10 rem -5"
      );
      Check
      (  From_Integer_64 (11) rem From_Integer_64 (-5),
         From_Integer_64 (1),
         "11 rem -5"
      );
      Check
      (  From_Integer_64 (12) rem From_Integer_64 (-5),
         From_Integer_64 (2),
         "12 rem -5"
      );
      Check
      (  From_Integer_64 (13) rem From_Integer_64 (-5),
         From_Integer_64 (3),
         "13 rem -5"
      );
      Check
      (  From_Integer_64 (14) rem From_Integer_64 (-5),
         From_Integer_64 (4),
         "14 rem -5"
      );
      Check
      (  From_Integer_64 (10) rem From_Integer_64 (5),
         From_Integer_64 (0),
         "10 rem 5"
      );
      Check
      (  From_Integer_64 (11) rem From_Integer_64 (5),
         From_Integer_64 (1),
         "11 rem 5"
      );
      Check
      (  From_Integer_64 (12) rem From_Integer_64 (5),
         From_Integer_64 (2),
         "12 rem 5"
      );
      Check
      (  From_Integer_64 (13) rem From_Integer_64 (5),
         From_Integer_64 (3),
         "13 rem 5"
      );
      Check
      (  From_Integer_64 (14) rem From_Integer_64 (5),
         From_Integer_64 (4),
         "14 rem 5"
      );

      Check
      (  From_Integer_64 (-10) mod From_Integer_64 (-5),
         From_Integer_64 (0),
         "-10 mod -5"
      );
      Check
      (  From_Integer_64 (-11) mod From_Integer_64 (-5),
         From_Integer_64 (-1),
         "-11 mod -5"
      );
      Check
      (  From_Integer_64 (-12) mod From_Integer_64 (-5),
         From_Integer_64 (-2),
         "-12 mod -5"
      );
      Check
      (  From_Integer_64 (-13) mod From_Integer_64 (-5),
         From_Integer_64 (-3),
         "-13 mod -5"
      );
      Check
      (  From_Integer_64 (-14) mod From_Integer_64 (-5),
         From_Integer_64 (-4),
         "-14 mod -5"
      );
      Check
      (  From_Integer_64 (-10) mod From_Integer_64 (5),
         From_Integer_64 (0),
         "-10 mod 5"
      );
      Check
      (  From_Integer_64 (-11) mod From_Integer_64 (5),
         From_Integer_64 (4),
         "-11 mod 5"
      );
      Check
      (  From_Integer_64 (-12) mod From_Integer_64 (5),
         From_Integer_64 (3),
         "-12 mod 5"
      );
      Check
      (  From_Integer_64 (-13) mod From_Integer_64 (5),
         From_Integer_64 (2),
         "-13 mod 5"
      );
      Check
      (  From_Integer_64 (-14) mod From_Integer_64 (5),
         From_Integer_64 (1),
         "-14 mod 5"
      );
      Check
      (  From_Integer_64 (10) mod From_Integer_64 (-5),
         From_Integer_64 (0),
         "10 mod -5"
      );
      Check
      (  From_Integer_64 (11) mod From_Integer_64 (-5),
         From_Integer_64 (-4),
         "11 mod -5"
      );
      Check
      (  From_Integer_64 (12) mod From_Integer_64 (-5),
         From_Integer_64 (-3),
         "12 mod -5"
      );
      Check
      (  From_Integer_64 (13) mod From_Integer_64 (-5),
         From_Integer_64 (-2),
         "13 mod -5"
      );
      Check
      (  From_Integer_64 (14) mod From_Integer_64 (-5),
         From_Integer_64 (-1),
         "14 mod -5"
      );
      Check
      (  From_Integer_64 (10) mod From_Integer_64 (5),
         From_Integer_64 (0),
         "10 mod 5"
      );
      Check
      (  From_Integer_64 (11) mod From_Integer_64 (5),
         From_Integer_64 (1),
         "11 mod 5"
      );
      Check
      (  From_Integer_64 (12) mod From_Integer_64 (5),
         From_Integer_64 (2),
         "12 mod 5"
      );
      Check
      (  From_Integer_64 (13) mod From_Integer_64 (5),
         From_Integer_64 (3),
         "13 mod 5"
      );
      Check
      (  From_Integer_64 (14) mod From_Integer_64 (5),
         From_Integer_64 (4),
         "14 mod 5"
      );

      Check
      (  From_Integer_64 (-10) / From_Integer_64 (-5),
         From_Integer_64 (2),
         "-10 / -5"
      );
      Check
      (  From_Integer_64 (-11) / From_Integer_64 (-5),
         From_Integer_64 (2),
         "-11 / -5"
      );
      Check
      (  From_Integer_64 (-12) / From_Integer_64 (-5),
         From_Integer_64 (2),
         "-12 / -5"
      );
      Check
      (  From_Integer_64 (-13) / From_Integer_64 (-5),
         From_Integer_64 (2),
         "-13 / -5"
      );
      Check
      (  From_Integer_64 (-14) / From_Integer_64 (-5),
         From_Integer_64 (2),
         "-14 / -5"
      );
      Check
      (  From_Integer_64 (-10) / From_Integer_64 (5),
         From_Integer_64 (-2),
         "-10 / 5"
      );
      Check
      (  From_Integer_64 (-11) / From_Integer_64 (5),
         From_Integer_64 (-2),
         "-11 / 5"
      );
      Check
      (  From_Integer_64 (-12) / From_Integer_64 (5),
         From_Integer_64 (-2),
         "-12 / 5"
      );
      Check
      (  From_Integer_64 (-13) / From_Integer_64 (5),
         From_Integer_64 (-2),
         "-13 / 5"
      );
      Check
      (  From_Integer_64 (-14) / From_Integer_64 (5),
         From_Integer_64 (-2),
         "-14 / 5"
      );
      Check
      (  From_Integer_64 (10) / From_Integer_64 (-5),
         From_Integer_64 (-2),
         "10 / -5"
      );
      Check
      (  From_Integer_64 (11) / From_Integer_64 (-5),
         From_Integer_64 (-2),
         "11 / -5"
      );
      Check
      (  From_Integer_64 (12) / From_Integer_64 (-5),
         From_Integer_64 (-2),
         "12 / -5"
      );
      Check
      (  From_Integer_64 (13) / From_Integer_64 (-5),
         From_Integer_64 (-2),
         "13 / -5"
      );
      Check
      (  From_Integer_64 (14) / From_Integer_64 (-5),
         From_Integer_64 (-2),
         "14 / -5"
      );
      Check
      (  From_Integer_64 (10) / From_Integer_64 (5),
         From_Integer_64 (2),
         "10 / 5"
      );
      Check
      (  From_Integer_64 (11) / From_Integer_64 (5),
         From_Integer_64 (2),
         "11 / 5"
      );
      Check
      (  From_Integer_64 (12) / From_Integer_64 (5),
         From_Integer_64 (2),
         "12 / 5"
      );
      Check
      (  From_Integer_64 (13) / From_Integer_64 (5),
         From_Integer_64 (2),
         "13 / 5"
      );
      Check
      (  From_Integer_64 (14) / From_Integer_64 (5),
         From_Integer_64 (2),
         "14 / 5"
      );
      Check
      (  From_Integer_64 (5) / From_Integer_64 (2),
         From_Integer_64 (2),
         "5 / 2"
      );
      Check
      (  From_Integer_64 (4) / From_Integer_64 (2),
         From_Integer_64 (2),
         "4 / 2"
      );
      Check
      (  From_Integer_64 (999999998000000001),
         Value ("999999998000000001"),
         "conversion 999999998000000001"
      );
      Check
      (  From_Integer_64 (-999999998000000001),
         Value ("-999999998000000001"),
         "conversion -999999998000000001"
      );
      Check
      (  From_Integer_64 (-1), Value ("-1"),
         "conversion -1"
      );
      Check
      (  From_Unsigned_64 (999999998000000001),
         Value ("999999998000000001"),
         "conversion 999999998000000001"
      );
      Check
      (  Value ("999999999") * Value ("999999999"),
         Value ("999999998000000001"),
         "999999999 * 999999999"
      );
      Check
      (  Value ("65535") * Value ("65535"),
         Value ("4294836225"),
         "65535 * 65535"
      );
      Check (Value ("2")  * Value ("2"), Value (" 4"), " 2 * 2");
      Check (Value ("-1") * Value ("1"), Value ("-1"), "-1 * 1");
      Check (Value ("1")  * Value ("1"), Value ( "1"),  "1 * 1");
      Check (Value ("0")  * Value ("0"), Value ( "0"),  "0 * 0");
      Check (Value ("0")  * Value ("1"), Value ( "0"),  "0 * 1");

      Check
      (  Value ("12345678901234565") - Value ("10000000000000000"),
         Value ("2345678901234565"),
         "12345678901234565 - 10000000000000000"
      );
      Check (Value ("-1") - Value ("-1"), Value (" 0"), "-1 - -1");
      Check (Value ( "1") - Value ("-1"), Value ( "2"),  "1 - -1");
      Check (Value ( "1") - Value ( "1"), Value ( "0"),  "1 -  1");

      declare
         X : IEEE_754.Integer_128;
      begin
         X := Integer_128_Last + Value ("1");
         Raise_Exception
         (  Data_Error'Identity,
            "Overflow check error"
         );
      exception
         when Constraint_Error =>
            null;
      end;
      declare
         X : IEEE_754.Integer_128;
      begin
         X := Integer_128_First + Value ("-1");
         Raise_Exception
         (  Data_Error'Identity,
            "Overflow check error"
         );
      exception
         when Constraint_Error =>
            null;
      end;
      Check (Value ("-1") + Value ("-1"), Value ("-2"), "-1 + -1");
      Check (Value ( "1") + Value ("-1"), Value ( "0"),  "1 + -1");
      Check (Value ( "1") + Value ( "1"), Value ( "2"),  "1 +  1");

      Check (Value ("-1"), -Value ("1"),  "Unary minus -1");
      Check (Value ( "1"), -Value ("-1"), "Unary minus -(-1)");

      Check
      (  "-12", "-9",
         LT => True, LE => True, GT => False, GE => False
      );
      Check
      (  "-12", "-12",
         LT => False, LE => True, GT => False, GE => True
      );
      Check
      (  "-12", "12",
         LT => True, LE => True, GT => False, GE => False
      );
      Check
      (  "1234567890", "1234567891",
         LT => True, LE => True, GT => False, GE => False
      );
      Check
      (  "123456", "123456",
         LT => False, LE => True, GT => False, GE => True
      );
      Check
      (  "-1", -24,
         16#B010_0000_0000_0000#, 16#0000_0000_0000_0001#
      );
      Check
      (  "1234567890123456789012345678901234", 6144 - 33,
         16#5FFE_3CDE_6FFF_9732#, 16#DE82_5CD0_7E96_AFF2#
      );
      Check
      (  "9999999999999999999999999999999999", 0,
         16#3041_ED09_BEAD_87C0#, 16#378D_8E63_FFFF_FFFF#
      );
      Check
      (  "1", 6111,
         16#5FFE_0000_0000_0000#, 16#0000_0000_0000_0001#
      );
      Check
      (  "1", 0,
         16#3040_0000_0000_0000#, 16#0000_0000_0000_0001#
      );

      Check_DPD
      (  "-9999999999999999999999999999999999", 6144 - 33,
         16#f7ff_cff3_fcff_3fcf#, 16#f3fc_ff3f_cff3_fcff#
      );
      Check_DPD
      (  "100000000000000000000000000000000", -6143 - 33,
         16#0000_0800_0000_0000#, 16#0000_0000_0000_0000#
      );
      Check_DPD
      (  "1000000000000000000000000000000001", -6143 - 33,
         16#0400_0000_0000_0000#, 16#0000_0000_0000_0001#
      );
      Check_DPD
      (  "1000000000000000000000000000000000", -6143 - 33,
         16#0400_0000_0000_0000#, 16#0000_0000_0000_0000#
      );
      Check_DPD
      (  "1", -6143,
         16#0008_4000_0000_0000#, 16#0000_0000_0000_0001#
      );
      Check_DPD
      (  "12345", -2,
         16#2207_8000_0000_0000#, 16#0000_0000_0000_49c5#
      );
      Check_DPD
      (  "123", -2,
         16#2207_8000_0000_0000#, 16#0000_0000_0000_00a3#
      );
      Check_DPD
      (  "1", 0,
         16#2208_0000_0000_0000#, 16#0000_0000_0000_0001#
      );
      Check_DPD
      (  "12", 0,
         16#2208_0000_0000_0000#, 16#0000_0000_0000_0012#
      );
      Check_DPD
      (  "123", 0,
         16#2208_0000_0000_0000#, 16#0000_0000_0000_00a3#
      );
      Check_DPD
      (  "-750", -2,
         16#A207_8000_0000_0000#, 16#0000_0000_0000_03D0#
      );
      Check_DPD
      (  "-750", 1,
         16#A208_4000_0000_0000#, 16#0000_0000_0000_03D0#
      );
      Check_DPD
      (  "-750", -1,
         16#A207_c000_0000_0000#, 16#0000_0000_0000_03D0#
      );
      Check_DPD
      (  "-750", -3,
         16#A207_4000_0000_0000#, 16#0000_0000_0000_03D0#
      );
      Check_DPD
      (  "-750", -4,
         16#A207_0000_0000_0000#, 16#0000_0000_0000_03D0#
      );
      Check_DPD
      (  "-750", -8,
         16#A206_0000_0000_0000#, 16#0000_0000_0000_03D0#
      );
      Check_DPD
      (  "-750", -9,
         16#A205_c000_0000_0000#, 16#0000_0000_0000_03D0#
      );
      Check_DPD
      (  "1234567890123456789012345678901234", 0,
         16#2608_134b_9c1e_28e5#, 16#6f3c_1271_7782_3534#
      );
      Check_DPD
      (  "-1234567890123456789012345678901234", 0,
         16#a608_134b_9c1e_28e5#, 16#6f3c_1271_7782_3534#
      );
      Check_DPD
      (  "1111111111111111111111111111111111", 0,
         16#2608_0912_4491_2449#, 16#1244_9124_4912_4491#
      );
      Check_DPD
      (  "9999999999999999999999999999999999", 6144 - 33,
         16#77ff_cff3_fcff_3fcf#, 16#f3fc_ff3f_cff3_fcff#
      );
      Check_DPD
      (  "1234567890123456789012345678901234", 6144 - 33,
         16#47ff_d34b_9c1e_28e5#, 16#6f3c_1271_7782_3534#
      );
      Check_DPD
      (  "12345", 0,
         16#2208_0000_0000_0000#, 16#0000_0000_0000_49c5#
      );
      Check_DPD
      (  "1234", 0,
         16#2208_0000_0000_0000#, 16#0000_0000_0000_0534#
      );
   end;
   declare
      use Universally_Unique_Identifiers;
      use Universally_Unique_Identifiers.Edit;

      function "abs" (Value : UUID_Value) return String is
         Result  : String (1..32);
         Pointer : Integer := Result'First;
      begin
         for Index in Value'Range loop
            Put
            (  Destination => Result,
               Pointer     => Pointer,
               Value       => Integer (Value (Index)),
               Base        => 16,
               Field       => 2,
               Justify     => Right,
               Fill        => '0'
            );
         end loop;
         return Result;
      end "abs";
      U : UUID_Value := Create;
      V : UUID_Value;
      T : String (1..80) := (others => ' ');
      P : Integer := 10;
   begin
      if Value (Image (U)) /= U then
         Raise_Exception
         (  Data_Error'Identity,
            Image (U) & "=" & abs U & " /=" & abs Value (Image (U))
         );
      end if;
      Put_Line ("UUID " & Image (U));
      U := Create ("Node A");
      Put_Line ("UUID " & Image (U));
      Put (T, P, U);
      if P /= 10 + 36 then
         Raise_Exception
         (  Data_Error'Identity,
            "Put pointer" & Image (P) & "/= 10 + 36"
         );
      end if;
      P := 10;
      Get (T, P, V);
      if P /= 10 + 36 then
         Raise_Exception
         (  Data_Error'Identity,
            "Get pointer" & Image (P) & "/= 10 + 36"
         );
      elsif V /= U then
         Raise_Exception
         (  Data_Error'Identity,
            abs U & " /=" & abs V
         );
      end if;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_IEEE_754;
