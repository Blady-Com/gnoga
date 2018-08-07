--                                                                    --
--  procedure Test_DDL              Copyright (c)  Dmitry A. Kazakov  --
--                                                 Luebeck            --
--  Test                                           Autumn, 2016       --
--                                                                    --
--                                Last revision :  11:20 04 Aug 2018  --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Streams;                    use Ada.Streams;
with Ada.Text_IO.Text_Streams;       use Ada.Text_IO;
with Interfaces;                     use Interfaces;
with Strings_Edit.ChaCha20;          use Strings_Edit.ChaCha20;
with Strings_Edit.ChaCha20.AEAD;    use Strings_Edit.ChaCha20.AEAD;
with Strings_Edit.ChaCha20.Poly1305; use Strings_Edit.ChaCha20.Poly1305;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Strings_Edit.Quoted;            use Strings_Edit.Quoted;

procedure Test_ChaCha20 is

   function Image (Data : Stream_Element_Array) return String is
      Result  : String (1..Data'Length * 2);
      Pointer : Integer := Result'First;
   begin
      for Index in Data'Range loop
         Put
         (  Destination => Result,
            Pointer     => Pointer,
            Value       => Integer (Data (Index)),
            Base        => 16,
            Fill        => '0',
            Field       => 2,
            Justify     => Strings_Edit.Right
         );
      end loop;
      return Result;
   end Image;

   function "+" (Text : String) return Stream_Element_Array is
      Result  : Stream_Element_Array (1..Text'Length / 2);
      Pointer : Integer := Text'First;
   begin
      for Index in Result'Range loop
         Result (Index) :=
            Stream_Element
            (  Value
               (  Text (Pointer..Pointer + 1),
                  Base => 16
            )  );
         Pointer := Pointer + 2;
      end loop;
      return Result;
   end "+";

   function "+" (Data : Stream_Element_Array) return String is
      Result : String (1..Data'Length);
   begin
      for Index in Result'Range loop
         Result (Index) :=
            Character'Val
            (  Data (Data'First + Stream_Element_Offset (Index) - 1)
            );
      end loop;
      return Result;
   end "+";

   procedure Check
             (  Key    : String;
                Nonce  : String;
                Count  : Unsigned_32;
                Stream : String
             )  is
      Test_Stream : constant Stream_Element_Array := +Stream;
      Cipher      : aliased ChaCha20_Cipher;
   begin
      Set_Key (Cipher, +Key, +Nonce, Count);
      declare
         This : constant Stream_Element_Array :=
                Get_Key_Stream (Cipher'Access);
      begin
         if This /= Test_Stream then
            Put_Line ("Key stream " & Image (This));
            Put_Line ("Expected   " & Stream);
            Raise_Exception
            (  Data_Error'Identity,
               (  "Error key stream "
               &  ", key "
               &  Key
               &  ", nonce "
               &  Nonce
            )  );
         end if;
      end;
   end Check;

   procedure Check
             (  Key    : String;
                Nonce  : String;
                Count  : Unsigned_32;
                Input  : Stream_Element_Array;
                Output : Stream_Element_Array
             )  is
      Cipher : ChaCha20_Cipher;
      Result : Stream_Element_Array (Output'Range);
   begin
      Set_Key (Cipher, +Key, +Nonce, Count);
      Encrypt (Cipher, Input, Result);
      if Output /= Result then
         Put_Line ("Result   " & Image (Result));
         Put_Line ("Expected " & Image (Output));
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error encryption "
            &  ", key "
            &  Key
            &  ", nonce "
            &  Nonce
         )  );
      end if;
   end Check;

   procedure Check
             (  Key    : String;
                Nonce  : String;
                Count  : Unsigned_32;
                Input  : String;
                Output : Stream_Element_Array
             )  is
      Cipher : ChaCha20_Cipher;
      Result : Stream_Element_Array (Output'Range);
   begin
      Set_Key (Cipher, +Key, +Nonce, Count);
      Encrypt (Cipher, Input, Result);
      if Output /= Result then
         Put_Line ("Result   " & Image (Result));
         Put_Line ("Expected " & Image (Output));
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error encryption "
            &  ", key "
            &  Key
            &  ", nonce "
            &  Nonce
         )  );
      end if;
   end Check;

   procedure Check
             (  Key    : String;
                Input  : Stream_Element_Array;
                Digest : ChaCha20_Tag
             )  is
      Tag : ChaCha20_Tag;
   begin
      Tag := Strings_Edit.ChaCha20.Poly1305.Digest (Input, +Key);
      if Tag /= Digest then
         Put_Line ("Result   " & Image (Tag));
         Put_Line ("Expected " & Image (Digest));
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error Poly1305 "
            &  ", key "
            &  Key
         )  );
      end if;
   end Check;

   procedure Check
             (  Key    : String;
                Input  : String;
                Digest : ChaCha20_Tag
             )  is
      Tag : ChaCha20_Tag;
   begin
      Tag := Strings_Edit.ChaCha20.Poly1305.Digest (Input, +Key);
      if Tag /= Digest then
         Put_Line ("Result   " & Image (Tag));
         Put_Line ("Expected " & Image (Digest));
         Raise_Exception
         (  Data_Error'Identity,
            (  "Error Poly1305 "
            &  ", key "
            &  Key
         )  );
      end if;
   end Check;
begin
   Check
   (  Key    => "000102030405060708090a0b0c0d0e0f" &
                "101112131415161718191a1b1c1d1e1f",
      Nonce  => "000000090000004a00000000",
      Count  => 1,
      Stream => "10f1e7e4d13b5915500fdd1fa32071c4" &
                "c7d1f4c733c068030422aa9ac3d46c4e" &
                "d2826446079faa0914c2d705d98b02a2" &
                "b5129cd1de164eb9cbd083e8a2503c4e"
   );
   Check
   (  Key    => "00000000000000000000000000000000" &
                "00000000000000000000000000000000",
      Nonce  => "000000000000000000000000",
      Count  => 0,
      Stream => "76b8e0ada0f13d90405d6ae55386bd28" &
                "bdd219b8a08ded1aa836efcc8b770dc7" &
                "da41597c5157488d7724e03fb8d84a37" &
                "6a43b8f41518a11cc387b669b2ee6586"
   );
   Check
   (  Key    => "00000000000000000000000000000000" &
                "00000000000000000000000000000000",
      Nonce  => "000000000000000000000000",
      Count  => 1,
      Stream => "9f07e7be5551387a98ba977c732d080d" &
                "cb0f29a048e3656912c6533e32ee7aed" &
                "29b721769ce64e43d57133b074d839d5" &
                "31ed1f28510afb45ace10a1f4b794d6f"
   );
   Check
   (  Key    => "00000000000000000000000000000000" &
                "00000000000000000000000000000001",
      Nonce  => "000000000000000000000000",
      Count  => 1,
      Stream => "3aeb5224ecf849929b9d828db1ced4dd" &
                "832025e8018b8160b82284f3c949aa5a" &
                "8eca00bbb4a73bdad192b5c42f73f2fd" &
                "4e273644c8b36125a64addeb006c13a0"
   );
   Check
   (  Key    => "00FF0000000000000000000000000000" &
                "00000000000000000000000000000000",
      Nonce  => "000000000000000000000000",
      Count  => 2,
      Stream => "72d54dfbf12ec44b362692df94137f32" &
                "8fea8da73990265ec1bbbea1ae9af0ca" &
                "13b25aa26cb4a648cb9b9d1be65b2c09" &
                "24a66c54d545ec1b7374f4872e99f096"
   );
   Check
   (  Key    => "00000000000000000000000000000000" &
                "00000000000000000000000000000000",
      Nonce  => "000000000000000000000002",
      Count  => 0,
      Stream => "c2c64d378cd536374ae204b9ef933fcd" &
                "1a8b2288b3dfa49672ab765b54ee27c7" &
                "8a970e0e955c14f3a88e741b97c286f7" &
                "5f8fc299e8148362fa198a39531bed6d"
   );
   Check
   (  Key    => "00000000000000000000000000000000" &
                "00000000000000000000000000000000",
      Nonce  => "000000000000000000000000",
      Count  => 0,
      Input  => + (  "00000000000000000000000000000000"
                  &  "00000000000000000000000000000000"
                  &  "00000000000000000000000000000000"
                  &  "00000000000000000000000000000000"
                  ),
      Output => + (  "76b8e0ada0f13d90405d6ae55386bd28"
                  &  "bdd219b8a08ded1aa836efcc8b770dc7"
                  &  "da41597c5157488d7724e03fb8d84a37"
                  &  "6a43b8f41518a11cc387b669b2ee6586"
   )              );
   Check
   (  Key   =>      "00000000000000000000000000000000" &
                    "00000000000000000000000000000000",
      Input => + (  "00000000000000000000000000000000"
                 &  "00000000000000000000000000000000"
                 &  "00000000000000000000000000000000"
                 &  "00000000000000000000000000000000"
                 ),
      Digest => +   "00000000000000000000000000000000"
   );
   Check
   (  Key   =>   "00000000000000000000000000000000" &
                 "36e5f6b5c5e06070f0efca96227a863e",
      Input =>   "Any submission to the IETF inten" &
                 "ded by the Contributor for publi" &
                 "cation as all or part of an IETF" &
                 " Internet-Draftor RFC and any s " &
                 "tatement made within the context" &
                 " of an IETF activity is consider" &
                 "ed an ""IETF Contribution"". Such " &
                 "statements include oral statemen" &
                 "ts in IETF sessions, as well as " &
                 "written and electronic communica" &
                 "tions made at any time or place," &
                 " which are addressed to",
      Digest => +"36e5f6b5c5e06070f0efca96227a863e"
   );
   Check
   (  Key   =>   "1c9240a5eb55d38af333888604f6b5f0" &
                 "473917c1402b80099dca5cbc207075c0",
      Input => +  (  "2754776173206272696c6c69672c2061"
                  &  "6e642074686520736c6974687920746f"
                  &  "7665730a446964206779726520616e64"
                  &  "2067696d626c6520696e207468652077"
                  &  "6162653a0a416c6c206d696d73792077"
                  &  "6572652074686520626f726f676f7665"
                  &  "732c0a416e6420746865206d6f6d6520"
                  &  "7261746873206f757467726162652e"
                  ),
      Digest => + "4541669a7eaaee61e708dc7cbcc5eb62"
   );
   declare
      Cipher_1 : aliased ChaCha20_Cipher;
      Cipher_2 : aliased ChaCha20_Cipher;
      This     : constant String :=
                          "The quick brown fox jumps over the lazy dog";
      That     : constant String :=
                          Decrypt
                          (  Cipher_1'Access,
                             Encrypt (Cipher_2'Access, This)
                          );
   begin
      if This /= That then
         Raise_Exception
         (  Data_Error'Identity,
            "Got " & That & " expected " & This
         );
      end if;
   end;
   declare
      File_1 : File_Type;
      File_2 : File_Type;
   begin
      Open (File_1, In_File, "test_chacha20.adb", "shared=no");
      Open (File_2, In_File, "test_chacha20.adb", "shared=no");
      declare
         use Ada.Text_IO.Text_Streams;
         Input_1  : Root_Stream_Type'Class renames Stream (File_1).all;
         Input_2  : aliased ChaCha20_Stream (Stream (File_2), 20);
         Cipher   : ChaCha20_Cipher;
         Buffer_1 : String (1..64 + 32);
         Buffer_2 : String (1..64 + 32);
      begin
         loop
            String'Read (Input_1'Access, Buffer_1);
            String'Read (Input_2'Access, Buffer_2);
            Encrypt (Cipher, Buffer_2);
            if Buffer_1 /= Buffer_2 then
               Put_Line ("Got        " & Quote (Buffer_2));
               Put_Line ("Expected   " & Quote (Buffer_1));
               Raise_Exception
               (  Data_Error'Identity,
                  "Stream test error"
               );
            end if;
         end loop;
      end;
   exception
      when End_Error =>
         Close (File_1);
         Close (File_2);
   end;
   if (  +"a8061dc1305136c6c22b8baf0c0127a9"
      /= Digest
         (  Key => + ( "85d6be7857556d337f4452fe42d506a8"
                     & "0103808afb0db2fd4abff6af4149f51b"
                     ),
            Message => "Cryptographic Forum Research Group"
      )  )  then
      Raise_Exception (Data_Error'Identity, "Poly1305 digest error");
   end if;
   if (  Digest
         (  Key => + ( "8ad5a08b905f81cc815040274ab29471"
                     & "a833b637e3fd0da508dbb8e2fdd1a646"
                     ),
            Message => "Cryptographic Forum Research Group"
         )
      /= Digest
         (  Key => + ( "808182838485868788898a8b8c8d8e8f" &
                       "909192939495969798999a9b9c9d9e9f"
                     ),
            Nonce => +"000000000001020304050607",
            Message => "Cryptographic Forum Research Group"
      )  )  then
      Raise_Exception
      (  Data_Error'Identity,
         "Poly1305 one-time key digest error"
      );
   end if;
   declare
      Cipher_1 : ChaCha20_Cipher;
      Cipher_2 : ChaCha20_Cipher;
      Input    : constant String (1..114) :=
                          "Ladies and Gentlemen of the class " &
                          "of '99: If I could offer you only " &
                          "one tip for the future, sunscreen " &
                          "would be it.";
      Output   : String (1..114);
      Data     : constant String := +(+"50515253c0c1c2c3c4c5c6c7");
      Key      : constant ChaCha20_Key :=
                         + (  "808182838485868788898a8b8c8d8e8f" &
                              "909192939495969798999a9b9c9d9e9f"
                           );
      Nonce    : constant ChaCha20_Nonce := +"070000004041424344454647";
      Message  : Stream_Element_Array (1..Input'Length + 16);
   begin
      Set_Key (Cipher_1, Key, Nonce);
      Set_Key (Cipher_2, Key, Nonce);
      Encrypt (Cipher_1, Input, Data, Message);
      Decrypt (Cipher_2, Message, Data, Output);
      if Input /= Output then
         Raise_Exception
         (  Data_Error'Identity,
            "AEAD error"
         );
      end if;
   end;
exception
   when Error : others =>
      Put_Line ("Fatal error: " & Exception_Information (Error));
end Test_ChaCha20;
