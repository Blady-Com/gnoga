--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Base64                                 Luebeck            --
--  Test                                           Autumn, 2014       --
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
with Ada.Streams;            use Ada.Streams;
with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit.Base64;    use Strings_Edit.Base64;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

procedure Test_Base64 is
   Encoder : aliased Base64_Encoder (10);
   Decoder : aliased Base64_Decoder (10);

   function To_String (Data : Stream_Element_Array) return String is
      Result  : String (1..Data'Length);
      Pointer : Integer := 1;
   begin
      for Index in Data'Range loop
         Result (Pointer) := Character'Val (Data (Index));
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end To_String;

   function "+" (Data : String) return Stream_Element_Array is
      use Strings_Edit;
      Result  : Stream_Element_Array (1..Data'Length);
      Element : Stream_Element_Offset := 1;
      Pointer : Integer := Data'First;
   begin
      Get (Data, Pointer);
      while Pointer <= Data'Last loop
         Get (Data, Pointer, Integer (Result (Element)), Base => 16);
         Get (Data, Pointer);
         Element := Element + 1;
      end loop;
      return Result (1..Element - 1);
   end "+";

   function "+"
            (  Left  : Stream_Element_Array;
               Right : String
            )  return Stream_Element_Array is
   begin
      return Left & (+Right);
   end "+";

   procedure Check_1 (Plain, Encoded : String) is
   begin
      Put_Line ("-- checking ------------ " & Plain & " vs " & Encoded);
      if To_Base64 (Plain) /= Encoded then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Encoding '"
            &  Plain
            &  "' error, got '"
            &  To_Base64 (Plain)
            &  "', expected '"
            &  Encoded
            &  '''
         )  );
      end if;
      if From_Base64 (Encoded) /= Plain then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Decoding '"
            &  Encoded
            &  "' error, got '"
            &  From_Base64 (Encoded)
            &  "', expected '"
            &  Plain
            &  '''
         )  );
      end if;
      ------------------------------------------------------------------
      Reset (Encoder);
      declare
         Pointer : Integer := Encoded'First;
         This    : Character;
         procedure Do_Read is
         begin
            while not Is_Empty (Encoder) loop
               Character'Read (Encoder'Access, This);
               if Pointer > Encoded'Last then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Encoding stream read overrun"
                  );
               elsif Encoded (Pointer) /= This then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Encoding stream read '"
                     &  This
                     &  "' error, '"
                     &  Encoded (Pointer)
                     &  "', expected"
                  )  );
               end if;
               Pointer := Pointer + 1;
            end loop;
         end Do_Read;
      begin
         for Index in Plain'Range loop
            Character'Write (Encoder'Access, Plain (Index));
            Do_Read;
         end loop;
         Flush (Encoder);
         Do_Read;
         if Pointer <= Encoded'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Encoding stream left unmatched "
               &  Encoded (Pointer..Encoded'Last)
            )  );
         end if;
      end;
      ------------------------------------------------------------------
      Reset (Decoder);
      declare
         Pointer : Integer := Plain'First;
         This    : Character;
         procedure Do_Read is
         begin
            while not Is_Empty (Decoder) loop
               Character'Read (Decoder'Access, This);
               if Pointer > Plain'Last then
                  Raise_Exception
                  (  Data_Error'Identity,
                     "Decoding stream read overrun"
                  );
               elsif Plain (Pointer) /= This then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Decoding stream read '"
                     &  This
                     &  "' error, '"
                     &  Plain (Pointer)
                     &  "', expected"
                  )  );
               end if;
               Pointer := Pointer + 1;
            end loop;
         end Do_Read;
      begin
         for Index in Encoded'Range loop
            Character'Write (Decoder'Access, Encoded (Index));
            Do_Read;
         end loop;
         Do_Read;
         if Pointer <= Plain'Last then
            Raise_Exception
            (  Data_Error'Identity,
               (  "Decoding stream left unmatched "
               &  Plain (Pointer..Plain'Last)
            )  );
         end if;
      end;
   end Check_1;

   procedure Check_2 (Encoded, Plain : String) is
   begin
      if From_Base64 (Encoded) /= Plain then
         Raise_Exception
         (  Data_Error'Identity,
            (  "Decoding '"
            &  Encoded
            &  "' error, got '"
            &  From_Base64 (Encoded)
            &  "', expected '"
            &  Plain
            &  '''
         )  );
      end if;
   end Check_2;
begin
   Check_2
   (  "Uf7NCJ0B1FSwiIiV5OoSi9HrCMBqtQPDmdef21qfZBk=",
      To_String (
     +"51 fe cd 08 9d 01 d4 54 b0 88 88 95 e4 ea 12 8b d1 eb 08 c0 6a"
     +"b5 03 c3 99 d7 9f db 5a 9f 64 19")
   );

   Check_1 ("Man",                  "TWFu");
   Check_1 ("sure.",                "c3VyZS4=");
   Check_1 ("asure.",               "YXN1cmUu");
   Check_1 ("any carnal pleasure.", "YW55IGNhcm5hbCBwbGVhc3VyZS4=");
   Check_1 ("any carnal pleasure",  "YW55IGNhcm5hbCBwbGVhc3VyZQ==");
   Check_1 ("any carnal pleasur",   "YW55IGNhcm5hbCBwbGVhc3Vy");
   Check_1 ("any carnal pleasu",    "YW55IGNhcm5hbCBwbGVhc3U=");
   Check_1 ("any carnal pleas",     "YW55IGNhcm5hbCBwbGVhcw==");
   Check_1 ("pleasure.",            "cGxlYXN1cmUu");
   Check_1 ("leasure.",             "bGVhc3VyZS4=");
   Check_1 ("easure.",              "ZWFzdXJlLg==");

   Check_2 ("YW55IGNhcm5hbCBwbGVhcw",   "any carnal pleas");
   Check_2 ("YW55IGNhcm5hbCBwbGVhc3U",  "any carnal pleasu");
   Check_2 ("YW55IGNhcm5hbCBwbGVhc3Vy", "any carnal pleasur");

   Check_1
   (  "The key difference between the BER format and the CER "   &
      "or DER formats is the flexibility provided by the Basic " &
      "Encoding Rules",
      "VGhlIGtleSBkaWZmZXJlbmNlIGJldHdlZW4gdGhlIEJFUiBmb3JtYXQg" &
      "YW5kIHRoZSBDRVIgb3IgREVSIGZvcm1hdHMgaXMgdGhlIGZsZXhpYmls" &
      "aXR5IHByb3ZpZGVkIGJ5IHRoZSBCYXNpYyBFbmNvZGluZyBSdWxlcw=="
   );
   Check_1
   (  To_String (
     +"30 82 04 f7 30 82 03 df a0 03 02 01 02 02 09 00 82 ed bf 41 c8"
     +"80 91 9d 30 0d 06 09 2a 86 48 86 f7 0d 01 01 05 05 00 30 4d 31"
     +"0b 30 09 06 03 55 04 06 13 02 58 59 31 26 30 24 06 03 55 04 0a"
     +"0c 1d 50 79 74 68 6f 6e 20 53 6f 66 74 77 61 72 65 20 46 6f 75"
     +"6e 64 61 74 69 6f 6e 20 43 41 31 16 30 14 06 03 55 04 03 0c 0d"
     +"6f 75 72 2d 63 61 2d 73 65 72 76 65 72 30 1e 17 0d 31 38 30 31"
     +"31 39 31 39 30 39 30 36 5a 17 0d 32 37 31 31 32 38 31 39 30 39"
     +"30 36 5a 30 62 31 0b 30 09 06 03 55 04 06 13 02 58 59 31 17 30"
     +"15 06 03 55 04 07 0c 0e 43 61 73 74 6c 65 20 41 6e 74 68 72 61"
     +"78 31 23 30 21 06 03 55 04 0a 0c 1a 50 79 74 68 6f 6e 20 53 6f"
     +"66 74 77 61 72 65 20 46 6f 75 6e 64 61 74 69 6f 6e 31 15 30 13"
     +"06 03 55 04 03 0c 0c 66 61 6b 65 68 6f 73 74 6e 61 6d 65 30 82"
     +"01 22 30 0d 06 09 2a 86 48 86 f7 0d 01 01 01 05 00 03 82 01 0f"
     +"00 30 82 01 0a 02 82 01 01 00 c7 ff be a1 64 06 47 e1 c4 95 0a"
     +"65 59 6b 91 c4 a8 a0 a3 08 aa dc 3f 00 44 0a cf 41 4b ed e2 6f"
     +"03 78 c7 d3 67 14 d3 fe b4 27 85 1a 02 57 97 b5 86 16 87 97 82"
     +"0d e8 2b ed 69 16 c3 96 a6 c4 47 b8 5d c1 76 a9 02 9d 28 52 c9"
     +"d9 93 f1 b2 da 18 7c 0a 78 bd 1c 3f 12 4f e5 3f ee 5b b7 b3 3c"
     +"a5 b6 55 65 c6 33 09 b0 45 a4 49 f5 d8 f6 54 25 2e db 4b c3 65"
     +"64 86 16 bf 73 8e 78 e8 25 cc c1 b4 7d d1 b1 55 20 a4 ae de b8"
     +"41 ee 43 9a 48 33 d4 12 39 a0 a8 fb 91 4f 47 bb 3b d6 75 3b 62"
     +"a6 c5 5a d7 7e 36 10 bc 3a 0d 46 e3 de c1 df 50 d0 94 77 22 17"
     +"a0 51 9b af dc 34 60 26 1d a0 04 ac a5 06 04 57 64 1a d1 4f 1c"
     +"19 f8 be 4b 68 12 0f 5f c3 12 40 64 bb c6 a6 84 d9 5a b0 78 1d"
     +"1b 11 c1 54 e6 15 d1 af 2b e2 57 2b 6d b9 c4 5d b7 01 74 57 a7"
     +"fb 27 85 2b 92 15 e2 e3 13 97 29 c8 92 7b 02 03 01 00 01 a3 82"
     +"01 c3 30 82 01 bf 30 17 06 03 55 1d 11 04 10 30 0e 82 0c 66 61"
     +"6b 65 68 6f 73 74 6e 61 6d 65 30 0e 06 03 55 1d 0f 01 01 ff 04"
     +"04 03 02 05 a0 30 1d 06 03 55 1d 25 04 16 30 14 06 08 2b 06 01"
     +"05 05 07 03 01 06 08 2b 06 01 05 05 07 03 02 30 0c 06 03 55 1d"
     +"13 01 01 ff 04 02 30 00 30 1d 06 03 55 1d 0e 04 16 04 14 f8 76"
     +"79 cb 11 85 f0 46 e5 95 e6 7e 69 cb 12 5e 4e aa ec 4d 30 7d 06"
     +"03 55 1d 23 04 76 30 74 80 14 9a cf cf 6e eb 71 3d db 3c f1 ae"
     +"88 6b 56 72 03 cb 08 a7 48 a1 51 a4 4f 30 4d 31 0b 30 09 06 03"
     +"55 04 06 13 02 58 59 31 26 30 24 06 03 55 04 0a 0c 1d 50 79 74"
     +"68 6f 6e 20 53 6f 66 74 77 61 72 65 20 46 6f 75 6e 64 61 74 69"
     +"6f 6e 20 43 41 31 16 30 14 06 03 55 04 03 0c 0d 6f 75 72 2d 63"
     +"61 2d 73 65 72 76 65 72 82 09 00 82 ed bf 41 c8 80 91 9b 30 81"
     +"83 06 08 2b 06 01 05 05 07 01 01 04 77 30 75 30 3c 06 08 2b 06"
     +"01 05 05 07 30 02 86 30 68 74 74 70 3a 2f 2f 74 65 73 74 63 61"
     +"2e 70 79 74 68 6f 6e 74 65 73 74 2e 6e 65 74 2f 74 65 73 74 63"
     +"61 2f 70 79 63 61 63 65 72 74 2e 63 65 72 30 35 06 08 2b 06 01"
     +"05 05 07 30 01 86 29 68 74 74 70 3a 2f 2f 74 65 73 74 63 61 2e"
     +"70 79 74 68 6f 6e 74 65 73 74 2e 6e 65 74 2f 74 65 73 74 63 61"
     +"2f 6f 63 73 70 2f 30 43 06 03 55 1d 1f 04 3c 30 3a 30 38 a0 36"
     +"a0 34 86 32 68 74 74 70 3a 2f 2f 74 65 73 74 63 61 2e 70 79 74"
     +"68 6f 6e 74 65 73 74 2e 6e 65 74 2f 74 65 73 74 63 61 2f 72 65"
     +"76 6f 63 61 74 69 6f 6e 2e 63 72 6c 30 0d 06 09 2a 86 48 86 f7"
     +"0d 01 01 05 05 00 03 82 01 01 00 6d 50 8d fb ee 4e 93 8b eb 47"
     +"56 ba 38 cc 80 e1 9d c7 e1 9e 1f 9c 22 0c d2 08 9b ed bf 31 d9"
     +"00 ee af 8c 56 78 92 d1 7c ba 4e 81 7f 82 1f f4 68 99 86 91 c6"
     +"cb 57 d3 b9 41 12 fa 75 53 fd 22 32 21 50 af 6b 4c b1 34 36 d1"
     +"a8 25 0a d0 f0 f8 81 7d 69 58 6e af e3 d2 c4 32 87 79 d7 cd ad"
     +"0c 56 f3 15 27 10 0c f9 57 59 53 00 ed af 5d 4d 07 86 7a e5 f3"
     +"97 88 bc 86 b4 f1 17 46 33 55 28 66 7b 70 d3 a5 12 b9 4f c7 ed"
     +"e6 13 20 2d f0 9e ec 17 64 cf fd 13 14 1b 76 ba 64 ac c5 51 b6"
     +"cd 13 0a 93 b1 fd 43 09 a0 0b 44 6c 77 45 43 0b e5 ed 70 b2 76"
     +"dc 08 4a 5b 73 5f c1 fc 7f 63 70 f8 b9 ca 3c 98 06 5f fd 98 d1"
     +"e4 e6 61 5f 09 8f 6c 18 86 98 9c cb 3f 73 7b 3f 38 f5 a7 09 20"
     +"ee a5 63 1c ff 8b a6 d1 8c e8 f4 84 3d 99 38 0f cc e0 52 03 f9"
     +"18 05 23 76 39 de 52 ce 8e fb a6 6e f5 4f c3"),
     "MIIE9zCCA9+gAwIBAgIJAILtv0HIgJGdMA0GCSqGSIb3DQEBBQUAME0xCzAJBgNV"&
     "BAYTAlhZMSYwJAYDVQQKDB1QeXRob24gU29mdHdhcmUgRm91bmRhdGlvbiBDQTEW"&
     "MBQGA1UEAwwNb3VyLWNhLXNlcnZlcjAeFw0xODAxMTkxOTA5MDZaFw0yNzExMjgx"&
     "OTA5MDZaMGIxCzAJBgNVBAYTAlhZMRcwFQYDVQQHDA5DYXN0bGUgQW50aHJheDEj"&
     "MCEGA1UECgwaUHl0aG9uIFNvZnR3YXJlIEZvdW5kYXRpb24xFTATBgNVBAMMDGZh"&
     "a2Vob3N0bmFtZTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMf/vqFk"&
     "BkfhxJUKZVlrkcSooKMIqtw/AEQKz0FL7eJvA3jH02cU0/60J4UaAleXtYYWh5eC"&
     "Degr7WkWw5amxEe4XcF2qQKdKFLJ2ZPxstoYfAp4vRw/Ek/lP+5bt7M8pbZVZcYz"&
     "CbBFpEn12PZUJS7bS8NlZIYWv3OOeOglzMG0fdGxVSCkrt64Qe5Dmkgz1BI5oKj7"&
     "kU9HuzvWdTtipsVa1342ELw6DUbj3sHfUNCUdyIXoFGbr9w0YCYdoASspQYEV2Qa"&
     "0U8cGfi+S2gSD1/DEkBku8amhNlasHgdGxHBVOYV0a8r4lcrbbnEXbcBdFen+yeF"&
     "K5IV4uMTlynIknsCAwEAAaOCAcMwggG/MBcGA1UdEQQQMA6CDGZha2Vob3N0bmFt"&
     "ZTAOBgNVHQ8BAf8EBAMCBaAwHQYDVR0lBBYwFAYIKwYBBQUHAwEGCCsGAQUFBwMC"&
     "MAwGA1UdEwEB/wQCMAAwHQYDVR0OBBYEFPh2ecsRhfBG5ZXmfmnLEl5OquxNMH0G"&
     "A1UdIwR2MHSAFJrPz27rcT3bPPGuiGtWcgPLCKdIoVGkTzBNMQswCQYDVQQGEwJY"&
     "WTEmMCQGA1UECgwdUHl0aG9uIFNvZnR3YXJlIEZvdW5kYXRpb24gQ0ExFjAUBgNV"&
     "BAMMDW91ci1jYS1zZXJ2ZXKCCQCC7b9ByICRmzCBgwYIKwYBBQUHAQEEdzB1MDwG"&
     "CCsGAQUFBzAChjBodHRwOi8vdGVzdGNhLnB5dGhvbnRlc3QubmV0L3Rlc3RjYS9w"&
     "eWNhY2VydC5jZXIwNQYIKwYBBQUHMAGGKWh0dHA6Ly90ZXN0Y2EucHl0aG9udGVz"&
     "dC5uZXQvdGVzdGNhL29jc3AvMEMGA1UdHwQ8MDowOKA2oDSGMmh0dHA6Ly90ZXN0"&
     "Y2EucHl0aG9udGVzdC5uZXQvdGVzdGNhL3Jldm9jYXRpb24uY3JsMA0GCSqGSIb3"&
     "DQEBBQUAA4IBAQBtUI377k6Ti+tHVro4zIDhncfhnh+cIgzSCJvtvzHZAO6vjFZ4"&
     "ktF8uk6Bf4If9GiZhpHGy1fTuUES+nVT/SIyIVCva0yxNDbRqCUK0PD4gX1pWG6v"&
     "49LEMod5182tDFbzFScQDPlXWVMA7a9dTQeGeuXzl4i8hrTxF0YzVShme3DTpRK5"&
     "T8ft5hMgLfCe7Bdkz/0TFBt2umSsxVG2zRMKk7H9QwmgC0Rsd0VDC+XtcLJ23AhK"&
     "W3Nfwfx/Y3D4uco8mAZf/ZjR5OZhXwmPbBiGmJzLP3N7Pzj1pwkg7qVjHP+LptGM"&
     "6PSEPZk4D8zgUgP5GAUjdjneUs6O+6Zu9U/D"
   );

end Test_Base64;
