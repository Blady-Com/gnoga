--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, Michael Rohan <mrohan@zanyblue.com>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--    * Neither the name of ZanyBlue nor the names of its contributors may
--      be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--
--  The ZanyBlue.Text.Codecs supports string encoding and decoding for
--  various encoding schemes, e.g., UTF-8, etc.
--

with ZanyBlue.OS;

package body ZanyBlue.Text.Codecs is

   use ZanyBlue.OS;

   type Encoder_Type is
      access function (Value       : Wide_String;
                       Replacement : Character) return String;
   type Decoder_Type is
      access function (Value : String) return Wide_String;

   type Codecs_Table_Entry is
      record
         Encoding : access Wide_String;
         Replacement : Character;
         Encoder : Encoder_Type;
         Decoder : Decoder_Type;
      end record;

   Unsupported_Encoding_Action : Unsupported_Encoding_Action_Type := Use_UTF8;
   Unicode_Encode_Action : Unicode_Encode_Action_Type := Replace;

   Default_Codecs_Index_Value : Positive := 1;

   function Decode_ASCII (Value : String) return Wide_String;
   function Encode_ASCII (Value       : Wide_String;
                          Replacement : Character) return String;

   function Decode_ISO8859_1 (Value : String) return Wide_String;
   function Encode_ISO8859_1 (Value       : Wide_String;
                              Replacement : Character) return String;

   function Decode_UTF8 (Value : String) return Wide_String;
   function Encode_UTF8 (Value       : Wide_String;
                         Replacement : Character) return String;
   function Find_Codecs (Encoding     : Wide_String;
                         Always_Raise : Boolean := False) return Positive;

   Codecs_Table : constant array (Positive range <>) of Codecs_Table_Entry := (
       (Encoding => new Wide_String'("UTF-8"),
        Replacement => '?',
        Encoder => Encode_UTF8'Access,
        Decoder => Decode_UTF8'Access),
       (Encoding => new Wide_String'("ISO8859-1"),
        Replacement => '?',
        Encoder => Encode_ISO8859_1'Access,
        Decoder => Decode_ISO8859_1'Access),
       (Encoding => new Wide_String'("ASCII"),
        Encoder => Encode_ASCII'Access,
        Replacement => '?',
        Decoder => Decode_ASCII'Access));

   ------------
   -- Decode --
   ------------

   function Decode (Encoding : Wide_String;
                    Value : String) return Wide_String is
   begin
      return Decode (Make_Codecs (Encoding), Value);
   end Decode;

   ------------
   -- Decode --
   ------------

   function Decode (Codecs : Codecs_Type;
                    Value : String) return Wide_String is
   begin
      return Codecs_Table (Codecs.Encoding_Index).Decoder (Value);
   end Decode;

   ------------------
   -- Decode_ASCII --
   ------------------

   function Decode_ASCII (Value : String) return Wide_String is
   begin
      return Result : Wide_String (1 .. Value'Length) do
         for I in Result'Range loop
            declare
               Ch : constant Character := Value (Value'First + I - 1);
               Code_Point : constant Natural := Character'Pos (Ch);
            begin
               Result (I) := Wide_Character'Val (Code_Point);
            end;
         end loop;
      end return;
   end Decode_ASCII;

   ----------------------
   -- Decode_ISO8859_1 --
   ----------------------

   function Decode_ISO8859_1 (Value : String) return Wide_String is
   begin
      return Result : Wide_String (1 .. Value'Length) do
         for I in Result'Range loop
            declare
               Ch : constant Character := Value (Value'First + I - 1);
               Code_Point : constant Natural := Character'Pos (Ch);
            begin
               Result (I) := Wide_Character'Val (Code_Point);
            end;
         end loop;
      end return;
   end Decode_ISO8859_1;

   -----------------
   -- Decode_UTF8 --
   -----------------

   function Decode_UTF8 (Value : String) return Wide_String is
   begin
      return From_UTF8 (Value);
   end Decode_UTF8;

   --------------------------
   -- Default_Codecs_Index --
   --------------------------

   function Default_Codecs_Index return Positive is
   begin
      return Default_Codecs_Index_Value;
   end Default_Codecs_Index;

   ------------
   -- Encode --
   ------------

   function Encode (Encoding : Wide_String;
                    Value : Wide_String) return String is
   begin
      return Encode (Make_Codecs (Encoding), Value);
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode (Codecs : Codecs_Type;
                    Value : Wide_String) return String is
      CE : constant Codecs_Table_Entry := Codecs_Table (Codecs.Encoding_Index);
   begin
      return CE.Encoder (Value, CE.Replacement);
   end Encode;

   ------------------
   -- Encode_ASCII --
   ------------------

   function Encode_ASCII (Value       : Wide_String;
                          Replacement : Character) return String is
   begin
      return Result : String (1 .. Value'Length) do
         for I in Result'Range loop
            declare
               WCh : constant Wide_Character := Value (Value'First + I - 1);
               Wide_Code_Point : constant Natural := Wide_Character'Pos (WCh);
               Overflow : constant Boolean := Wide_Code_Point > 128;
               Code_Point : constant Natural := Wide_Code_Point mod 128;
            begin
               if Overflow then
                  case Unicode_Encode_Action is
                  when Replace =>
                     Result (I) := Replacement;
                  when Raise_Exception =>
                     raise Unicode_Encode_Error
                        with "'ASCII' codecs cannot encode character #" &
                           Natural'Image (Code_Point);
                  end case;
               else
                  Result (I) := Character'Val (Code_Point);
               end if;
            end;
         end loop;
      end return;
   end Encode_ASCII;

   ----------------------
   -- Encode_ISO8859_1 --
   ----------------------

   function Encode_ISO8859_1 (Value       : Wide_String;
                              Replacement : Character) return String is
   begin
      return Result : String (1 .. Value'Length) do
         for I in Result'Range loop
            declare
               WCh : constant Wide_Character := Value (Value'First + I - 1);
               Wide_Code_Point : constant Natural := Wide_Character'Pos (WCh);
               Overflow : constant Boolean := Wide_Code_Point > 255;
               Code_Point : constant Natural := Wide_Code_Point mod 256;
            begin
               if Overflow then
                  case Unicode_Encode_Action is
                  when Replace =>
                     Result (I) := Replacement;
                  when Raise_Exception =>
                     raise Unicode_Encode_Error
                        with "'ISO8859-1' codecs cannot encode character #" &
                           Natural'Image (Code_Point);
                  end case;
               else
                  Result (I) := Character'Val (Code_Point);
               end if;
            end;
         end loop;
      end return;
   end Encode_ISO8859_1;

   -----------------
   -- Encode_UTF8 --
   -----------------

   function Encode_UTF8 (Value       : Wide_String;
                         Replacement : Character) return String is
      pragma Unreferenced (Replacement);
   begin
      return To_UTF8 (Value);
   end Encode_UTF8;

   -----------------
   -- Find_Codecs --
   -----------------

   function Find_Codecs (Encoding     : Wide_String;
                         Always_Raise : Boolean := False) return Positive is
   begin
      for I in Codecs_Table'Range loop
         if Encoding = Codecs_Table (I).Encoding.all then
            return I;
         end if;
      end loop;
      if Always_Raise or Unsupported_Encoding_Action = Raise_Exception then
         raise Unsupported_Encoding with
             "Unsupported encoding '" & Encode_UTF8 (Encoding, '?') & "'";
      end if;
      return Default_Codecs_Index_Value;
   end Find_Codecs;

   -----------------
   -- Make_Codecs --
   -----------------

   function Make_Codecs (Encoding : Wide_String) return Codecs_Type is
   begin
      return Codecs_Type'(Encoding_Index => Find_Codecs (Encoding));
   end Make_Codecs;

   ----------
   -- Name --
   ----------

   function Name (Codecs : Codecs_Type) return Wide_String is
   begin
      return Codecs_Table (Codecs.Encoding_Index).Encoding.all;
   end Name;

   -------------------------------
   -- Set_Unicode_Encode_Action --
   -------------------------------

   procedure Set_Unicode_Encode_Action (Action : Unicode_Encode_Action_Type) is
   begin
      Unicode_Encode_Action := Action;
   end Set_Unicode_Encode_Action;

   -------------------------------------
   -- Set_Unsupported_Encoding_Action --
   -------------------------------------

   procedure Set_Unsupported_Encoding_Action
      (Action : Unsupported_Encoding_Action_Type) is
   begin
      Unsupported_Encoding_Action := Action;
   end Set_Unsupported_Encoding_Action;

begin
   Default_Codecs_Index_Value := Find_Codecs ("UTF-8", Always_Raise => True);
end ZanyBlue.Text.Codecs;
