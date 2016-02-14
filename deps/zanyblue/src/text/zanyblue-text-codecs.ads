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

package ZanyBlue.Text.Codecs is

   type Codecs_Type is tagged private;

   Unsupported_Encoding : exception;
   Unicode_Encode_Error : exception;
   Unicode_Decode_Error : exception;

   type Unsupported_Encoding_Action_Type is (Use_UTF8, Raise_Exception);
   type Unicode_Encode_Action_Type is (Replace, Raise_Exception);

   procedure Set_Unsupported_Encoding_Action
      (Action : Unsupported_Encoding_Action_Type);

   procedure Set_Unicode_Encode_Action (Action : Unicode_Encode_Action_Type);

   function Make_Codecs (Encoding : Wide_String) return Codecs_Type;
   function Name (Codecs : Codecs_Type) return Wide_String;

   function Encode (Codecs : Codecs_Type;
                    Value : Wide_String) return String;
   function Decode (Codecs : Codecs_Type;
                    Value : String) return Wide_String;
   function Encode (Encoding : Wide_String;
                    Value : Wide_String) return String;
   function Decode (Encoding : Wide_String;
                    Value : String) return Wide_String;

private

   function Default_Codecs_Index return Positive;

   type Codecs_Type is tagged
      record
         Encoding_Index : Positive := Default_Codecs_Index;
      end record;

end ZanyBlue.Text.Codecs;
