--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
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

package body ZanyBlue.Test.Text.Codecs is

   --------------------
   -- Check_Encoding --
   --------------------

   procedure Check_Encoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCh : Unicode_Character;
                             Ch  : Character) is
      pragma Warnings (Off);
      Encoded : constant String := CS.Encode ("" & WCh);
      ECh : Character;
   begin
      WAssert (T, Encoded'Length = 1,
               CS.Name & " wrong length for single char ("
             & Natural'Wide_Image (Encoded'Length)
             & ")");
      ECh := Encoded (Encoded'First);
      WAssert (T, ECh = Ch,
               CS.Name & " wrong char for single char "
              & " ["
              & Natural'Wide_Image (Character'Pos (ECh))
              & "] /= ["
              & Natural'Wide_Image (Character'Pos (Ch))
              & "]");
   end Check_Encoding;

   --------------------
   -- Check_Encoding --
   --------------------

   procedure Check_Encoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCh : Unicode_Character;
                             Ch1 : Character;
                             Ch2 : Character) is
      pragma Warnings (Off);
      Encoded : constant String := CS.Encode ("" & WCh);
      ECh1, ECh2 : Character;
   begin
      WAssert (T, Encoded'Length = 2,
               CS.Name & " wrong length for double char ("
             & Natural'Wide_Image (Encoded'Length)
             & ")");
      ECh1 := Encoded (Encoded'First);
      ECh2 := Encoded (Encoded'First + 1);
      WAssert (T, ECh1 = Ch1,
               CS.Name & " wrong char for first char "
              & " ["
              & Natural'Wide_Image (Character'Pos (ECh1))
              & "] /= ["
              & Natural'Wide_Image (Character'Pos (Ch1))
              & "]");
      WAssert (T, ECh2 = Ch2,
               CS.Name & " wrong char for second char "
              & " ["
              & Natural'Wide_Image (Character'Pos (ECh2))
              & "] /= ["
              & Natural'Wide_Image (Character'Pos (Ch2))
              & "]");
   end Check_Encoding;

   --------------------
   -- Check_Encoding --
   --------------------

   procedure Check_Encoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCP : Natural;
                             CP  : Natural) is
   begin
      Check_Encoding (T, CS, Unicode_Character'Val (WCP), Character'Val (CP));
   end Check_Encoding;

   --------------------
   -- Check_Encoding --
   --------------------

   procedure Check_Encoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCP : Natural;
                             CP1 : Natural;
                             CP2 : Natural) is
   begin
      Check_Encoding (T, CS, Unicode_Character'Val (WCP),
                      Character'Val (CP1), Character'Val (CP2));
   end Check_Encoding;

   --------------------
   -- Check_Encoding --
   --------------------

   procedure Check_Encoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCh : Unicode_Character;
                             CP1 : Natural;
                             CP2 : Natural) is
   begin
      Check_Encoding (T, CS, Unicode_Character'Pos (WCh), CP1, CP2);
   end Check_Encoding;

end ZanyBlue.Test.Text.Codecs;
