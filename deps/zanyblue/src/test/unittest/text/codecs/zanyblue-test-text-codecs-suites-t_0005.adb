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

separate (ZanyBlue.Test.Text.Codecs.Suites)
procedure T_0005 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text.Codecs;

   procedure Ignore_Codecs (Codecs : Codecs_Type);

   procedure Ignore_Codecs (Codecs : Codecs_Type) is
   begin
      null;
   end Ignore_Codecs;

begin
   Set_Unsupported_Encoding_Action (Raise_Exception);
   Ignore_Codecs (Make_Codecs ("NO-SUCH-CODECS"));
   Set_Unsupported_Encoding_Action (Use_UTF8);
   WAssert (R, False, "Expected Unsupported_Encoding exception not raised");
exception
when Unsupported_Encoding =>
   Set_Unsupported_Encoding_Action (Use_UTF8);
   WAssert (R, True, "Expected Unsupported_Encoding error raised");
end T_0005;
