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

separate (ZanyBlue.Test.Text.Utils.Suites)
procedure T_0005 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Text;

   procedure Invalid (Source : String);
   procedure OK (Source : String; Expected : Wide_String);

   procedure Invalid (Source : String) is
   begin
      Discard (Unescape_String (Source));
      WAssert (R, False,
               "Failed to raise exception for " & To_Wide_String (Source));
   exception
   when Unicode_Format_Error =>
      WAssert (R, True, "Expected Unicode_Format_Error raised");
   end Invalid;

   procedure OK (Source : String; Expected : Wide_String) is
   begin
      WAssert (R, Unescape_String (Source) = Expected,
              "Unescaped """ & To_Wide_String (Source) & """ failed");
   end OK;

begin
   OK ("This is \u03c0", "This is π");
   OK ("This is \u03C0", "This is π");
   OK ("This is \X03C0", "This is X03C0");
   Invalid ("\u");
   Invalid ("\u0");
   Invalid ("\u03");
   Invalid ("\u03c");
   Invalid ("\u03cx");
end T_0005;
