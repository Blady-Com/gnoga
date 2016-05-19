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

separate (ZanyBlue.Test.Text.Codecs.Encoding.CP932.Suites)
procedure T_0013 (T : in out Test_Case'Class) is
begin
   Check_String (T, "みむめも扱宛姐虻飴絢",
                 Character'Val (16#82#) & Character'Val (16#dd#) &
                 Character'Val (16#82#) & Character'Val (16#de#) &
                 Character'Val (16#82#) & Character'Val (16#df#) &
                 Character'Val (16#82#) & Character'Val (16#e0#) &
                 Character'Val (16#88#) & Character'Val (16#b5#) &
                 Character'Val (16#88#) & Character'Val (16#b6#) &
                 Character'Val (16#88#) & Character'Val (16#b7#) &
                 Character'Val (16#88#) & Character'Val (16#b8#) &
                 Character'Val (16#88#) & Character'Val (16#b9#) &
                 Character'Val (16#88#) & Character'Val (16#ba#));
end T_0013;
