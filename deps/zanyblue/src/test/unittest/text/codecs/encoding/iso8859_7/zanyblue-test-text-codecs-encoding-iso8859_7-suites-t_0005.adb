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

separate (ZanyBlue.Test.Text.Codecs.Encoding.ISO8859_7.Suites)
procedure T_0005 (T : in out Test_Case'Class) is

   Codecs : constant Codecs_Type := Make_Codecs ("ISO8859-7");

   procedure Check_String (Source : String;
                           Expect : String);

   procedure Check_String (Source : String;
                           Expect : String) is
      Encoded : constant String := Codecs.Encode (Source);
   begin
      WAssert (T, Encoded = Expect,
               "Example string encoding failed");
   end Check_String;

begin
   Check_String (" !""#$%&'()*",
                 Character'Val (16#20#) & Character'Val (16#21#) &
                 Character'Val (16#22#) & Character'Val (16#23#) &
                 Character'Val (16#24#) & Character'Val (16#25#) &
                 Character'Val (16#26#) & Character'Val (16#27#) &
                 Character'Val (16#28#) & Character'Val (16#29#) &
                 Character'Val (16#2a#));
   Check_String ("+,-./012345",
                 Character'Val (16#2b#) & Character'Val (16#2c#) &
                 Character'Val (16#2d#) & Character'Val (16#2e#) &
                 Character'Val (16#2f#) & Character'Val (16#30#) &
                 Character'Val (16#31#) & Character'Val (16#32#) &
                 Character'Val (16#33#) & Character'Val (16#34#) &
                 Character'Val (16#35#));
   Check_String ("6789:;<=>?@",
                 Character'Val (16#36#) & Character'Val (16#37#) &
                 Character'Val (16#38#) & Character'Val (16#39#) &
                 Character'Val (16#3a#) & Character'Val (16#3b#) &
                 Character'Val (16#3c#) & Character'Val (16#3d#) &
                 Character'Val (16#3e#) & Character'Val (16#3f#) &
                 Character'Val (16#40#));
   Check_String ("ABCDEFGHIJK",
                 Character'Val (16#41#) & Character'Val (16#42#) &
                 Character'Val (16#43#) & Character'Val (16#44#) &
                 Character'Val (16#45#) & Character'Val (16#46#) &
                 Character'Val (16#47#) & Character'Val (16#48#) &
                 Character'Val (16#49#) & Character'Val (16#4a#) &
                 Character'Val (16#4b#));
   Check_String ("LMNOPQRSTUV",
                 Character'Val (16#4c#) & Character'Val (16#4d#) &
                 Character'Val (16#4e#) & Character'Val (16#4f#) &
                 Character'Val (16#50#) & Character'Val (16#51#) &
                 Character'Val (16#52#) & Character'Val (16#53#) &
                 Character'Val (16#54#) & Character'Val (16#55#) &
                 Character'Val (16#56#));
   Check_String ("WXYZ[\]^_`a",
                 Character'Val (16#57#) & Character'Val (16#58#) &
                 Character'Val (16#59#) & Character'Val (16#5a#) &
                 Character'Val (16#5b#) & Character'Val (16#5c#) &
                 Character'Val (16#5d#) & Character'Val (16#5e#) &
                 Character'Val (16#5f#) & Character'Val (16#60#) &
                 Character'Val (16#61#));
   Check_String ("bcdefghijkl",
                 Character'Val (16#62#) & Character'Val (16#63#) &
                 Character'Val (16#64#) & Character'Val (16#65#) &
                 Character'Val (16#66#) & Character'Val (16#67#) &
                 Character'Val (16#68#) & Character'Val (16#69#) &
                 Character'Val (16#6a#) & Character'Val (16#6b#) &
                 Character'Val (16#6c#));
   Check_String ("mnopqrstuvw",
                 Character'Val (16#6d#) & Character'Val (16#6e#) &
                 Character'Val (16#6f#) & Character'Val (16#70#) &
                 Character'Val (16#71#) & Character'Val (16#72#) &
                 Character'Val (16#73#) & Character'Val (16#74#) &
                 Character'Val (16#75#) & Character'Val (16#76#) &
                 Character'Val (16#77#));
   Check_String ("xyz{|}~‘’£€",
                 Character'Val (16#78#) & Character'Val (16#79#) &
                 Character'Val (16#7a#) & Character'Val (16#7b#) &
                 Character'Val (16#7c#) & Character'Val (16#7d#) &
                 Character'Val (16#7e#) & Character'Val (16#a1#) &
                 Character'Val (16#a2#) & Character'Val (16#a3#) &
                 Character'Val (16#a4#));
   Check_String ("₯¦§¨©ͺ«¬­―°",
                 Character'Val (16#a5#) & Character'Val (16#a6#) &
                 Character'Val (16#a7#) & Character'Val (16#a8#) &
                 Character'Val (16#a9#) & Character'Val (16#aa#) &
                 Character'Val (16#ab#) & Character'Val (16#ac#) &
                 Character'Val (16#ad#) & Character'Val (16#af#) &
                 Character'Val (16#b0#));
   Check_String ("±²³΄΅Ά·ΈΉΊ»",
                 Character'Val (16#b1#) & Character'Val (16#b2#) &
                 Character'Val (16#b3#) & Character'Val (16#b4#) &
                 Character'Val (16#b5#) & Character'Val (16#b6#) &
                 Character'Val (16#b7#) & Character'Val (16#b8#) &
                 Character'Val (16#b9#) & Character'Val (16#ba#) &
                 Character'Val (16#bb#));
   Check_String ("Ό½ΎΏΐΑΒΓΔΕΖ",
                 Character'Val (16#bc#) & Character'Val (16#bd#) &
                 Character'Val (16#be#) & Character'Val (16#bf#) &
                 Character'Val (16#c0#) & Character'Val (16#c1#) &
                 Character'Val (16#c2#) & Character'Val (16#c3#) &
                 Character'Val (16#c4#) & Character'Val (16#c5#) &
                 Character'Val (16#c6#));
   Check_String ("ΗΘΙΚΛΜΝΞΟΠΡ",
                 Character'Val (16#c7#) & Character'Val (16#c8#) &
                 Character'Val (16#c9#) & Character'Val (16#ca#) &
                 Character'Val (16#cb#) & Character'Val (16#cc#) &
                 Character'Val (16#cd#) & Character'Val (16#ce#) &
                 Character'Val (16#cf#) & Character'Val (16#d0#) &
                 Character'Val (16#d1#));
   Check_String ("ΣΤΥΦΧΨΩΪΫάέ",
                 Character'Val (16#d3#) & Character'Val (16#d4#) &
                 Character'Val (16#d5#) & Character'Val (16#d6#) &
                 Character'Val (16#d7#) & Character'Val (16#d8#) &
                 Character'Val (16#d9#) & Character'Val (16#da#) &
                 Character'Val (16#db#) & Character'Val (16#dc#) &
                 Character'Val (16#dd#));
   Check_String ("ήίΰαβγδεζηθ",
                 Character'Val (16#de#) & Character'Val (16#df#) &
                 Character'Val (16#e0#) & Character'Val (16#e1#) &
                 Character'Val (16#e2#) & Character'Val (16#e3#) &
                 Character'Val (16#e4#) & Character'Val (16#e5#) &
                 Character'Val (16#e6#) & Character'Val (16#e7#) &
                 Character'Val (16#e8#));
   Check_String ("ικλμνξοπρςσ",
                 Character'Val (16#e9#) & Character'Val (16#ea#) &
                 Character'Val (16#eb#) & Character'Val (16#ec#) &
                 Character'Val (16#ed#) & Character'Val (16#ee#) &
                 Character'Val (16#ef#) & Character'Val (16#f0#) &
                 Character'Val (16#f1#) & Character'Val (16#f2#) &
                 Character'Val (16#f3#));
   Check_String ("τυφχψωϊϋόύώ",
                 Character'Val (16#f4#) & Character'Val (16#f5#) &
                 Character'Val (16#f6#) & Character'Val (16#f7#) &
                 Character'Val (16#f8#) & Character'Val (16#f9#) &
                 Character'Val (16#fa#) & Character'Val (16#fb#) &
                 Character'Val (16#fc#) & Character'Val (16#fd#) &
                 Character'Val (16#fe#));
end T_0005;
