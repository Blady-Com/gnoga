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

separate (ZanyBlue.Test.Text.Codecs.Decoding.CP932.Suites)
procedure T_0005 (T : in out Test_Case'Class) is

begin
   Check_Decoding (T, My_Codecs,
                   "A￡￡",
                   Character'Val (16#41#) &
                   Character'Val (16#81#) & Character'Val (16#92#) &
                   Character'Val (16#81#) & Character'Val (16#92#));
   Check_Decoding (T, My_Codecs,
                   "ABCDEFGHIJ",
                   Character'Val (16#41#) & Character'Val (16#42#) &
                   Character'Val (16#43#) & Character'Val (16#44#) &
                   Character'Val (16#45#) & Character'Val (16#46#) &
                   Character'Val (16#47#) & Character'Val (16#48#) &
                   Character'Val (16#49#) & Character'Val (16#4a#));
   Check_Decoding (T, My_Codecs,
                   "KLMNOPQRST",
                   Character'Val (16#4b#) & Character'Val (16#4c#) &
                   Character'Val (16#4d#) & Character'Val (16#4e#) &
                   Character'Val (16#4f#) & Character'Val (16#50#) &
                   Character'Val (16#51#) & Character'Val (16#52#) &
                   Character'Val (16#53#) & Character'Val (16#54#));
   Check_Decoding (T, My_Codecs,
                   "UVWXYZ―￡かが",
                   Character'Val (16#55#) & Character'Val (16#56#) &
                   Character'Val (16#57#) & Character'Val (16#58#) &
                   Character'Val (16#59#) & Character'Val (16#5a#) &
                   Character'Val (16#81#) & Character'Val (16#5c#) &
                   Character'Val (16#81#) & Character'Val (16#92#) &
                   Character'Val (16#82#) & Character'Val (16#a9#) &
                   Character'Val (16#82#) & Character'Val (16#aa#));
   Check_Decoding (T, My_Codecs,
                   "きぎくぐけげこごさざ",
                   Character'Val (16#82#) & Character'Val (16#ab#) &
                   Character'Val (16#82#) & Character'Val (16#ac#) &
                   Character'Val (16#82#) & Character'Val (16#ad#) &
                   Character'Val (16#82#) & Character'Val (16#ae#) &
                   Character'Val (16#82#) & Character'Val (16#af#) &
                   Character'Val (16#82#) & Character'Val (16#b0#) &
                   Character'Val (16#82#) & Character'Val (16#b1#) &
                   Character'Val (16#82#) & Character'Val (16#b2#) &
                   Character'Val (16#82#) & Character'Val (16#b3#) &
                   Character'Val (16#82#) & Character'Val (16#b4#));
   Check_Decoding (T, My_Codecs,
                   "しじすずせぜそぞただ",
                   Character'Val (16#82#) & Character'Val (16#b5#) &
                   Character'Val (16#82#) & Character'Val (16#b6#) &
                   Character'Val (16#82#) & Character'Val (16#b7#) &
                   Character'Val (16#82#) & Character'Val (16#b8#) &
                   Character'Val (16#82#) & Character'Val (16#b9#) &
                   Character'Val (16#82#) & Character'Val (16#ba#) &
                   Character'Val (16#82#) & Character'Val (16#bb#) &
                   Character'Val (16#82#) & Character'Val (16#bc#) &
                   Character'Val (16#82#) & Character'Val (16#bd#) &
                   Character'Val (16#82#) & Character'Val (16#be#));
   Check_Decoding (T, My_Codecs,
                   "ちぢっつづてでとどな",
                   Character'Val (16#82#) & Character'Val (16#bf#) &
                   Character'Val (16#82#) & Character'Val (16#c0#) &
                   Character'Val (16#82#) & Character'Val (16#c1#) &
                   Character'Val (16#82#) & Character'Val (16#c2#) &
                   Character'Val (16#82#) & Character'Val (16#c3#) &
                   Character'Val (16#82#) & Character'Val (16#c4#) &
                   Character'Val (16#82#) & Character'Val (16#c5#) &
                   Character'Val (16#82#) & Character'Val (16#c6#) &
                   Character'Val (16#82#) & Character'Val (16#c7#) &
                   Character'Val (16#82#) & Character'Val (16#c8#));
   Check_Decoding (T, My_Codecs,
                   "にぬねのはばぱひびぴ",
                   Character'Val (16#82#) & Character'Val (16#c9#) &
                   Character'Val (16#82#) & Character'Val (16#ca#) &
                   Character'Val (16#82#) & Character'Val (16#cb#) &
                   Character'Val (16#82#) & Character'Val (16#cc#) &
                   Character'Val (16#82#) & Character'Val (16#cd#) &
                   Character'Val (16#82#) & Character'Val (16#ce#) &
                   Character'Val (16#82#) & Character'Val (16#cf#) &
                   Character'Val (16#82#) & Character'Val (16#d0#) &
                   Character'Val (16#82#) & Character'Val (16#d1#) &
                   Character'Val (16#82#) & Character'Val (16#d2#));
   Check_Decoding (T, My_Codecs,
                   "ふぶぷへべぺほぼぽま",
                   Character'Val (16#82#) & Character'Val (16#d3#) &
                   Character'Val (16#82#) & Character'Val (16#d4#) &
                   Character'Val (16#82#) & Character'Val (16#d5#) &
                   Character'Val (16#82#) & Character'Val (16#d6#) &
                   Character'Val (16#82#) & Character'Val (16#d7#) &
                   Character'Val (16#82#) & Character'Val (16#d8#) &
                   Character'Val (16#82#) & Character'Val (16#d9#) &
                   Character'Val (16#82#) & Character'Val (16#da#) &
                   Character'Val (16#82#) & Character'Val (16#db#) &
                   Character'Val (16#82#) & Character'Val (16#dc#));
   Check_Decoding (T, My_Codecs,
                   "みむめも扱宛姐虻飴絢",
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
   Check_Decoding (T, My_Codecs,
                   "確穫覚角赫較郭閣隔革",
                   Character'Val (16#8a#) & Character'Val (16#6d#) &
                   Character'Val (16#8a#) & Character'Val (16#6e#) &
                   Character'Val (16#8a#) & Character'Val (16#6f#) &
                   Character'Val (16#8a#) & Character'Val (16#70#) &
                   Character'Val (16#8a#) & Character'Val (16#71#) &
                   Character'Val (16#8a#) & Character'Val (16#72#) &
                   Character'Val (16#8a#) & Character'Val (16#73#) &
                   Character'Val (16#8a#) & Character'Val (16#74#) &
                   Character'Val (16#8a#) & Character'Val (16#75#) &
                   Character'Val (16#8a#) & Character'Val (16#76#));
   Check_Decoding (T, My_Codecs,
                   "学岳鶏芸迎鯨劇戟撃激",
                   Character'Val (16#8a#) & Character'Val (16#77#) &
                   Character'Val (16#8a#) & Character'Val (16#78#) &
                   Character'Val (16#8c#) & Character'Val (16#7b#) &
                   Character'Val (16#8c#) & Character'Val (16#7c#) &
                   Character'Val (16#8c#) & Character'Val (16#7d#) &
                   Character'Val (16#8c#) & Character'Val (16#7e#) &
                   Character'Val (16#8c#) & Character'Val (16#80#) &
                   Character'Val (16#8c#) & Character'Val (16#81#) &
                   Character'Val (16#8c#) & Character'Val (16#82#) &
                   Character'Val (16#8c#) & Character'Val (16#83#));
   Check_Decoding (T, My_Codecs,
                   "隙桁傑霻靃靍靏靑靕顗",
                   Character'Val (16#8c#) & Character'Val (16#84#) &
                   Character'Val (16#8c#) & Character'Val (16#85#) &
                   Character'Val (16#8c#) & Character'Val (16#86#) &
                   Character'Val (16#fb#) & Character'Val (16#ee#) &
                   Character'Val (16#fb#) & Character'Val (16#ef#) &
                   Character'Val (16#fb#) & Character'Val (16#f0#) &
                   Character'Val (16#fb#) & Character'Val (16#f1#) &
                   Character'Val (16#fb#) & Character'Val (16#f2#) &
                   Character'Val (16#fb#) & Character'Val (16#f3#) &
                   Character'Val (16#fb#) & Character'Val (16#f4#));
   Check_Decoding (T, My_Codecs,
                   "顥飯飼餧館馞驎髙髜魵",
                   Character'Val (16#fb#) & Character'Val (16#f5#) &
                   Character'Val (16#fb#) & Character'Val (16#f6#) &
                   Character'Val (16#fb#) & Character'Val (16#f7#) &
                   Character'Val (16#fb#) & Character'Val (16#f8#) &
                   Character'Val (16#fb#) & Character'Val (16#f9#) &
                   Character'Val (16#fb#) & Character'Val (16#fa#) &
                   Character'Val (16#fb#) & Character'Val (16#fb#) &
                   Character'Val (16#fb#) & Character'Val (16#fc#) &
                   Character'Val (16#fc#) & Character'Val (16#40#) &
                   Character'Val (16#fc#) & Character'Val (16#41#));
   Check_Decoding (T, My_Codecs,
                   "魲鮏鮱鮻鰀鵰鵫鶴鸙黑",
                   Character'Val (16#fc#) & Character'Val (16#42#) &
                   Character'Val (16#fc#) & Character'Val (16#43#) &
                   Character'Val (16#fc#) & Character'Val (16#44#) &
                   Character'Val (16#fc#) & Character'Val (16#45#) &
                   Character'Val (16#fc#) & Character'Val (16#46#) &
                   Character'Val (16#fc#) & Character'Val (16#47#) &
                   Character'Val (16#fc#) & Character'Val (16#48#) &
                   Character'Val (16#fc#) & Character'Val (16#49#) &
                   Character'Val (16#fc#) & Character'Val (16#4a#) &
                   Character'Val (16#fc#) & Character'Val (16#4b#));
end T_0005;
