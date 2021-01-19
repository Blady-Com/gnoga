--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
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

package body ZanyBlue.Text.Pseudo is

   Diamond_With_Left_Half_Black : constant Wide_Character :=
     Wide_Character'Val (16#2B16#);
   --  ⬖
   Diamond_With_Right_Half_Black : constant Wide_Character :=
     Wide_Character'Val (16#2B17#);
   --  ⬗
   Pseudo_Start_Val : constant Wide_Character := Diamond_With_Left_Half_Black;
   Pseudo_End_Val   : constant Wide_Character := Diamond_With_Right_Half_Black;

   Format_Start_Val : constant Wide_Character := Wide_Character'Val (16#AB#);
   --  «
   Format_End_Val : constant Wide_Character := Wide_Character'Val (16#BB#);
   --  »

   function Make_Map
     (Source     : Wide_Character;
      Code_Point : Natural)
      return Pseudo_Character_Map;

   -----------------
   -- Add_Mapping --
   -----------------

   procedure Add_Mapping
     (Pseudo_Map : in out Pseudo_Map_Type;
      Mapping    :        Pseudo_Map_Vector)
   is
      From : Wide_Character_Set := Null_Set;
      To   : Wide_Character_Set := Null_Set;
   begin
      for I in Mapping'Range loop
         From := From or To_Set (Mapping (I).Source);
         To   := To or To_Set (Mapping (I).Target);
      end loop;
      Pseudo_Map.Mapping := To_Mapping (To_Sequence (From), To_Sequence (To));
   end Add_Mapping;

   -------------------------------
   -- Enclosed_Alphanumeric_Map --
   -------------------------------

   function Enclosed_Alphanumeric_Map return Pseudo_Map_Vector is
      Result : constant Pseudo_Map_Vector :=
        (1  => Make_Map ('1', 16#2460#),    --  '①'
         2  => Make_Map ('2', 16#2461#),    --  '②'
         3  => Make_Map ('3', 16#2462#),    --  '③'
         4  => Make_Map ('4', 16#2463#),    --  '④'
         5  => Make_Map ('5', 16#2464#),    --  '⑤'
         6  => Make_Map ('6', 16#2465#),    --  '⑥'
         7  => Make_Map ('7', 16#2466#),    --  '⑦'
         8  => Make_Map ('8', 16#2467#),    --  '⑧'
         9  => Make_Map ('9', 16#2468#),    --  '⑨'
         10 => Make_Map ('A', 16#24B6#),    --  'Ⓐ'
         11 => Make_Map ('B', 16#24B7#),    --  'Ⓑ'
         12 => Make_Map ('C', 16#24B8#),    --  'Ⓒ'
         13 => Make_Map ('D', 16#24B9#),    --  'Ⓓ'
         14 => Make_Map ('E', 16#24BA#),    --  'Ⓔ'
         15 => Make_Map ('F', 16#24BB#),    --  'Ⓕ'
         16 => Make_Map ('G', 16#24BC#),    --  'Ⓖ'
         17 => Make_Map ('H', 16#24BD#),    --  'Ⓗ'
         18 => Make_Map ('I', 16#24BE#),    --  'Ⓘ'
         19 => Make_Map ('J', 16#24BF#),    --  'Ⓙ'
         20 => Make_Map ('K', 16#24C0#),    --  'Ⓚ'
         21 => Make_Map ('L', 16#24C1#),    --  'Ⓛ'
         22 => Make_Map ('M', 16#24C2#),    --  'Ⓜ'
         23 => Make_Map ('N', 16#24C3#),    --  'Ⓝ'
         24 => Make_Map ('O', 16#24C4#),    --  'Ⓞ'
         25 => Make_Map ('P', 16#24C5#),    --  'Ⓟ'
         26 => Make_Map ('Q', 16#24C6#),    --  'Ⓠ'
         27 => Make_Map ('R', 16#24C7#),    --  'Ⓡ'
         28 => Make_Map ('S', 16#24C8#),    --  'Ⓢ'
         29 => Make_Map ('T', 16#24C9#),    --  'Ⓣ'
         30 => Make_Map ('U', 16#24CA#),    --  'Ⓤ'
         31 => Make_Map ('V', 16#24CB#),    --  'Ⓥ'
         32 => Make_Map ('W', 16#24CC#),    --  'Ⓦ'
         33 => Make_Map ('X', 16#24CD#),    --  'Ⓧ'
         34 => Make_Map ('Y', 16#24CE#),    --  'Ⓨ'
         35 => Make_Map ('Z', 16#24CF#),    --  'Ⓩ'
         36 => Make_Map ('a', 16#24D0#),    --  'ⓐ'
         37 => Make_Map ('b', 16#24D1#),    --  'ⓑ'
         38 => Make_Map ('c', 16#24D2#),    --  'ⓒ'
         39 => Make_Map ('d', 16#24D3#),    --  'ⓓ'
         40 => Make_Map ('e', 16#24D4#),    --  'ⓔ'
         41 => Make_Map ('f', 16#24D5#),    --  'ⓕ'
         42 => Make_Map ('g', 16#24D6#),    --  'ⓖ'
         43 => Make_Map ('h', 16#24D7#),    --  'ⓗ'
         44 => Make_Map ('i', 16#24D8#),    --  'ⓘ'
         45 => Make_Map ('j', 16#24D9#),    --  'ⓙ'
         46 => Make_Map ('k', 16#24DA#),    --  'ⓚ'
         47 => Make_Map ('l', 16#24DB#),    --  'ⓛ'
         48 => Make_Map ('m', 16#24DC#),    --  'ⓜ'
         49 => Make_Map ('n', 16#24DD#),    --  'ⓝ'
         50 => Make_Map ('o', 16#24DE#),    --  'ⓞ'
         51 => Make_Map ('p', 16#24DF#),    --  'ⓟ'
         52 => Make_Map ('q', 16#24E0#),    --  'ⓠ'
         53 => Make_Map ('r', 16#24E1#),    --  'ⓡ'
         54 => Make_Map ('s', 16#24E2#),    --  'ⓢ'
         55 => Make_Map ('t', 16#24E3#),    --  'ⓣ'
         56 => Make_Map ('u', 16#24E4#),    --  'ⓤ'
         57 => Make_Map ('v', 16#24E5#),    --  'ⓥ'
         58 => Make_Map ('w', 16#24E6#),    --  'ⓦ'
         59 => Make_Map ('x', 16#24E7#),    --  'ⓧ'
         60 => Make_Map ('y', 16#24E8#),    --  'ⓨ'
         61 => Make_Map ('z', 16#24E9#));   --  'ⓩ'
   begin
      return Result;
   end Enclosed_Alphanumeric_Map;

   ----------------
   -- Format_End --
   ----------------

   function Format_End return Wide_Character is
   begin
      return Format_End_Val;
   end Format_End;

   ------------------
   -- Format_Start --
   ------------------

   function Format_Start return Wide_Character is
   begin
      return Format_Start_Val;
   end Format_Start;

   -------------------------
   -- Halfwidth_Forms_Map --
   -------------------------

   function Halfwidth_Forms_Map return Pseudo_Map_Vector is
      Result : constant Pseudo_Map_Vector :=
        (1  => Make_Map (' ', 16#2001#),    --  ' '
         2  => Make_Map ('!', 16#FF01#),    --  '！'
         3  => Make_Map ('"', 16#FF02#),    --  '＂'
         4  => Make_Map ('#', 16#FF03#),    --  '＃'
         5  => Make_Map ('$', 16#FF04#),    --  '＄'
         6  => Make_Map ('%', 16#FF05#),    --  '％'
         7  => Make_Map ('&', 16#FF06#),    --  '＆'
         8  => Make_Map (''', 16#FF07#),    --  '＇'
         9  => Make_Map ('(', 16#FF08#),    --  '（'
         10 => Make_Map (')', 16#FF09#),    --  '）'
         11 => Make_Map ('*', 16#FF0A#),    --  '＊'
         12 => Make_Map ('+', 16#FF0B#),    --  '＋'
         13 => Make_Map (',', 16#FF0C#),    --  '，'
         14 => Make_Map ('-', 16#FF0D#),    --  '－'
         15 => Make_Map ('.', 16#FF0E#),    --  '．'
         16 => Make_Map ('/', 16#FF0F#),    --  '／'
         17 => Make_Map ('0', 16#FF10#),    --  '０'
         18 => Make_Map ('1', 16#FF11#),    --  '１'
         19 => Make_Map ('2', 16#FF12#),    --  '２'
         20 => Make_Map ('3', 16#FF13#),    --  '３'
         21 => Make_Map ('4', 16#FF14#),    --  '４'
         22 => Make_Map ('5', 16#FF15#),    --  '５'
         23 => Make_Map ('6', 16#FF16#),    --  '６'
         24 => Make_Map ('7', 16#FF17#),    --  '７'
         25 => Make_Map ('8', 16#FF18#),    --  '８'
         26 => Make_Map ('9', 16#FF19#),    --  '９'
         27 => Make_Map (':', 16#FF1A#),    --  '：'
         28 => Make_Map (';', 16#FF1B#),    --  '；'
         29 => Make_Map ('<', 16#FF1C#),    --  '＜'
         30 => Make_Map ('=', 16#FF1D#),    --  '＝'
         31 => Make_Map ('>', 16#FF1E#),    --  '＞'
         32 => Make_Map ('?', 16#FF1F#),    --  '？'
         33 => Make_Map ('@', 16#FF20#),    --  '＠'
         34 => Make_Map ('A', 16#FF21#),    --  'Ａ'
         35 => Make_Map ('B', 16#FF22#),    --  'Ｂ'
         36 => Make_Map ('C', 16#FF23#),    --  'Ｃ'
         37 => Make_Map ('D', 16#FF24#),    --  'Ｄ'
         38 => Make_Map ('E', 16#FF25#),    --  'Ｅ'
         39 => Make_Map ('F', 16#FF26#),    --  'Ｆ'
         40 => Make_Map ('G', 16#FF27#),    --  'Ｇ'
         41 => Make_Map ('H', 16#FF28#),    --  'Ｈ'
         42 => Make_Map ('I', 16#FF29#),    --  'Ｉ'
         43 => Make_Map ('J', 16#FF2A#),    --  'Ｊ'
         44 => Make_Map ('K', 16#FF2B#),    --  'Ｋ'
         45 => Make_Map ('L', 16#FF2C#),    --  'Ｌ'
         46 => Make_Map ('M', 16#FF2D#),    --  'Ｍ'
         47 => Make_Map ('N', 16#FF2E#),    --  'Ｎ'
         48 => Make_Map ('O', 16#FF2F#),    --  'Ｏ'
         49 => Make_Map ('P', 16#FF30#),    --  'Ｐ'
         50 => Make_Map ('Q', 16#FF31#),    --  'Ｑ'
         51 => Make_Map ('R', 16#FF32#),    --  'Ｒ'
         52 => Make_Map ('S', 16#FF33#),    --  'Ｓ'
         53 => Make_Map ('T', 16#FF34#),    --  'Ｔ'
         54 => Make_Map ('U', 16#FF35#),    --  'Ｕ'
         55 => Make_Map ('V', 16#FF36#),    --  'Ｖ'
         56 => Make_Map ('W', 16#FF37#),    --  'Ｗ'
         57 => Make_Map ('X', 16#FF38#),    --  'Ｘ'
         58 => Make_Map ('Y', 16#FF39#),    --  'Ｙ'
         59 => Make_Map ('Z', 16#FF3A#),    --  'Ｚ'
         60 => Make_Map ('[', 16#FF3B#),    --  '［'
         61 => Make_Map ('\', 16#FF3C#),    --  '＼'
         62 => Make_Map (']', 16#FF3D#),    --  '］'
         63 => Make_Map ('^', 16#FF3E#),    --  '＾'
         64 => Make_Map ('_', 16#FF3F#),    --  '＿'
         65 => Make_Map ('`', 16#FF40#),    --  '｀'
         66 => Make_Map ('a', 16#FF41#),    --  'ａ'
         67 => Make_Map ('b', 16#FF42#),    --  'ｂ'
         68 => Make_Map ('c', 16#FF43#),    --  'ｃ'
         69 => Make_Map ('d', 16#FF44#),    --  'ｄ'
         70 => Make_Map ('e', 16#FF45#),    --  'ｅ'
         71 => Make_Map ('f', 16#FF46#),    --  'ｆ'
         72 => Make_Map ('g', 16#FF47#),    --  'ｇ'
         73 => Make_Map ('h', 16#FF48#),    --  'ｈ'
         74 => Make_Map ('i', 16#FF49#),    --  'ｉ'
         75 => Make_Map ('j', 16#FF4A#),    --  'ｊ'
         76 => Make_Map ('k', 16#FF4B#),    --  'ｋ'
         77 => Make_Map ('l', 16#FF4C#),    --  'ｌ'
         78 => Make_Map ('m', 16#FF4D#),    --  'ｍ'
         79 => Make_Map ('n', 16#FF4E#),    --  'ｎ'
         80 => Make_Map ('o', 16#FF4F#),    --  'ｏ'
         81 => Make_Map ('p', 16#FF50#),    --  'ｐ'
         82 => Make_Map ('q', 16#FF51#),    --  'ｑ'
         83 => Make_Map ('r', 16#FF52#),    --  'ｒ'
         84 => Make_Map ('s', 16#FF53#),    --  'ｓ'
         85 => Make_Map ('t', 16#FF54#),    --  'ｔ'
         86 => Make_Map ('u', 16#FF55#),    --  'ｕ'
         87 => Make_Map ('v', 16#FF56#),    --  'ｖ'
         88 => Make_Map ('w', 16#FF57#),    --  'ｗ'
         89 => Make_Map ('x', 16#FF58#),    --  'ｘ'
         90 => Make_Map ('y', 16#FF59#),    --  'ｙ'
         91 => Make_Map ('z', 16#FF5A#),    --  'ｚ'
         92 => Make_Map ('{', 16#FF5B#),    --  '｛'
         93 => Make_Map ('|', 16#FF5C#),    --  '｜'
         94 => Make_Map ('}', 16#FF5D#),    --  '｝'
         95 => Make_Map ('~', 16#FF5E#));    -- '～'
   begin
      return Result;
   end Halfwidth_Forms_Map;

   -------------------
   -- Lowercase_Map --
   -------------------

   function Lowercase_Map return Pseudo_Map_Vector is
      Result : constant Pseudo_Map_Vector :=
        (1  => (Source => 'A', Target => 'a'),
         2  => (Source => 'B', Target => 'b'),
         3  => (Source => 'C', Target => 'c'),
         4  => (Source => 'D', Target => 'd'),
         5  => (Source => 'E', Target => 'e'),
         6  => (Source => 'F', Target => 'f'),
         7  => (Source => 'G', Target => 'g'),
         8  => (Source => 'H', Target => 'h'),
         9  => (Source => 'I', Target => 'i'),
         10 => (Source => 'J', Target => 'j'),
         11 => (Source => 'K', Target => 'k'),
         12 => (Source => 'L', Target => 'l'),
         13 => (Source => 'M', Target => 'm'),
         14 => (Source => 'N', Target => 'n'),
         15 => (Source => 'O', Target => 'o'),
         16 => (Source => 'P', Target => 'p'),
         17 => (Source => 'Q', Target => 'q'),
         18 => (Source => 'R', Target => 'r'),
         19 => (Source => 'S', Target => 's'),
         20 => (Source => 'T', Target => 't'),
         21 => (Source => 'U', Target => 'u'),
         22 => (Source => 'V', Target => 'v'),
         23 => (Source => 'W', Target => 'w'),
         24 => (Source => 'X', Target => 'x'),
         25 => (Source => 'Y', Target => 'y'),
         26 => (Source => 'Z', Target => 'z'));
   begin
      return Result;
   end Lowercase_Map;

   --------------
   -- Make_Map --
   --------------

   function Make_Map
     (Source     : Wide_Character;
      Code_Point : Natural)
      return Pseudo_Character_Map
   is
   begin
      return Result : Pseudo_Character_Map do
         Result.Source := Source;
         Result.Target := Wide_Character'Val (Code_Point);
      end return;
   end Make_Map;

   ---------
   -- Map --
   ---------

   function Map
     (Pseudo_Map : Pseudo_Map_Type;
      Ch         : Wide_Character)
      return Wide_Character
   is
   begin
      return Value (Pseudo_Map.Mapping, Ch);
   end Map;

   --------------
   -- Null_Map --
   --------------

   function Null_Map return Pseudo_Map_Vector is
      Result : constant Pseudo_Map_Vector :=
        (1 => (Source => 'a', Target => 'a'));

   begin
      return Result;
   end Null_Map;

   ----------------
   -- Pseudo_End --
   ----------------

   function Pseudo_End return Wide_Character is
   begin
      return Pseudo_End_Val;
   end Pseudo_End;

   ------------------
   -- Pseudo_Start --
   ------------------

   function Pseudo_Start return Wide_Character is
   begin
      return Pseudo_Start_Val;
   end Pseudo_Start;

   -------------------
   -- Uppercase_Map --
   -------------------

   function Uppercase_Map return Pseudo_Map_Vector is
      Result : constant Pseudo_Map_Vector :=
        (1  => (Source => 'a', Target => 'A'),
         2  => (Source => 'b', Target => 'B'),
         3  => (Source => 'c', Target => 'C'),
         4  => (Source => 'd', Target => 'D'),
         5  => (Source => 'e', Target => 'E'),
         6  => (Source => 'f', Target => 'F'),
         7  => (Source => 'g', Target => 'G'),
         8  => (Source => 'h', Target => 'H'),
         9  => (Source => 'i', Target => 'I'),
         10 => (Source => 'j', Target => 'J'),
         11 => (Source => 'k', Target => 'K'),
         12 => (Source => 'l', Target => 'L'),
         13 => (Source => 'm', Target => 'M'),
         14 => (Source => 'n', Target => 'N'),
         15 => (Source => 'o', Target => 'O'),
         16 => (Source => 'p', Target => 'P'),
         17 => (Source => 'q', Target => 'Q'),
         18 => (Source => 'r', Target => 'R'),
         19 => (Source => 's', Target => 'S'),
         20 => (Source => 't', Target => 'T'),
         21 => (Source => 'u', Target => 'U'),
         22 => (Source => 'v', Target => 'V'),
         23 => (Source => 'w', Target => 'W'),
         24 => (Source => 'x', Target => 'X'),
         25 => (Source => 'y', Target => 'Y'),
         26 => (Source => 'z', Target => 'Z'));
   begin
      return Result;
   end Uppercase_Map;

end ZanyBlue.Text.Pseudo;
