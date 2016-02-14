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

package body ZanyBlue.Text.Pseudo is

   Diamond_With_Left_Half_Black  : constant Wide_Character
                                          := Wide_Character'Val (16#2B16#);
   Diamond_With_Right_Half_Black : constant Wide_Character
                                          := Wide_Character'Val (16#2B17#);
   Pseudo_Start_Val : constant Wide_Character := Diamond_With_Left_Half_Black;
   Pseudo_End_Val   : constant Wide_Character := Diamond_With_Right_Half_Black;

   Format_Start_Val : constant Wide_Character := '«';
   Format_End_Val   : constant Wide_Character := '»';

   Null_Map_Data : constant Pseudo_Map_Vector := (
              1 => (Source => 'a', Target => 'a'));

   Uppercase_Map_Data : constant Pseudo_Map_Vector := (
              1 => (Source => 'a', Target => 'A'),
              2 => (Source => 'b', Target => 'B'),
              3 => (Source => 'c', Target => 'C'),
              4 => (Source => 'd', Target => 'D'),
              5 => (Source => 'e', Target => 'E'),
              6 => (Source => 'f', Target => 'F'),
              7 => (Source => 'g', Target => 'G'),
              8 => (Source => 'h', Target => 'H'),
              9 => (Source => 'i', Target => 'I'),
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

   Lowercase_Map_Data : constant Pseudo_Map_Vector := (
              1 => (Source => 'A', Target => 'a'),
              2 => (Source => 'B', Target => 'b'),
              3 => (Source => 'C', Target => 'c'),
              4 => (Source => 'D', Target => 'd'),
              5 => (Source => 'E', Target => 'e'),
              6 => (Source => 'F', Target => 'f'),
              7 => (Source => 'G', Target => 'g'),
              8 => (Source => 'H', Target => 'h'),
              9 => (Source => 'I', Target => 'i'),
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

   Halfwidth_Forms_Map_Data : constant Pseudo_Map_Vector := (
              1 => (Source => ' ', Target => ' '),       --  U+2001
              2 => (Source => '!', Target => '！'),      --  U+FF01
              3 => (Source => '"', Target => '＂'),      --  U+FF02
              4 => (Source => '#', Target => '＃'),      --  U+FF03
              5 => (Source => '$', Target => '＄'),      --  U+FF04
              6 => (Source => '%', Target => '％'),      --  U+FF05
              7 => (Source => '&', Target => '＆'),      --  U+FF06
              8 => (Source => ''', Target => '＇'),      --  U+FF07
              9 => (Source => '(', Target => '（'),      --  U+FF08
             10 => (Source => ')', Target => '）'),      --  U+FF09
             11 => (Source => '*', Target => '＊'),      --  U+FF0A
             12 => (Source => '+', Target => '＋'),      --  U+FF0B
             13 => (Source => ',', Target => '，'),      --  U+FF0C
             14 => (Source => '-', Target => '－'),      --  U+FF0D
             15 => (Source => '.', Target => '．'),      --  U+FF0E
             16 => (Source => '/', Target => '／'),      --  U+FF0F
             17 => (Source => '0', Target => '０'),      --  U+FF10
             18 => (Source => '1', Target => '１'),      --  U+FF11
             19 => (Source => '2', Target => '２'),      --  U+FF12
             20 => (Source => '3', Target => '３'),      --  U+FF13
             21 => (Source => '4', Target => '４'),      --  U+FF14
             22 => (Source => '5', Target => '５'),      --  U+FF15
             23 => (Source => '6', Target => '６'),      --  U+FF16
             24 => (Source => '7', Target => '７'),      --  U+FF17
             25 => (Source => '8', Target => '８'),      --  U+FF18
             26 => (Source => '9', Target => '９'),      --  U+FF19
             27 => (Source => ':', Target => '：'),      --  U+FF1A
             28 => (Source => ';', Target => '；'),      --  U+FF1B
             29 => (Source => '<', Target => '＜'),      --  U+FF1C
             30 => (Source => '=', Target => '＝'),      --  U+FF1D
             31 => (Source => '>', Target => '＞'),      --  U+FF1E
             32 => (Source => '?', Target => '？'),      --  U+FF1F
             33 => (Source => '@', Target => '＠'),      --  U+FF20
             34 => (Source => 'A', Target => 'Ａ'),      --  U+FF21
             35 => (Source => 'B', Target => 'Ｂ'),      --  U+FF22
             36 => (Source => 'C', Target => 'Ｃ'),      --  U+FF23
             37 => (Source => 'D', Target => 'Ｄ'),      --  U+FF24
             38 => (Source => 'E', Target => 'Ｅ'),      --  U+FF25
             39 => (Source => 'F', Target => 'Ｆ'),      --  U+FF26
             40 => (Source => 'G', Target => 'Ｇ'),      --  U+FF27
             41 => (Source => 'H', Target => 'Ｈ'),      --  U+FF28
             42 => (Source => 'I', Target => 'Ｉ'),      --  U+FF29
             43 => (Source => 'J', Target => 'Ｊ'),      --  U+FF2A
             44 => (Source => 'K', Target => 'Ｋ'),      --  U+FF2B
             45 => (Source => 'L', Target => 'Ｌ'),      --  U+FF2C
             46 => (Source => 'M', Target => 'Ｍ'),      --  U+FF2D
             47 => (Source => 'N', Target => 'Ｎ'),      --  U+FF2E
             48 => (Source => 'O', Target => 'Ｏ'),      --  U+FF2F
             49 => (Source => 'P', Target => 'Ｐ'),      --  U+FF30
             50 => (Source => 'Q', Target => 'Ｑ'),      --  U+FF31
             51 => (Source => 'R', Target => 'Ｒ'),      --  U+FF32
             52 => (Source => 'S', Target => 'Ｓ'),      --  U+FF33
             53 => (Source => 'T', Target => 'Ｔ'),      --  U+FF34
             54 => (Source => 'U', Target => 'Ｕ'),      --  U+FF35
             55 => (Source => 'V', Target => 'Ｖ'),      --  U+FF36
             56 => (Source => 'W', Target => 'Ｗ'),      --  U+FF37
             57 => (Source => 'X', Target => 'Ｘ'),      --  U+FF38
             58 => (Source => 'Y', Target => 'Ｙ'),      --  U+FF39
             59 => (Source => 'Z', Target => 'Ｚ'),      --  U+FF3A
             60 => (Source => '[', Target => '［'),      --  U+FF3B
             61 => (Source => '\', Target => '＼'),      --  U+FF3C
             62 => (Source => ']', Target => '］'),      --  U+FF3D
             63 => (Source => '^', Target => '＾'),      --  U+FF3E
             64 => (Source => '_', Target => '＿'),      --  U+FF3F
             65 => (Source => '`', Target => '｀'),      --  U+FF40
             66 => (Source => 'a', Target => 'ａ'),      --  U+FF41
             67 => (Source => 'b', Target => 'ｂ'),      --  U+FF42
             68 => (Source => 'c', Target => 'ｃ'),      --  U+FF43
             69 => (Source => 'd', Target => 'ｄ'),      --  U+FF44
             70 => (Source => 'e', Target => 'ｅ'),      --  U+FF45
             71 => (Source => 'f', Target => 'ｆ'),      --  U+FF46
             72 => (Source => 'g', Target => 'ｇ'),      --  U+FF47
             73 => (Source => 'h', Target => 'ｈ'),      --  U+FF48
             74 => (Source => 'i', Target => 'ｉ'),      --  U+FF49
             75 => (Source => 'j', Target => 'ｊ'),      --  U+FF4A
             76 => (Source => 'k', Target => 'ｋ'),      --  U+FF4B
             77 => (Source => 'l', Target => 'ｌ'),      --  U+FF4C
             78 => (Source => 'm', Target => 'ｍ'),      --  U+FF4D
             79 => (Source => 'n', Target => 'ｎ'),      --  U+FF4E
             80 => (Source => 'o', Target => 'ｏ'),      --  U+FF4F
             81 => (Source => 'p', Target => 'ｐ'),      --  U+FF50
             82 => (Source => 'q', Target => 'ｑ'),      --  U+FF51
             83 => (Source => 'r', Target => 'ｒ'),      --  U+FF52
             84 => (Source => 's', Target => 'ｓ'),      --  U+FF53
             85 => (Source => 't', Target => 'ｔ'),      --  U+FF54
             86 => (Source => 'u', Target => 'ｕ'),      --  U+FF55
             87 => (Source => 'v', Target => 'ｖ'),      --  U+FF56
             88 => (Source => 'w', Target => 'ｗ'),      --  U+FF57
             89 => (Source => 'x', Target => 'ｘ'),      --  U+FF58
             90 => (Source => 'y', Target => 'ｙ'),      --  U+FF59
             91 => (Source => 'z', Target => 'ｚ'),      --  U+FF5A
             92 => (Source => '{', Target => '｛'),      --  U+FF5B
             93 => (Source => '|', Target => '｜'),      --  U+FF5C
             94 => (Source => '}', Target => '｝'),      --  U+FF5D
             95 => (Source => '~', Target => '～'));     --  U+FF5E

   Enclosed_Alphanumeric_Map_Data : constant Pseudo_Map_Vector := (
              1 => (Source => '1', Target => '①'),       --  U+2460
              2 => (Source => '2', Target => '②'),       --  U+2461
              3 => (Source => '3', Target => '③'),       --  U+2462
              4 => (Source => '4', Target => '④'),       --  U+2463
              5 => (Source => '5', Target => '⑤'),       --  U+2464
              6 => (Source => '6', Target => '⑥'),       --  U+2465
              7 => (Source => '7', Target => '⑦'),       --  U+2466
              8 => (Source => '8', Target => '⑧'),       --  U+2467
              9 => (Source => '9', Target => '⑨'),       --  U+2468
             10 => (Source => 'A', Target => 'Ⓐ'),       --  U+24B6
             11 => (Source => 'B', Target => 'Ⓑ'),       --  U+24B7
             12 => (Source => 'C', Target => 'Ⓒ'),       --  U+24B8
             13 => (Source => 'D', Target => 'Ⓓ'),       --  U+24B9
             14 => (Source => 'E', Target => 'Ⓔ'),       --  U+24BA
             15 => (Source => 'F', Target => 'Ⓕ'),       --  U+24BB
             16 => (Source => 'G', Target => 'Ⓖ'),       --  U+24BC
             17 => (Source => 'H', Target => 'Ⓗ'),       --  U+24BD
             18 => (Source => 'I', Target => 'Ⓘ'),       --  U+24BE
             19 => (Source => 'J', Target => 'Ⓙ'),       --  U+24BF
             20 => (Source => 'K', Target => 'Ⓚ'),       --  U+24C0
             21 => (Source => 'L', Target => 'Ⓛ'),       --  U+24C1
             22 => (Source => 'M', Target => 'Ⓜ'),       --  U+24C2
             23 => (Source => 'N', Target => 'Ⓝ'),       --  U+24C3
             24 => (Source => 'O', Target => 'Ⓞ'),       --  U+24C4
             25 => (Source => 'P', Target => 'Ⓟ'),       --  U+24C5
             26 => (Source => 'Q', Target => 'Ⓠ'),       --  U+24C6
             27 => (Source => 'R', Target => 'Ⓡ'),       --  U+24C7
             28 => (Source => 'S', Target => 'Ⓢ'),       --  U+24C8
             29 => (Source => 'T', Target => 'Ⓣ'),       --  U+24C9
             30 => (Source => 'U', Target => 'Ⓤ'),       --  U+24CA
             31 => (Source => 'V', Target => 'Ⓥ'),       --  U+24CB
             32 => (Source => 'W', Target => 'Ⓦ'),       --  U+24CC
             33 => (Source => 'X', Target => 'Ⓧ'),       --  U+24CD
             34 => (Source => 'Y', Target => 'Ⓨ'),       --  U+24CE
             35 => (Source => 'Z', Target => 'Ⓩ'),       --  U+24CF
             36 => (Source => 'a', Target => 'ⓐ'),       --  U+24D0
             37 => (Source => 'b', Target => 'ⓑ'),       --  U+24D1
             38 => (Source => 'c', Target => 'ⓒ'),       --  U+24D2
             39 => (Source => 'd', Target => 'ⓓ'),       --  U+24D3
             40 => (Source => 'e', Target => 'ⓔ'),       --  U+24D4
             41 => (Source => 'f', Target => 'ⓕ'),       --  U+24D5
             42 => (Source => 'g', Target => 'ⓖ'),       --  U+24D6
             43 => (Source => 'h', Target => 'ⓗ'),       --  U+24D7
             44 => (Source => 'i', Target => 'ⓘ'),       --  U+24D8
             45 => (Source => 'j', Target => 'ⓙ'),       --  U+24D9
             46 => (Source => 'k', Target => 'ⓚ'),       --  U+24DA
             47 => (Source => 'l', Target => 'ⓛ'),       --  U+24DB
             48 => (Source => 'm', Target => 'ⓜ'),       --  U+24DC
             49 => (Source => 'n', Target => 'ⓝ'),       --  U+24DD
             50 => (Source => 'o', Target => 'ⓞ'),       --  U+24DE
             51 => (Source => 'p', Target => 'ⓟ'),       --  U+24DF
             52 => (Source => 'q', Target => 'ⓠ'),       --  U+24E0
             53 => (Source => 'r', Target => 'ⓡ'),       --  U+24E1
             54 => (Source => 's', Target => 'ⓢ'),       --  U+24E2
             55 => (Source => 't', Target => 'ⓣ'),       --  U+24E3
             56 => (Source => 'u', Target => 'ⓤ'),       --  U+24E4
             57 => (Source => 'v', Target => 'ⓥ'),       --  U+24E5
             58 => (Source => 'w', Target => 'ⓦ'),       --  U+24E6
             59 => (Source => 'x', Target => 'ⓧ'),       --  U+24E7
             60 => (Source => 'y', Target => 'ⓨ'),       --  U+24E8
             61 => (Source => 'z', Target => 'ⓩ'));      --  U+24E9

   -----------------
   -- Add_Mapping --
   -----------------

   procedure Add_Mapping (Pseudo_Map : in out Pseudo_Map_Type;
                          Mapping    : Pseudo_Map_Vector) is
      From : Wide_Character_Set := Null_Set;
      To   : Wide_Character_Set := Null_Set;
   begin
      for I in Mapping'Range loop
         From := From or To_Set (Mapping (I).Source);
         To := To or To_Set (Mapping (I).Target);
      end loop;
      Pseudo_Map.Mapping := To_Mapping (To_Sequence (From), To_Sequence (To));
   end Add_Mapping;

   -------------------------------
   -- Enclosed_Alphanumeric_Map --
   -------------------------------

   function Enclosed_Alphanumeric_Map return Pseudo_Map_Vector is
   begin
      return Enclosed_Alphanumeric_Map_Data;
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
   begin
      return Halfwidth_Forms_Map_Data;
   end Halfwidth_Forms_Map;

   -------------------
   -- Lowercase_Map --
   -------------------

   function Lowercase_Map return Pseudo_Map_Vector is
   begin
      return Lowercase_Map_Data;
   end Lowercase_Map;

   ---------
   -- Map --
   ---------

   function Map (Pseudo_Map : Pseudo_Map_Type;
                 Ch         : Wide_Character) return Wide_Character is
   begin
      return Value (Pseudo_Map.Mapping, Ch);
   end Map;

   --------------
   -- Null_Map --
   --------------

   function Null_Map return Pseudo_Map_Vector is
   begin
      return Null_Map_Data;
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
   begin
      return Uppercase_Map_Data;
   end Uppercase_Map;

end ZanyBlue.Text.Pseudo;
