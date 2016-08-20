--  -*- coding: utf-8 -*-
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

with Ada.Wide_Text_IO;
with Ada.Wide_Characters.Handling;
with ZanyBlue.Text.Codecs;
with ZanyBlue.Text.Formatting;
with ZBInfo_Messages.ZBInfo_Prints;

-------------------
-- Dump_Encoding --
-------------------

procedure ZBInfo.Dump_Encoding (Name            : Wide_String;
                                Reverse_Mapping : Boolean) is

   use Ada.Wide_Text_IO;
   use Ada.Wide_Characters.Handling;
   use ZanyBlue.Text.Codecs;
   use ZanyBlue.Text.Formatting;
   use ZBInfo_Messages.ZBInfo_Prints;
   use Implementation_Arguments;

   Codecs : constant Codecs_Type := Make_Codecs (Name);

   procedure Dump_Encoding (WCh : Wide_Character;
                            Encoding : String);
   --  Iteration handler to dump the encoding for an individual wide
   --  character (Unicode Code Point order).

   procedure Dump_Decoding (WCh : Wide_Character;
                            Encoding : String);
   --  Iteration handler to dump the encoding for an individual wide
   --  character (Encoded Code Points order).

   -------------------
   -- Dump_Decoding --
   -------------------

   procedure Dump_Decoding (WCh : Wide_Character;
                            Encoding : String) is
      Code_Point : constant Natural := Wide_Character'Pos (WCh);
   begin
      if Encoding'Length = 2 then
         if Is_Graphic (WCh) then
            Print_00035 (+Character'Pos (Encoding (Encoding'First)),
                         +Character'Pos (Encoding (Encoding'First + 1)),
                         +WCh, +Code_Point);
         else
            Print_00033 (+Character'Pos (Encoding (Encoding'First)),
                         +Character'Pos (Encoding (Encoding'First + 1)),
                         +Code_Point);
         end if;
      else
         if Is_Graphic (WCh) then
            Print_00036 (+Character'Pos (Encoding (Encoding'First)),
                         +WCh, +Code_Point);
         else
            Print_00034 (+Character'Pos (Encoding (Encoding'First)),
                         +Code_Point);
         end if;
      end if;
   end Dump_Decoding;

   -------------------
   -- Dump_Encoding --
   -------------------

   procedure Dump_Encoding (WCh : Wide_Character;
                            Encoding : String) is
      Code_Point : constant Natural := Wide_Character'Pos (WCh);
   begin
      if Is_Graphic (WCh) then
         Print_00029 (+Code_Point, +WCh, With_NL => False);
      else
         Print_00030 (+Code_Point, With_NL => False);
      end if;
      for I in Encoding'Range loop
         Print_00031 (+Character'Pos (Encoding (I)), With_NL => False);
      end loop;
      New_Line;
   end Dump_Encoding;

begin
   Print_00025 (+Name, +Codecs.Name);
   Print_00026 (+Codecs.Implementation);
   if Codecs.Implementation = Internal then
      return;
   end if;
   Print_00027;
   if Reverse_Mapping then
      Print_00032;
      Codecs.Iterate_Decodings (Dump_Decoding'Access);
   else
      Print_00028;
      Codecs.Iterate_Encodings (Dump_Encoding'Access);
   end if;
end ZBInfo.Dump_Encoding;
