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

--
--  Language 2-letter codes and names defined by Library of Congress:
--
--  http://www.loc.gov/standards/iso639-2
--
--  Territory codes defined by ISO 3166
--
--  http://www.iso.org/iso/list-en1-semic-3.txt
--

with Ada.Characters.Handling;
with Ada.Characters.Conversions;

package body ZanyBlue.Text.Utils is

   function Escape_Character
     (C : Unicode_Character)
      return String;
   --  Return the String value for a Wide Character, either the character
   --  itself if within Character range or the Unicode_Escape string for
   --  the character.

   function Requires_Unicode_Escape
     (C : Unicode_Character)
      return Boolean;
   --  Determine if a Wide character requires Unicode escaping for strings.

   function Unicode_Escape
     (C : Unicode_Character)
      return String;
   --  Return Unicode escape sequence for a wide character, e.g., "\u009e".

   -----------------------
   --  ASCII_Capitalize --
   -----------------------

   procedure ASCII_Capitalize (S : in out String) is
   begin
      for I in S loop
         S.Replace_Unicode (I, ASCII_Lowercase (S (I)));
      end loop;
      S.Replace_Unicode ((S.First), ASCII_Uppercase (S (S.First)));
   end ASCII_Capitalize;

   ---------------------
   -- ASCII_Lowercase --
   ---------------------

   procedure ASCII_Lowercase (S : in out String) is
   begin
      for I in S loop
         S.Replace_Unicode (I, ASCII_Lowercase (S (I)));
      end loop;
   end ASCII_Lowercase;

   ---------------------
   -- ASCII_Lowercase --
   ---------------------

   function ASCII_Lowercase
     (C : Unicode_Character)
      return Unicode_Character
   is

      use Ada.Characters.Handling;
      use Ada.Characters.Conversions;

      N_C : constant Character := To_Character (C);

   begin
      if Is_Letter (N_C) and then Is_Upper (N_C) then
         return To_Wide_Wide_Character (To_Lower (N_C));
      else
         return C;
      end if;
   end ASCII_Lowercase;

   ---------------------
   -- ASCII_Uppercase --
   ---------------------

   procedure ASCII_Uppercase (S : in out String) is
   begin
      for I in S loop
         S.Replace_Unicode (I, ASCII_Uppercase (S (I)));
      end loop;
   end ASCII_Uppercase;

   ---------------------
   -- ASCII_Uppercase --
   ---------------------

   function ASCII_Uppercase
     (C : Unicode_Character)
      return Unicode_Character
   is

      use Ada.Characters.Handling;
      use Ada.Characters.Conversions;

      N_C : constant Character := To_Character (C);

   begin
      if Is_Letter (N_C) and then Is_Lower (N_C) then
         return To_Wide_Wide_Character (To_Upper (N_C));
      else
         return C;
      end if;
   end ASCII_Uppercase;

   ----------------------
   -- Escape_Character --
   ----------------------

   function Escape_Character
     (C : Unicode_Character)
      return String
   is
   begin
      if Requires_Unicode_Escape (C) then
         return Unicode_Escape (C);
      else
         return From_Unicode (C);
      end if;
   end Escape_Character;

   -------------------
   -- Escape_String --
   -------------------

   function Escape_String
     (Source : String)
      return String
   is
      Result : String;
   begin
      for I in Source loop
         Result := Result & Escape_Character (Source (I));
      end loop;
      return Result;
   end Escape_String;

   ----------------------
   -- Non_Blank_Prefix --
   ----------------------

   function Non_Blank_Prefix
     (S : String)
      return String
   is
   begin
      for I in S loop
         if S (I) = ' ' then
            return S.Slice (S.First, I - 1);
         end if;
      end loop;
      return S;
   end Non_Blank_Prefix;

   -----------------------------
   -- Requires_Unicode_Escape --
   -----------------------------

   function Requires_Unicode_Escape
     (C : Unicode_Character)
      return Boolean
   is
      Pos : constant Natural := Unicode_Character'Pos (C);
      Ch  : Character;
   begin
      if Pos >= 127 then
         return True;
      end if;
      Ch := Character'Val (Pos);
      return Ch = '"' or else not Ada.Characters.Handling.Is_Graphic (Ch);
   end Requires_Unicode_Escape;

   -----------------
   -- Starts_With --
   -----------------

   function Starts_With
     (S      : String;
      Start  : Positive;
      Prefix : String)
      return Boolean
   is
   begin
      return Head (S.Slice (Start, S.Last), Prefix.Length) = Prefix;
   end Starts_With;

   ---------------------
   -- Unescape_String --
   ---------------------

   function Unescape_String
     (Source : String)
      return String
   is

      use Ada.Characters.Conversions;

      Wide_BS : constant Unicode_Character :=
        To_Wide_Wide_Character (ASCII.BS);
      Wide_HT : constant Unicode_Character :=
        To_Wide_Wide_Character (ASCII.HT);
      Wide_LF : constant Unicode_Character :=
        To_Wide_Wide_Character (ASCII.LF);
      Wide_FF : constant Unicode_Character :=
        To_Wide_Wide_Character (ASCII.FF);
      Wide_CR : constant Unicode_Character :=
        To_Wide_Wide_Character (ASCII.CR);

      Buffer : String;
      WCh    : Unicode_Character;
      Ch     : Unicode_Character;
      I      : Positive := Source.First;
      Done   : Boolean;

      procedure Get
        (Ch   : out Unicode_Character;
         Done : out Boolean);
      procedure Get
        (X    : out Natural;
         Done : out Boolean);

      procedure Get
        (Ch   : out Unicode_Character;
         Done : out Boolean)
      is
      begin
         Ch   := 'x';
         Done := I > Source.Last;
         if not Done then
            Ch := Source (I);
            I  := I + 1;
         end if;
      end Get;

      procedure Get
        (X    : out Natural;
         Done : out Boolean)
      is
         Offset : Natural;
         Base   : Natural;
         Ch     : Unicode_Character;
      begin
         Get (Ch, Done);
         case Ch is
            when '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
               Offset := Character'Pos ('0');
               Base   := 0;
            when 'a' | 'b' | 'c' | 'd' | 'e' | 'f' =>
               Offset := Character'Pos ('a');
               Base   := 10;
            when 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
               Offset := Character'Pos ('A');
               Base   := 10;
            when others =>
               raise Unicode_Format_Error
                 with To_Latin_1 (Source) & ':' & To_Character (Ch);
         end case;
         X := Base + Unicode_Character'Pos (Ch) - Offset;
      end Get;

      Digit : Natural;
      Value : Natural;

   begin
      Character_Loop :
      loop
         Get (Ch, Done);
         exit Character_Loop when Done;
         if Ch = '\' then
            Get (Ch, Done);
            exit Character_Loop when Done;
            case Ch is

               when 'b' =>               -- \b, backspace
                  WCh := Wide_BS;

               when 'f' =>               -- \f, form feed
                  WCh := Wide_FF;

               when 'n' =>               -- \n, newline
                  WCh := Wide_LF;

               when 'r' =>               -- \r, carriage return
                  WCh := Wide_CR;

               when 't' =>              -- \t, horizontal tab
                  WCh := Wide_HT;

               when 'u' =>               -- Unicode escape
                  Value := 0;
                  Unicode_Character_Loop :
                  for I in 1 .. 4 loop
                     Get (Digit, Done);
                     Value := Value * 16 + Digit;
                  end loop Unicode_Character_Loop;
                  WCh := Unicode_Character'Val (Value);

               when others =>
                  WCh := Ch;

            end case;
         else
            WCh := Ch;
         end if;
         Append (Buffer, WCh);
      end loop Character_Loop;
      return Buffer;
   end Unescape_String;

   --------------------
   -- Unicode_Escape --
   --------------------

   function Unicode_Escape
     (C : Unicode_Character)
      return String
   is
      Hex_Map : constant String := "0123456789abcdef";
      Result  : String;
      Pos     : Natural         := Unicode_Character'Pos (C);
   begin
      for I in 1 .. 4 loop
         Result.Replace_Unicode ((5 - I), Hex_Map ((Pos rem 16) + 1));
         Pos := Pos / 16;
      end loop;
      return "\u" & Result;
   end Unicode_Escape;

end ZanyBlue.Text.Utils;
