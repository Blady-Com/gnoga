--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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

with ZanyBlue.Text.Format_Parser;
with ZanyBlue.Text.Generic_Buffer;

package body ZanyBlue.Text.Generic_Integers is

   package Integer_Buffer is new ZanyBlue.Text.Generic_Buffer
     (Integer_Type => Integer_Type);

   use ZanyBlue.Text.Format_Parser;
   use Integer_Buffer;

   ------------
   -- Create --
   ------------

   function Create
     (Value : Integer_Type)
      return Integer_Argument_Type
   is
   begin
      return Integer_Argument_Type'(Data => Value);
   end Create;

   ------------
   -- Format --
   ------------

   overriding function Format
     (Value     : Integer_Argument_Type;
      Type_Name : String;
      Template  : String;
      Locale    : Locale_Type)
      return String
   is
      pragma Unreferenced (Type_Name);

      Formatting : constant Format_Type  := Parse (Template, Locale);
      Width      : Integer               := Formatting.Width;
      Buffer     : Buffer_Type;
      Lowercase  : Boolean               := True;
      Base       : Positive range 2 .. 16;
      X          : constant Integer_Type := Value.Data;

   begin
      --  Use the data type to determine the base to use
      case Formatting.Data is
         when 'b' =>
            Base := 2;
         when 'o' =>
            Base := 8;
         when 'x' | 'X' =>
            Base      := 16;
            Lowercase := Formatting.Data = 'x';
         when others =>
            Base := 10;
      end case;
      if X < 0 then
         --  Negative add '-' to the left buffer
         Add (Buffer, Numeric_Item (Locale, Minus_Character));
         Width := Width - 1;
      else
         --  Positive add '+' or ' ', if user requested
         case Formatting.Sign is
            when None | Minus =>
               null;
            when Plus =>
               Add (Buffer, Numeric_Item (Locale, Plus_Character));
               Width := Width - 1;
            when Space =>
               Add (Buffer, ' ');
               Width := Width - 1;
         end case;
      end if;
      if Formatting.Include_Base and then Base /= 10 then
         --  Decorator with base information if base /= 10 and user requested
         case Base is
            when 2 =>
               Add (Buffer, "2#");
               Width := Width - 3;
            when 8 =>
               Add (Buffer, "8#");
               Width := Width - 3;
            when 16 =>
               Add (Buffer, "16#");
               Width := Width - 4;
            when others =>
               null;
         end case;
      end if;
      if Formatting.Align = Numeric then
         if Formatting.Fill_Defined then
            Accumulate
              (Buffer, X, Locale, Width => Natural'Max (Width, 1),
               Fill                     => "" & Formatting.Fill, Base => Base,
               Lowercase                => Lowercase);
         else
            Accumulate
              (Buffer, X, Locale, Width => Natural'Max (Width, 1), Fill => "",
               Base                     => Base, Lowercase => Lowercase);
         end if;
      else
         Accumulate (Buffer, X, Locale, Base => Base, Lowercase => Lowercase);
      end if;
      if Formatting.Include_Base and then Base /= 10 then
         Add (Buffer, '#');
      end if;
      --  Apply alignment and return
      return
        Align
          (To_String (Buffer), Formatting.Fill, Formatting.Width,
           Formatting.Align);
   end Format;

end ZanyBlue.Text.Generic_Integers;
