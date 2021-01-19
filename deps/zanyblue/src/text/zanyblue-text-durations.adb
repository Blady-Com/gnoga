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

with ZanyBlue.Text.Buffer;
with ZanyBlue.Text.Format_Parser;

package body ZanyBlue.Text.Durations is

   use ZanyBlue.Text.Buffer;
   use ZanyBlue.Text.Format_Parser;

   procedure Remainder_And_Scale
     (Value     : in out Natural;
      Remainder :    out Natural;
      Scale     :        Positive);
   --  Assign (Value rem Scale) to Remainder and then scale Value by Scale.

   ------------
   -- Create --
   ------------

   function Create
     (Duration_Value : Duration)
      return Duration_Argument_Type
   is
   begin
      return Duration_Argument_Type'(Data => Duration_Value);
   end Create;

   ------------
   -- Format --
   ------------

   overriding function Format
     (Value     : Duration_Argument_Type;
      Type_Name : Wide_String;
      Template  : Wide_String;
      Locale    : Locale_Type)
      return Wide_String
   is
      pragma Unreferenced (Type_Name);

      Formatting : constant Format_Type := Parse (Template, Locale);
      Buffer                                      : Buffer_Type;
      X : Natural              := Natural (Value.Data * 1_000.0);
      Milliseconds, Seconds, Minutes, Hours, Days : Natural;

   begin
      Remainder_And_Scale (X, Milliseconds, 1_000);
      Remainder_And_Scale (X, Seconds, 60);
      Remainder_And_Scale (X, Minutes, 60);
      Remainder_And_Scale (X, Hours, 24);
      Days := X;
      if Days > 0 then
         Accumulate (Buffer, Days, Locale, Width => 0);
         Add (Buffer, " ");
      end if;
      Accumulate (Buffer, Hours, Locale, Width => 1);
      Add (Buffer, ":");
      Accumulate (Buffer, Minutes, Locale, Width => 2);
      Add (Buffer, ":");
      Accumulate (Buffer, Seconds, Locale, Width => 2);
      Add (Buffer, ".");
      Accumulate (Buffer, Milliseconds, Locale, Width => 3);
      return
        Align
          (To_String (Buffer), Formatting.Fill, Formatting.Width,
           Formatting.Align);
   end Format;

   procedure Remainder_And_Scale
     (Value     : in out Natural;
      Remainder :    out Natural;
      Scale     :        Positive)
   is
   begin
      Remainder := Value rem Scale;
      Value     := Value / Scale;
   end Remainder_And_Scale;

end ZanyBlue.Text.Durations;
