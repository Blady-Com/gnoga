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

with Ada.Strings.Wide_Unbounded;
with ZanyBlue.Text.Locales;

generic
   type Integer_Type is range <>;
package ZanyBlue.Text.Generic_Buffer is

   use ZanyBlue.Text.Locales;

   type Buffer_Type is private;

   Null_Character : constant Wide_Character := Wide_Character'Val (0);
   --  Utility character value used to indicate no character.

   procedure Add (Buffer : in out Buffer_Type;
                  Data   : Wide_Character);
   --  Append a character to left side of the buffer.

   procedure Add (Buffer : in out Buffer_Type;
                  Data   : Wide_String);
   --  Append a string to left side of the buffer.

   procedure Accumulate (Buffer     : in out Buffer_Type;
                         Value      : Integer_Type;
                         Locale     : Locale_Type;
                         Width      : Natural := 1;
                         Fill       : Wide_String := "";
                         Base       : Positive := 10;
                         Lowercase  : Boolean := True);
   --  Add (accumulate a numeric value to the left side of the buffer.
   --  The number is formatted with a field width of "Width" character
   --  filled to the left with the first character of the fill string
   --  or the locale "0" character if the fill string is empty.

   function To_String (Buffer : Buffer_Type) return Wide_String;
   --  Return the value generated on the left.

private

   use Ada.Strings.Wide_Unbounded;

   type Buffer_Type is
      record
         Data : Unbounded_Wide_String;
      end record;

end ZanyBlue.Text.Generic_Buffer;
