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

package ZanyBlue.Text.Utils is

   procedure ASCII_Capitalize (S : in out Wide_String);
   --  Convert a string to capitalize (in place).

   procedure ASCII_Lowercase (S : in out Wide_String);
   --  Convert a wide string to lowercase (in place).

   procedure ASCII_Uppercase (S : in out Wide_String);
   --  Convert a wide string to uppercase (in place).

   function ASCII_Uppercase (C : Wide_Character) return Wide_Character;
   --  Convert a wide character to uppercase.

   function ASCII_Lowercase (C : Wide_Character) return Wide_Character;
   --  Convert a wide character to lowercase.

   function Escape_String (Source : Wide_String) return String;
   --  Return the String value associated with a Wide_String containing
   --  Java style \u escape sequences, e.g.,
   --
   --  "This is (PI)" => "This is \u03c0"

   function Non_Blank_Prefix (S : Wide_String) return Wide_String;
   --  Return a non-blank prefix of a string.  E.g., "ar " returns "ar".
   --  This is a helper function for the codes for languages, scripts and
   --  territories.

   function Starts_With (S      : Wide_String;
                         Start  : Positive;
                         Prefix : Wide_String) return Boolean;
   --  Determine if a string begins with the Prefix starting at Start.

   function Unescape_String (Source : String) return Wide_String;
   --  Return the Wide_String value associated with a simple String containing
   --  Java style \u escape sequences, e.g.,
   --
   --  "This is \u03c0" => "This is (PI)"

end ZanyBlue.Text.Utils;
