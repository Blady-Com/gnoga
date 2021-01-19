--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2017, Michael Rohan <mrohan@zanyblue.com>
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
--  This is a simple wrapper package around the GNAT GNAT.Regexp
--  package with Wide_String arguments and functions.  The underlying
--  Strings to/from GNAT.Regexp are simply interpreted as UTF-8 encoded
--  strings.
--

with ZanyBlue.Text;

package body ZanyBlue.Wide_Wide_Regexp is

   use ZanyBlue.Text;

   -------------
   -- Compile --
   -------------

   function Compile
     (Pattern        : Wide_Wide_String;
      Glob           : Boolean := False;
      Case_Sensitive : Boolean := True)
      return Regexp
   is
   begin
      return
        GNAT.Regexp.Compile
          (Wide_Wide_To_UTF8 (Pattern), Glob, Case_Sensitive);
   end Compile;

   -----------
   -- Match --
   -----------

   function Match
     (S : Wide_Wide_String;
      R : Regexp)
      return Boolean
   is
   begin
      return GNAT.Regexp.Match (Wide_Wide_To_UTF8 (S), R);
   end Match;

end ZanyBlue.Wide_Wide_Regexp;
