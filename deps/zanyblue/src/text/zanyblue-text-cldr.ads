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

with ZanyBlue.Text.Locales;

package ZanyBlue.Text.CLDR is

   use ZanyBlue.Text.Locales;

   procedure Initialize;
   --  Initialize the CLDR database.  This routine must be called prior to
   --  using any of the other rountines defined in here.

   function Language_Name (Code    : Wide_String;
                           Unknown : Wide_String := "";
                           Locale  : Locale_Type := Current_Locale)
      return Wide_String;
   --
   --  Return the language name associated with an ISO language code, e.g.,
   --  in an English locale,
   --
   --     Language_Name ("fr") = "French"
   --
   --  For unkown languages, the argument string Unknown is returned, e.g.,
   --
   --     Language_Name ("xx", "UNKNOWN") = "UNKNOWN"
   --

   function Script_Name (Code    : Wide_String;
                         Unknown : Wide_String := "";
                         Locale  : Locale_Type := Current_Locale)
      return Wide_String;
   --
   --  Return the  name associated with an ISO script code, e.g.,
   --  in an English locale,
   --
   --     Script_Name ("Cyrl") = "Cyrillic"
   --
   --  For unkown scripts, the argument string Unknown is returned, e.g.,
   --
   --     Script_Name ("xx", "UNKNOWN") = "UNKNOWN"
   --

   function Territory_Name (Code    : Wide_String;
                            Unknown : Wide_String := "";
                            Locale  : Locale_Type := Current_Locale)
      return Wide_String;
   --
   --  Return the territory name associated with an ISO two letter territory
   --  code, e.g., in an English locale,
   --
   --     Territory_Name ("FR") = "France"
   --
   --  For unkown territories, the argument string Unknown is returned, e.g.,
   --
   --     Territory_Name ("xx", "UNKNOWN") = "UNKNOWN"
   --

   function Full_Locale_Name (Value  : Locale_Type;
                              Locale : Locale_Type := Current_Locale)
      return Wide_String;
   --
   --  Return the full locale name, language and territory
   --

end ZanyBlue.Text.CLDR;
