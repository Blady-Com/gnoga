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
with My_Package;
with Ada.Wide_Text_IO;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;

procedure Main is

   use Ada.Wide_Text_IO;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Formatting;

   procedure Dump_Values (Facility : Wide_String;
                          Locale   : Locale_Type;
                          Extra    : Boolean);

   procedure Dump_Values (Facility : Wide_String;
                          Locale   : Locale_Type;
                          Extra    : Boolean) is
   begin
      Put_Line ("DUMPING LOCALE: """ & Locale_Name (Locale) & """");
      Print_Line (Facility, "hello", Locale => Locale);
      Print_Line (Facility, "goodbye", Locale => Locale);
      if Extra then
         Print_Line (Facility, "extra", Locale => Locale);
      end if;
   end Dump_Values;

begin
   My_Package.Initialize;
   Dump_Values ("myapp1", Root_Locale, False);
   Dump_Values ("myapp1", Make_Locale ("fr"), False);
   Dump_Values ("myapp1", Make_Locale ("de"), False);
   Dump_Values ("myapp2", Root_Locale, True);
   Dump_Values ("myapp2", Make_Locale ("fr"), True);
   Dump_Values ("myapp2", Make_Locale ("de"), True);
end Main;
