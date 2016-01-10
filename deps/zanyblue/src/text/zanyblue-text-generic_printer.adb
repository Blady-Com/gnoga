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

with Ada.Strings.Wide_Fixed;

procedure ZanyBlue.Text.Generic_Printer (
             Destination : File_Type;
             Facility    : Wide_String;
             Key         : Wide_String;
             Locale      : ZanyBlue.Text.Locales.Locale_Type;
             Arguments   : ZanyBlue.Text.Arguments.Argument_List;
             Message     : Wide_String;
             With_NL     : Boolean) is

   pragma Unreferenced (Facility);
   pragma Unreferenced (Key);
   pragma Unreferenced (Locale);
   pragma Unreferenced (Arguments);

   use Ada.Strings.Wide_Fixed;

   NL     : constant Wide_String := "" & Wide_Character'Val (10);
   Done   : Boolean := False;
   Start  : Positive := Message'First;
   Finish : Natural;

begin
   --  Write the given text to the destination splitting on "\n"
   --  characters
   while not Done loop
      Finish := Index (Message, NL, Start);
      if Finish = 0 then
         Done := True;
         Finish := Message'Last + 1;
      end if;
      Put (Destination, Message (Start .. Finish - 1));
      if not Done then
         New_Line (Destination);
      end if;
      Start := Finish + 1;
   end loop;
   if With_NL then
      New_Line (Destination);
   end if;
end ZanyBlue.Text.Generic_Printer;
