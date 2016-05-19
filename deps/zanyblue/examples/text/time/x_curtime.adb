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

with Ada.Calendar;
with Ada.Command_Line;
with Curtime_Messages;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Version_Status_Arguments;

procedure X_CurTime is

   use Ada.Calendar;
   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Text.Version_Status_Arguments;

   type Mode_Type is (Normal, Help);

   Usage_Error : exception;

   procedure Process_Command_Line (Mode : in out Mode_Type);

   procedure Process_Command_Line (Mode : in out Mode_Type) is
      use Ada.Command_Line;
      use ZanyBlue.Text.Pseudo;
      use ZanyBlue.Text.Locales;
   begin
      for I in 1 .. Argument_Count loop
         declare
            Option : constant String := Argument (I);
         begin
            if Option = "-xh" or Option = "-x" then
               Pseudo_Translate (Halfwidth_Forms_Map);
            elsif Option = "-xe" then
               Pseudo_Translate (Enclosed_Alphanumeric_Map);
            elsif Option = "-xl" then
               Pseudo_Translate (Lowercase_Map);
            elsif Option = "-xu" then
               Pseudo_Translate (Uppercase_Map);
            elsif Option = "-xn" then
               null;
            elsif Option (1 .. 2) = "-l" then
               Set_Locale (Option (3 .. Option'Last));
            elsif Option = "-h" then
               Mode := Help;
            else
               raise Usage_Error;
            end if;
         end;
      end loop;
   end Process_Command_Line;

   Time_Format : constant String := "EEE MMM d HH:mm:ss yyyy";

   Now : constant Time := Clock;
   Mode : Mode_Type := Normal;

begin
   Print_Line ("curtime", "0001", +ZanyBlue.Version_Major,
                                  +ZanyBlue.Version_Minor,
                                  +ZanyBlue.Version_Patch,
                                  +ZanyBlue.Version_Status);
   Process_Command_Line (Mode);
   case Mode is
   when Normal =>
      Print_Line ("curtime", "0002", +Now);
      Print_Line ("curtime", "0003", +Now);
      Print_Line ("curtime", "0004", +Now);
      Print_Line ("curtime", "0005", +Now);
      Print_Line ("curtime", "0006", +Now);
      Print_Line ("curtime", "0007", +Now);
      Print_Line ("curtime", "0008", +Now);
      Print_Line ("curtime", "0009", +Now);
      Print_Line ("curtime", "0010", +Now);
      Print_Line ("curtime", "0011", +Now, +Time_Format);
   when Help =>
      Print_Line ("curtime", "0012");
   end case;
end X_CurTime;
