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

with Ada.Command_Line;
with Ada.Wide_Text_IO;
with Moons_Messages;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with Definitions;

procedure ZBX_Moons is

   use Ada.Command_Line;
   use Ada.Wide_Text_IO;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Formatting;
   use Definitions;
   use Definitions.Planet_Name_Formatting;
   use Definitions.Planet_Name_IO;

   Planet : Planet_Names;

begin
   if Argument_Count = 1 then
      Set_Locale (Argument (1));
   end if;
   Print_Line ("moons", "0001", +ZanyBlue.Version_Major,
                                +ZanyBlue.Version_Minor,
                                +ZanyBlue.Version_Patch);
   loop
      Print ("moons", "0002");
      Get (Planet);
      if Number_Of_Moons (Planet) /= 1 then
         Print_Line ("moons", "0003", +Number_Of_Moons (Planet),
                                      +Planet);
      else
         Print_Line ("moons", "0004", +Planet);
      end if;
   end loop;
exception
when End_Error | Data_Error =>
   New_Line;
   Print_Line ("moons", "0005");
end ZBX_Moons;
