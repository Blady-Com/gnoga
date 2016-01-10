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

with ZBTest.Functions;

separate (ZBTest.Commands)
procedure Help_Command (State : in out State_Type;
                        Args  : in List_Type) is
   pragma Unreferenced (State);

   use ZBTest.Functions;

   procedure Print_Command_Summary;
   --  Print a summary list of available commands.

   procedure Print_Function_Summary;
   --  Print a summary list of available functions.

   ---------------------------
   -- Print_Command_Summary --
   ---------------------------

   procedure Print_Command_Summary is
   begin
      Print_00007;
      for I in 1 .. Number_Of_Commands loop
         Print_Command_Summary (Command_Name (I), I);
      end loop;
      Print_00008;
   end Print_Command_Summary;

   ----------------------------
   -- Print_Function_Summary --
   ----------------------------

   procedure Print_Function_Summary is
   begin
      Print_00041;
      for I in 1 .. Number_Of_Functions loop
         Print_Function_Summary (Function_Name (I), I);
      end loop;
      Print_00042;
   end Print_Function_Summary;

   Command_Help  : Boolean := False;
   Function_Help : Boolean := False;
   Item_Index    : Natural := 0;

begin
   for I in 2 .. Length (Args) loop
      if Value (Args, I) = "-c" then
         Command_Help := True;
      elsif Value (Args, I) = "-f" then
         Function_Help := True;
      elsif Item_Index = 0 then
         Item_Index := I;
      else
         raise Command_Usage_Error;
      end if;
   end loop;
   if Function_Help and then Item_Index = 0 then
      Print_Function_Summary;
   elsif Function_Help and then Item_Index /= 0 then
      Print_Function_Help (Value (Args, Item_Index));
   elsif Command_Help and then Item_Index = 0 then
      Print_Command_Summary;
   elsif Item_Index /= 0 then
      Print_Command_Help (Value (Args, Item_Index));
   else
      --  No arguments or options, give help on help
      Print_Command_Help ("help");
   end if;
end Help_Command;
