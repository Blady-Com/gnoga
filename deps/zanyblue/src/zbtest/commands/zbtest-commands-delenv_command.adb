--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2017, Michael Rohan <mrohan@zanyblue.com>
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

--  @usage delenv name
--  @summary delete an environment variable
--  @start-doc
--  Delete an environment variable.  The value of the variable, if any,
--  is restored on exiting the current scope.
--
--  Examples::
--
--    ZBTest> begin
--    ZBTest> delenv HOME
--    Deleting environment variable "HOME"
--    ZBTest> end
--    Executing the 'undo' action "setenv HOME "/home/mrohan""
--    Setting the environment variable "HOME" to "/home/mrohan"
--
--  Any commands executed after the "delenv" until the end of the scope will
--  not see a value for the environment variable.
--

with Ada.Environment_Variables;

separate (ZBTest.Commands)
procedure Delenv_Command (State : in out State_Type;
                          Args  : List_Type) is

   use Ada.Environment_Variables;

   procedure Delenv (State : in out State_Type;
                     Name  : String);
   --  Perform the actual environment variable deletion

   ------------
   -- Delenv --
   ------------

   procedure Delenv (State : in out State_Type;
                     Name  : String) is
   begin
      if Exists (To_UTF_8 (Name)) then
         State.Add_Undo_Action (Format ("setenv {0} ""{1}""",
                                        +Name, +Value (To_UTF_8 (Name))));
         Clear (To_UTF_8 (Name));
         Print_00040 (+Name);
      else
         Print_00039 (+Name);
      end if;
   end Delenv;

begin
   if Length (Args) /= 2 then
      raise Command_Usage_Error;
   end if;
   Delenv (State, Value (Args, 2));
end Delenv_Command;
