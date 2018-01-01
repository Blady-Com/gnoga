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

--  @usage end
--  @summary end a parameter scope returning to previous scope
--  @start-doc
--  Exit a parameter scope.  Any assignments to made duing the scope
--  are lost.  Previous definitions are restored, e.g.,::
--
--      ZBTest> set xyz abc
--      ZBTest> begin
--      ZBTest> set xyz 123
--      ZBTest> echo $xyz
--      ZBTest> end
--      ZBTest> echo $xyz
--
--  The first "echo" prints the value "123" which the second prints the value
--  "abc".
--
--  It not normally necessary to use the "begin" and "end" commands as running
--  a test script automatically start a new scope which is ended when the
--  script completes.
--
--  The "end" command also executes any "end actions" defined by commands
--  executed during the scope.  E.g., the "copy" command add an "end action"
--  to remove the file or directory copied into the test area.
--

separate (ZBTest.Commands)
procedure End_Command (State : in out State_Type;
                       Args  : List_Type) is

   pragma Unreferenced (Args);

begin
   if State.Is_Defined ("_implicit_scope", False) then
      --  The _implicit_scope parameter is defined by running a script,
      --  cannot end implicit scope.
      Print_10039;
      return;
   end if;
   if State.Get_Integer ("_level") > 0 then
      State.End_Scope;
   else
      Print_10001;
   end if;
end End_Command;
