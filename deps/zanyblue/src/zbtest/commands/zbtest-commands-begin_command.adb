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

--  @usage begin
--  @summary begin a new parameter scope
--  @start-doc
--  The ``begin`` command starts a new ZBTest scope for parameters definitions.
--  This is similar to standard scoping in programming languages, local
--  definitions "hide" definitions in enclosing scopes, e.g.,::
--
--     set x 10
--     begin
--     set x 11
--     # References to the parameter "x" yield 11 here
--     end
--     # References to the parameter "x" yield 10 here
--
--  Scopes allow tests to inherit definitions from enclosing scopes and
--  localize parameter definitions and changes to the current test, restoring
--  values on scope exit via the :ref:`zb-zbtest-commands-end` command.
--
--  It is not normally necessary to explicitly begin a scope using the
--  ``begin`` command, scopes are implicitly created on starting the execution
--  of a ZBTest script via the :ref:`zb-zbtest-commands-run` command.

separate (ZBTest.Commands)
procedure Begin_Command (State   : in out State_Type;
                         Args    : List_Type) is

begin
   if Length (Args) /= 1 then
      raise Command_Usage_Error;
   end if;
   State.New_Scope;
end Begin_Command;
