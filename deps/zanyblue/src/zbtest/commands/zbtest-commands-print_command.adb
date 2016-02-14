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

separate (ZBTest.Commands)
procedure Print_Command (State : in out State_Type;
                         Args  : List_Type) is

   use Ada.Strings.Wide_Fixed;

   procedure Handle_Argument (State    : in out State_Type;
                              Argument : Wide_String;
                              Scalar   : in out Boolean);
   --  Handle the printing of a value (scalar or list).

   procedure Print_List (State : in out State_Type;
                         Name  : Wide_String);

   ---------------------
   -- Handle_Argument --
   ---------------------

   procedure Handle_Argument (State    : in out State_Type;
                              Argument : Wide_String;
                              Scalar   : in out Boolean) is
   begin
      if Argument = "-l" then
         Scalar := False;
      elsif Argument = "-s" then
         Scalar := True;
      elsif Head (Argument, 1) = "-" then
         raise Command_Usage_Error;
      elsif Scalar then
         Print_00004 (+State.Get_String (Argument));
      else
         Print_List (State, Argument);
      end if;
   exception
   when Not_Defined_Error =>
      Print_10005 (+Argument);
   end Handle_Argument;

   ----------------
   -- Print_List --
   ----------------

   procedure Print_List (State : in out State_Type;
                         Name  : Wide_String) is
      List : constant List_Type := State.Get_List (Name);
      N_Elem : constant Natural := Length (List);
   begin
      if N_Elem = 0 then
         Print_00005;
      else
         for I in 1 .. N_Elem loop
            Print_00006 (+I, +Value (List, I));
         end loop;
      end if;
   end Print_List;

   Scalar : Boolean := True;

begin
   for I in 2 .. Length (Args) loop
      Handle_Argument (State, Value (Args, I), Scalar);
   end loop;
end Print_Command;
