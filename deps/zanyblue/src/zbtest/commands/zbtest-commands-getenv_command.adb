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

--  @usage getenv [ -l | -s | -p | -a ] name [ parameter ]
--  @summary define a parameter based on an environment variable
--  @start-doc
--  Define an internal parameter based on the value of an environment
--  variable.  For example,::
--
--      ZBTest> print HOME
--      The parameter "HOME" is not defined
--      ZBTest> getenv HOME
--      ZBTest> print HOME
--      /u/mrohan
--
--  The options available are
--
--  -l
--     Define an internal list parameter by splitting on the pathsep
--
--  -s
--     Define a simple scalar (string) parameter (default)
--
--  -a
--     Append the values (implies the -l option)
--
--  -p
--     Prepend the values (implies the -l option)
--
--  If the target is not given then import to name.
--

with Ada.Environment_Variables;

separate (ZBTest.Commands)
procedure Getenv_Command (State : in out State_Type;
                          Args  : List_Type) is

   use Ada.Environment_Variables;

   procedure Get_List_Value (State         : in out State_Type;
                             Source        : Wide_String;
                             Target        : Wide_String;
                             Append_Values : Boolean);
   --  Get an environment variable as a list value.

   --------------------
   -- Get_List_Value --
   --------------------

   procedure Get_List_Value (State         : in out State_Type;
                             Source        : Wide_String;
                             Target        : Wide_String;
                             Append_Values : Boolean) is

      procedure Parse_Values (List_Values : in out List_Type;
                              Definition  : Wide_String);

      ------------------
      -- Parse_Values --
      ------------------

      procedure Parse_Values (List_Values : in out List_Type;
                              Definition  : Wide_String) is

         PSep   : constant Wide_Character := State.Get_Character ("_pathsep");
         Start  : Positive := Definition'First;
         Next   : Positive := Definition'First;

      begin
         while Next <= Definition'Last loop
            if Definition (Next) = PSep then
               Append (List_Values, Definition (Start .. Next - 1));
               Start := Next + 1;
            end if;
            Next := Next + 1;
         end loop;
         Append (List_Values, Definition (Start .. Next - 1));
      end Parse_Values;

      List_Values : List_Type;

   begin  -- Get_List_Value
      if Exists (Wide_To_UTF8 (Source)) then
         Parse_Values (List_Values,
                       To_Wide_String (Value (Wide_To_UTF8 (Source))));
         if Append_Values then
            for I in 1 .. Length (List_Values) loop
               State.Append (Target, Value (List_Values, I));
            end loop;
         else
            for I in reverse 1 .. Length (List_Values) loop
               State.Prepend (Target, Value (List_Values, I));
            end loop;
         end if;
      else
         Print_10007 (+Source);
      end if;
   end Get_List_Value;

   Get_To_List   : Boolean := False;
   Append_Values : Boolean := True;
   Source_Idx    : Natural := 0;
   Target_Idx    : Natural := 0;

begin
   for I in 2 .. Length (Args) loop
      if Value (Args, I) = "-l" then
         Get_To_List := True;
      elsif Value (Args, I) = "-s" then
         Get_To_List := False;
      elsif Value (Args, I) = "-a" then
         Get_To_List := True;
         Append_Values := True;
      elsif Value (Args, I) = "-p" then
         Get_To_List := True;
         Append_Values := False;
      elsif Source_Idx = 0 then
         Source_Idx := I;
      elsif Target_Idx = 0 then
         Target_Idx := I;
      else
         raise Command_Usage_Error;
      end if;
   end loop;
   if Source_Idx = 0 then
      raise Command_Usage_Error;
   end if;
   if Target_Idx = 0 then
      Target_Idx := Source_Idx;
   end if;
   if Get_To_List then
      Get_List_Value (State, Value (Args, Source_Idx),
                      Value (Args, Target_Idx), Append_Values);
   else
      State.Set_String (
            Value (Args, Target_Idx),
            To_Wide_String (Value (Wide_To_UTF8 (Value (Args, Source_Idx)))));
   end if;
end Getenv_Command;
