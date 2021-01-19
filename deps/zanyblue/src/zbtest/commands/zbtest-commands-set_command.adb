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

--  @usage set [ -s | -i | -b | -f | -t | -u ] parameter value
--  @summary set the value of a parameter
--  @start-doc
--  Set the value of a parameter.  The options selects type: integer,
--  boolean, etc. or, for "-u" conditionally set the parameter only if it is
--  not already defined, i.e., provide fall-back values for parameters that
--  can be set on the command line.
--
--  Examples,
--
--  * Set a parameter to a string value, the "-s" is optional::
--
--      ZBTest> set SFO "San Francisco"
--      ZBTest> print SFO
--      San Francisco
--      ZBTest> set -s LLW Lilongwe
--      ZBTest> print LLW
--      Lilongwe
--
--  * Set a parameter to an integer value, the "-i" is required::
--
--      ZBTest> set -i ten 10
--      ZBTest> print ten
--      10
--
--  * Set a parameter to a Boolean value, the "-b" is required::
--
--      ZBTest> set -b flag true
--      ZBTest> print flag
--      TRUE
--
--  * Set a parameter to a floating point value, the "-f" is required::
--
--      ZBTest> set -f pi 3.141592
--      ZBTest> print pi
--      3.14159E+00
--
--  * Set a parameter to a time value, the "-t" is required.  The only time
--    value supported is the special time "now"::
--
--      ZBTest> set -t start now
--      ZBTest> print start
--      1:59 PM 11/21/16
--
--  * Set a parameter if not already defined, e.g., via the command line::
--
--      ZBTest> set -u -s build_opt ""
--

with Ada.Calendar;
with Ada.Strings.Wide_Fixed;

separate (ZBTest.Commands)
procedure Set_Command (State : in out State_Type;
                       Args  : List_Type) is

   use Ada.Strings.Wide_Fixed;

   type Type_Char_Type is ('s', 'i', 'b', 'f', 't');

   procedure Set_Value (State        : in out State_Type;
                        Undef_Check  : Boolean;
                        Type_Char    : Type_Char_Type;
                        Name         : String;
                        Value        : String);
   --  Set a parameter value.

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (State        : in out State_Type;
                        Undef_Check  : Boolean;
                        Type_Char    : Type_Char_Type;
                        Name         : String;
                        Value        : String) is
   begin
      if Name (Name'First) = '_' then
         Print_10041 (+Name);
      elsif Undef_Check and then State.Is_Defined (Name) then
         Print_00030 (+Name);
      else
         case Type_Char is
         when 's' =>
            State.Set_String (Name, Value);
         when 'i' =>
            State.Set_Integer (Name, Integer'Wide_Value (Value));
         when 'b' =>
            State.Set_Boolean (Name, Boolean'Wide_Value (Value));
         when 'f' =>
            State.Set_Float (Name, Float'Wide_Value (Value));
         when 't' =>
            if Value = "now" then
               State.Set_Time (Name, Ada.Calendar.Clock);
            else
               Print_10006 (+Value);
            end if;
         end case;
      end if;
   exception
   when Constraint_Error =>
      Print_10004 (+Value);
   end Set_Value;

   Type_Char : Type_Char_Type := 's';
   Undef_Check  : Boolean := False;
   Param_Idx : Natural := 0;
   Value_Idx : Natural := 0;

begin
   for I in 2 .. Length (Args) loop
      if Value (Args, I) = "-s" then
         Type_Char := 's';
      elsif Value (Args, I) = "-i" then
         Type_Char := 'i';
      elsif Value (Args, I) = "-b" then
         Type_Char := 'b';
      elsif Value (Args, I) = "-f" then
         Type_Char := 'f';
      elsif Value (Args, I) = "-t" then
         Type_Char := 't';
      elsif Value (Args, I) = "-u" then
         Undef_Check := True;
      elsif Head (Value (Args, I), 1) = "-" then
         raise Command_Usage_Error;
      elsif Param_Idx = 0 then
         Param_Idx := I;
      elsif Value_Idx = 0 then
         Value_Idx := I;
      else
         raise Command_Usage_Error;
      end if;
   end loop;
   if Param_Idx = 0 or else Value_Idx = 0 then
      raise Command_Usage_Error;
   end if;
   Set_Value (State, Undef_Check, Type_Char,
                     Value (Args, Param_Idx),
                     Value (Args, Value_Idx));
end Set_Command;
