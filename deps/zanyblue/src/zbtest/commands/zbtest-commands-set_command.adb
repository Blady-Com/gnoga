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

with Ada.Calendar;
with Ada.Strings.Wide_Fixed;

separate (ZBTest.Commands)
procedure Set_Command (State : in out State_Type;
                       Args  : in List_Type) is

   use Ada.Strings.Wide_Fixed;

   type Type_Char_Type is ('s', 'i', 'b', 'f', 't');

   procedure Set_Value (State        : in out State_Type;
                        Undef_Check  : in Boolean;
                        Type_Char    : in Type_Char_Type;
                        Name         : in Wide_String;
                        Value        : in Wide_String);
   --  Set a parameter value.

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (State        : in out State_Type;
                        Undef_Check  : in Boolean;
                        Type_Char    : in Type_Char_Type;
                        Name         : in Wide_String;
                        Value        : in Wide_String) is
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
