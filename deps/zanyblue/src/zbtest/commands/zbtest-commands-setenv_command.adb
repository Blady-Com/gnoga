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

--  @usage setenv [ -l | -p ] variable value
--  @summary set an environment variable
--  @start-doc
--  Set an environment variable to a value, e.g.,::
--
--      ZBTest> setenv PROFILE yes
--      Setting the environment variable "PROFILE" to "yes"
--
--  sets the variable "PROFILE" to the value "yes" for the current process and
--  any processes created using the "execute" command.  The definition is
--  scoped and is reverted when the current scope exits (normally at the end of
--  a test script).  The reversion is either by deleting the environment
--  variable it did not have a current value or by restoring the value prior
--  to the setenv.
--
--  The options allow the definition of environment variables based on the
--  value of parameters.  For simple definitions, the "-p" option can be used,
--  e.g.,::
--
--      ZBTest> set LLW Lilongwe
--      ZBTest> setenv -p DESTINATION LLW
--      Setting the environment variable "DESTINATION" to "Lilongwe"
--
--  parameters are converted to strings for these simple definitions.  If the
--  parameter is a list, the "-l" option can be used which concatenates the
--  list elements into string separated by the OS separator character.   This
--  can be used to set PATH values, etc., e.g.,::
--
--      ZBTest> set mypath /bin
--      ZBTest> append mypath /usr/bin
--      ZBTest> setenv -l PATH mypath
--      Setting the environment variable "PATH" to "/bin:/usr/bin"
--

with Ada.Environment_Variables;
with Ada.Strings.Wide_Unbounded;

separate (ZBTest.Commands)
procedure Setenv_Command (State : in out State_Type;
                          Args  : List_Type) is

   use Ada.Environment_Variables;

   procedure Set_Literal_Value (State       : in out State_Type;
                                Name        : Wide_String;
                                Definition  : Wide_String);
   --  Set an environment variable from a literal value.

   procedure Set_List_Value (State     : in out State_Type;
                             Name      : Wide_String;
                             Parameter : Wide_String);
   --  Set an environment variable from a list parameter value.

   procedure Set_Scalar_Value (State     : in out State_Type;
                               Name      : Wide_String;
                               Parameter : Wide_String);
   --  Set an environment variable from a scalar parameter value.

   --------------------
   -- Set_List_Value --
   --------------------

   procedure Set_List_Value (State     : in out State_Type;
                             Name      : Wide_String;
                             Parameter : Wide_String) is
      use Ada.Strings.Wide_Unbounded;
      List_Value : constant List_Type := State.Get_List (Parameter);
      PSep       : constant Wide_Character := State.Get_Character ("_pathsep");
      N_Values   : constant Natural := Length (List_Value);
      Buffer     : Unbounded_Wide_String;
   begin
      for I in 1 .. N_Values loop
         Append (Buffer, Value (List_Value, I));
         if I /= N_Values then
            Append (Buffer, PSep);
         end if;
      end loop;
      Set_Literal_Value (State, Name, To_Wide_String (Buffer));
   end Set_List_Value;

   -----------------------
   -- Set_Literal_Value --
   -----------------------

   procedure Set_Literal_Value (State       : in out State_Type;
                                Name        : Wide_String;
                                Definition  : Wide_String) is
   begin
      if Exists (Wide_To_UTF8 (Name)) then
         State.Add_Undo_Action (Format ("setenv {0} ""{1}""",
                                        +Name, +Value (Wide_To_UTF8 (Name))));
      else
         State.Add_Undo_Action (Format ("delenv {0}", +Name));
      end if;
      Print_00038 (+Name, +Definition);
      Set (Wide_To_UTF8 (Name), Wide_To_UTF8 (Definition));
   end Set_Literal_Value;

   ----------------------
   -- Set_Scalar_Value --
   ----------------------

   procedure Set_Scalar_Value (State     : in out State_Type;
                               Name      : Wide_String;
                               Parameter : Wide_String) is
   begin
      Set_Literal_Value (State, Name, State.Get_String (Parameter));
   end Set_Scalar_Value;

   Set_From_Parameter : Boolean := False;
   Set_From_List      : Boolean := False;
   Name_Idx           : Natural := 0;
   Value_Idx          : Natural := 0;

begin
   for I in 2 .. Length (Args) loop
      if Value (Args, I) = "-l" then
         Set_From_List := True;
      elsif Value (Args, I) = "-p" then
         Set_From_Parameter := True;
      elsif Name_Idx = 0 then
         Name_Idx := I;
      elsif Value_Idx = 0 then
         Value_Idx := I;
      else
         raise Command_Usage_Error;
      end if;
   end loop;
   if Name_Idx = 0 then
      raise Command_Usage_Error;
   end if;
   if Value_Idx = 0 then
      raise Command_Usage_Error;
   end if;
   if Set_From_List then
      Set_List_Value (State, Value (Args, Name_Idx),
                             Value (Args, Value_Idx));
   elsif Set_From_Parameter then
      Set_Scalar_Value (State, Value (Args, Name_Idx),
                               Value (Args, Value_Idx));
   else
      Set_Literal_Value (State, Value (Args, Name_Idx),
                                Value (Args, Value_Idx));
   end if;
end Setenv_Command;
