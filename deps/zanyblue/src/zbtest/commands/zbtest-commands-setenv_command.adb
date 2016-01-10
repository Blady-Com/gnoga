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

with Ada.Environment_Variables;
with Ada.Strings.Wide_Unbounded;

separate (ZBTest.Commands)
procedure Setenv_Command (State : in out State_Type;
                          Args  : in List_Type) is

   use Ada.Environment_Variables;

   procedure Set_Literal_Value (State       : in out State_Type;
                                Name        : in Wide_String;
                                Definition  : in Wide_String);
   --  Set an environment variable from a literal value.

   procedure Set_List_Value (State     : in out State_Type;
                             Name      : in Wide_String;
                             Parameter : in Wide_String);
   --  Set an environment variable from a list parameter value.

   procedure Set_Scalar_Value (State     : in out State_Type;
                               Name      : in Wide_String;
                               Parameter : in Wide_String);
   --  Set an environment variable from a scalar parameter value.

   --------------------
   -- Set_List_Value --
   --------------------

   procedure Set_List_Value (State     : in out State_Type;
                             Name      : in Wide_String;
                             Parameter : in Wide_String) is
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
                                Name        : in Wide_String;
                                Definition  : in Wide_String) is
   begin
      if Exists (To_UTF8 (Name)) then
         State.Add_Undo_Action (Format ("setenv {0} ""{1}""",
                                        +Name, +Value (To_UTF8 (Name))));
      else
         State.Add_Undo_Action (Format ("delenv {0}", +Name));
      end if;
      Print_00038 (+Name, +Definition);
      Set (To_UTF8 (Name), To_UTF8 (Definition));
   end Set_Literal_Value;

   ----------------------
   -- Set_Scalar_Value --
   ----------------------

   procedure Set_Scalar_Value (State     : in out State_Type;
                               Name      : in Wide_String;
                               Parameter : in Wide_String) is
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
