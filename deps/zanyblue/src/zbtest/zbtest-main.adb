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
with Ada.Wide_Text_IO;
with Ada.Command_Line;
with Ada.Strings.Wide_Fixed;
with ZanyBlue.Utils;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Wide_Command_Line;
with ZBTest.States;
with ZBTest_Messages.ZBTest_Exceptions;
with ZBTest_Messages.ZBTest_Wide_Prints;

procedure ZBTest.Main is

   use Ada.Wide_Text_IO;
   use ZanyBlue.Text;
   use ZanyBlue.Utils;
   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Wide_Command_Line;
   use ZBTest.States;
   use ZBTest_Messages.ZBTest_Exceptions;
   use ZBTest_Messages.ZBTest_Wide_Prints;

   Usage_Error : exception;

   procedure Process_Command_Line (State : in out State_Type);

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line (State : in out State_Type) is

      procedure Handle_Argument (Value : Wide_String;
                                 Index : in out Positive);

      procedure Set_Option_Value (Parameter : Wide_String;
                                  Index     : in out Positive);

      procedure Set_Parameter_Value (Index     : in out Positive);

      procedure Handle_Argument (Value : Wide_String;
                                 Index : in out Positive) is
         use Ada.Strings.Wide_Fixed;
      begin
         if Value = "-h" then
            Print_00010;
            State.Set_Boolean ("_terminate", True);
         elsif Value = "-d" then
            Set_Parameter_Value (Index);
         elsif Value = "-N" then
            Set_Option_Value ("_testname", Index);
         elsif Value = "-t" then
            Set_Option_Value ("_testarea", Index);
         elsif Value = "-X" then
            Set_Option_Value ("_xml_file", Index);
         elsif Head (Value, 1) = "-" then
            Raise_10016 (Usage_Error'Identity, +Wide_Argument (Index));
         elsif not State.Is_Defined ("_testscript") then
            State.Set_String ("_testscript", Value);
         else
            Raise_10016 (Usage_Error'Identity, +Wide_Argument (Index));
         end if;
         Index := Index + 1;
      end Handle_Argument;

      ----------------------
      -- Set_Option_Value --
      ----------------------

      procedure Set_Option_Value (Parameter : Wide_String;
                                  Index     : in out Positive) is
      begin
         if Index < Wide_Argument_Count then
            Index := Index + 1;
            State.Set_String (Parameter, Wide_Argument (Index));
         else
            Raise_10015 (Usage_Error'Identity, +Wide_Argument (Index));
         end if;
      end Set_Option_Value;

      procedure Set_Parameter_Value (Index : in out Positive) is
      begin
         if Index + 1 < Wide_Argument_Count then
            Print_00031 (+Wide_Argument (Index + 1),
                         +Wide_Argument (Index + 2));
            State.Set_String (Wide_Argument (Index + 1),
                              Wide_Argument (Index + 2));
            Index := Index + 2;
         else
            Raise_10035 (Usage_Error'Identity, +Wide_Argument (Index));
         end if;
      end Set_Parameter_Value;

      Index : Positive := 1;

   begin
      while Index <= Wide_Argument_Count loop
         Handle_Argument (Wide_Argument (Index), Index);
      end loop;
      State.Set_Boolean ("_xml_p", State.Is_Defined ("_xml_file"));
   end Process_Command_Line;

   Start_Time : Ada.Calendar.Time;
   State : State_Type;

begin
   ZBTest_Messages.Initialize;
   Start_Time := Banner ("ZBTest");
   State.Define_Initial_Parameters;
   Process_Command_Line (State);
   Define_XML_Initial_Parameters (State);
   if not State.Get_Boolean ("_terminate") then
      State.Setup_Test_Area;
      if State.Is_Defined ("_testscript") then
         State.Execute_Line ("run " & State.Get_String ("_testscript"), True);
      else
         State.Read_Eval_Loop (Standard_Input, True);
      end if;
   end if;
   State.Write_XML_Report;
   Trailer ("ZBTest", Start_Time);
exception
when E : Usage_Error =>
   Print_10017 (+E);
   Trailer ("ZBTest", Start_Time);
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
when Invalid_Environment =>
   Trailer ("ZBTest", Start_Time);
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
when E : others =>
   Print_10033 (+E);
   Trailer ("ZBTest", Start_Time);
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end ZBTest.Main;
