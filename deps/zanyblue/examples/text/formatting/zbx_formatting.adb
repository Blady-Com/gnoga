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

--
--  Example of the use of the ZanyBlue.Text packages
--

with Ada.Command_Line;
with Definitions;
with XFormatting_Messages;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Version_Status_Arguments;

procedure ZBX_Formatting is

   use Ada.Command_Line;
   use ZanyBlue.Text.Locales;
   use ZanyBlue.Text.Pseudo;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Text.Version_Status_Arguments;
   use Definitions;
   use Definitions.Long_Integer_Arguments;
   use Definitions.Long_Long_Integer_Arguments;

   Usage_Error, Help_Error : exception;

   procedure Process_Command_Line;

   procedure Display_Value (Value : Float);
   procedure Display_Value (Value : Float; Precision : Natural);
   procedure Display_Value (Value : Argument_Type'Class; Name : String);

   procedure Process_Command_Line is
   begin
      for I in 1 .. Argument_Count loop
         declare
            Option : constant String := Argument (I);
         begin
            if Option = "-xh" or Option = "-x" then
               Pseudo_Translate (Halfwidth_Forms_Map);
            elsif Option = "-xe" then
               Pseudo_Translate (Enclosed_Alphanumeric_Map);
            elsif Option = "-xl" then
               Pseudo_Translate (Lowercase_Map);
            elsif Option = "-xu" then
               Pseudo_Translate (Uppercase_Map);
            elsif Option = "-xn" then
               null;
            elsif Option = "-h" then
               raise Help_Error;
            elsif Option (1 .. 2) = "-l" then
               Set_Locale (Option (3 .. Option'Last));
            else
               raise Usage_Error;
            end if;
         end;
      end loop;
   end Process_Command_Line;

   procedure Display_Value (Value : Argument_Type'Class; Name : String) is
   begin
      Print_Line ("xformatting", "title", Value, +Name);
      Print_Line ("xformatting", "00001", Value);
      Print_Line ("xformatting", "00002", Value);
      Print_Line ("xformatting", "00003", Value);
      Print_Line ("xformatting", "00004", Value);
      Print_Line ("xformatting", "00005", Value);
      Print_Line ("xformatting", "00006", Value);
      Print_Line ("xformatting", "00007", Value);
      Print_Line ("xformatting", "00008", Value);
      Print_Line ("xformatting", "00009", Value);
      Print_Line ("xformatting", "00010", Value);
      Print_Line ("xformatting", "00011", Value);
      Print_Line ("xformatting", "00012", Value);
      Print_Line ("xformatting", "00013", Value);
      Print_Line ("xformatting", "00014", Value);
      Print_Line ("xformatting", "00015", Value);
      Print_Line ("xformatting", "00016", Value);
      Print_Line ("xformatting", "00017", Value);
      Print_Line ("xformatting", "00018", Value);
      Print_Line ("xformatting", "00019", Value);
      Print_Line ("xformatting", "00020", Value);
   end Display_Value;

   procedure Display_Value (Value : Float) is
   begin
      Print_Line ("xformatting", "00022", +Value);
   end Display_Value;

   procedure Display_Value (Value : Float; Precision : Natural) is
   begin
      Print_Line ("xformatting", "00021", +Value, +Precision);
   end Display_Value;

   X : constant Float := 1.2345678901e10;

begin
   Process_Command_Line;
   Print_Line ("xformatting", "banner", +ZanyBlue.Version_Major,
                                        +ZanyBlue.Version_Minor,
                                        +ZanyBlue.Version_Patch,
                                        +ZanyBlue.Version_Status);
   Display_Value (+Integer'(1964), "Simple positive value");
   Display_Value (+Integer'(-1964), "Simple negative value");
   Display_Value (+Integer'First, "First integer value");
   Display_Value (+Integer'Last, "Last integer value");
   Display_Value (+Long_Integer'First, "First long integer first value");
   Display_Value (+Long_Integer'Last, "Last long integer last value");
   Display_Value (+Long_Long_Integer'First, "First long long integer value");
   Display_Value (+Long_Long_Integer'Last, "Last long long integer value");
   Display_Value (+(Long_Long_Integer'Last - 1964),
                  "Random long long integer value");
   Display_Value (X);
   Display_Value (X, 0);
   Display_Value (X, 1);
   Display_Value (X, 2);
   Display_Value (X, 3);
   Display_Value (X, 4);
   Display_Value (X, 5);
   Display_Value (X, 6);
   Display_Value (X, 7);
   Display_Value (X, 8);
   Disable_Exceptions;
   Print_Line ("xformatting", "00023");
exception
when Help_Error =>
   Print_Line ("xformatting", "help");
when Usage_Error =>
   Print_Line ("xformatting", "usage");
end ZBX_Formatting;
