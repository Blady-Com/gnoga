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
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Version_Status_Arguments;
with Definitions;

procedure X_Simple is

   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Text.Version_Status_Arguments;
   use Definitions.Long_Long_Float_Arguments;

   function Get_NaN return Float;
   procedure Process_Command_Line;

   Usage_Error : exception;

   function Get_NaN return Float is
      function Divide (X, Y : Float) return Float;
      function Divide (X, Y : Float) return Float is
      begin
         return X / Y;
      end Divide;
   begin
      return Divide (0.0, 0.0);
   end Get_NaN;

   procedure Process_Command_Line is
      use Ada.Command_Line;
      use ZanyBlue.Text.Pseudo;
      use ZanyBlue.Text.Locales;
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
            elsif Option (1 .. 2) = "-l" then
               Set_Locale (Option (3 .. Option'Last));
            else
               raise Usage_Error;
            end if;
         end;
      end loop;
   end Process_Command_Line;

   X              : constant Long_Long_Float := 3.21e+505;

begin
   Process_Command_Line;
   Print_Line ("This is SIMPLE_FORMAT, Version {0}.{1}.{2} - {3}",
               +ZanyBlue.Version_Major, +ZanyBlue.Version_Minor,
               +ZanyBlue.Version_Patch, +ZanyBlue.Version_Status);
   Print_Line ("X is {0,f}", +X);
   Print_Line ("Here is an ""{0}"" embedded string", +String'("a value"));
   Print_Line ("{1} base 16 in a field of width {0} is |{1:>{0,*}x}|",
               +10, +1964);
   Print_Line ("A NaN: {0}", +Get_NaN);
   Print_Line ("Expect a missing argument exception for the following");
   Print_Line ("Expect an exception for missing argument: ""{0}"", ""{1}""",
               +10);
end X_Simple;
