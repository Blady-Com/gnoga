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
--  Example of the use of the ZanyBlue.Text_IO packages based on the
--  Oracle JDBC messages (from ojdbc.jar).
--

with Ada.Command_Line;
with Jenkins.Messages;
with ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Null_Object;
with ZanyBlue.Text.Formatting;
with ZanyBlue.Text.Version_Status_Arguments;

procedure X_Jenkins is

   use ZanyBlue.Text.Null_Object;
   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Formatting;
   use ZanyBlue.Text.Version_Status_Arguments;

   Help_Error  : exception;
   Usage_Error : exception;

   procedure Display (Key : Wide_String;
                      Argument0 : Argument_Type'Class := Null_Argument;
                      Argument1 : Argument_Type'Class := Null_Argument;
                      Argument2 : Argument_Type'Class := Null_Argument);

   procedure Process_Command_Line;

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

   procedure Display (Key : Wide_String;
                      Argument0 : Argument_Type'Class := Null_Argument;
                      Argument1 : Argument_Type'Class := Null_Argument;
                      Argument2 : Argument_Type'Class := Null_Argument) is
   begin
      Print_Line ("Jenkins", Key, Argument0, Argument1, Argument2);
   end Display;

   Name1    : constant String := "This";
   Name2    : constant String := "That";
   Name3    : constant String := "Other";
   Num1     : constant Integer := 10;
   Num2     : constant Integer := 100;

begin
   Jenkins.Messages.Initialize;
   Print_Line ("App", "Banner", +ZanyBlue.Version_Major,
                                +ZanyBlue.Version_Minor,
                                +ZanyBlue.Version_Patch,
                                +ZanyBlue.Version_Status);
   Process_Command_Line;
   Display ("FilePath.validateAntFileMask.whitespaceSeprator");
   Display ("FilePath.validateAntFileMask.doesntMatchAndSuggest",
            +Name1, +Name2);
   Display ("FilePath.validateAntFileMask.portionMatchAndSuggest",
            +Name1, +Name2);
   Display ("FilePath.validateAntFileMask." &
            "portionMatchButPreviousNotMatchAndSuggest",
            +Name1, +Name2, +Name3);
   Display ("FilePath.validateAntFileMask.doesntMatchAnything", +Name1);
   Display ("FilePath.validateAntFileMask.doesntMatchAnythingAndSuggest",
            +Name1, +Name2);
   Display ("FilePath.validateRelativePath.wildcardNotAllowed");
   Display ("FilePath.validateRelativePath.notFile", +Name1);
   Display ("FilePath.validateRelativePath.notDirectory", +Name1);
   Display ("FilePath.validateRelativePath.noSuchFile", +Name1);
   Display ("FilePath.validateRelativePath.noSuchDirectory", +Name1);
   Display ("PluginManager.PluginDoesntSupportDynamicLoad.RestartRequired",
            +Name1);
   Display ("PluginManager.PluginIsAlreadyInstalled.RestartRequired",
            +Name1);
   Display ("FilePath.TildaDoesntWork");
   Display ("PluginManager.DisplayName");
   Display ("PluginManager.PortNotANumber");
   Display ("PluginManager.PortNotInRange", +Num1, +Num2);
   Display ("AboutJenkins.DisplayName");
   Display ("AboutJenkins.Description");
exception
when Usage_Error =>
   Print_Line ("App", "Usage");
when Help_Error =>
   Print_Line ("App", "Help");
end X_Jenkins;
