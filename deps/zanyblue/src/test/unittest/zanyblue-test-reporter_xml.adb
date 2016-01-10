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

with GNAT.IO;            use GNAT.IO;

package body ZanyBlue.Test.Reporter_XML is

   use AUnit;

   procedure Write_String (Level   : Positive;
                           Prefix  : String;
                           Value   : String;
                           Postfix : String);
   procedure Dump_Result_List (L : Result_Lists.List);
   --  List failed assertions

   procedure Report_Test (Test : Test_Result);
   --  Report a single assertion failure or unexpected exception

   function Elapsed_Time (Test : Test_Result) return String;

   ----------------------
   -- Dump_Result_List --
   ----------------------

   procedure Dump_Result_List (L : Result_Lists.List) is

      use Result_Lists;

      C : Cursor := First (L);

   begin

      --  Note: can't use Iterate because it violates restriction
      --  No_Implicit_Dynamic_Code

      while Has_Element (C) loop
         Report_Test (Element (C));
         Next (C);
      end loop;
   end Dump_Result_List;

   ------------------
   -- Elapsed_Time --
   ------------------

   function Elapsed_Time (Test : Test_Result) return String is
      pragma Unreferenced (Test);
   begin
      return "0.0";
   end Elapsed_Time;

   ------------
   -- Report --
   ------------

   procedure Report (Engine : XML_Reporter;
                     R      : in out Result'Class;
                     Options : AUnit_Options := Default_Options)
   is
      pragma Unreferenced (Engine);
      pragma Unreferenced (Options);
   begin
      Put_Line ("<?xml version=""1.0"" encoding=""utf-8""?>");
      Put_Line ("<testsuites>");

      declare
         S : Result_Lists.List;
      begin
         Successes (R, S);
         Dump_Result_List (S);
      end;

      declare
         F : Result_Lists.List;
      begin
         Failures (R, F);
         Dump_Result_List (F);
      end;

      declare
         E : Result_Lists.List;
      begin
         Errors (R, E);
         Dump_Result_List (E);
      end;

      Put_Line ("</testsuites>");
   end Report;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Test (Test : Test_Result) is
   begin
      Write_String (1, "<testsuite name=""", Test.Test_Name.all, """");
      Write_String (2, "package=""", Test.Test_Name.all, """>");

      if Test.Routine_Name /= null then
         Write_String (2, "<testcase name=""", Test.Routine_Name.all, """");
      else
         Write_String (2, "<testcase name=""", "NONE", """");
      end if;
      Write_String (3, "time=""", Elapsed_Time (Test), """>");

      if Test.Failure /= null then
         Write_String (2, "<failure type=""", "assertion", """>");
         Write_String (3, "", Test.Failure.Message.all, "");
         if Test.Failure.Source_Name /= null then
            Write_String (3, "", Test.Failure.Source_Name.all,
                             ":" & Natural'Image (Test.Failure.Line));
         end if;
         Write_String (2, "</failure>", "", "");
      elsif Test.Error /= null then
         Write_String (2, "<failure type=""", "error", """");
         Write_String (3, "message=""", Test.Error.Exception_Name.all, """>");
         if Test.Error.Exception_Message /= null then
            Write_String (3, "", Test.Error.Exception_Message.all, "");
         end if;
         if Test.Error.Traceback /= null then
            Write_String (3, "", Test.Error.Traceback.all, "");
         end if;
         Write_String (2, "</failure>", "", "");
      end if;

      Put_Line ("    </testcase>");
      Put_Line ("  </testsuite>");

   end Report_Test;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (Level   : Positive;
                           Prefix  : String;
                           Value   : String;
                           Postfix : String) is
   begin
      for I in 1 .. Level loop
         Put ("  ");
      end loop;
      Put_Line (Prefix & Value & Postfix);
   end Write_String;

end ZanyBlue.Test.Reporter_XML;
