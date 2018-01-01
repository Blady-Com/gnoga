--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2016, 2017, Michael Rohan <mrohan@zanyblue.com>
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

--------------------
-- Write_Test_XML --
--------------------

separate (ZBTest.States)
procedure Write_XML (State : in out State_Type) is

   use Ada.Calendar;

   N_Fail : constant Natural := State.Get_Integer ("_n_fail");
   N_Test : constant Natural := State.Get_Integer ("_n_tests");
   Elapsed : constant Float := Float (Clock - State.Get_Time ("_last_test"));
   Test_Name : constant Wide_String := State.Get_String ("_fulltestname");
   XML_Results : constant List_Type
                        := State.Get_List ("_xml_results", Deep => False);
   XML_File : File_Type;

begin
   if N_Test = 0 then
      --  No tests executed for the scope, skip the creation of the XML file
      return;
   end if;
   Create (XML_File, Out_File, "ZBTest-" & Wide_To_UTF8 (Test_Name) & ".xml");
   Print_01001 (XML_File);
   Print_01002 (+N_Fail, +N_Test, +Elapsed, +Test_Name,
      Destination => XML_File);
   for I in 1 .. Length (XML_Results) loop
      Print_00004 (+Value (XML_Results, I), Destination => XML_File);
   end loop;
   Print_01005 (XML_File);
   Close (XML_File);
end Write_XML;
