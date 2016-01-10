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

with Ada.Strings.Wide_Fixed;

separate (ZBTest.Functions)
function Nextlog_Function (State : access State_Type;
                           Args  : in List_Type) return Wide_String is

   use Ada.Strings.Wide_Fixed;

   function Counter_Log_Name (State        : access State_Type;
                              Counter_Name : in Wide_String;
                              With_Undo    : in Boolean) return Wide_String;

   function Log_Name (State     : access State_Type;
                      Base_Name : in Wide_String;
                      Log_Num   : in Positive;
                      With_Undo : in Boolean) return Wide_String;

   ----------------------
   -- Counter_Log_Name --
   ----------------------

   function Counter_Log_Name (State        : access State_Type;
                              Counter_Name : in Wide_String;
                              With_Undo    : in Boolean) return Wide_String is
      Test_Name : constant Wide_String := State.Get_String ("_testname");
   begin
      if Counter_Name'Length = 0 or else Head (Counter_Name, 1) = "_" then
         raise Function_Usage_Error;
      end if;
      if not State.Is_Defined (Counter_Name) then
         State.Set_Integer (Counter_Name, 0);
      end if;
      State.Increment (Counter_Name, Deep => False);
      return Log_Name (State, Test_Name & "-" & Counter_Name,
                       State.Get_Integer (Counter_Name), With_Undo);
   end Counter_Log_Name;

   --------------
   -- Log_Name --
   --------------

   function Log_Name (State     : access State_Type;
                      Base_Name : in Wide_String;
                      Log_Num   : in Positive;
                      With_Undo : in Boolean) return Wide_String is

      Result : constant Wide_String := Format ("{0}-{1,=2}.log",
                                               +Base_Name, +Log_Num);
   begin
      if With_Undo then
         State.Add_Undo_Action ("compare " & Result);
      end if;
      return Result;
   end Log_Name;

   Counter_Index : Natural := 0;
   With_Undo     : Boolean := True;
   Index         : Positive := 2;

begin
   while Index <= Length (Args) loop
      if Value (Args, Index) = "-n" then
         With_Undo := not With_Undo;
      elsif Value (Args, Index) = "-c" and then Index < Length (Args) then
         Index := Index + 1;
         Counter_Index := Index;
         With_Undo := not With_Undo;
      else
         raise Function_Usage_Error;
      end if;
      Index := Index + 1;
   end loop;
   if Counter_Index /= 0 then
      return Counter_Log_Name (State, Value (Args, Counter_Index),
                               With_Undo);
   else
      State.Increment ("_lognum", Deep => False);
      return Log_Name (State, State.Get_String ("_testname"),
                       State.Get_Integer ("_lognum"), With_Undo);
   end if;
end Nextlog_Function;
