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

separate (ZanyBlue.Test.Parameters.Scopes.Suites)
procedure T_0010 (R : in out AUnit.Test_Cases.Test_Case'Class) is

   use ZanyBlue.Parameters;

   function Check_List1 (L  : in List_Type;
                         V1 : in Wide_String) return Boolean;

   function Check_List2 (L  : in List_Type;
                         V1 : in Wide_String;
                         V2 : in Wide_String) return Boolean;

   -----------------
   -- Check_List1 --
   -----------------

   function Check_List1 (L  : in List_Type;
                         V1 : in Wide_String) return Boolean is
   begin
      if Length (L) /= 1 then
         return False;
      end if;
      if Value (L, 1) /= V1 then
         return False;
      end if;
      return True;
   end Check_List1;

   -----------------
   -- Check_List2 --
   -----------------

   function Check_List2 (L  : in List_Type;
                         V1 : in Wide_String;
                         V2 : in Wide_String) return Boolean is
   begin
      if Length (L) /= 2 then
         return False;
      end if;
      if Value (L, 1) /= V1 then
         return False;
      end if;
      if Value (L, 2) /= V2 then
         return False;
      end if;
      return True;
   end Check_List2;

   Scope : Parameter_Stack_Type;

begin
   Scope.New_Scope;
   WAssert (R, not Scope.Is_Defined ("l"), "'l' should not be defined");
   Scope.Set_Integer ("l", 10);
   WAssert (R, Check_List1 (Scope.Get_List ("l", Deep => True), "10"),
            "'l' should be [10]");
   Scope.New_Scope;
   Scope.Set_Integer ("l", 11);
   WAssert (R, Check_List2 (Scope.Get_List ("l", Deep => True), "11", "10"),
            "'l' should be [11, 10]");
   WAssert (R, Check_List1 (Scope.Get_List ("l", Deep => False), "11"),
            "'l' should be [11]");
   Scope.End_Scope;
   WAssert (R, Check_List1 (Scope.Get_List ("l", Deep => True), "10"),
            "'l' should be [10]");
end T_0010;
