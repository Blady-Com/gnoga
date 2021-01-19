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

with ZanyBlue.Text.Pseudo;

package body ZanyBlue.Test.Text.Pseudo.Suites is

   use Ahven.Framework;
   use ZanyBlue.Text.Pseudo;

   procedure Check_Mapping (T : in out Test_Case'Class;
                            Mapping : Pseudo_Map_Type;
                            Source  : String;
                            Target  : String);

   procedure T_0001 (T : in out Test_Case'Class);
   procedure T_0002 (T : in out Test_Case'Class);
   procedure T_0003 (T : in out Test_Case'Class);
   procedure T_0004 (T : in out Test_Case'Class);

   procedure Check_Mapping (T : in out Test_Case'Class;
                            Mapping : Pseudo_Map_Type;
                            Source  : String;
                            Target  : String) is
      function Expected (Ch : Unicode_Character) return Unicode_Character;

      function Expected (Ch : Unicode_Character) return Unicode_Character is
      begin
         for I in Source'Range loop
            if Source (I) = Ch then
               return Target (I);
            end if;
         end loop;
         return Ch;
      end Expected;

   begin
      for Ch in Unicode_Character'Range loop
         WAssert (T, Map (Mapping, Ch) = Expected (Ch),
                  "Failed to Map (" & Ch & ") to " & Expected (Ch));
      end loop;
   end Check_Mapping;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Pseudo");
      Add_Test_Routine (T, T_0001'Access, "T_0001, Uppercase mappings");
      Add_Test_Routine (T, T_0002'Access, "T_0002, Lowercase mappings");
      Add_Test_Routine (T, T_0003'Access, "T_0003, Halfwidth_Forms mappings");
      Add_Test_Routine (T, T_0004'Access,
                        "T_0004, Enclosed_Alphanumeric mappings");
   end Initialize;

   function Suite return Test_Suite is
   begin
      return S : Test_Suite do
         Add_Test (S, new Test);
      end return;
   end Suite;

   procedure T_0001 (T : in out Test_Case'Class) is separate;
   procedure T_0002 (T : in out Test_Case'Class) is separate;
   procedure T_0003 (T : in out Test_Case'Class) is separate;
   procedure T_0004 (T : in out Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Pseudo.Suites;
