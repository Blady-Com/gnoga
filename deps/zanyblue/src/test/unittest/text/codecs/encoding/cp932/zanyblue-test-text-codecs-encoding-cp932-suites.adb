--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2016, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Strings.Wide_Unbounded;

package body ZanyBlue.Test.Text.Codecs.Encoding.CP932.Suites is

   procedure T_0001 (T : in out Test_Case'Class);
   procedure T_0002 (T : in out Test_Case'Class);
   procedure T_0003 (T : in out Test_Case'Class);
   procedure T_0004 (T : in out Test_Case'Class);
   procedure T_0005 (T : in out Test_Case'Class);
   procedure T_0006 (T : in out Test_Case'Class);
   procedure T_0007 (T : in out Test_Case'Class);
   procedure T_0008 (T : in out Test_Case'Class);
   procedure T_0009 (T : in out Test_Case'Class);
   procedure T_0010 (T : in out Test_Case'Class);
   procedure T_0011 (T : in out Test_Case'Class);
   procedure T_0012 (T : in out Test_Case'Class);
   procedure T_0013 (T : in out Test_Case'Class);
   procedure T_0014 (T : in out Test_Case'Class);
   procedure T_0015 (T : in out Test_Case'Class);
   procedure T_0016 (T : in out Test_Case'Class);
   procedure T_0017 (T : in out Test_Case'Class);
   procedure T_0018 (T : in out Test_Case'Class);

   procedure Check_String (T      : in out Test_Case'Class;
                           Source : String;
                           Expect : String);

   Codecs : constant Codecs_Type := Make_Codecs ("CP932");

   ------------------
   -- Check_String --
   ------------------

   procedure Check_String (T      : in out Test_Case'Class;
                           Source : String;
                           Expect : String) is

      use Ada.Strings.Wide_Unbounded;
      Hex_Digits : constant String := "0123456789ABCDEF";
      Encoded : constant String := Codecs.Encode (Source);
      Message : Unbounded_Wide_String;
   begin
      Append (Message, "Example string encoding failed, got");
      for I in Encoded'Range loop
         declare
            Ch_V : constant Natural := Character'Pos (Encoded (I));
            D1 : constant Natural := Ch_V / 16#10#;
            D2 : constant Natural := Ch_V rem 16#10#;
            Ch1 : constant Unicode_Character := Hex_Digits (D1 + 1);
            Ch2 : constant Unicode_Character := Hex_Digits (D2 + 1);
         begin
            Append (Message, ' ');
            Append (Message, Ch1);
            Append (Message, Ch2);
         end;
      end loop;
      WAssert (T, Encoded = Expect, To_Wide_String (Message));
   end Check_String;

   overriding
   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "ZanyBlue.Text.Codecs.Encoding.CP932");
      Add_Test_Routine (T, T_0001'Access, "T_0001, ASCII");
      Add_Test_Routine (T, T_0002'Access, "T_0002, First");
      Add_Test_Routine (T, T_0003'Access, "T_0003, Random");
      Add_Test_Routine (T, T_0004'Access, "T_0004, Last");
      Add_Test_Routine (T, T_0005'Access, "T_0005, CP932 example string#1");
      Add_Test_Routine (T, T_0006'Access, "T_0006, CP932 example string#2");
      Add_Test_Routine (T, T_0007'Access, "T_0007, CP932 example string#3");
      Add_Test_Routine (T, T_0008'Access, "T_0008, CP932 example string#4");
      Add_Test_Routine (T, T_0009'Access, "T_0009, CP932 example string#4");
      Add_Test_Routine (T, T_0010'Access, "T_0010, CP932 example string#5");
      Add_Test_Routine (T, T_0011'Access, "T_0011, CP932 example string#6");
      Add_Test_Routine (T, T_0012'Access, "T_0012, CP932 example string#7");
      Add_Test_Routine (T, T_0013'Access, "T_0013, CP932 example string#8");
      Add_Test_Routine (T, T_0014'Access, "T_0014, CP932 example string#9");
      Add_Test_Routine (T, T_0015'Access, "T_0015, CP932 example string#10");
      Add_Test_Routine (T, T_0016'Access, "T_0016, CP932 example string#11");
      Add_Test_Routine (T, T_0017'Access, "T_0017, CP932 example string#12");
      Add_Test_Routine (T, T_0018'Access, "T_0018, CP932 example string#13");
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
   procedure T_0005 (T : in out Test_Case'Class) is separate;
   procedure T_0006 (T : in out Test_Case'Class) is separate;
   procedure T_0007 (T : in out Test_Case'Class) is separate;
   procedure T_0008 (T : in out Test_Case'Class) is separate;
   procedure T_0009 (T : in out Test_Case'Class) is separate;
   procedure T_0010 (T : in out Test_Case'Class) is separate;
   procedure T_0011 (T : in out Test_Case'Class) is separate;
   procedure T_0012 (T : in out Test_Case'Class) is separate;
   procedure T_0013 (T : in out Test_Case'Class) is separate;
   procedure T_0014 (T : in out Test_Case'Class) is separate;
   procedure T_0015 (T : in out Test_Case'Class) is separate;
   procedure T_0016 (T : in out Test_Case'Class) is separate;
   procedure T_0017 (T : in out Test_Case'Class) is separate;
   procedure T_0018 (T : in out Test_Case'Class) is separate;

end ZanyBlue.Test.Text.Codecs.Encoding.CP932.Suites;
