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

with ZanyBlue.Text.Format_Errors;

package ZanyBlue.Test.Text.Format_Errors.Handler is

   use ZanyBlue.Text.Format_Errors;

   type Test_Handler_Type is new Error_Handler_Type with
      record
         N_Format_Not_Closed : Natural := 0;
         N_Illegal_Character : Natural := 0;
         N_Missing_Argument  : Natural := 0;
         Argument_Count      : Natural := 0;
      end record;

   overriding
   procedure Format_Not_Closed (Error_Handler  : in out Test_Handler_Type;
                                Message        : Wide_String;
                                Position       : Positive;
                                Level          : Natural;
                                Raise_Errors   : Boolean);

   overriding
   procedure Illegal_Character (Error_Handler  : in out Test_Handler_Type;
                                Message        : Wide_String;
                                Position       : Positive;
                                Ch             : Wide_Character;
                                Level          : Natural;
                                Raise_Errors   : Boolean);

   overriding
   procedure Missing_Argument (Error_Handler  : in out Test_Handler_Type;
                               Message        : Wide_String;
                               Position       : Natural;
                               Type_Name      : Wide_String;
                               Raise_Errors   : Boolean);

end ZanyBlue.Test.Text.Format_Errors.Handler;
