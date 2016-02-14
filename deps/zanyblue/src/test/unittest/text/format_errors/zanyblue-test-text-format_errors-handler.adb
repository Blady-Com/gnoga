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

package body ZanyBlue.Test.Text.Format_Errors.Handler is

   overriding
   procedure Format_Not_Closed (Error_Handler  : in out Test_Handler_Type;
                                Message        : Wide_String;
                                Position       : Positive;
                                Level          : Natural;
                                Raise_Errors   : Boolean) is
      pragma Unreferenced (Message);
      pragma Unreferenced (Position);
      pragma Unreferenced (Level);
      pragma Unreferenced (Raise_Errors);
   begin
      Error_Handler. N_Format_Not_Closed := Error_Handler. N_Format_Not_Closed
                                            + 1;
   end Format_Not_Closed;

   overriding
   procedure Illegal_Character (Error_Handler  : in out Test_Handler_Type;
                                Message        : Wide_String;
                                Position       : Positive;
                                Ch             : Wide_Character;
                                Level          : Natural;
                                Raise_Errors   : Boolean) is
      pragma Unreferenced (Message);
      pragma Unreferenced (Position);
      pragma Unreferenced (Ch);
      pragma Unreferenced (Level);
      pragma Unreferenced (Raise_Errors);
   begin
      Error_Handler.N_Illegal_Character := Error_Handler.N_Illegal_Character
                                           + 1;
   end Illegal_Character;

   overriding
   procedure Missing_Argument (Error_Handler  : in out Test_Handler_Type;
                               Message        : Wide_String;
                               Position       : Natural;
                               Type_Name      : Wide_String;
                               Raise_Errors   : Boolean) is
      pragma Unreferenced (Message);
      pragma Unreferenced (Type_Name);
      pragma Unreferenced (Raise_Errors);
   begin
      Error_Handler.N_Missing_Argument := Error_Handler.N_Missing_Argument
                                          + 1;
      if Position + 1 > Error_Handler.Argument_Count then
         Error_Handler.Argument_Count := Position + 1;
      end if;
   end Missing_Argument;

end ZanyBlue.Test.Text.Format_Errors.Handler;
