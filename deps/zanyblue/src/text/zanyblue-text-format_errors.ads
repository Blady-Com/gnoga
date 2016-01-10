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

package ZanyBlue.Text.Format_Errors is

   type Error_Handler_Type is tagged null record;

   procedure Format_Not_Closed (Error_Handler  : in out Error_Handler_Type;
                                Message        : in Wide_String;
                                Position       : in Positive;
                                Level          : in Natural;
                                Raise_Errors   : in Boolean);
   --  Handle the format reference in a message text string missing a
   --  closing brace, e.g., "Arg {0".

   procedure Illegal_Character (Error_Handler  : in out Error_Handler_Type;
                                Message        : in Wide_String;
                                Position       : in Positive;
                                Ch             : in Wide_Character;
                                Level          : in Natural;
                                Raise_Errors   : in Boolean);
   --  Handle an illegal character in a format string, e.g., only digits
   --  are expected after an opening brace, "Arg {x}"

   procedure Missing_Argument (Error_Handler  : in out Error_Handler_Type;
                               Message        : in Wide_String;
                               Position       : in Natural;
                               Type_Name      : in Wide_String;
                               Raise_Errors   : in Boolean);
   --  Handle a reference to a missing argument when formatting a message.

   Standard_Error_Handler : aliased Error_Handler_Type := (others => <>);
   --  The standard formatting handler: simply raise exceptions.

end ZanyBlue.Text.Format_Errors;
