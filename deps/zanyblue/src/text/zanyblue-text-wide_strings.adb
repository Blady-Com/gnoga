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

with ZanyBlue.Text.Format_Parser;

package body ZanyBlue.Text.Wide_Strings is

   use ZanyBlue.Text.Format_Parser;

   ------------
   -- Create --
   ------------

   function Create (Wide_String_Value : in Wide_String)
      return Wide_String_Argument_Type is
   begin
      return Wide_String_Argument_Type'(Length => Wide_String_Value'Length,
                                        Data => Wide_String_Value);
   end Create;

   ------------
   -- Format --
   ------------

   function Format (Value     : in Wide_String_Argument_Type;
                    Type_Name : in Wide_String;
                    Template  : in Wide_String;
                    Locale    : in Locale_Type) return Wide_String is
      pragma Unreferenced (Type_Name);

      Formatting : constant Format_Type := Parse (Template, Locale);

   begin
      return Align (Value.Data, Formatting.Fill, Formatting.Width,
                    Formatting.Align);
   end Format;

end ZanyBlue.Text.Wide_Strings;
