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

with ZanyBlue.Text.Buffer;
with ZanyBlue.Text.Format_Parser;

package body ZanyBlue.Text.Generic_Modulars is

   use Ada.Characters.Conversions;
   use ZanyBlue.Text.Buffer;
   use ZanyBlue.Text.Format_Parser;

   ------------
   -- Create --
   ------------

   function Create (Value : in Modular_Type) return Modular_Argument_Type is
   begin
      return Modular_Argument_Type'(Data => Value);
   end Create;

   ------------
   -- Format --
   ------------

   function Format (Value     : in Modular_Argument_Type;
                    Type_Name : in Wide_String;
                    Template  : in Wide_String;
                    Locale    : in Locale_Type) return Wide_String is
      pragma Unreferenced (Type_Name);

      procedure Generate_Number (X : in Modular_Type'Base);

      Formatting : constant Format_Type := Parse (Template, Locale);
      Buffer     : Buffer_Type;
      Lowercase  : Boolean := True;
      Digit_Map  : Wide_String (1 .. 16);
      Base       : Modular_Type'Base range 2 .. 16;

      procedure Generate_Number (X : in Modular_Type'Base) is
      begin
         if X / Base > 0 then
            Generate_Number (X / Base);
         end if;
         Add (Buffer, Digit_Map (Integer (X rem Base) + 1));
      end Generate_Number;

   begin
      --  Use the data type to determine the base to use
      case Formatting.Data is
         when 'b' =>       Base := 2;
         when 'o' =>       Base := 8;
         when 'x' | 'X' => Base := 16;
                           Lowercase := Formatting.Data = 'x';
         when others =>    Base := 10;
      end case;
      Digit_Map := Locale_Digits (Locale, Lowercase);
      --  Positive add '+' or ' ', if user requested, negative not possible!
      case Formatting.Sign is
         when None | Minus =>
            null;
         when Plus =>
            Add (Buffer, Numeric_Item (Locale, Plus_Character));
         when Space =>
            Add (Buffer, ' ');
      end case;
      if Formatting.Include_Base and then Base /= 10 then
         --  Decorator with base information if base /= 10 and user requested
         case Base is
            when 2 =>      Add (Buffer, "2#");
            when 8 =>      Add (Buffer, "8#");
            when 16 =>     Add (Buffer, "16#");
            when others => null;
         end case;
      end if;
      Generate_Number (Value.Data);
      if Formatting.Include_Base and then Base /= 10 then
         --  Last character in the number buffer should be '#'
         Add (Buffer, '#');
      end if;
      --  Apply alignment and return
      return Align (To_String (Buffer),
                    Formatting.Fill, Formatting.Width, Formatting.Align);
   end Format;

end ZanyBlue.Text.Generic_Modulars;
