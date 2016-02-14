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

package body ZBMCompile.Message_Filter is

   -----------------
   -- Is_Filtered --
   -----------------
   --      E00000 - E99999   Error
   --      W00000 - W99999   Warnings
   --      S00000 - S99999   Status
   --      I00000 - I99999   Informational
   --      V00000 - V99999   Verbose
   --      D00000 - D99999   Debug

   overriding
   function Is_Filtered (Filter    : Verbose_Filter_Type;
                         Facility  : Wide_String;
                         Key       : Wide_String) return Boolean is
      Result        : Boolean := False;
      Key_Character : Wide_Character;
   begin
      if Facility /= ZBMCompile_Facility or Key'Length < 1 then
         --  Non user message or degenerate key, print the message
         Result := False;
      else
         Key_Character := Key (Key'First);
         case Filter.Output_Level is
         when Silent  =>
            --  Silent, nothing is printed, everything is filtered
            return True;
         when Quiet =>
            case Key_Character is
            when 'E' | 'W' | 'S' =>
               Result := False;
            when 'I' | 'V' | 'D' =>
               Result := True;
            when others =>
               Result := False;
            end case;
         when Normal =>
            case Key_Character is
            when 'E' | 'W' | 'S' | 'I' =>
               Result := False;
            when 'V' | 'D' =>
               Result := True;
            when others =>
               Result := False;
            end case;
         when Verbose =>
            case Key_Character is
            when 'E' | 'W' | 'S' | 'I' | 'V' =>
               Result := False;
            when 'D' =>
               Result := True;
            when others =>
               Result := False;
            end case;
         when Debug =>
            case Key_Character is
            when 'E' | 'W' | 'S' | 'I' | 'V' | 'D' =>
               Result := False;
            when others =>
               Result := False;
            end case;
         end case;
      end if;
      return Result;
   end Is_Filtered;

end ZBMCompile.Message_Filter;
