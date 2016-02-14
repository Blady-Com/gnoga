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

with Ada.Wide_Characters.Unicode;

package body ZBTest.Input_Parser is

   use Ada.Wide_Characters.Unicode;

   -----------------
   -- Parse_Words --
   -----------------

   function Parse_Words (Line : Wide_String) return List_Type is
      type State_Type is (Space, Word, String, Comment);
      Result    : List_Type;
      State     : State_Type := Space;
      End_Quote : Wide_Character := '"';
      First     : Positive := Line'First;
   begin
      for I in Line'Range loop
         case State is
         when Space =>
            if Line (I) = '"' or else Line (I) = ''' then
               First := I + 1;
               State := String;
               End_Quote := Line (I);
            elsif Line (I) = '#' then
               State := Comment;
            elsif not Is_Space (Line (I)) then
               First := I;
               State := Word;
            end if;
         when Word =>
            if Line (I) = '"' or else Line (I) = ''' then
               Append (Result, Line (First .. I - 1));
               First := I + 1;
               State := String;
               End_Quote := Line (I);
            elsif Is_Space (Line (I)) or else Line (I) = '#' then
               Append (Result, Line (First .. I - 1));
               if Line (I) = '#' then
                  State := Comment;
               else
                  State := Space;
               end if;
            end if;
         when String =>
            if Line (I) = End_Quote then
               Append (Result, Line (First .. I - 1));
               First := I + 1;
               State := Space;
            end if;
         when Comment =>
               null;
         end case;
      end loop;
      case State is
      when Space | Comment =>
         null;
      when Word =>
         Append (Result, Line (First .. Line'Last));
      when String =>
         raise Unterminated_String;
      end case;
      return Result;
   end Parse_Words;

end ZBTest.Input_Parser;
