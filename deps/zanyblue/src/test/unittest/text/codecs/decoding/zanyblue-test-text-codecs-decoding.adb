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

package body ZanyBlue.Test.Text.Codecs.Decoding is

   --------------------
   -- Check_Decoding --
   --------------------

   procedure Check_Decoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCh : Wide_Character;
                             Ch  : Character) is
      pragma Warnings (Off);
      Decoded : constant Wide_String := CS.Decode ("" & Ch);
      ECh : Wide_Character;
   begin
      WAssert (T, Decoded'Length = 1,
               CS.Name & " wrong length for single char ("
             & Natural'Wide_Image (Decoded'Length)
             & ")");
      ECh := Decoded (Decoded'First);
      WAssert (T, ECh = WCh,
               CS.Name & " wrong char for single char "
              & " ["
              & Natural'Wide_Image (Wide_Character'Pos (ECh))
              & "] /= ["
              & Natural'Wide_Image (Character'Pos (Ch))
              & "]");
   end Check_Decoding;

   --------------------
   -- Check_Decoding --
   --------------------

   procedure Check_Decoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCh : Wide_Character;
                             Ch1 : Character;
                             Ch2 : Character) is
      pragma Warnings (Off);
      Decoded : constant Wide_String := CS.Decode ("" & Ch1 & Ch2);
      ECh : Wide_Character;
   begin
      WAssert (T, Decoded'Length = 1,
               CS.Name & " wrong length for double char ("
             & Natural'Wide_Image (Decoded'Length)
             & ")");
      ECh := Decoded (Decoded'First);
      WAssert (T, ECh = WCh,
               CS.Name & " wrong char for first char "
              & " ["
              & Natural'Wide_Image (Wide_Character'Pos (ECh))
              & "] /= ["
              & Natural'Wide_Image (Wide_Character'Pos (WCh))
              & "]");
   end Check_Decoding;

   --------------------
   -- Check_Decoding --
   --------------------

   procedure Check_Decoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCP : Natural;
                             CP  : Natural) is
   begin
      Check_Decoding (T, CS, Wide_Character'Val (WCP), Character'Val (CP));
   end Check_Decoding;

   --------------------
   -- Check_Decoding --
   --------------------

   procedure Check_Decoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCP : Natural;
                             CP1 : Natural;
                             CP2 : Natural) is
   begin
      Check_Decoding (T, CS, Wide_Character'Val (WCP),
                      Character'Val (CP1), Character'Val (CP2));
   end Check_Decoding;

   --------------------
   -- Check_Decoding --
   --------------------

   procedure Check_Decoding (T   : in out Test_Case'Class;
                             CS  : Codecs_Type;
                             WCh : Wide_Character;
                             CP1 : Natural;
                             CP2 : Natural) is
   begin
      Check_Decoding (T, CS, Wide_Character'Pos (WCh), CP1, CP2);
   end Check_Decoding;

   --------------------
   -- Check_Decoding --
   --------------------

   procedure Check_Decoding (T      : in out Test_Case'Class;
                             Codecs : Codecs_Type;
                             Expect : Wide_String;
                             Source : String) is

      function Get_Char (S     : Wide_String;
                         Index : Natural) return Wide_Character;
      function Hex4 (Ch : Wide_Character) return Wide_String;

      Hex_Digits : constant Wide_String := "0123456789ABCDEF";

      --------------
      -- Get_Char --
      --------------

      function Get_Char (S     : Wide_String;
                         Index : Natural) return Wide_Character is
      begin
         if Index <= S'Length then
            return S (S'First + Index - 1);
         else
            return 'X';
         end if;
      end Get_Char;

      ----------
      -- Hex4 --
      ----------

      function Hex4 (Ch : Wide_Character) return Wide_String is
         Code_Point : Natural := Wide_Character'Pos (Ch);
         Result : Wide_String (1 .. 4) := (others => '0');
      begin
         for I in Result'Range loop
            declare
               Hex_Index : constant Natural := (Code_Point rem 16) + 1;
            begin
               Result (Result'Last - I + 1) := Hex_Digits (Hex_Index);
               Code_Point := Code_Point / 16;
            end;
         end loop;
         return Result;
      end Hex4;

      use Ada.Strings.Wide_Unbounded;
      Decoded : constant Wide_String := Codecs.Decode (Source);
      Message : Unbounded_Wide_String;
      Printing : Boolean := False;
   begin
      for I in 1 .. Integer'Max (Decoded'Length, Expect'Length) loop
         declare
            E_Ch : constant Wide_Character := Get_Char (Expect, I);
            D_Ch : constant Wide_Character := Get_Char (Decoded, I);
         begin
            if E_Ch /= D_Ch then
               Printing := True;
            end if;
            if Printing then
               Append (Message, " [");
               Append (Message, E_Ch);
               Append (Message, " ");
               Append (Message, Hex4 (E_Ch));
               if E_Ch /= D_Ch then
                  Append (Message, " /= ");
               else
                  Append (Message, " = ");
               end if;
               Append (Message, D_Ch);
               Append (Message, " ");
               Append (Message, Hex4 (D_Ch));
               Append (Message, "]");
            end if;
         end;
      end loop;
      WAssert (T, Decoded = Expect, To_Wide_String (Message));
   end Check_Decoding;

end ZanyBlue.Test.Text.Codecs.Decoding;
