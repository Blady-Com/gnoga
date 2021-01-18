--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2018, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Exceptions;
with Ada.Characters.Conversions;

package body ZanyBlue.Text.Format_Errors is

   use Ada.Exceptions;
   use Ada.Characters.Conversions;

   -----------------------
   -- Format_Not_Closed --
   -----------------------

   procedure Format_Not_Closed (Error_Handler  : in out Error_Handler_Type;
                                Message        : Wide_String;
                                Position       : Positive;
                                Level          : Natural;
                                Raise_Errors   : Boolean) is
      pragma Unreferenced (Error_Handler);
      pragma Unreferenced (Message);
      pragma Unreferenced (Level);
   begin
      if Raise_Errors then
         Raise_Exception (Invalid_Format_Error'Identity,
                          Message => "NOTCLOSED:" & Positive'Image (Position));
      end if;
   end Format_Not_Closed;

   -----------------------
   -- Illegal_Character --
   -----------------------

   procedure Illegal_Character (Error_Handler  : in out Error_Handler_Type;
                                Message        : Wide_String;
                                Position       : Positive;
                                Ch             : Wide_Character;
                                Level          : Natural;
                                Raise_Errors   : Boolean) is
      pragma Unreferenced (Error_Handler);
      pragma Unreferenced (Message);
      pragma Unreferenced (Level);
   begin
      if Raise_Errors then
         Raise_Exception (Invalid_Format_Error'Identity,
                          Message => "ILLCHAR:"
                                   & Positive'Image (Position)
                                   & ":"
                                   & To_Character (Ch, Substitute => '?'));
      end if;
   end Illegal_Character;

   ----------------------
   -- Missing_Argument --
   ----------------------

   procedure Missing_Argument (Error_Handler  : in out Error_Handler_Type;
                               Message        : Wide_String;
                               Position       : Natural;
                               Type_Name      : Wide_String;
                               Raise_Errors   : Boolean) is
      pragma Unreferenced (Error_Handler);
      pragma Unreferenced (Message);
      pragma Unreferenced (Type_Name);
   begin
      if Raise_Errors then
         Raise_Exception (No_Such_Argument_Error'Identity,
                          Message => Positive'Image (Position));
      end if;
   end Missing_Argument;

end ZanyBlue.Text.Format_Errors;
