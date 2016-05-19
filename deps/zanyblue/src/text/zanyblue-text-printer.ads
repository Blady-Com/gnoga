--  -*- coding: utf-8 -*-
--
--  ZanyBlue, an Ada library and framework for finite element analysis.
--
--  Copyright (c) 2012, 2016, Michael Rohan <mrohan@zanyblue.com>
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

with Ada.Text_IO;
with Ada.Wide_Text_IO;
with ZanyBlue.Text.Arguments;
with ZanyBlue.Text.Locales;

package ZanyBlue.Text.Printer is

   use ZanyBlue.Text.Arguments;
   use ZanyBlue.Text.Locales;

   type Printer_Type is abstract tagged private;
   type Printer_Access is not null access all Printer_Type'Class;

   procedure Print (Printer     : Printer_Type;
                    Destination : Ada.Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean) is
      abstract;

   procedure Print (Printer     : Printer_Type;
                    Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean) is
      abstract;

   type Standard_Printer_Type is new Printer_Type with private;

   overriding
   procedure Print (Printer     : Standard_Printer_Type;
                    Destination : Ada.Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean);

   overriding
   procedure Print (Printer     : Standard_Printer_Type;
                    Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean);

   function Standard_Printer return Printer_Access;

private

   type Printer_Type is abstract tagged null record;
   type Standard_Printer_Type is new Printer_Type with null record;

end ZanyBlue.Text.Printer;
