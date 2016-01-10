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

with ZanyBlue.Text.Generic_Printer;
with ZanyBlue.OS;

package body ZanyBlue.Text.Printer is

   The_Standard_Printer : aliased Standard_Printer_Type;

   -----------
   -- Print --
   -----------

   procedure Print (Printer     : Standard_Printer_Type;
                    Destination : Ada.Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean) is

      pragma Unreferenced (Printer);

      procedure New_Line (File : Ada.Text_IO.File_Type);
      procedure Put (File : Ada.Text_IO.File_Type; Text : Wide_String);

      procedure Impl is
         new ZanyBlue.Text.Generic_Printer (
                File_Type => Ada.Text_IO.File_Type,
                Put => Put,
                New_Line => New_Line);

      procedure New_Line (File : Ada.Text_IO.File_Type) is
      begin
         Ada.Text_IO.New_Line (File);
      end New_Line;

      procedure Put (File : Ada.Text_IO.File_Type; Text : Wide_String) is
      begin
         Ada.Text_IO.Put (File, ZanyBlue.OS.To_UTF8 (Text));
      end Put;

   begin
      Impl (Destination, Facility, Key, Locale, Arguments, Message, With_NL);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Printer     : Standard_Printer_Type;
                    Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean) is

      pragma Unreferenced (Printer);

      procedure New_Line (File : Ada.Wide_Text_IO.File_Type);

      procedure Impl is
         new ZanyBlue.Text.Generic_Printer (
                File_Type => Ada.Wide_Text_IO.File_Type,
                Put => Ada.Wide_Text_IO.Put,
                New_Line => New_Line);

      procedure New_Line (File : Ada.Wide_Text_IO.File_Type) is
      begin
         Ada.Wide_Text_IO.New_Line (File);
      end New_Line;

   begin
      Impl (Destination, Facility, Key, Locale, Arguments, Message, With_NL);
   end Print;

   function Standard_Printer return Printer_Access is
   begin
      return The_Standard_Printer'Access;
   end Standard_Printer;

end ZanyBlue.Text.Printer;
