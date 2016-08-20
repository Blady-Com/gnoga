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

with Ada.Text_IO.Text_Streams;
with ZanyBlue.OS;
with ZanyBlue.Text.Generic_Printer;

package body ZanyBlue.Text.Printer is

   The_Standard_Printer : aliased Standard_Printer_Type;

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (Printer     : in out Standard_Printer_Type;
                    Destination : Ada.Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean) is

      pragma Unreferenced (Printer);

      use Ada.Text_IO.Text_Streams;

      procedure New_Line (The_Stream : Stream_Access);
      procedure Put (The_Stream : Stream_Access; Text : Wide_String);

      The_Stream : constant Stream_Access := Stream (Destination);

      ----------
      -- Impl --
      ----------

      procedure Impl is
         new ZanyBlue.Text.Generic_Printer (
                File_Type => Stream_Access,
                Put => Put,
                New_Line => New_Line);

      --------------
      -- New_Line --
      --------------

      procedure New_Line (The_Stream : Stream_Access) is
      begin
         Put (The_Stream, ZanyBlue.OS.OS_New_Line);
      end New_Line;

      ---------
      -- Put --
      ---------

      procedure Put (The_Stream : Stream_Access; Text : Wide_String) is
         Encoded : constant String := Locale.Encode_To_String (Text);
      begin
         String'Write (The_Stream, Encoded);
      end Put;

   begin
      Impl (The_Stream, Facility, Key, Locale, Arguments, Message, With_NL);
   end Print;

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (Printer     : in out Standard_Printer_Type;
                    Destination : Ada.Wide_Text_IO.File_Type;
                    Facility    : Wide_String;
                    Key         : Wide_String;
                    Locale      : Locale_Type;
                    Arguments   : Argument_List;
                    Message     : Wide_String;
                    With_NL     : Boolean) is

      pragma Unreferenced (Printer);

      use Ada.Characters.Conversions;

      procedure New_Line (File : Ada.Wide_Text_IO.File_Type);
      --  procedure Put(File : Ada.Wide_Text_IO.File_Type; Text : Wide_String);

      ----------
      -- Impl --
      ----------

      procedure Impl is
         new ZanyBlue.Text.Generic_Printer (
                File_Type => Ada.Wide_Text_IO.File_Type,
                Put => Ada.Wide_Text_IO.Put,
                New_Line => New_Line);

      --------------
      -- New_Line --
      --------------

      procedure New_Line (File : Ada.Wide_Text_IO.File_Type) is
      begin
         Ada.Wide_Text_IO.New_Line (File);
      end New_Line;

      ---------
      -- Put --
      ---------

      --  procedure Put (File : Ada.Wide_Text_IO.File_Type; Text
      --  : Wide_String) is
      --  begin
         --  Ada.Wide_Text_IO.Put (File, Text);
      --  end Put;

   begin
      Impl (Destination, Facility, Key, Locale, Arguments, Message, With_NL);
   end Print;

   ----------------------
   -- Standard_Printer --
   ----------------------

   function Standard_Printer return Printer_Access is
   begin
      return The_Standard_Printer'Access;
   end Standard_Printer;

end ZanyBlue.Text.Printer;
