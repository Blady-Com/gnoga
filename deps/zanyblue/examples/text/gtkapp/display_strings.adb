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

with Gtk.Enums;                use Gtk.Enums;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Widget;               use Gtk.Widget;
with Locale_Buttons;           use Locale_Buttons;
with ZanyBlue.Text.CLDR;       use ZanyBlue.Text.CLDR;
with ZanyBlue.Text.Locales;    use ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting; use ZanyBlue.Text.Formatting;

with AppMsg;
with Jenkins.Messages;
with Gtk.Button;               use Gtk.Button;
with Gtk.Label;                use Gtk.Label;
with Gtk.Separator;            use Gtk.Separator;
with Text_Display;             use Text_Display;

procedure Display_Strings (B : access Locale_Button_Record'Class) is

   procedure Add_Line (Dialog : Text_Display.Text_Display;
                       Text : String);
   procedure Add_Message (Dialog : Text_Display.Text_Display;
                          Key    : Wide_String;
                          Locale : Locale_Type);

   procedure Add_Line (Dialog : Text_Display.Text_Display;
                       Text : String) is
      Label  : Gtk_Label;
   begin
      Gtk_New (Label, Text);
      Pack_Start (Dialog.Vbox, Label);
   end Add_Line;

   procedure Add_Message (Dialog : Text_Display.Text_Display;
                          Key    : Wide_String;
                          Locale : Locale_Type) is
   begin
      Add_Line (Dialog, Format ("Jenkins", Key, Locale => Locale));
   end Add_Message;

   Locale : constant Locale_Type := B.Locale;
   Dialog : Text_Display.Text_Display;
   Button : Gtk_Button;
   Sep    : Gtk_Separator;

begin
   Gtk_New (Dialog);
   Set_Border_Width (Dialog, 10);

   if Territory (Locale) /= "" then
      Add_Line (Dialog, Format ("appmsg", "langname",
                                +Full_Locale_Name (Locale)));
   else
      Add_Line (Dialog, Format ("appmsg", "langname",
                                +Language_Name (Language (Locale))));
   end if;
   Gtk_New_Hseparator (Sep);
   Pack_Start (Dialog.Vbox, Sep);

   Add_Message (Dialog,
                "AboutJenkins.DisplayName",
                Locale);
   Add_Message (Dialog,
                "AboutJenkins.Description",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateAntFileMask.whitespaceSeprator",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateAntFileMask.doesntMatchAndSuggest",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateAntFileMask.portionMatchAndSuggest",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateAntFileMask."
              & "portionMatchButPreviousNotMatchAndSuggest",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateAntFileMask.doesntMatchAnything",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateAntFileMask."
              & "doesntMatchAnythingAndSuggest",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateRelativePath.wildcardNotAllowed",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateRelativePath.notFile",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateRelativePath.notDirectory",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateRelativePath.noSuchFile",
                Locale);
   Add_Message (Dialog,
                "FilePath.validateRelativePath.noSuchDirectory",
                Locale);

   Gtk_New (Button, Format ("appmsg", "close"));
   Pack_Start (Dialog.Action_Area, Button, True, True, 0);

   Show_All (Dialog);
end Display_Strings;

