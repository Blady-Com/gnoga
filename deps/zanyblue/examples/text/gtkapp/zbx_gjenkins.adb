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

with Ada.Command_Line;         use Ada.Command_Line;
with Gtk.Window;               use Gtk.Window;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Widget;               use Gtk.Widget;
with Locale_Buttons;           use Locale_Buttons;
with ZanyBlue.Text.CLDR;       use ZanyBlue.Text.CLDR;
with ZanyBlue.Text.Pseudo;     use ZanyBlue.Text.Pseudo;
with ZanyBlue.Text.Locales;    use ZanyBlue.Text.Locales;
with ZanyBlue.Text.Formatting; use ZanyBlue.Text.Formatting;

with AppMsg;
with Jenkins.Messages;
with Button_Cb;
with Display_Strings;

procedure ZBX_GJenkins is

   use ZanyBlue.Text;

   Usage_Error : exception;

   procedure Add_Buttons (Container    : Gtk_Box;
                          Locale_Name1 : Wide_String;
                          Locale_Name2 : Wide_String);
   procedure Process_Command_Line;

   procedure Process_Command_Line is
   begin
      for I in 1 .. Argument_Count loop
         declare
            Option : constant String := Argument (I);
         begin
            if Option = "-xh" or Option = "-x" then
               Pseudo_Translate (Halfwidth_Forms_Map);
            elsif Option = "-xe" then
               Pseudo_Translate (Enclosed_Alphanumeric_Map);
            elsif Option = "-xl" then
               Pseudo_Translate (Lowercase_Map);
            elsif Option = "-xu" then
               Pseudo_Translate (Uppercase_Map);
            elsif Option (1 .. 2) = "-l" then
               Set_Locale (Option (3 .. Option'Last));
            else
               raise Usage_Error;
            end if;
         end;
      end loop;
   end Process_Command_Line;

   procedure Add_Buttons (Container    : Gtk_Box;
                          Locale_Name1 : Wide_String;
                          Locale_Name2 : Wide_String) is
      Button  : Locale_Button;
      Hbox    : Gtk_Box;
   begin
      Gtk_New_Hbox (Hbox);
      Add (Container, Hbox);
      New_Locale_Button (Button, Make_Locale (Locale_Name1));
      Button_Cb.Connect (Button, "clicked",
                         Button_Cb.To_Marshaller (Display_Strings'Access));
      Pack_Start (Hbox, Button);
      New_Locale_Button (Button, Make_Locale (Locale_Name2));
      Button_Cb.Connect (Button, "clicked",
                         Button_Cb.To_Marshaller (Display_Strings'Access));
      Pack_Start (Hbox, Button);
   end Add_Buttons;

   Main_W           : Gtk_Window;
   VBox             : Gtk_Box;

begin
   Print_Line ("appmsg", "banner", +ZanyBlue.Version_Major,
                                   +ZanyBlue.Version_Minor,
                                   +ZanyBlue.Version_Patch);
   ZanyBlue.Text.CLDR.Initialize;
   AppMsg.Initialize;
   Jenkins.Messages.Initialize;
   Disable_Exceptions;
   Process_Command_Line;
   Gtk.Main.Init;

   Gtk_New (Main_W, Window_Toplevel);

   Gtk_New_Vbox (VBox);
   Add (Main_W, VBox);

   Add_Buttons (VBox, "", "da");
   Add_Buttons (VBox, "de", "es");
   Add_Buttons (VBox, "fr", "ja");
   Add_Buttons (VBox, "nl", "pt_BR");
   Add_Buttons (VBox, "ru", "tr");
   Add_Buttons (VBox, "zh_CN", "");

   Show_All (Main_W);
   Gtk.Main.Main;
exception
when Usage_Error =>
   Print_Line ("appmsg", "usage");
end ZBX_GJenkins;
