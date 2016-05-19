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

with Gtk.Separator;
with Gtk.Enums;

package body Text_Display is

   procedure Gtk_New (Dialog : out Text_Display) is
   begin
      Dialog := new Text_Display_Record;
      Initialize (Dialog);
   end Gtk_New;

   procedure Initialize (Dialog : access Text_Display_Record'Class) is
      Sep : Gtk.Separator.Gtk_Separator;
   begin
      Gtk.Window.Initialize (Dialog, Gtk.Enums.Window_Toplevel);

      Gtk.Box.Gtk_New_Vbox (Dialog.Vbox, False, 0);
      Add (Dialog, Dialog.Vbox);
      Gtk.Box.Show (Dialog.Vbox);

      Gtk.Box.Gtk_New_Hbox (Dialog.Action_Area, True, 5);
      Gtk.Box.Set_Border_Width (Dialog.Action_Area, 10);
      Gtk.Box.Pack_End (Dialog.Vbox, Dialog.Action_Area, False, True, 0);
      Gtk.Box.Show (Dialog.Action_Area);

      Gtk.Separator.Gtk_New_Hseparator (Sep);
      Gtk.Box.Pack_End (Dialog.Vbox, Sep, False, True, 0);
      Gtk.Separator.Show (Sep);
   end Initialize;

end Text_Display;
