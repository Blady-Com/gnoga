-----------------------------------------------------------------------
--
--     Copyright (C) 2003 Dúlio Matos Leite de Carvalho e Silva
--
--     This file is part of LinXtris.
--
--     LinXtris is free software; you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation; either version 2 of the License, or
--     (at your option) any later version.
--
--     This program is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
--
--     You should have received a copy of the GNU General Public License
--     along with this program; if not, write to the Free Software
--     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-----------------------------------------------------------------------

--  with Gdk.Event;  use Gdk.Event;
--  with Gtk.Widget; use Gtk.Widget;
--  with Gtk.Handlers;    use Gtk.Handlers;
--  with Gtk.Menu_Item;   use Gtk.Menu_Item;
with Gnoga.Gui.Base;

package Main_Window_Pkg.Callbacks is

--     package Main_Window_Callback is new
--       Gtk.Handlers.Callback (Widget_Type => Main_Window_Record);
--
--     package Main_Window_Return_Callback is new Gtk.Handlers.Return_Callback
--       (Widget_Type => Main_Window_Record,
--        Return_Type => Boolean);
--
--     package Menu_Item_Callback is new
--       Gtk.Handlers.Callback (Widget_Type => Gtk_Menu_Item_Record);

--     function On_Main_Window_Delete
--       (Win   : access Main_Window_Record'Class;
--        Event : Gdk_Event) return Boolean;

--     procedure On_Main_Window_Destroy
--       (Win : access Main_Window_Record'Class);

   procedure On_Item_Game_Quit_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Item_Game_New_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Item_Game_Pause_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Item_Game_Scores_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Item_Settings_Preferences_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Item_Help_About_Pressed (Object : in out Gnoga.Gui.Base.Base_Type'Class);

--     function On_Main_Window_Key_Pressed
--       (Win   : access Main_Window_Record'Class;
--        Event : Gdk.Event.Gdk_Event) return Boolean;
   procedure On_Main_Window_Key_Pressed
     (Win   : in out Gnoga.Gui.Base.Base_Type'Class;
      Event : in     Gnoga.Gui.Base.Keyboard_Event_Record);

end Main_Window_Pkg.Callbacks;
