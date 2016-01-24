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

--  with Gtk.Main;               use Gtk.Main;
--  with Gtk;
with Game_Engine;
with Preferences_Window_Pkg; use Preferences_Window_Pkg;
with Scores_Window_pkg;      use Scores_Window_pkg;
with About_Dialog_Pkg;       use About_Dialog_Pkg;
with Gnoga.Application.Singleton;
with Main_Window_Pkg;        use Main_Window_Pkg;

package body Main_Window_Pkg.Callbacks is

--     function On_Main_Window_Delete
--       (Win   : access Main_Window_Record'Class;
--        Event : Gdk_Event) return Boolean
--     is
--        pragma Unreferenced (Event, Win);
--     begin
--        return False;
--     end On_Main_Window_Delete;
--
--     procedure On_Main_Window_Destroy
--       (Win : access Main_Window_Record'Class) is
--        pragma Unreferenced (Win);
--     begin
--        Gtk.Main.Main_Quit;
--     end On_Main_Window_Destroy;

   procedure On_Item_Game_Quit_Pressed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      --        Gtk.Main.Main_Quit;
      Game_Engine.Terminate_Game;
      Gnoga.Application.Singleton.End_Application;
   end On_Item_Game_Quit_Pressed;

--     function On_Main_Window_Key_Pressed
--       (Win   : access Main_Window_Record'Class;
--        Event : Gdk.Event.Gdk_Event) return Boolean
--     is
--     begin
--        return Game_Engine.Process_Key_Press (Win, Event);
--     end On_Main_Window_Key_Pressed;

   procedure On_Main_Window_Key_Pressed
     (Win   : in out Gnoga.Gui.Base.Base_Type'Class;
      Event : in     Gnoga.Gui.Base.Keyboard_Event_Record)
   is
   begin
      Game_Engine.Process_Key_Press (Main_Window_Record (Win), Event);
   end On_Main_Window_Key_Pressed;

   procedure On_Item_Game_New_Pressed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      Game_Engine.New_Game;
   end On_Item_Game_New_Pressed;

   procedure On_Item_Game_Pause_Pressed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      if Main_Window.Exists_Game then
         Main_Window.Pause (not Main_Window.Game_Paused);
      end if;
   end On_Item_Game_Pause_Pressed;

   procedure On_Item_Settings_Preferences_Pressed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      --        Show_All (Pref_Win);
      Pref_Win.Hidden (False);
   end On_Item_Settings_Preferences_Pressed;

   procedure On_Item_Help_About_Pressed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
--        Show_All (About_Dialog);
      About_Dialog.Hidden (False);
   end On_Item_Help_About_Pressed;

   procedure On_Item_Game_Scores_Pressed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      --        Show_All (Scores_Win);
      Scores_Win.Hidden (False);
   end On_Item_Game_Scores_Pressed;

end Main_Window_Pkg.Callbacks;
