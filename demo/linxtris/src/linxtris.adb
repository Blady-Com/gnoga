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

--  with Gtk.Main;
with Gnoga;
with Ada.Exceptions;

with Main_Window_Pkg;        use Main_Window_Pkg;
with Preferences_Window_Pkg; use Preferences_Window_Pkg;
with Scores_Window_pkg;      use Scores_Window_pkg;
with Game_Engine;            use Game_Engine;
with New_Score_Dialog_Pkg;   use New_Score_Dialog_Pkg;
with About_Dialog_Pkg;       use About_Dialog_Pkg;

procedure LinXtris is
begin
--     Gtk.Main.Init;
   Gtk_New (Main_Window);
   Gtk_new (Pref_Win);
   Gtk_new (Scores_Win);
   Gtk_New (New_Score_Dialog);
   Gtk_New (About_Dialog);
--     Show_All (Main_Window);
   Clear_Screen;
   Clear_Prev;
--     Gtk.Main.Main;
   exception
      when E : others =>
         Gnoga.Log ("LinXtris Error:");
         Gnoga.Log (Ada.Exceptions.Exception_Information (E));
end LinXtris;
