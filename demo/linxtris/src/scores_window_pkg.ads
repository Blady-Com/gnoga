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

--  with Gtk.Button; use Gtk.Button;
--  with Gtk.Window; use Gtk.Window;
--  with Gtk.Label;  use Gtk.Label;
with Gnoga.Gui.View;
with Gnoga_Extras;
with Gnoga.Gui.Element.Common;
with Ada.Direct_IO;

package Scores_Window_pkg is

   type Label_Array is array (1 .. 10) of Gnoga.Gui.View.View_Type;

   type Scores_Window_Record is new Gnoga_Extras.View_Type with record
      OK_Button    : Gnoga.Gui.Element.Common.Button_Type;
      Scores_Label : Label_Array;
      Names_Label  : Label_Array;
      Pause_Status : Boolean;
   end record;

   type Scores_Window is access all Scores_Window_Record'Class;

   Scores_Win : Scores_Window;

   procedure Gtk_new (Win : out Scores_Window);
   procedure Initialize (Win : access Scores_Window_Record'Class);

   procedure Get_Scores;
   procedure Set_Score
     (Name  : Gnoga.String;
      Score : Natural);
   function Get_Minimum_Score return Natural;
   function Get_Maximum_Score return Natural;
   Minimum_Score : Natural := 0;

   type Score_Type is record
      Name  : String (1 .. 20);
      Score : Natural;
   end record;

   package Scores_IO is new Ada.Direct_IO (Score_Type);

end Scores_Window_pkg;
