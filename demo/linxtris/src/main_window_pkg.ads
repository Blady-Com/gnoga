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

--  with Gtk.Window;            use Gtk.Window;
--  with Gtk.Menu_Bar;  use Gtk.Menu_Bar;
--  with Gtk.Menu;      use Gtk.Menu;
--  with Gtk.Menu_Item; use Gtk.Menu_Item;
--  with Gtk.Table;             use Gtk.Table;
--  with Double_Buffer; use Double_Buffer;
--  with Gdk.Pixbuf;    use Gdk.Pixbuf;
--  with Gdk;
--  with Gtk.Box;               use Gtk.Box;
--  with Gtk.Label;             use Gtk.Label;
with Block_Engine;
--  with Glib.Main;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D;

package Main_Window_Pkg is

   type Engine_Access is access Block_Engine.Object;
   type Animation_Type is (Line_Completed_Animation, Game_Over_Animation);

   type Main_Window_Record is new Gnoga.Gui.Window.Window_Type with record
      Menu_Bar    : Gnoga.Gui.View.Grid.Grid_View_Type;
      Game_Screen : Gnoga.Gui.Element.Canvas.Canvas_Type;
      Prev_Screen : Gnoga.Gui.Element.Canvas.Canvas_Type;
      Table       : Gnoga.Gui.View.Grid.Grid_View_Type;
--        Item_Game                 : Gtk_Menu_Item;
--        Item_Game_Submenu         : Gtk_Menu;
      Item_Game_New   : Gnoga.Gui.Element.Common.Button_Type;
      Item_Game_Pause : Gnoga.Gui.Element.Common.Button_Type;
--        Item_Game_Sep1            : Gtk_Menu_Item;
      Item_Game_Scores : Gnoga.Gui.Element.Common.Button_Type;
--        Item_Game_Sep2            : Gtk_Menu_Item;
      Item_Game_Quit : Gnoga.Gui.Element.Common.Button_Type;
--        Item_Settings             : Gtk_Menu_Item;
--        Item_Settings_Submenu     : Gtk_Menu;
      Item_Settings_Preferences : Gnoga.Gui.Element.Common.Button_Type;
--        Item_Help                 : Gtk_Menu_Item;
--        Item_Help_Submenu         : Gtk_Menu;
      Item_Help_About       : Gnoga.Gui.Element.Common.Button_Type;
      Box                   : Gnoga.Gui.View.Grid.Grid_View_Type;
      Blue_Pix              : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Green_Pix             : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Red_Pix               : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Grey_Pix              : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Blank_Pix             : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Yellow_Pix            : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Magenta_Pix           : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Cyan_Pix              : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Ghost_Pix             : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      White_Pix             : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Blue_Prev_Pix         : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Green_Prev_Pix        : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Red_Prev_Pix          : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Grey_Prev_Pix         : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Yellow_Prev_Pix       : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Magenta_Prev_Pix      : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Cyan_Prev_Pix         : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Blank_Prev_Pix        : Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access;
      Score_Label           : Gnoga.Gui.View.View_Type;
      Level_Label           : Gnoga.Gui.View.View_Type;
      Pieces_Label          : Gnoga.Gui.View.View_Type;
      Lines_Label           : Gnoga.Gui.View.View_Type;
      Record_Label          : Gnoga.Gui.View.View_Type;
      Engine                : Engine_Access;
      Game_Paused           : Boolean  := False;
      Exists_Game           : Boolean  := False;
      Game_Quit             : Boolean  := False;
      Continuous_Movement   : Boolean  := True;
      On_Animation          : Boolean  := False;
      Animation             : Boolean  := True;
      Animation_Count       : Natural  := 0;
      Animation_Kind        : Animation_Type;
      Piece_Preview         : Boolean  := True;
      Down_Timeout_ID       : Natural  := 0;
      Down_Timeout_Interval : Duration;
      Down_Count            : Natural  := 0;
      Delta_Y               : Integer  := 0;
      Max_Down_Count        : Positive := 40;
   end record;

   type Main_Window_Type is access all Main_Window_Record'Class;

   Main_Window : Main_Window_Type;
   Data_Dir    : Gnoga.String;

   procedure Gtk_New (Win : out Main_Window_Type);
   procedure Initialize (Win : access Main_Window_Record'Class);
   procedure Pause
     (Win   : access Main_Window_Record'Class;
      Value : Boolean);
private
   procedure Initialize_Pixmaps (Win : access Main_Window_Record'Class);
end Main_Window_Pkg;
