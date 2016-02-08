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

--  with Gtk.Enums;
with Main_Window_Pkg.Callbacks; use Main_Window_Pkg.Callbacks;
--  with Gtk.Frame;
--  with Gtk.Alignment;
--  with Gtk.Accel_Group;
--  with Gdk.Types;
--  with Gdk.Types.Keysyms;
with Scores_Window_pkg;
--  with Glib;             use Glib;
with Ada.Command_Line; use Ada.Command_Line;
--  with Glib.Error;                use Glib.Error;
with Gnoga.Application;
with Gnoga.Application.Singleton;
with Gnoga.Types;

package body Main_Window_Pkg is

   procedure Gtk_New (Win : out Main_Window_Type) is
   begin
      Win        := new Main_Window_Record;
      Win.Engine := new Block_Engine.Object (10, 23);
      Block_Engine.Init (Win.Engine.all);
      Main_Window_Pkg.Initialize (Win);
   end Gtk_New;

   procedure Initialize (Win : access Main_Window_Record'Class) is
--        lbl    : Gtk.Label.Gtk_Label;
--        Frame  : Gtk.Frame.Gtk_Frame;
--        Alg    : Gtk.Alignment.Gtk_Alignment;
--        AGroup : Gtk.Accel_Group.Gtk_Accel_Group;
   begin

      --        Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);
      Gnoga.Application.Title ("LinXtris GNOGA");
      Gnoga.Application.Singleton.Initialize (Win.all);
      Win.Table.Create
      (Win.all,
       ((Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL)));
--        Gtk.Menu_Bar.Gtk_New (Win.Menu_Bar);
      Win.Menu_Bar.Create
      (Win.Table.Panel (1, 1).all,
       ((Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL,
         Gnoga.Gui.View.Grid.COL)));
      --        Gtk.Menu_Item.Gtk_New_With_Mnemonic (Win.Item_Game, "_Game");
--        Gtk.Menu_Bar.Add (Win.Menu_Bar, Win.Item_Game);
--        Gtk.Menu.Gtk_New (Win.Item_Game_Submenu);
--        Gtk.Menu_Item.Set_Submenu (Win.Item_Game, Win.Item_Game_Submenu);
--        Gtk.Menu_Item.Gtk_New_With_Mnemonic (Win.Item_Game_New, "_New");
      Win.Item_Game_New.Create (Win.Menu_Bar.Panel (1, 1).all, "New");
--        Gtk.Menu.Add (Win.Item_Game_Submenu, Win.Item_Game_New);
--        Gtk.Menu_Item.Gtk_New_With_Mnemonic (Win.Item_Game_Pause, "_Pause");
      Win.Item_Game_Pause.Create (Win.Menu_Bar.Panel (2, 1).all, "Pause");
      --        Gtk.Menu.Add (Win.Item_Game_Submenu, Win.Item_Game_Pause);
      Win.Item_Game_Pause.Disabled;
--        Gtk.Menu_Item.Set_Sensitive (Win.Item_Game_Pause, False);
--        Gtk.Menu_Item.Gtk_New (Win.Item_Game_Sep1);
--        Gtk.Menu.Add (Win.Item_Game_Submenu, Win.Item_Game_Sep1);
--        Gtk.Menu_Item.Gtk_New_With_Mnemonic
--          (Win.Item_Game_Scores, "_Scores...");
      Win.Item_Game_Scores.Create (Win.Menu_Bar.Panel (3, 1).all, "Scores");
--        Gtk.Menu.Add (Win.Item_Game_Submenu, Win.Item_Game_Scores);
--        Gtk.Menu_Item.Gtk_New (Win.Item_Game_Sep2);
--        Gtk.Menu.Add (Win.Item_Game_Submenu, Win.Item_Game_Sep2);
--        Gtk.Menu_Item.Gtk_New_With_Mnemonic (Win.Item_Game_Quit, "_Quit");
      Win.Item_Game_Quit.Create (Win.Menu_Bar.Panel (4, 1).all, "Quit");
--        Gtk.Menu.Add (Win.Item_Game_Submenu, Win.Item_Game_Quit);
--
--        Gtk.Menu_Item.Gtk_New_With_Mnemonic (Win.Item_Settings, "_Settings");
--        Gtk.Menu_Bar.Add (Win.Menu_Bar, Win.Item_Settings);
--        Gtk.Menu.Gtk_New (Win.Item_Settings_Submenu);
--        Gtk.Menu_Item.Set_Submenu (Win.Item_Settings,
--          Win.Item_Settings_Submenu);
--        Gtk.Menu_Item.Gtk_New_With_Mnemonic
--          (Win.Item_Settings_Preferences, "_Preferences");
      Win.Item_Settings_Preferences.Create
      (Win.Menu_Bar.Panel (1, 2).all, "Preferences");
      --        Gtk.Menu.Add (Win.Item_Settings_Submenu,
--                            Win.Item_Settings_Preferences);
--
--        Gtk.Menu_Item.Gtk_New_With_Mnemonic (Win.Item_Help, "_Help");
--        Gtk.Menu_Bar.Add (Win.Menu_Bar, Win.Item_Help);
--        Gtk.Menu.Gtk_New (Win.Item_Help_Submenu);
--        Gtk.Menu_Item.Set_Submenu (Win.Item_Help, Win.Item_Help_Submenu);
--        Gtk.Menu_Item.Gtk_New_With_Mnemonic (Win.Item_Help_About, "_About");
      Win.Item_Help_About.Create (Win.Menu_Bar.Panel (1, 3).all, "About");
--        Gtk.Menu.Add (Win.Item_Help_Submenu, Win.Item_Help_About);
--
--        Gtk.Accel_Group.Gtk_New (AGroup);
--        Add_Accel_Group (Win, AGroup);
--        Gtk.Menu_Item.Add_Accelerator
--          (Win.Item_Game_New,
--           "activate",
--           AGroup,
--           Gdk.Types.Keysyms.GDK_LC_n,
--           Gdk.Types.Control_Mask,
--           Gtk.Accel_Group.Accel_Visible);
--
--        Gtk.Menu_Item.Add_Accelerator
--          (Win.Item_Game_Quit,
--           "activate",
--           AGroup,
--           Gdk.Types.Keysyms.GDK_LC_q,
--           Gdk.Types.Control_Mask,
--           Gtk.Accel_Group.Accel_Visible);
--
--        Gtk.Menu_Item.Add_Accelerator
--          (Win.Item_Game_Pause,
--           "activate",
--           AGroup,
--           Gdk.Types.Keysyms.GDK_Pause,
--           Gdk.Types.Release_Mask,
--           Gtk.Accel_Group.Accel_Visible);

--        Gtk.Table.Gtk_New (Table => Win.Table, Rows => 2, Columns => 2,
--          Homogeneous => False);
--        Add (Win, Win.Table);
--        Gtk.Table.Set_Row_Spacing (Win.Table, 1, 100);
--
--        Gtk.Table.Attach (Win.Table, Win.Menu_Bar, 0, 2, 0, 1);
--
--        Gtk.Frame.Gtk_New (Frame);
--        Gtk.Frame.Set_Shadow_Type (Frame, Gtk.Enums.Shadow_In);
--        Gtk.Frame.Set_Border_Width (Frame, 10);
--
--        Double_Buffer.Gtk_New (Win.Game_Screen);
      Win.Game_Screen.Create (Win.Table.Panel (2, 1).all, 240, 480);
--        Gtk.Frame.Add (Frame, Win.Game_Screen);
--        Win.Game_Screen.Set_Size_Request (240, 480);
--        Gtk.Table.Attach (Win.Table, Frame, 0, 1, 1, 2);
--
--        Gtk.Box.Gtk_New_Vbox (Win.Box);
--        Gtk.Table.Attach (Win.Table, Win.Box, 1, 2, 1, 2, Xpadding => 10);
      Win.Box.Create
      (Win.Table.Panel (2, 2).all,
       ((1 => Gnoga.Gui.View.Grid.COL),
        (1 => Gnoga.Gui.View.Grid.COL),
        (1 => Gnoga.Gui.View.Grid.COL),
        (1 => Gnoga.Gui.View.Grid.COL),
        (1 => Gnoga.Gui.View.Grid.COL),
        (1 => Gnoga.Gui.View.Grid.COL),
        (1 => Gnoga.Gui.View.Grid.COL),
        (1 => Gnoga.Gui.View.Grid.COL),
        (1 => Gnoga.Gui.View.Grid.COL),
        (1 => Gnoga.Gui.View.Grid.COL)));
--
--        Double_Buffer.Gtk_New (Win.Prev_Screen);
      Win.Prev_Screen.Create (Win.Table.Panel (1, 2).all, 85, 85);
--        Win.Prev_Screen.Set_Size_Request (85, 85);
--        Gtk.Frame.Gtk_New (Frame);
--        Gtk.Frame.Set_Shadow_Type (Frame, Gtk.Enums.Shadow_In);
--        Gtk.Frame.Add (Frame, Win.Prev_Screen);
--        Gtk.Alignment.Gtk_New (Alg, 0.5, 0.5, 1.0, 0.0);
--        Gtk.Alignment.Add (Alg, Frame);
--        Gtk.Box.Pack_Start (Win.Box, Alg);
--
--        Gtk.Label.Gtk_New (lbl, "Score");
--        Gtk.Label.Set_Alignment (lbl, 0.5, 1.0);
--        Gtk.Box.Pack_Start (Win.Box, lbl);
      Win.Box.Panel (1, 1).Put ("Score");
--        Gtk.Label.Set_Size_Request (lbl, 85);
--        Gtk.Label.Gtk_New (Win.Score_Label, "0");
      Win.Score_Label.Create (Win.Box.Panel (2, 1).all);
      Win.Score_Label.Text ("0");
--        Gtk.Label.Set_Alignment (Win.Score_Label, 0.5, 0.0);
--        Gtk.Frame.Gtk_New (Frame);
--        Gtk.Frame.Set_Shadow_Type (Frame, Gtk.Enums.Shadow_In);
--        Gtk.Frame.Add (Frame, Win.Score_Label);
--        Gtk.Alignment.Gtk_New (Alg, 0.5, 0.0, 1.0, 0.0);
--        Gtk.Alignment.Add (Alg, Frame);
--        Gtk.Box.Pack_Start (Win.Box, Alg);
--
--        Gtk.Label.Gtk_New (lbl, "Level");
      Win.Box.Panel (3, 1).Put ("Level");
--        Gtk.Box.Pack_Start (Win.Box, lbl);
--        Gtk.Label.Set_Alignment (lbl, 0.5, 1.0);
--        lbl.Set_Size_Request (85, -1);
--        Gtk.Label.Gtk_New (Win.Level_Label, "1");
      Win.Level_Label.Create (Win.Box.Panel (4, 1).all);
      Win.Level_Label.Text ("1");
--        Gtk.Label.Set_Alignment (Win.Level_Label, 0.5, 0.0);
--        Gtk.Frame.Gtk_New (Frame);
--        Gtk.Frame.Set_Shadow_Type (Frame, Gtk.Enums.Shadow_In);
--        Gtk.Frame.Add (Frame, Win.Level_Label);
--        Gtk.Alignment.Gtk_New (Alg, 0.5, 0.0, 1.0, 0.0);
--        Gtk.Alignment.Add (Alg, Frame);
--        Gtk.Box.Pack_Start (Win.Box, Alg);
--
--        Gtk.Label.Gtk_New (lbl, "Lines");
      Win.Box.Panel (5, 1).Put ("Lines");
--        Gtk.Box.Pack_Start (Win.Box, lbl);
--        Gtk.Label.Set_Alignment (lbl, 0.5, 1.0);
--        Gtk.Label.Set_Size_Request (lbl, 85);
--        Gtk.Label.Gtk_New (Win.Lines_Label, "0");
      Win.Lines_Label.Create (Win.Box.Panel (6, 1).all);
      Win.Lines_Label.Text ("0");
--        Gtk.Label.Set_Alignment (Win.Lines_Label, 0.5, 0.0);
--        Gtk.Frame.Gtk_New (Frame);
--        Gtk.Frame.Set_Shadow_Type (Frame, Gtk.Enums.Shadow_In);
--        Gtk.Frame.Add (Frame, Win.Lines_Label);
--        Gtk.Alignment.Gtk_New (Alg, 0.5, 0.0, 1.0, 0.0);
--        Gtk.Alignment.Add (Alg, Frame);
--        Gtk.Box.Pack_Start (Win.Box, Alg);
--
--        Gtk.Label.Gtk_New (lbl, "Pieces");
      Win.Box.Panel (7, 1).Put ("Pieces");
--        Gtk.Box.Pack_Start (Win.Box, lbl);
--        Gtk.Label.Set_Alignment (lbl, 0.5, 1.0);
--        lbl.Set_Size_Request (85, lbl.Get_Allocated_Height);
--        Gtk.Label.Gtk_New (Win.Pieces_Label, "0");
      Win.Pieces_Label.Create (Win.Box.Panel (8, 1).all);
      Win.Pieces_Label.Text ("0");
--        Gtk.Label.Set_Alignment (Win.Pieces_Label, 0.5, 0.0);
--        Gtk.Frame.Gtk_New (Frame);
--        Gtk.Frame.Set_Shadow_Type (Frame, Gtk.Enums.Shadow_In);
--        Gtk.Frame.Add (Frame, Win.Pieces_Label);
--        Gtk.Alignment.Gtk_New (Alg, 0.5, 0.0, 1.0, 0.0);
--        Gtk.Alignment.Add (Alg, Frame);
--        Gtk.Box.Pack_Start (Win.Box, Alg);
--
--        Gtk.Label.Gtk_New (lbl, "Record");
      Win.Box.Panel (9, 1).Put ("Record");
--        Gtk.Box.Pack_Start (Win.Box, lbl);
--        Gtk.Label.Set_Size_Request (lbl, 85);
--        lbl.Set_Size_Request (85, -1);
--        Gtk.Label.Gtk_New (Win.Record_Label,
--          Integer'Image (Scores_Window_pkg.Get_Maximum_Score));
      Win.Record_Label.Create (Win.Box.Panel (10, 1).all);
      Win.Record_Label.Text
      (Integer'Image (Scores_Window_pkg.Get_Maximum_Score));
--        Gtk.Label.Set_Alignment (Win.Record_Label, 0.5, 0.0);
--        Gtk.Frame.Gtk_New (Frame);
--        Gtk.Frame.Set_Shadow_Type (Frame, Gtk.Enums.Shadow_In);
--        Gtk.Frame.Add (Frame, Win.Record_Label);
--        Gtk.Alignment.Gtk_New (Alg, 0.5, 0.0, 1.0, 0.0);
--        Gtk.Alignment.Add (Alg, Frame);
--        Gtk.Box.Pack_Start (Win.Box, Alg);
--
--        Set_Title (Win, "LinXtris");
--        Set_Resizable (Win, False);
--        Set_Position (Win, Gtk.Enums.Win_Pos_Center_Always);
--
--        Main_Window_Return_Callback.Connect
--          (Win,
--           "delete_event",
--           Main_Window_Return_Callback.To_Marshaller
--             (On_Main_Window_Delete'Access));
--
--        Main_Window_Callback.Connect
--          (Win,
--           "destroy",
--           Main_Window_Callback.To_Marshaller
--             (On_Main_Window_Destroy'Access));
--
--        Main_Window_Return_Callback.Connect
--          (Win,
--           "key_press_event",
--           Main_Window_Return_Callback.To_Marshaller
--             (On_Main_Window_Key_Pressed'Access));
      Win.On_Key_Down_Handler (On_Main_Window_Key_Pressed'Access);
--
--        Menu_Item_Callback.Connect
--          (Win.Item_Game_Quit,
--           "activate",
--        Menu_Item_Callback.To_Marshaller (On_Item_Game_Quit_Pressed'Access));
      Win.Item_Game_Quit.On_Click_Handler (On_Item_Game_Quit_Pressed'Access);
--
--        Menu_Item_Callback.Connect
--          (Win.Item_Game_New,
--           "activate",
--        Menu_Item_Callback.To_Marshaller (On_Item_Game_New_Pressed'Access));
      Win.Item_Game_New.On_Click_Handler (On_Item_Game_New_Pressed'Access);
--
--      Menu_Item_Callback.Connect
--          (Win.Item_Game_Pause,
--           "activate",
--      Menu_Item_Callback.To_Marshaller (On_Item_Game_Pause_Pressed'Access));
      Win.Item_Game_Pause.On_Click_Handler (On_Item_Game_Pause_Pressed'Access);
--
--      Menu_Item_Callback.Connect
--          (Win.Item_Game_Scores,
--           "activate",
--      Menu_Item_Callback.To_Marshaller (On_Item_Game_Scores_Pressed'Access));
      Win.Item_Game_Scores.On_Click_Handler
      (On_Item_Game_Scores_Pressed'Access);
--
--      Menu_Item_Callback.Connect
--          (Win.Item_Settings_Preferences,
--           "activate",
--      Menu_Item_Callback.To_Marshaller
--        (On_Item_Settings_Preferences_Pressed'Access));
      Win.Item_Settings_Preferences.On_Click_Handler
      (On_Item_Settings_Preferences_Pressed'Access);
--
--      Menu_Item_Callback.Connect
--          (Win.Item_Help_About,
--           "activate",
--      Menu_Item_Callback.To_Marshaller (On_Item_Help_About_Pressed'Access));
      Win.Item_Help_About.On_Click_Handler (On_Item_Help_About_Pressed'Access);

      Initialize_Pixmaps (Win);

   end Initialize;

   procedure Initialize_Pixmaps (Win : access Main_Window_Record'Class) is
      Xpm_Name : Unbounded_String;
      Xpm_Data : Gnoga.Types.Pixel_Data_Access;
      Cr       : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
--        Err      : Glib.Error.GError;
      use Gnoga.Gui.Element.Canvas.Context_2D;
   begin
--        Realize (Win);

      if Argument_Count = 2 then
         if Argument (1) = "-data_dir" then
            Data_Dir := To_Unbounded_String (Argument (2));
         end if;
      end if;

      Cr.Get_Drawing_Context_2D (Win.Game_Screen);

      Xpm_Name := Data_Dir & "pixmaps/blue.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Blue_Pix := new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Blue_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Blue_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/green.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Green_Pix := new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Green_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Green_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/red.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Red_Pix := new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Red_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Red_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/yellow.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Yellow_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Yellow_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Yellow_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/grey.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Grey_Pix := new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Grey_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Grey_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/blank.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Blank_Pix := new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Blank_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Blank_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/cyan.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Cyan_Pix := new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Cyan_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Cyan_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/magenta.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Magenta_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Magenta_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Magenta_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/ghost.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Ghost_Pix := new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Ghost_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Ghost_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/white.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.White_Pix := new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.White_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.White_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/blue_prev.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Blue_Prev_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Blue_Prev_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Blue_Prev_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/green_prev.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Green_Prev_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Green_Prev_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Green_Prev_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/red_prev.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Red_Prev_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Red_Prev_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Red_Prev_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/yellow_prev.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Yellow_Prev_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Yellow_Prev_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Yellow_Prev_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/grey_prev.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Grey_Prev_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Grey_Prev_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Grey_Prev_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/cyan_prev.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Cyan_Prev_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Cyan_Prev_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Cyan_Prev_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/magenta_prev.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Magenta_Prev_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Magenta_Prev_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Magenta_Prev_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);

      Xpm_Name := Data_Dir & "pixmaps/blank_prev.xpm";
      New_From_XPM (Xpm_Data, To_String (Xpm_Name));
      Win.Blank_Prev_Pix :=
        new Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Type;
      Cr.Create_Image_Data (Win.Blank_Prev_Pix.all, Xpm_Data'Length (1),
                            Xpm_Data'Length (2));
      Win.Blank_Prev_Pix.Data (Xpm_Data.all);
      Free (Xpm_Data);
   end Initialize_Pixmaps;

   procedure Pause (Win : access Main_Window_Record'Class; Value : Boolean) is
   begin
      Win.Game_Paused := Value;
      if Value then
         Win.Item_Game_Pause.Text ("Continue");
      else
         Win.Item_Game_Pause.Text ("Pause");
      end if;
   end Pause;

end Main_Window_Pkg;
