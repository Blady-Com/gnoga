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

--  with Gtk.Label;             use Gtk.Label;
--  with Gtk.Table;             use Gtk.Table;
--  with Gtk.Enums; use Gtk.Enums;
--  with Gtk.Separator;         use Gtk.Separator;
--  with Gtk.Stock;
--  with Gtk.Alignment;         use Gtk.Alignment;
--  with Gtk.Adjustment;        use Gtk.Adjustment;
--  with Gtk.Handlers;          use Gtk.Handlers;
--  with Gdk.Event;             use Gdk.Event;
with Block_Engine;
with Main_Window_Pkg;       use Main_Window_Pkg;
with Game_Engine;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Glib;                  use Glib;
with Gnoga.Gui.Document;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Form;

package body Preferences_Window_Pkg is

   package Callbacks is

--        package Pref_Window_Callback is new Gtk.Handlers.Callback
--          (Widget_Type => Preferences_Window_Record);
--
--        package Pref_Window_Return_Callback is new
--          Gtk.Handlers.Return_Callback
--          (Widget_Type => Preferences_Window_Record,
--           Return_Type => Boolean);
--
--        package Button_Callback is new
--          Gtk.Handlers.Callback (Widget_Type => Gtk_Button_Record);

--        function On_Pref_Window_Delete
--          (Win   : access Preferences_Window_Record'Class;
--           Event : Gdk_Event) return Boolean;

      procedure On_OK_Button_Clicked
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);

      procedure On_Pref_Window_Show
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);

      procedure On_Pref_Window_Hide
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);

      procedure On_Range_Change
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   end Callbacks;

   package body Callbacks is

--        function On_Pref_Window_Delete
--          (Win   : access Preferences_Window_Record'Class;
--           Event : Gdk_Event) return Boolean
--        is
--           pragma Unreferenced (Event, Win);
--        begin
--           Hide (Win);
--           return True;
--        end On_Pref_Window_Delete;

      procedure On_OK_Button_Clicked
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
         pragma Unreferenced (Object);
         Level, Lines : Integer;
      begin
         Level := Pref_Win.Level_Spin.Value;
         Lines := Pref_Win.Lines_Spin.Value;
         Block_Engine.Set_Initial_Level (Main_Window.Engine.all, Level);
         Block_Engine.Set_Initial_Lines (Main_Window.Engine.all, Lines);
         Main_Window.Continuous_Movement := Pref_Win.Cont_Move_Check.Checked;
         Main_Window.Animation           := Pref_Win.Animation_Check.Checked;
         Main_Window.Piece_Preview       := Pref_Win.Preview_Check.Checked;
         if Main_Window.Piece_Preview and Main_Window.Exists_Game then
            Game_Engine.Paint_Prev;
         else
            Game_Engine.Clear_Prev;
         end if;
         Block_Engine.Set_Has_Ghost
           (Main_Window.Engine.all,
            Pref_Win.Ghost_Check.Checked);
         Set_Preferences;
         --           Hide (Pref_Win);
         Pref_Win.Hidden;
      end On_OK_Button_Clicked;

      procedure On_Pref_Window_Show
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
      begin
         Preferences_Window_Record (Object).Pause_Status :=
           Main_Window.Game_Paused;
         Main_Window.Pause (True);
         Get_Preferences;
      end On_Pref_Window_Show;

      procedure On_Pref_Window_Hide
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
      begin
         Main_Window.Pause (Preferences_Window_Record (Object).Pause_Status);
      end On_Pref_Window_Hide;

      procedure On_Range_Change
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
      begin
         Gnoga_Extras.Labeled_Range_Type (Object).Label.Text
         (Gnoga_Extras.Labeled_Range_Type (Object).Value);
      end On_Range_Change;

   end Callbacks;

   use Callbacks;

   procedure Gtk_new (Win : out Preferences_Window) is
   begin
      Win := new Preferences_Window_Record;
      Initialize (Win => Win);
   end Gtk_new;

   procedure Initialize (Win : access Preferences_Window_Record'Class) is
--        lbl    : Gnoga.Gui.Element.Form.Label_Type;
--        Table  : Gtk_Table;
      Form : Gnoga.Gui.Element.Form.Form_Type;
--        sep    : Gtk_Separator;
--        alg    : Gtk_Alignment;
--        adjust : Gtk_Adjustment;
      use type Gnoga.Gui.Document.Ready_State_Type;
   begin
--        Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);
      Win.Create (Main_Window_Pkg.Main_Window.Table.Panel (1, 4).all);
      Win.Hidden;
--        Set_Title (Win, "Preferences");
--        Set_Transient_For (Win, Main_Window_Pkg.Main_Window);
--        Set_Resizable (Win, False);
--        Set_Position (Win, Gtk.Enums.Win_Pos_Center_Always);
--
--        Gtk_New (Table, 8, 2, False);
--        Set_Border_Width (Table, 10);
--        Add (Win, Table);
      Form.Create (Win.all);
--
--        Gtk_New (lbl, "Starting Level");
--        Set_Alignment (lbl, 0.0, 0.5);
--        Attach (Table, lbl, 0, 1, 0, 1, Xpadding => 5, Ypadding => 5);
--
--        Gtk_New (adjust, 1.0, 1.0, 10.0, 1.0, 1.0, 1.0);
--        Gtk_New (Win.Level_Spin, adjust, 0.0, 0);
--        Set_Numeric (Win.Level_Spin, True);
--        Set_Update_Policy (Win.Level_Spin, Update_If_Valid);
--        Attach (Table, Win.Level_Spin, 1, 2, 0, 1,
--          Xpadding => 5, Ypadding => 5);
      Form.Put ("Starting Level ");
      Win.Level_Spin.Create (Form);
      Win.Level_Spin.Minimum (1);
      Win.Level_Spin.Maximum (9);
      Win.Level_Spin.Value (1);
      Win.Level_Spin.Label.Create (Form, Win.Level_Spin, "1");
      Form.New_Line;
--
--        Gtk_New (lbl, "Number of pre-filled rows");
--        Set_Alignment (lbl, 0.0, 0.5);
--        Attach (Table, lbl, 0, 1, 1, 2, Xpadding => 5, Ypadding => 5);
--
--        Gtk_New (adjust, 0.0, 0.0, 15.0, 1.0, 1.0, 1.0);
--        Gtk_New (Win.Lines_Spin, adjust, 0.0, 0);
--        Set_Numeric (Win.Lines_Spin, True);
--        Attach (Table, Win.Lines_Spin, 1, 2, 1, 2,
--          Xpadding => 5, Ypadding => 5);
      Form.Put ("Number of pre-filled rows ");
      Win.Lines_Spin.Create (Form);
      Win.Lines_Spin.Minimum (0);
      Win.Lines_Spin.Maximum (14);
      Win.Lines_Spin.Value (0);
      Win.Lines_Spin.Label.Create (Form, Win.Lines_Spin, "0");
      Form.New_Line;
--
--        Gtk_New (Win.Preview_Check, "Piece Preview");
--        Attach (Table, Win.Preview_Check, 0, 2, 2, 3,
--          Xpadding => 5, Ypadding => 5);
--        Set_Active (Win.Preview_Check, True);
      Win.Preview_Check.Create (Form);
      Win.Preview_Check.Label.Create
      (Form, Win.Preview_Check, "Piece Preview", False);
      Win.Preview_Check.Checked (True);
      Form.New_Line;
--
--        Gtk_New (Win.Cont_Move_Check, "Continuous Movement");
--        Attach (Table, Win.Cont_Move_Check, 0, 2, 3, 4, Xpadding => 5,
--          Ypadding => 5);
--        Set_Active (Win.Cont_Move_Check, True);
      Win.Cont_Move_Check.Create (Form);
      Win.Cont_Move_Check.Label.Create
      (Form, Win.Cont_Move_Check, "Continuous Movement", False);
      Win.Cont_Move_Check.Checked (True);
      Form.New_Line;
--
--        Gtk_New (Win.Animation_Check, "Animation");
--        Attach (Table, Win.Animation_Check, 0, 2, 4, 5, Xpadding => 5,
--          Ypadding => 5);
--        Set_Active (Win.Animation_Check, True);
      Win.Animation_Check.Create (Form);
      Win.Animation_Check.Label.Create
      (Form, Win.Animation_Check, "Animation", False);
      Win.Animation_Check.Checked (True);
      Form.New_Line;
--
--        Gtk_New (Win.Ghost_Check, "Ghost Mode");
--        Attach (Table, Win.Ghost_Check, 0, 2, 5, 6, Xpadding => 5,
--          Ypadding => 5);
      Win.Ghost_Check.Create (Form);
      Win.Ghost_Check.Label.Create
      (Form, Win.Ghost_Check, "Ghost Mode", False);
      Win.Ghost_Check.Checked (True);
--
--        Gtk_New_Hseparator (sep);
--        Attach (Table, sep, 0, 2, 6, 7, Xpadding => 5, Ypadding => 5);
      Win.Horizontal_Rule;
--
--        Gtk_New_From_Stock (Win.OK_Button, Gtk.Stock.Stock_Ok);
--        Set_Size_Request (Win.OK_Button, 80);
--        Gtk_New (alg, 1.0, 0.5, 0.0, 0.0);
--        Add (alg, Win.OK_Button);
--        Attach (Table, alg, 0, 2, 7, 8, Ypadding => 5);
      Win.OK_Button.Create (Win.all, "OK");
--
--        Button_Callback.Connect
--          (Win.OK_Button,
--           "clicked",
--           Button_Callback.To_Marshaller (On_OK_Button_Clicked'Access));
      Win.OK_Button.On_Click_Handler (On_OK_Button_Clicked'Access);
--
--        Pref_Window_Callback.Connect
--          (Win,
--           "show",
--           Pref_Window_Callback.To_Marshaller (On_Pref_Window_Show'Access));
      Win.On_Show_Handler (On_Pref_Window_Show'Access);
--
--        Pref_Window_Callback.Connect
--          (Win,
--           "hide",
--           Pref_Window_Callback.To_Marshaller (On_Pref_Window_Hide'Access));
      Win.On_Hide_Handler (On_Pref_Window_Hide'Access);
--
--        Pref_Window_Return_Callback.Connect
--          (Win,
--           "delete_event",
--           Pref_Window_Return_Callback.To_Marshaller
--             (On_Pref_Window_Delete'Access));

      Win.Level_Spin.On_Change_Handler (On_Range_Change'Access);
      Win.Lines_Spin.On_Change_Handler (On_Range_Change'Access);
   end Initialize;

   procedure Get_Preferences is
      use Preferences_IO;
      Filename : constant String :=
        To_String (Main_Window_Pkg.Data_Dir & "preferences");
      File : File_Type;
      Pref : Preferences_Type;
      procedure Create_File;
      procedure Create_File is
      begin
         Create (File, Out_File, Filename);
         Write (File, Pref);
         Close (File);
      end Create_File;
   begin
      Open (File, In_File, Filename);
      Read (File, Pref);
      Close (File);
      Block_Engine.Set_Initial_Level
        (Main_Window.Engine.all,
         Pref.Initial_Level);
      Block_Engine.Set_Initial_Lines
        (Main_Window.Engine.all,
         Pref.Initial_Lines);
      Main_Window.Continuous_Movement := Pref.Continuous_Movement;
      Main_Window.Animation           := Pref.Animation;
      Main_Window.Piece_Preview       := Pref.Piece_Preview;
      Block_Engine.Set_Has_Ghost (Main_Window.Engine.all, Pref.Ghost);

      Pref_Win.Level_Spin.Value (Pref.Initial_Level);
      Pref_Win.Level_Spin.Label.Text (Pref_Win.Level_Spin.Value);
      Pref_Win.Lines_Spin.Value (Pref.Initial_Lines);
      Pref_Win.Lines_Spin.Label.Text (Pref_Win.Lines_Spin.Value);
      Pref_Win.Cont_Move_Check.Checked (Pref.Continuous_Movement);
      Pref_Win.Animation_Check.Checked (Pref.Animation);
      Pref_Win.Preview_Check.Checked (Pref.Piece_Preview);
      Pref_Win.Ghost_Check.Checked (Pref.Ghost);
   exception
      when Name_Error =>
         Create_File;

   end Get_Preferences;

   procedure Set_Preferences is
      use Preferences_IO;
      Filename : constant String :=
        To_String (Main_Window_Pkg.Data_Dir & "preferences");
      File : File_Type;
      pref : Preferences_Type;
   begin
      Open (File, Out_File, Filename);
      pref.Initial_Level :=
        Block_Engine.Get_Initial_Level (Main_Window.Engine.all);
      pref.Initial_Lines :=
        Block_Engine.Get_Initial_Lines (Main_Window.Engine.all);
      pref.Continuous_Movement := Main_Window.Continuous_Movement;
      pref.Animation           := Main_Window.Animation;
      pref.Piece_Preview       := Main_Window.Piece_Preview;
      pref.Ghost := Block_Engine.Get_Has_Ghost (Main_Window.Engine.all);
      Write (File, pref);
      Close (File);
   end Set_Preferences;

end Preferences_Window_Pkg;
