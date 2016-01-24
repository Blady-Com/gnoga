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

--  with Gtk.Separator; use Gtk.Separator;
--  with Gtk.Label;     use Gtk.Label;
--  with Gtk.Box;         use Gtk.Box;
with Main_Window_Pkg; use Main_Window_Pkg;
--  with Gtk.Alignment;   use Gtk.Alignment;
--  with Gtk.Stock;
--  with Gtk.Enums;
--  with Gtk.Handlers;
--  with Gdk.Event; use Gdk.Event;
--  with Pango.Font;      use Pango.Font;
with Gnoga.Gui.Base;
with Gnoga.Gui.Document;

package body About_Dialog_Pkg is

   package Callbacks is

--        package About_Dialog_Callback is new Gtk.Handlers.Callback
--          (Widget_Type => About_Dialog_Record);
--  package About_Dialog_Return_Callback is new Gtk.Handlers.Return_Callback
--          (Widget_Type => About_Dialog_Record,
--           Return_Type => Boolean);
--  package Button_Callback is new Gtk.Handlers.Callback
--            (Widget_Type => Gtk_Button_Record);
--        function On_About_Dialog_Delete
--          (Win   : access About_Dialog_Record'Class;
--           Event : Gdk_Event) return Boolean;
--  procedure On_OK_Button_Clicked (Button : access Gtk_Button_Record'Class);
      procedure On_OK_Button_Clicked
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure On_About_Dialog_Show
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure On_About_Dialog_Hide
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   end Callbacks;

   package body Callbacks is

--        function On_About_Dialog_Delete
--          (Win   : access About_Dialog_Record'Class;
--           Event : Gdk_Event) return Boolean
--        is
--           pragma Unreferenced (Event, Win);
--        begin
--           Hide (Win);
--           return True;
--        end On_About_Dialog_Delete;

      procedure On_OK_Button_Clicked
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
         pragma Unreferenced (Object);
      begin
         --           Hide (About_Dialog);
         About_Dialog.Hidden;
      end On_OK_Button_Clicked;

      procedure On_About_Dialog_Show
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
      begin
         About_Dialog_Record (Object).Pause_Status := Main_Window.Game_Paused;
         Main_Window.Pause (True);
      end On_About_Dialog_Show;

      procedure On_About_Dialog_Hide
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
      begin
         Main_Window.Pause (About_Dialog_Record (Object).Pause_Status);
      end On_About_Dialog_Hide;

   end Callbacks;

   procedure Gtk_New (Win : out About_Dialog_Type) is
   begin
      Win := new About_Dialog_Record;
      Initialize (Win => Win);
   end Gtk_New;

   procedure Initialize (Win : access About_Dialog_Record'Class) is
      use Callbacks;
      use type Gnoga.Gui.Document.Ready_State_Type;
   begin
      --        Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);
      Win.Create (Main_Window_Pkg.Main_Window.Table.Panel (2, 4).all);
      Win.Hidden;
--        Set_Title (Win, "About LinXtris");
--        Set_Transient_For (Win, Main_Window_Pkg.Main_Window);
--        Set_Resizable (Win, False);
--        Set_Position (Win, Gtk.Enums.Win_Pos_Center_Always);
--        Set_Border_Width (Win, 10);

--        Gtk_New_Vbox (Box);
--        Set_Spacing (Box, 10);
--        Add (Win, Box);
--
--        Gtk_New (Alg, 0.5, 0.5, 0.0, 0.0);
--        Gtk_New (Lbl, "LinXtris 0.1a2");
      Win.Put_Line ("LinXtris 10.0a");
--        Modify_Font (Lbl, From_String ("Bold 18"));
--        Add (Alg, Lbl);
--        Pack_Start (Box, Alg);
--
--        Gtk_New (Alg, 0.5, 0.5, 0.0, 0.0);
--        Gtk_New (Lbl, "Send comments to: dulio@users.sourceforge.net");
      Win.Put_Line ("Send comments to: xxxxxx");
--        Add (Alg, Lbl);
--        Pack_Start (Box, Alg);
--
--        Gtk_New (Alg, 0.5, 0.5, 0.0, 0.0);
--        Gtk_New (Lbl, "Copyright (C) 2003 Dulio Matos Leite de C. e Silva");
      Win.Put_Line
      ("Based on GTKAda LinXtris" &
       " copyright (C) 2003 Dulio Matos Leite de C. e Silva");
--        Modify_Font (Lbl, From_String ("8"));
--        Add (Alg, Lbl);
--        Pack_Start (Box, Alg);
--
--        Gtk_New (Alg, 0.5, 0.5, 0.0, 0.0);
--        Gtk_New (Lbl, "(20150406 - GTKAda 3 port by Pascal Pignard)");
      Win.Put_Line ("(20150xxx - GNOGA port by Pascal Pignard)");
--        Modify_Font (Lbl, From_String ("8"));
--        Add (Alg, Lbl);
--        Pack_Start (Box, Alg);
--
--        Gtk_New_Hseparator (Sep);
      Win.Horizontal_Rule;
--        Pack_Start (Box, Sep);
--
--        Gtk_New_From_Stock (Win.OK_Button, Gtk.Stock.Stock_Ok);
      Win.OK_Button.Create (Win.all, "OK");
--        Set_Size_Request (Win.OK_Button, 80);
--        Gtk_New (Alg, 1.0, 0.0, 0.0, 0.0);
--        Add (Alg, Win.OK_Button);
--        Pack_Start (Box, Alg);

--        Button_Callback.Connect
--          (Win.OK_Button,
--           "clicked",
--           Button_Callback.To_Marshaller (On_OK_Button_Clicked'Access));
      Win.OK_Button.On_Click_Handler (On_OK_Button_Clicked'Access);
--        About_Dialog_Callback.Connect
--          (Win,
--           "show",
--         About_Dialog_Callback.To_Marshaller (On_About_Dialog_Show'Access));
      Win.On_Show_Handler (On_About_Dialog_Show'Access);
--        About_Dialog_Callback.Connect
--          (Win,
--           "hide",
--         About_Dialog_Callback.To_Marshaller (On_About_Dialog_Hide'Access));
      Win.On_Hide_Handler (On_About_Dialog_Hide'Access);
--        About_Dialog_Return_Callback.Connect
--          (Win,
--           "delete_event",
--  About_Dialog_Return_Callback.To_Marshaller
--      (On_About_Dialog_Delete'Access));
   end Initialize;

end About_Dialog_Pkg;
