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
with Main_Window_Pkg; use Main_Window_Pkg;
--  with Gtk.Handlers;
--  with Gdk.Event;       use Gdk.Event;
--  with Gtk.Stock;
--  with Gtk.Alignment;   use Gtk.Alignment;
--  with Gtk.Label;       use Gtk.Label;
with Scores_Window_pkg;
with Block_Engine;
--  with Gtk.Table;       use Gtk.Table;
with Gnoga.Gui.Base;

package body New_Score_Dialog_Pkg is

   package Callbacks is

--        package New_Score_Dlg_Callback is new Gtk.Handlers.Callback
--          (Widget_Type => New_Score_Dialog_Record);
--        package New_Score_Dlg_Return_Callback is new
--          Gtk.Handlers.Return_Callback
--          (Widget_Type => New_Score_Dialog_Record,
--           Return_Type => Boolean);
--        package Button_Callback is new
--          Gtk.Handlers.Callback (Widget_Type => Gtk_Button_Record);
--        function On_New_Score_Dialog_Delete
--          (Win   : access New_Score_Dialog_Record'Class;
--           Event : Gdk_Event) return Boolean;
      procedure On_Enter_Key (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure On_OK_Button_Clicked (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure On_New_Score_Dialog_Show (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure On_New_Score_Dialog_Hide (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   end Callbacks;

   package body Callbacks is

--        function On_New_Score_Dialog_Delete
--          (Win   : access New_Score_Dialog_Record'Class;
--           Event : Gdk_Event) return Boolean
--        is
--           pragma Unreferenced (Event);
--           use Scores_Window_pkg;
--        begin
--           Hide (Win);
--           Set_Score (Win.Name_Entry.Text, Block_Engine.Get_Score
--             (Main_Window.Engine.all));
--           return True;
--        end On_New_Score_Dialog_Delete;

      procedure On_Enter_Key (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
         use Scores_Window_pkg;
      begin
         if not New_Score_Dialog.Hidden then
            --           Hide (New_Score_Dialog);
            New_Score_Dialog.Hidden;
            Set_Score (New_Score_Dialog.Name_Entry.Value, Block_Engine.Get_Score (Main_Window.Engine.all));
            --           Show_All (Scores_Win);
            Scores_Win.Hidden (False);
         end if;
      end On_Enter_Key;

      procedure On_OK_Button_Clicked (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
         use Scores_Window_pkg;
      begin
--           Hide (New_Score_Dialog);
         New_Score_Dialog.Hidden;
         Set_Score (New_Score_Dialog.Name_Entry.Value, Block_Engine.Get_Score (Main_Window.Engine.all));
--           Show_All (Scores_Win);
         Scores_Win.Hidden (False);
      end On_OK_Button_Clicked;

      procedure On_New_Score_Dialog_Show (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
      begin
         Main_Window.Pause (True);
      end On_New_Score_Dialog_Show;

      procedure On_New_Score_Dialog_Hide (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
      begin
         Main_Window.Pause (False);
      end On_New_Score_Dialog_Hide;

   end Callbacks;

   procedure Gtk_New (Win : out New_Score_Dialog_Type) is
   begin
      Win := new New_Score_Dialog_Record;
      Initialize (Win => Win);
   end Gtk_New;

   procedure Initialize (Win : access New_Score_Dialog_Record'Class) is
      use Callbacks;
--        Alg   : Gtk_Alignment;
--        Table : Gtk_Table;
      Lbl : Gnoga.Gui.Element.Form.Label_Type;
   begin
--        Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);
      Win.Create (Main_Window_Pkg.Main_Window.Table.Panel (2, 3).all);
      Win.Hidden;
--        Set_Title (Win, "Top Ten Score");
--        Set_Transient_For (Win, Main_Window_Pkg.Main_Window);
--        Set_Resizable (Win, False);
--        Set_Position (Win, Gtk.Enums.Win_Pos_Center_Always);

--        Gtk_New (Table, 2, 2, False);
--        Set_Col_Spacings (Table, 5);
--        Set_Row_Spacings (Table, 7);
--        Add (Win, Table);
--        Set_Border_Width (Win, 7);

--        Gtk_New (Lbl, "Name:");
--        Attach (Table, Lbl, 0, 1, 0, 1);

--        Gtk_New (Win.Name_Entry);
--        Set_Max_Length (Win.Name_Entry, 20);
--        Attach (Table, Win.Name_Entry, 1, 2, 0, 1);
      Win.Form.Create (Win.all);
      Win.Name_Entry.Create (Win.Form);
      Win.Name_Entry.Autofocus;
      Lbl.Create (Win.Form, Win.Name_Entry, "Name:");

--        Gtk_New (Alg, 1.0, 0.5, 0.0, 0.0);
--        Gtk_New_From_Stock (Win.OK_Button, Gtk.Stock.Stock_Ok);
      Win.OK_Button.Create (Win.Form, "OK");
--        Set_Size_Request (Win.OK_Button, 80);
--        Add (Alg, Win.OK_Button);
--        Attach (Table, Alg, 0, 2, 1, 2);

      --        Win.Name_Entry.On_Activate (On_Enter_Key'Access);
      Win.Form.On_Submit_Handler (On_Enter_Key'Access);
--        Button_Callback.Connect
--          (Win.OK_Button,
--           "clicked",
--           Button_Callback.To_Marshaller (On_OK_Button_Clicked'Access));
      Win.OK_Button.On_Click_Handler (On_OK_Button_Clicked'Access);
--        New_Score_Dlg_Callback.Connect
--          (Win,
--           "show",
--           New_Score_Dlg_Callback.To_Marshaller
--             (On_New_Score_Dialog_Show'Access));
      Win.On_Show_Handler (On_New_Score_Dialog_Show'Access);
--        New_Score_Dlg_Callback.Connect
--          (Win,
--           "hide",
--           New_Score_Dlg_Callback.To_Marshaller
--             (On_New_Score_Dialog_Hide'Access));
      Win.On_Hide_Handler (On_New_Score_Dialog_Hide'Access);
--        New_Score_Dlg_Return_Callback.Connect
--          (Win,
--           "delete_event",
--           New_Score_Dlg_Return_Callback.To_Marshaller
--             (On_New_Score_Dialog_Delete'Access));
   end Initialize;

end New_Score_Dialog_Pkg;
