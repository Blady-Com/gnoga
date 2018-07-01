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

--  with Gtk.Stock;
--  with Gtk.Alignment;         use Gtk.Alignment;
--  with Gtk.Table;             use Gtk.Table;
--  with Gtk.Enums;
--  with Gtk.Frame;             use Gtk.Frame;
--  with Glib;                  use Glib;
with Main_Window_Pkg; use Main_Window_Pkg;
--  with Gtk.Handlers;          use Gtk.Handlers;
--  with Gdk.Event;             use Gdk.Event;
--  with Gtk.Widget;            use Gtk.Widget;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Base;
with Gnoga.Application.Singleton;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Scores_Window_pkg is

   package Callbacks is

--        package Scores_Window_Callback is new Gtk.Handlers.Callback
--          (Widget_Type => Scores_Window_Record);
--        package Scores_Window_Return_Callback is new
--          Gtk.Handlers.Return_Callback
--          (Widget_Type => Scores_Window_Record,
--           Return_Type => Boolean);
--        package Button_Callback is new Gtk.Handlers.Callback
--          (Widget_Type => Gtk_Button_Record);
--        function On_Scores_Window_Delete
--          (Win   : access Scores_Window_Record'Class;
--           Event : Gdk_Event) return Boolean;
      procedure On_OK_Button_Clicked
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure On_Scores_Window_Show
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);
      procedure On_Scores_Window_Hide
        (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   end Callbacks;

   package body Callbacks is

--        function On_Scores_Window_Delete
--          (Win   : access Scores_Window_Record'Class;
--           Event : Gdk_Event) return Boolean
--        is
--           pragma Unreferenced (Event, Win);
--        begin
--           Hide (Win);
--           return True;
--        end On_Scores_Window_Delete;

      procedure On_OK_Button_Clicked
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
         pragma Unreferenced (Object);
      begin
         --           Hide (Scores_Win);
         Scores_Win.Hidden;
         if Main_Window.Game_Quit then
            Gnoga.Application.Singleton.End_Application;
         end if;
      end On_OK_Button_Clicked;

      procedure On_Scores_Window_Show
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
      begin
         Scores_Window_Record (Object).Pause_Status := Main_Window.Game_Paused;
         Main_Window.Pause (True);
         Get_Scores;
      end On_Scores_Window_Show;

      procedure On_Scores_Window_Hide
        (Object : in out Gnoga.Gui.Base.Base_Type'Class)
      is
      begin
         Main_Window.Pause (Scores_Window_Record (Object).Pause_Status);
      end On_Scores_Window_Hide;

   end Callbacks;

   procedure Gtk_new (Win : out Scores_Window) is
   begin
      Win := new Scores_Window_Record;
      Initialize (Win => Win);
   end Gtk_new;

   procedure Initialize (Win : access Scores_Window_Record'Class) is
      use Callbacks;
--        lbl   : Gtk_Label;
      Table : Gnoga.Gui.View.Grid.Grid_View_Type;
--        alg   : Gtk_Alignment;
--        frame : Gtk_Frame;
   begin
--        Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);
      Win.Create (Main_Window_Pkg.Main_Window.Table.Panel (1, 3).all);
--        Win.Hidden; -- Layout issue
--        Set_Title (Win, "Scores");
--        Set_Transient_For (Win, Main_Window_Pkg.Main_Window);
--        Set_Resizable (Win, False);
--        Set_Position (Win, Gtk.Enums.Win_Pos_Center_Always);

      --        Gtk_New (Table, 13, 2, False);
      Table.Create
      (Win.all,
       ((Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
        (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL)));
--        Set_Border_Width (Table, 10);
--        Set_Row_Spacings (Table, 7);

--        Gtk_New (lbl, "Name");
--        Attach (Table, lbl, 0, 1, 0, 1);
      Table.Panel (1, 1).Text ("Name");

--        Gtk_New (lbl, "Score");
--        Attach (Table, lbl, 1, 2, 0, 1);
--        table.Panel(1,2).Text( "Score");
      Table.Panel (1, 2).Text ("Score");

--        Add (Win, Table);
      for i in Win.Scores_Label'Range loop
--           Gtk_New (Win.Scores_Label (i), "0");
--           Set_Size_Request (Win.Scores_Label (i), 80);
--           Gtk_New (frame);
--           Gtk.Frame.Set_Shadow_Type (frame, Gtk.Enums.Shadow_In);
--           Add (frame, Win.Scores_Label (i));
--           Attach (Table, frame, 1, 2, Guint (i), Guint (i + 1));
         Table.Panel (i + 1, 2).Border;
         Win.Scores_Label (i).Create (Table.Panel (i + 1, 2).all);
         Win.Scores_Label (i).Fill_Parent;
         Win.Scores_Label (i).Text ("0");
      end loop;
      for i in Win.Names_Label'Range loop
--           Gtk_New (Win.Names_Label (i), "no user");
--           Set_Justify (Win.Names_Label (i), Gtk.Enums.Justify_Left);
--           Set_Size_Request (Win.Names_Label (i), 120);
--           Gtk_New (frame);
--           Gtk.Frame.Set_Shadow_Type (frame, Gtk.Enums.Shadow_In);
--           Add (frame, Win.Names_Label (i));
--           Attach (Table, frame, 0, 1, Guint (i), Guint (i + 1));
         Table.Panel (i + 1, 1).Border;
         Win.Names_Label (i).Create (Table.Panel (i + 1, 1).all);
         Win.Names_Label (i).Fill_Parent;
         Win.Names_Label (i).Text ("no user");
      end loop;

--        Gtk_New_From_Stock (Win.OK_Button, Gtk.Stock.Stock_Ok);
      Win.OK_Button.Create (Table.Panel (13, 2).all, "OK");
--        Set_Size_Request (Win.OK_Button, 80);
--        Gtk_New (alg, 1.0, 0.5, 0.0, 0.0);
--        Add (alg, Win.OK_Button);
--        Attach (Table, alg, 0, 2, 12, 13, Ypadding => 5);
--        win.OK_Button.Create(table.Panel(12,2).all, "OK");
--        win.OK_Button.Fill_Parent;

--        Button_Callback.Connect
--          (Win.OK_Button,
--           "clicked",
--           Button_Callback.To_Marshaller (On_OK_Button_Clicked'Access));
      Win.OK_Button.On_Click_Handler (On_OK_Button_Clicked'Access);
--        Scores_Window_Callback.Connect
--          (Win,
--           "show",
--           Scores_Window_Callback.To_Marshaller
--             (On_Scores_Window_Show'Access));
      Win.On_Show_Handler (On_Scores_Window_Show'Access);
--
--        Scores_Window_Callback.Connect
--          (Win,
--           "hide",
--           Scores_Window_Callback.To_Marshaller
--             (On_Scores_Window_Hide'Access));
      Win.On_Hide_Handler (On_Scores_Window_Hide'Access);
--
--        Scores_Window_Return_Callback.Connect
--          (Win,
--           Gtk.Widget.Signal_Delete_Event,
--           Scores_Window_Return_Callback.To_Marshaller
--             (On_Scores_Window_Delete'Access));
      Win.Pause_Status := False;
      Win.Hidden;
   end Initialize;

   function Get_Maximum_Score return Natural is
      use Scores_IO;
      Filename : constant String :=
        To_String (Main_Window_Pkg.Data_Dir & "scores");
      File : File_Type;
      scr  : Score_Type;
   begin
      Open (File, In_File, Filename);
      Read (File, scr);
      Close (File);
      return scr.Score;
   exception
      when Name_Error =>
         return 0;
   end Get_Maximum_Score;

   function Get_Minimum_Score return Natural is
   begin
      Get_Scores;
      return Minimum_Score;
   end Get_Minimum_Score;

   procedure Set_Score (Name : String; Score : Natural) is
      use Scores_IO;
      Filename : constant String :=
        To_String (Main_Window_Pkg.Data_Dir & "scores");
      File : File_Type;
      scr  : Score_Type;
      I    : Scores_IO.Count := 10;
      J    : Scores_IO.Count;
   begin
      Open (File, Inout_File, Filename);
      loop
         Set_Index (File, I);
         Read (File, scr);
         if (scr.Score > Score) or I = 1 then
            if I > 1 then
               I := I + 1;
            end if;
            if I < 10 then
               J := 10;
               while J > I loop
                  Set_Index (File, J - 1);
                  Read (File, scr);
                  Set_Index (File, J);
                  Write (File, scr);
                  J := J - 1;
               end loop;
            end if;
            scr.Score := Score;
            if Name'Length > scr.Name'Length then
               for i in scr.Name'Range loop
                  scr.Name (i) := Name (i);
               end loop;
            else
               for i in Name'Range loop
                  scr.Name (i) := Name (i);
               end loop;
               for i in Name'Last + 1 .. scr.Name'Last loop
                  scr.Name (i) := ' ';
               end loop;
            end if;
            Set_Index (File, I);
            Write (File, scr);
            exit;
         end if;
         I := I - 1;
         exit when I = 0;
      end loop;
      Close (File);
      Get_Scores;
   end Set_Score;

   procedure Get_Scores is
      use Scores_IO;
      Filename : constant String :=
        To_String (Main_Window_Pkg.Data_Dir & "scores");
      File : File_Type;
      scr  : Score_Type;
      procedure Create_File;
      procedure Create_File is
      begin
         Create (File, Out_File, Filename);
         scr.Name  := "no user             ";
         scr.Score := 0;
         for i in 1 .. 10 loop
            Write (File, scr);
         end loop;
         Close (File);
      end Create_File;
   begin
      Open (File, In_File, Filename);
      for i in 1 .. 10 loop
         Read (File, scr);
         Scores_Win.Names_Label (i).Text (scr.Name);
         Scores_Win.Scores_Label (i).Text (Integer'Image (scr.Score));
      end loop;
      Minimum_Score := scr.Score;
      Close (File);
   exception
      when Name_Error =>
         Create_File;

   end Get_Scores;

end Scores_Window_pkg;
