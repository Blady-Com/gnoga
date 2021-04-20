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

--  with Cairo;
--  with Gdk;
--  with Gdk.Cairo;
--  with Double_Buffer; use Double_Buffer;
--  with Glib; use Glib;
--  with Gtk.Label;
--  with Gtk.Menu_Item;
with Scores_Window_pkg; use Scores_Window_pkg;
--  with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
--  with Gdk.Types;         use Gdk.Types;
--  with Gdk.Event;         use Gdk.Event;
--  with Glib.Main;
with New_Score_Dialog_Pkg;
with Preferences_Window_Pkg;
with Gnoga.Types.Key_Codes;
with Gnoga.Application.Singleton;

package body Game_Engine is

   procedure Clear_Screen is
   begin
      for i in 1 .. 10 loop
         for j in 1 .. 20 loop
            Paint_Block (i, j, Block_Engine.Blank);
         end loop;
      end loop;

   end Clear_Screen;

   procedure Clear_Prev is
      Dummy_Cr : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin
--        Cr := Cairo.Create (Get_Pixmap (Main_Window.Prev_Screen));
      Dummy_Cr.Get_Drawing_Context_2D (Main_Window.Prev_Screen);
--      Gdk.Cairo.Set_Source_Pixbuf (Cr, Main_Window.Blank_Prev_Pix, 0.0, 0.0);
--        Cairo.Paint (Cr);
      Dummy_Cr.Put_Image_Data (Main_Window.Blank_Prev_Pix.all, 0, 0);
--        Cairo.Destroy (Cr);
--        Gdk.Drawable.Draw_Drawable( Get_Pixmap( Main_Window.Prev_Screen ),
--                                 Main_Window.GC, Main_Window.Blank_Prev_Pix,
--                                 0, 0,
--                                 0, 0,
--                                 85, 85
--                                  );
--        Main_Window.Prev_Screen.Queue_Draw;
   end Clear_Prev;

   procedure Paint_Piece_Block
     (X     : Integer;
      Y     : Integer;
      Color : Block_Engine.Color);
   procedure Paint_Piece_Block
     (X     : Integer;
      Y     : Integer;
      Color : Block_Engine.Color)
   is
      Dummy_Cr : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin
      if Y > 20 then
         return;
      end if;
--        Cr := Cairo.Create (Get_Pixmap (Main_Window.Game_Screen));
      Dummy_Cr.Get_Drawing_Context_2D (Main_Window.Game_Screen);
--        Gdk.Cairo.Set_Source_Pixbuf
--          (Cr,
--           Color_To_Pix (Color),
--           Gdouble (X_Drawing_Coordinate (X)),
--           Gdouble (Y_Drawing_Coordinate (Y) + Gint (Main_Window.Delta_Y)));
--        Cairo.Paint (Cr);
      Dummy_Cr.Put_Image_Data
        (Color_To_Pix (Color).all, X_Drawing_Coordinate (X), Y_Drawing_Coordinate (Y) + Main_Window.Delta_Y);
--        Cairo.Destroy (Cr);
--        Gdk.Drawable.Draw_Drawable( Get_Pixmap( Main_Window.Game_Screen ),
--                       Main_Window.GC, Color_To_Pix(Color),
--                       0, 0,
--                       X_Drawing_Coordinate(X),
--                       Y_Drawing_Coordinate(Y) + Gint(Main_Window.Delta_Y),
--                       24, 24
--                       );
   end Paint_Piece_Block;

   procedure Paint_Piece;
   procedure Paint_Piece is
      use Block_Engine;
   begin
      if Main_Window.Continuous_Movement then
         Main_Window.Delta_Y := (24 * Main_Window.Down_Count) / Main_Window.Max_Down_Count - 24;
         for I in 1 .. 4 loop
            if Block_Engine.Piece_Y (Main_Window.Engine.all, I) < 20 then
               if Block_Engine.Get_Position_Color
                   (Main_Window.Engine.all, Block_Engine.Piece_X (Main_Window.Engine.all, I),
                    Block_Engine.Piece_Y (Main_Window.Engine.all, I) + 1) /=
                 Block_Engine.Blank
               then
                  Main_Window.Delta_Y := 0;
                  exit;
               end if;
            end if;
         end loop;
      else
         Main_Window.Delta_Y := 0;
      end if;
      for i in 1 .. 4 loop
         Paint_Piece_Block
           (Block_Engine.Piece_X (Main_Window.Engine.all, i), Block_Engine.Piece_Y (Main_Window.Engine.all, i),
            Block_Engine.Get_Piece_Color (Main_Window.Engine.all));
      end loop;
   end Paint_Piece;

   procedure Paint_Screen is
   begin
      for i in 1 .. 10 loop
         for j in 1 .. 20 loop
            Paint_Block (i, j, Block_Engine.Get_Position_Color (Main_Window.Engine.all, i, j));
         end loop;
      end loop;
      if Block_Engine.Get_Has_Ghost (Main_Window.Engine.all) then
         for i in 1 .. 4 loop
            Paint_Block
              (Block_Engine.Ghost_X (Main_Window.Engine.all, i), Block_Engine.Ghost_Y (Main_Window.Engine.all, i),
               Block_Engine.Ghost);
         end loop;
      end if;
      Paint_Piece;
   end Paint_Screen;

   procedure Paint_Block
     (X     : Integer;
      Y     : Integer;
      Color : Block_Engine.Color)
   is
      Dummy_Cr : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin
--        Cr := Cairo.Create (Get_Pixmap (Main_Window.Game_Screen));
      Dummy_Cr.Get_Drawing_Context_2D (Main_Window.Game_Screen);
--        Gdk.Cairo.Set_Source_Pixbuf
--          (Cr,
--           Color_To_Pix (Color),
--           Gdouble (X_Drawing_Coordinate (X)),
--           Gdouble (Y_Drawing_Coordinate (Y)));
--        Cairo.Paint (Cr);
      Dummy_Cr.Put_Image_Data (Color_To_Pix (Color).all, X_Drawing_Coordinate (X), Y_Drawing_Coordinate (Y));
--        Cairo.Destroy (Cr);
--        Gdk.Drawable.Draw_Pixmap( Get_Pixmap( Main_Window.Game_Screen ),
--                                  Main_Window.GC, Color_To_Pix(Color),
--                                  0, 0,
--                                  X_Drawing_Coordinate(X),
--                                  Y_Drawing_Coordinate(Y),
--                                  24, 24
--                                );
   end Paint_Block;

   procedure Paint_White_Block
     (X : Integer;
      Y : Integer);
   procedure Paint_White_Block
     (X : Integer;
      Y : Integer)
   is
      Dummy_Cr : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin
--        Cr := Cairo.Create (Get_Pixmap (Main_Window.Game_Screen));
      Dummy_Cr.Get_Drawing_Context_2D (Main_Window.Game_Screen);
--        Gdk.Cairo.Set_Source_Pixbuf
--          (Cr,
--           Main_Window.White_Pix,
--           Gdouble (X_Drawing_Coordinate (X)),
--           Gdouble (Y_Drawing_Coordinate (Y)));
--        Cairo.Paint (Cr);
      Dummy_Cr.Put_Image_Data
        (Main_Window.White_Pix.all, X_Drawing_Coordinate (X), Y_Drawing_Coordinate (Y) + Main_Window.Delta_Y);
--        Cairo.Destroy (Cr);
--        Gdk.Drawable.Draw_Drawable( Get_Pixmap( Main_Window.Game_Screen ),
--                                    Main_Window.GC, Main_Window.White_Pix,
--                                    0, 0,
--                                    X_Drawing_Coordinate(X),
--                                    Y_Drawing_Coordinate(Y),
--                                    24, 24
--                                  );
   end Paint_White_Block;

   function X_Drawing_Coordinate
     (X : Integer)
      return Integer
   is
   begin
      return 24 * (X - 1);
   end X_Drawing_Coordinate;

   function Y_Drawing_Coordinate
     (Y : Integer)
      return Integer
   is
   begin
      return 480 - 24 * Y;
   end Y_Drawing_Coordinate;

   function Color_To_Pix
     (Color : Block_Engine.Color)
      return Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access
   is
   begin
      case Color is
         when Block_Engine.Blue =>
            return Main_Window.Blue_Pix;
         when Block_Engine.Red =>
            return Main_Window.Red_Pix;
         when Block_Engine.Yellow =>
            return Main_Window.Yellow_Pix;
         when Block_Engine.Grey =>
            return Main_Window.Grey_Pix;
         when Block_Engine.Blank =>
            return Main_Window.Blank_Pix;
         when Block_Engine.Green =>
            return Main_Window.Green_Pix;
         when Block_Engine.Cyan =>
            return Main_Window.Cyan_Pix;
         when Block_Engine.Magenta =>
            return Main_Window.Magenta_Pix;
         when Block_Engine.Ghost =>
            return Main_Window.Ghost_Pix;
      end case;
   end Color_To_Pix;

   function Color_To_Prev_Pix
     (Color : Block_Engine.Color)
      return Gnoga.Gui.Element.Canvas.Context_2D.Image_Data_Access
   is
   begin
      case Color is
         when Block_Engine.Blue =>
            return Main_Window.Blue_Prev_Pix;
         when Block_Engine.Red =>
            return Main_Window.Red_Prev_Pix;
         when Block_Engine.Yellow =>
            return Main_Window.Yellow_Prev_Pix;
         when Block_Engine.Grey =>
            return Main_Window.Grey_Prev_Pix;
         when Block_Engine.Blank =>
            return Main_Window.Blank_Prev_Pix;
         when Block_Engine.Green =>
            return Main_Window.Green_Prev_Pix;
         when Block_Engine.Cyan =>
            return Main_Window.Cyan_Prev_Pix;
         when Block_Engine.Magenta =>
            return Main_Window.Magenta_Prev_Pix;
         when Block_Engine.Ghost =>
            return Main_Window.Blank_Prev_Pix;
      end case;
   end Color_To_Prev_Pix;

   procedure Paint_Prev is
      Dummy_Cr : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
   begin
      if not Main_Window.Piece_Preview then
         return;
      end if;
      --        Cr := Cairo.Create (Get_Pixmap (Main_Window.Prev_Screen));
      Dummy_Cr.Get_Drawing_Context_2D (Main_Window.Prev_Screen);
--        Gdk.Cairo.Set_Source_Pixbuf
--          (Cr,
--           Color_To_Prev_Pix (Block_Engine.Get_Next_Piece_Color
--             (Main_Window.Engine.all)),
--           0.0,
--           0.0);
--        Cairo.Paint (Cr);
      Dummy_Cr.Put_Image_Data
        (Color_To_Prev_Pix (Block_Engine.Get_Next_Piece_Color (Main_Window.Engine.all)).all, 0, 0);
--        Cairo.Destroy (Cr);
--        Gdk.Drawable.Draw_Drawable( Get_Pixmap( Main_Window.Prev_Screen ),
--                                    Main_Window.GC,
--                                    Color_To_Prev_Pix(
--      Block_Engine.Get_Next_Piece_Color(Main_Window.Engine.all) ),
--                                    0, 0,
--                                    0, 0,
--                                    85, 85
--                                  );
--        Main_Window.Prev_Screen.Queue_Draw;
   end Paint_Prev;

   type TO_Action_Type is access procedure;
   protected Time_out is
      entry Add
        (Interval :     Duration;
         Action   :     TO_Action_Type;
         ID       : out Natural);
      procedure Remove (ID : out Natural);
      function ID return Natural;
   private
      TO_ID : Natural := 0;
   end Time_out;

   procedure Terminate_Game is
      use New_Score_Dialog_Pkg;
   begin
      --        Glib.Main.Remove (Main_Window.Down_Timeout_ID);
      Time_out.Remove (Main_Window.Down_Timeout_ID);
      Main_Window.Exists_Game := False;
      --     Gtk.Menu_Item.Set_Sensitive (Main_Window.Item_Game_Pause, False);
      Main_Window.Item_Game_Pause.Disabled;
      if Block_Engine.Get_Score (Main_Window.Engine.all) > Get_Minimum_Score then
         --           Show_All (New_Score_Dialog);
         New_Score_Dialog.Hidden (False);
      else
         Gnoga.Application.Singleton.End_Application;
      end if;
   end Terminate_Game;

   procedure Process_And_Draw is
      Score, Level, Pieces, Lines : Integer;
   begin
      Block_Engine.Process_Move (Main_Window.Engine.all);
      Score  := Block_Engine.Get_Score (Main_Window.Engine.all);
      Level  := Block_Engine.Get_Level (Main_Window.Engine.all);
      Pieces := Block_Engine.Get_Pieces_Number (Main_Window.Engine.all);
      Lines  := Block_Engine.Get_Lines_Number (Main_Window.Engine.all);
      Main_Window.Score_Label.Text (Integer'Image (Score));
      Main_Window.Level_Label.Text (Integer'Image (Level));
      Main_Window.Pieces_Label.Text (Integer'Image (Pieces));
      Main_Window.Lines_Label.Text (Integer'Image (Lines));
      if (Block_Engine.Get_Lines_Completed (Main_Window.Engine.all) > 0) and Main_Window.Animation then
         Main_Window.On_Animation   := True;
         Main_Window.Animation_Kind := Line_Completed_Animation;
      end if;
      if Block_Engine.Level_Changed (Main_Window.Engine.all) then
         Main_Window.Down_Count := 0;
         declare
            Tmp : constant Integer := Main_Window.Max_Down_Count;
         begin
            Main_Window.Max_Down_Count := Integer (Float (Main_Window.Max_Down_Count) / 1.35);
            if Main_Window.Max_Down_Count = Tmp then
               Main_Window.Max_Down_Count := Main_Window.Max_Down_Count / 2;
            end if;
         end;
      end if;
      Paint_Prev;
      Game_Engine.Paint_Screen;
--        Main_Window.Game_Screen.Queue_Draw;
      if Block_Engine.Game_Over (Main_Window.Engine.all) then
         Clear_Prev;
         if Main_Window.Animation then
            Main_Window.On_Animation   := True;
            Main_Window.Animation_Kind := Game_Over_Animation;
         else
            Terminate_Game;
         end if;
      end if;
   end Process_And_Draw;

   --     function Auto_Down_Timeout return Boolean is
   procedure Auto_Down_Timeout;
   procedure Auto_Down_Timeout is
   begin
      if Main_Window.On_Animation then
         Do_Animation;
         return;
      end if;
      if Main_Window.Game_Paused then
         return;
      end if;
      if Main_Window.Down_Count /= Main_Window.Max_Down_Count then
         Main_Window.Down_Count := Main_Window.Down_Count + 1;
         if Main_Window.Continuous_Movement then
            Game_Engine.Paint_Screen;
--              Main_Window.Game_Screen.Queue_Draw;
         end if;
         return;
      end if;
      Main_Window.Down_Count := 0;
      Block_Engine.Set_Auto_Down (Main_Window.Engine.all);
      Game_Engine.Process_And_Draw;
--        return True;
   end Auto_Down_Timeout;

   procedure Do_Animation is
   begin
      if Main_Window.Animation_Kind = Line_Completed_Animation then
         Do_Line_Completed_Animation;
      elsif Main_Window.Animation_Kind = Game_Over_Animation then
         Do_Game_Over_Animation;
      end if;
   end Do_Animation;

   procedure Do_Line_Completed_Animation is
      Base_Line, Lines : Natural;
   begin
      Base_Line                   := Block_Engine.Get_Lowest_Line_Completed (Main_Window.Engine.all);
      Lines                       := Block_Engine.Get_Lines_Completed (Main_Window.Engine.all);
      Main_Window.Animation_Count := Main_Window.Animation_Count + 1;
      if Main_Window.Animation_Count = 1 then
         for J in Base_Line .. (Base_Line + Lines - 1) loop
            for I in 1 .. 10 loop
               Paint_White_Block (I, J);
            end loop;
         end loop;
         for j in Base_Line .. 20 loop
            for i in 1 .. 10 loop
               Paint_Block (i, j + Lines, Block_Engine.Get_Position_Color (Main_Window.Engine.all, i, j));
            end loop;
         end loop;
--           Main_Window.Game_Screen.Queue_Draw;
      elsif Main_Window.Animation_Count = 5 then
         for J in Base_Line .. (Base_Line + Lines - 1) loop
            for I in 1 .. 10 loop
               Paint_Block (I, J, Block_Engine.Blank);
            end loop;
         end loop;
--           Main_Window.Game_Screen.Queue_Draw;
      elsif Main_Window.Animation_Count = 10 then
         for J in Base_Line .. (Base_Line + Lines - 1) loop
            for I in 1 .. 10 loop
               Paint_White_Block (I, J);
            end loop;
         end loop;
--           Main_Window.Game_Screen.Queue_Draw;
      elsif Main_Window.Animation_Count = 15 then
         Main_Window.On_Animation    := False;
         Main_Window.Animation_Count := 0;
         Paint_Screen;
--           Main_Window.Game_Screen.Queue_Draw;
         return;
      end if;
   end Do_Line_Completed_Animation;

   procedure Do_Game_Over_Animation is
   begin
      Main_Window.Animation_Count := Main_Window.Animation_Count + 1;
      if (Main_Window.Animation_Count rem 2) = 0 then
         return;
      end if;
      for I in 1 .. 10 loop
         Paint_White_Block (I, (Main_Window.Animation_Count + 1) / 2);
      end loop;
      if Main_Window.Animation_Count = 39 then
         Main_Window.On_Animation    := False;
         Main_Window.Animation_Count := 0;
         Terminate_Game;
      end if;
--        Main_Window.Game_Screen.Queue_Draw;
   end Do_Game_Over_Animation;

   task type Action_Task is
      entry Start
        (Interval : Duration;
         Action   : TO_Action_Type);
   end Action_Task;
   type Action_Task_Access is access Action_Task;

   task body Action_Task is
      AT_Interval : Duration;
      AT_Action   : TO_Action_Type;
   begin
      accept Start
        (Interval : Duration;
         Action   : TO_Action_Type) do
         AT_Interval := Interval;
         AT_Action   := Action;
      end Start;
      loop
         delay AT_Interval;
         if Time_out.ID = 0 then
            exit;
         end if;
         AT_Action.all;
      end loop;
   end Action_Task;

   protected body Time_out is
      entry Add
        (Interval :     Duration;
         Action   :     TO_Action_Type;
         ID       : out Natural) when TO_ID = 0
      is
      begin
         TO_ID := 1;
         ID    := TO_ID;
         Action_Task_Access'(new Action_Task).Start (Interval, Action);
      end Add;
      procedure Remove (ID : out Natural) is
      begin
         TO_ID := 0;
         ID    := TO_ID;
      end Remove;
      function ID return Natural is (TO_ID);
   end Time_out;

   procedure New_Game is
      Initial_Level : Integer;
      use Preferences_Window_Pkg;
--        use type Glib.Main.G_Source_Id;
   begin
      Get_Preferences;
--        Gtk.Menu_Item.Set_Sensitive (Main_Window.Item_Game_Pause);
      Main_Window.Item_Game_Pause.Disabled (False);
      Main_Window.Exists_Game := True;
      Main_Window.Game_Quit   := False;
      Main_Window.Pause (False);
      Game_Engine.Clear_Screen;
--        Main_Window.Game_Screen.Queue_Draw;
      Block_Engine.Init (Main_Window.Engine.all);
      Game_Engine.Paint_Prev;
      Initial_Level := Block_Engine.Get_Initial_Level (Main_Window.Engine.all);

      Main_Window.Down_Count     := 0;
      Main_Window.Max_Down_Count := 40;
      while Initial_Level > 1 loop
         Main_Window.Max_Down_Count := Integer (Float (Main_Window.Max_Down_Count) / 1.35);
         Initial_Level              := Initial_Level - 1;
      end loop;
      Main_Window.Down_Timeout_Interval := 0.020; -- milliseconds

      if Main_Window.Down_Timeout_ID /= 0 then
--                    Glib.Main.Remove (Main_Window.Down_Timeout_ID);
         Time_out.Remove (Main_Window.Down_Timeout_ID);
         --  Let some time for removing Time_out
         delay Main_Window.Down_Timeout_Interval;
      end if;
--        Main_Window.Down_Timeout_ID :=
--          Glib.Main.Timeout_Add (Main_Window.Down_Timeout_Interval,
--             Auto_Down_Timeout'Access);
      Time_out.Add (Main_Window.Down_Timeout_Interval, Auto_Down_Timeout'Access, Main_Window.Down_Timeout_ID);
      Main_Window.Record_Label.Text (Integer'Image (Scores_Window_pkg.Get_Maximum_Score));
   end New_Game;

--     function Process_Key_Press
--       (Win   : access Main_Window_Record'Class;
--        Event : Gdk.Event.Gdk_Event) return Boolean
   procedure Process_Key_Press
     (Win   : in out Main_Window_Record'Class;
      Event : in     Gnoga.Gui.Base.Keyboard_Event_Record)
   is
      Key : Integer;
   begin
      if Main_Window.On_Animation then
         return;
      end if;
      if Win.Game_Paused or (not Win.Exists_Game) then
         return;
      end if;
      Key := Event.Key_Code;
      if Key = Gnoga.Types.Key_Codes.Key_Down then
         Block_Engine.Set_Next_Move (Win.Engine.all, Block_Engine.Down);
      elsif Key = Gnoga.Types.Key_Codes.Key_Up then
         Block_Engine.Set_Next_Move (Win.Engine.all, Block_Engine.Rotate);
      elsif Key = Gnoga.Types.Key_Codes.Key_Left then
         Block_Engine.Set_Next_Move (Win.Engine.all, Block_Engine.Left);
      elsif Key = Gnoga.Types.Key_Codes.Key_Right then
         Block_Engine.Set_Next_Move (Win.Engine.all, Block_Engine.Right);
      elsif Key = Gnoga.Types.Key_Codes.Key_Space then
         Block_Engine.Set_Next_Move (Win.Engine.all, Block_Engine.Drop);
      end if;
      Game_Engine.Process_And_Draw;
--        return False;
   end Process_Key_Press;

end Game_Engine;
