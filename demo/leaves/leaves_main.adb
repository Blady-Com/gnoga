--  Leaves_main: GNOGA application with a "match-3"-style game.
--  All pictures are taken by the author with a Nokia 301 phone, some are reworked with GIMP.

--  Legal licensing note:

--  Copyright (c) 2017 Gautier de Montmollin

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

with Match_3;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
--  with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Plugin.Pixi;
with Gnoga.Gui.Plugin.Pixi.Graphics;
with Gnoga.Gui.Plugin.Pixi.Sprite;
with Gnoga.Gui.View;
with Gnoga.Types;

with Ada.Numerics.Float_Random;

with UXStrings.Conversions;

procedure Leaves_main is
   use Match_3;
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;
   use Gnoga.Gui.Element.Canvas;
   use Gnoga.Gui.Plugin.Pixi;
   use Gnoga.Gui.Plugin.Pixi.Sprite;
   use Gnoga.Gui.View;

   use all type Gnoga.String;

   function Bitmap
     (c : Cell)
      return Gnoga.String
   is
   begin
      case c is
         when empty =>
            return "";
         when tile_1 =>
            return "d_0536.png";
         when tile_2 =>
            return "d_0537.png";
         when tile_3 =>
            return "d_0544.png";
         when tile_4 =>
            return "d_0541.png";
         when tile_5 =>
            return "d_0540.png";
         when tile_6 =>
            return "d_0542.png";
         when tile_7 =>
            return "d_0543.png";
         when tile_8 =>
            return "d_0539.png";
         when tile_9 =>
            return "d_0545.png";
         when tile_10 =>
            return "d_0546.png";
      end case;
   end Bitmap;

   type Board_Sprite is array (Positive range <>, Positive range <>) of Sprite_Type;

   type Game_App_Data (columns, rows : Positive) is new Connection_Data_Type with record
      Main_Window    : Window.Pointer_To_Window_Class;
      Background     : View_Type;
      My_Canvas      : aliased Canvas.Canvas_Type;
      A              : Plugin.Pixi.Application_Type;
      C              : Plugin.Pixi.Container_Type;
      R              : Plugin.Pixi.Renderer_Type;
      My_Board       : Match_3.Board (1 .. columns, 1 .. rows);
      Sprite         : Board_Sprite (1 .. columns, 1 .. rows);
      Bkg_pic        : Sprite_Type;  --  Background image
      Tile_choice    : Sprite_Type;  --  Highlighting the chosen tile
      Btn_Down_pix_x : Natural;
      Btn_Down_pix_y : Natural;
      Btn_Down_pos   : Cell_pos;
      Btn_Up_pos     : Cell_pos;
      Click_1_pos    : Cell_pos;
      Click_2_pos    : Cell_pos;
   end record;
   type App_Access is access all Game_App_Data;

   procedure Unclick (App : in out Game_App_Data) is
   begin
      App.Click_2_pos  := null_cell_pos;
      App.Btn_Down_pos := null_cell_pos;
   end Unclick;

   cell_size        : constant := 68;
   cell_padding     : constant := 1;
   grid_width       : constant := 2;
   cell_size_padded : constant := cell_size + cell_padding * 2 + grid_width;

   function Pix_hor_pos
     (cp : Cell_pos)
      return Integer
   is
   begin
      return grid_width + (cp.x - 1) * cell_size_padded;
   end Pix_hor_pos;

   function Pix_ver_pos
     (cp : Cell_pos)
      return Integer
   is
   begin
      return grid_width + (cp.y - 1) * cell_size_padded;
   end Pix_ver_pos;

   procedure Create_tile_sprite
     (App : in out Game_App_Data;
      cp  :        Cell_pos)
   is
      cl : constant Cell := App.My_Board (cp.x, cp.y);
   begin
      if cl = empty then
         return;
      end if;
      App.Sprite (cp.x, cp.y).Create (App.C, "img/" & Bitmap (cl), Column => Pix_hor_pos (cp), Row => Pix_ver_pos (cp));
   end Create_tile_sprite;

   trace : constant Boolean := True;

   procedure Game_log (s : Gnoga.String) is
   begin
      if trace then
         Gnoga.Log (s);
      end if;
   end Game_log;

   procedure Swap_tiles
     (App    : in out Game_App_Data;
      c1, c2 :        Cell_pos)
   is
      swap_time          : constant               := 0.2;
      dx                 : constant Integer       := cell_size_padded * (c2.x - c1.x);
      dy                 : constant Integer       := cell_size_padded * (c2.y - c1.y);
      vx                 : constant Velocity_Type := Velocity_Type (dx) / swap_time;
      vy                 : constant Velocity_Type := Velocity_Type (dy) / swap_time;
      ox1, oy1, ox2, oy2 : Integer;
      aux                : Cell;
   begin
      Game_log ("Swapping: (" & Image (c1.x) & Image (c1.y) & ") and (" & Image (c2.x) & Image (c2.y) & ")");
      Unclick (App);
      App.Sprite (c1.x, c1.y).Position (ox1, oy1);
      App.Sprite (c2.x, c2.y).Position (ox2, oy2);
      App.Sprite (c1.x, c1.y).Motion (Column_Velocity => +vx, Row_Velocity => +vy);
      App.Sprite (c2.x, c2.y).Motion (Column_Velocity => -vx, Row_Velocity => -vy);
      delay swap_time;
      --  Stop movement
      App.Sprite (c1.x, c1.y).Motion (0.0, 0.0);
      App.Sprite (c2.x, c2.y).Motion (0.0, 0.0);
      --  Set new exact position (sometimes the animation's displacement doesn't match time * speed)
      App.Sprite (c1.x, c1.y).Locate (ox2, oy2);
      App.Sprite (c2.x, c2.y).Locate (ox1, oy1);
      --  Swap the board's contents
      aux                       := App.My_Board (c1.x, c1.y);
      App.My_Board (c1.x, c1.y) := App.My_Board (c2.x, c2.y);
      App.My_Board (c2.x, c2.y) := aux;
      --  Swap the sprites themselves (Delete & Re-Create)
      App.Sprite (c1.x, c1.y).Finalize;
      App.Sprite (c2.x, c2.y).Finalize;
      Create_tile_sprite (App, (c1.x, c1.y));
      Create_tile_sprite (App, (c2.x, c2.y));
   end Swap_tiles;

   level_probs : constant Tile_prob := (tile_1 .. tile_6 => 1.0 / 6.0, others => 0.0);

   --  Process_matches: show matches, empty the cells, count points, let new tiles down
   procedure Process_matches (App : in out Game_App_Data) is
      ma : constant Match_list := Matches (App.My_Board);
      use Ada.Numerics.Float_Random;
      gen                : Generator;
      animation_slowdown : constant := 1;
      --
      procedure Show_matches is
         fx : array (ma'Range) of Sprite_Type;
         function Image is new UXStrings.Conversions.Scalar_Image (Match_kind);
      begin
         for tch in ma'Range loop
            fx (tch).Create
              (App.C, "img/" & To_Lower (Image (ma (tch).kind)) & ".png", Column => Pix_hor_pos (ma (tch).pos),
               Row => Pix_ver_pos (ma (tch).pos), Column_Velocity => 6.0 * (Random (gen) - 0.5),
               Row_Velocity                                                      => Random (gen) - 4.0);
         end loop;
         Game_log ("[--Delay--] Show matches");
         delay 0.41;
         for tch in ma'Range loop
            fx (tch).Finalize;
         end loop;
      end Show_matches;
      --
      procedure Delete_matches is
      begin
         Empty_matching_cells (App.My_Board, ma);
         for x in 1 .. App.columns loop
            for y in 1 .. App.rows loop
               if App.My_Board (x, y) = empty then
                  App.Sprite (x, y).Finalize;
               end if;
            end loop;
         end loop;
         delay 0.11 * animation_slowdown;
      end Delete_matches;
      --
      procedure New_tiles is
         no_more_move : Boolean;
         procedure Visual_gravity_step is
            gl                        : constant Gravity_list := Gravity_step (App.My_Board, level_probs);
            dx, dy, dst, dmax, nx, ny : Float;
            vx, vy                    : Velocity_Type;
            fall_time_one_cell        : constant              := 0.19 * animation_slowdown;
            stopped                   : Natural               := 0;
         begin
            no_more_move := gl'Length = 0;
            if no_more_move then
               return;
            end if;
            Apply_gravity_moves (App.My_Board, gl);
            --  App.R.Auto_Rendering (App.C, False);
            dmax := 0.0;
            for g of gl loop
               if g.from.y > 0 then  --  On the board
                  App.Sprite (g.from.x, g.from.y).Finalize;
               end if;
               Create_tile_sprite (App, (g.to.x, g.to.y));
               App.Sprite (g.to.x, g.to.y).Locate (Row => Pix_ver_pos (g.from), Column => Pix_hor_pos (g.from));
               dx   := Float (Pix_hor_pos (g.to) - Pix_hor_pos (g.from));
               dy   := Float (Pix_ver_pos (g.to) - Pix_ver_pos (g.from));
               dst  := Float'Max (abs dx, abs dy);  --  Actually always = dy
               dmax := Float'Max (dmax, dst);
               if dst < 1.0 then
                  nx := 0.0;
                  ny := 0.0;
               else
                  nx := dx * Float (cell_size_padded) / dst;
                  ny := dy * Float (cell_size_padded) / dst;
               end if;
               vx := nx / fall_time_one_cell;
               vy := ny / fall_time_one_cell;
               App.Sprite (g.to.x, g.to.y).Motion (Column_Velocity => vx, Row_Velocity => vy);
            end loop;
            --  App.R.Auto_Rendering (App.C, True);
            Game_log ("[--Delays--] Falling tiles");
            multi_tile :
            for tile_step in 1 .. App.My_Board'Length (2) loop
               delay fall_time_one_cell;
               for g of gl loop
                  if g.to.y - g.from.y = tile_step then
                     --  Stop movement
                     App.Sprite (g.to.x, g.to.y).Motion (0.0, 0.0);
                     --  Set new exact position (sometimes the animation's displacement unaccuracy)
                     App.Sprite (g.to.x, g.to.y).Locate (Row => Pix_ver_pos (g.to), Column => Pix_hor_pos (g.to));
                     stopped := stopped + 1;
                     exit multi_tile when stopped = gl'Length;
                  end if;
               end loop;
            end loop multi_tile;
         end Visual_gravity_step;
      begin
         loop
            Visual_gravity_step;
            exit when no_more_move;
         end loop;
      end New_tiles;
   begin
      Reset (gen);
      Show_matches;
      Delete_matches;
      New_tiles;
   end Process_matches;

   procedure Mouse_Down
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);

   procedure Check_match
     (App            : in out Game_App_Data;
      swap_1, swap_2 :        Cell_pos)
   is
   begin
      App.My_Canvas.On_Mouse_Down_Handler (null);
      --  Un-display choice highlighter
      App.Tile_choice.Finalize;
      Game_log
        ("Trying to swap: (" & Image (swap_1.x) & Image (swap_1.y) & ") and (" & Image (swap_2.x) & Image (swap_2.y) &
         ")");
      if swap_1.x in App.My_Board'Range (1) and then swap_1.y in App.My_Board'Range (2)
        and then swap_2.x in App.My_Board'Range (1) and then swap_2.y in App.My_Board'Range (2)
        and then App.My_Board (swap_1.x, swap_1.y) in Movable_tile
        and then App.My_Board (swap_2.x, swap_2.y) in Movable_tile
        and then
        ((abs (swap_1.x - swap_2.x) = 1 and swap_1.y = swap_2.y) or
         (abs (swap_1.y - swap_2.y) = 1 and swap_1.x = swap_2.x))
      then
         Swap_tiles (App, swap_1, swap_2);
         if Find_any_match (App.My_Board) then
            --  Possible chain reaction: when gaps left from previous matches
            --  are filled, other matches may occur. Hence the following loop.
            loop
               Process_matches (App);
               exit when not Find_any_match (App.My_Board);
            end loop;
         else
            Swap_tiles (App, swap_1, swap_2);  --  No match, swap the tiles back.
         end if;
      end if;
      App.My_Canvas.On_Mouse_Down_Handler (Mouse_Down'Unrestricted_Access);
   end Check_match;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);
   procedure Mouse_Up
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new Game_App_Data (13, 7);
      G   : Plugin.Pixi.Graphics.Graphics_Type;
   begin
      Main_Window.Connection_Data (App);

      App.Main_Window := Main_Window'Unchecked_Access;

      --  App.Background.Create (Main_Window);
      --  App.Background.Background_Color ("orange");

      Plugin.Pixi.Load_PIXI (Main_Window);

      App.My_Canvas.Create
        (App.Main_Window.all, grid_width + App.My_Board'Length (1) * cell_size_padded,
         grid_width + App.My_Board'Length (2) * cell_size_padded);
      App.My_Canvas.Border;
      App.My_Canvas.Place_Inside_Bottom_Of (App.Main_Window.Document.Body_Element.all);
      App.My_Canvas.On_Mouse_Down_Handler (Mouse_Down'Unrestricted_Access);

      Fill (App.My_Board, level_probs);
      Unclick (App.all);

      App.A.Create
        (App.My_Canvas, grid_width + App.My_Board'Length (1) * cell_size_padded,
         grid_width + App.My_Board'Length (2) * cell_size_padded);
      App.R.Create (App.A);
      App.C.Create (App.A);
      G.Create (App.C);

      --  G.Fill_Color ("yellow");
      --  G.Fill_Rectangle ((0, 0, App.My_Canvas.Width, App.My_Canvas.Height));
      --  R.Render (App.C);

--        C.Font (Height => "40px");
--        G.Stroke_Text (R, "Hello World!", 100, 100);

      App.Bkg_pic.Create (App.C, "img/board_bkg_1.jpg", 0, 0);
      for x in App.My_Board'Range (1) loop
         for y in App.My_Board'Range (2) loop
            Create_tile_sprite (App.all, (x, y));
         end loop;
      end loop;
      App.R.Auto_Rendering (App.C, True);

--        loop
--           delay 1.0;
--        end loop;
--  XX on loop exit XX     Gnoga.Gui.Plugin.Pixi.Sprite.Delete_All (App.C);
--  XX on loop exit XX      App.R.Auto_Rendering (App.C, False);

--  XX on loop exit XX      Connection.Hold;
   end On_Connect;

   function Locate_cell
     (mx, my : Natural)
      return Cell_pos
   is
   begin
      return (1 + mx / cell_size_padded, 1 + my / cell_size_padded);
   end Locate_cell;

   procedure Mouse_Down
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Btn_Down_pix_x := Mouse_Event.X;
      App.Btn_Down_pix_y := Mouse_Event.Y;
      App.Btn_Down_pos   := Locate_cell (App.Btn_Down_pix_x, App.Btn_Down_pix_y);
      --  Un-display previous choice highlighter
      App.Tile_choice.Finalize;
      --  Highlight new choice
      if App.Btn_Down_pos.x in App.My_Board'Range (1) and then App.Btn_Down_pos.y in App.My_Board'Range (2) then
         App.Tile_choice.Create
           (App.C, "img/select.png", Column => Pix_hor_pos (App.Btn_Down_pos) - 2,
            Row                             => Pix_ver_pos (App.Btn_Down_pos) - 2);
      else
         App.Btn_Down_pos := null_cell_pos;
      end if;
      Game_log ("Mouse Down; Down pos =" & Image (App.Btn_Down_pix_x) & Image (App.Btn_Down_pix_y));
      App.My_Canvas.On_Mouse_Move_Handler (Mouse_Move'Unrestricted_Access);
      App.My_Canvas.On_Mouse_Up_Handler (Mouse_Up'Unrestricted_Access);
   end Mouse_Down;

   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      App         : constant App_Access := App_Access (Object.Connection_Data);
      dx, dy      : Integer;
      target_cell : Cell_pos            := Locate_cell (Mouse_Event.X, Mouse_Event.Y);
      slide       : Boolean             := False;
   begin
      Game_log ("Mouse Move" & Image (Mouse_Event.X) & Image (Mouse_Event.Y));
      if App.Btn_Down_pos = null_cell_pos then
         return;
      end if;
      dx := Mouse_Event.X - App.Btn_Down_pix_x;
      dy := Mouse_Event.Y - App.Btn_Down_pix_y;
      if abs dx > cell_size / 2 then
         Game_log ("Hor. Slide, delta= " & Image (dx));
         slide := True;
         if dx < 0 then
            target_cell := (Integer'Max (1, App.Btn_Down_pos.x - 1), App.Btn_Down_pos.y);
         else
            target_cell := (Integer'Min (App.columns, App.Btn_Down_pos.x + 1), App.Btn_Down_pos.y);
         end if;
      elsif abs dy > cell_size / 2 then
         Game_log ("Ver. Slide, delta= " & Image (dy));
         slide := True;
         if dy < 0 then
            target_cell := (App.Btn_Down_pos.x, Integer'Max (1, App.Btn_Down_pos.y - 1));
         else
            target_cell := (App.Btn_Down_pos.x, Integer'Min (App.rows, App.Btn_Down_pos.y + 1));
         end if;
      end if;
      if slide then
         App.My_Canvas.On_Mouse_Move_Handler (null);
         Check_match (App.all, App.Btn_Down_pos, target_cell);
      end if;
   end Mouse_Move;

   procedure Mouse_Up
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Game_log ("Mouse Up" & Image (Mouse_Event.X) & Image (Mouse_Event.Y));
      App.My_Canvas.On_Mouse_Move_Handler (null);
      App.My_Canvas.On_Mouse_Up_Handler (null);
      App.Btn_Up_pos := Locate_cell (Mouse_Event.X, Mouse_Event.Y);
      if App.Btn_Down_pos = App.Btn_Up_pos then
         --  It's a click (down then up in the same cell),
         --  even a slow click or with some movement between down and up.
         Game_log ("Click");
         App.Click_1_pos := App.Click_2_pos;
         App.Click_2_pos := App.Btn_Up_pos;
         if not (App.Click_2_pos.x in App.My_Board'Range (1) and then App.Click_2_pos.y in App.My_Board'Range (2)) then
            App.Click_2_pos := null_cell_pos;
         end if;
         if App.Click_1_pos = App.Click_2_pos then
            --  Two clicks in the same cell: do nothing
            null;
         elsif App.Click_1_pos /= null_cell_pos then
            Check_match (App.all, App.Click_1_pos, App.Click_2_pos);
         end if;
      end if;
   end Mouse_Up;

begin
   Application.Multi_Connect.Initialize (Event => On_Connect'Unrestricted_Access, Boot => "debug.html");

   Application.Title ("Plant mania");
   Application.HTML_On_Close ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end Leaves_main;
