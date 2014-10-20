with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;

with Gnoga.Types;
with Gnoga.Gui.View;
with Gnoga.Gui.Window;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D;

package body Snake.Connection is
   use Gnoga.Types;
   use Gnoga.Gui.Element.Common;
   use Gnoga.Gui.View;
   use Gnoga.Gui.Element.Canvas;
   use Gnoga.Gui.Element.Canvas.Context_2D;

   --------------------
   -- Display_Splash --
   --------------------

   procedure Display_Splash (Main_Window : in out Window_Type'Class) is
      Display : DIV_Type;
   begin
      Display.Create
        (Main_Window,
         "<H1>" & Snake.Title & "</H1>" &
           "<br />" &
           "<p>Use your kebyoard to move Sparky to pick up batteries.</p>" &
           "<i>Becareful...</i><br />" &
           "If sparky hits his tail he electrocutes himself to <b>death!!</b>" &
           "<br /><br />" &
           "Use the arrow keys or a,w,s,d for direction keys.<br/><br/>");
      Main_Window.Set_View (Display);
      Display.Text_Alignment (Gnoga.Gui.Element.Center);

      for i in 1 .. 15 loop
         declare
            Blip : Span_Type;
         begin
            Blip.Create (Display, "&nbsp;*&nbsp;");
            Blip.Place_Inside_Bottom_Of (Display);
            delay 0.3;
         end;
      end loop;

      Display.Visible (False);
   end Display_Splash;

   type Snake_Direction_Type is (Left, Right, Up, Down);

   package Snake_Arrays is new Ada.Containers.Vectors (Natural, Point_Type);

   function New_Food return Point_Type is

      subtype X_Range is Integer range 0 .. Display_Width / Segment_Size;
      subtype Y_Range is Integer range 0 .. Display_Height / Segment_Size;

      package Random_X is new Ada.Numerics.Discrete_Random (X_Range);
      package Random_Y is new Ada.Numerics.Discrete_Random (Y_Range);

      X_Gen : Random_X.Generator;
      Y_Gen : Random_Y.Generator;

      Food_Cell : Point_Type;
   begin
      Random_X.Reset (X_Gen);
      Random_Y.Reset (Y_Gen);

      Food_Cell :=  (Random_X.Random (X_Gen), Random_Y.Random (Y_Gen));

      return Food_Cell;
   end New_Food;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Pointer_To_Window_Class;
         Background  : View_Type;
         Display     : Canvas_Type;

         Score           : Integer              := 0;
         Snake           : Snake_Arrays.Vector;
         Snake_Direction : Snake_Direction_Type := Right;
         Food            : Point_Type           := New_Food;
      end record;
   type App_Access is access all App_Data;

   procedure Paint (Context   : in out Context_2D_Type;
                    App       : in     App_Access;
                    Game_Over : out    Boolean);

   ----------------
   -- Start_Game --
   ----------------

   procedure Start_Game (Main_Window : in out Window_Type'Class) is
      App : App_Access := App_Access (Main_Window.Connection_Data);

      Context   : Context_2D_Type;
      Game_Over : Boolean;
   begin
      App.Main_Window.On_Key_Down_Handler (On_Key_Down'Access);

      App.Background.Create (Main_Window);
      App.Background.Background_Color ("orange");

      App.Display.Create (App.Background, Display_Width, Display_Height);
      App.Display.Display ("block");
      App.Display.Margin ("auto", "auto", "auto", "auto");
      App.Display.Border (Width => "thin");
      App.Display.Border_Radius ("10px");
      App.Display.Background_Color ("white");
      App.Display.Shadow (Horizontal_Position => "3px",
                          Vertical_Position   => "3px",
                          Blur                => "5px");

      -- Initialize Snake
      for i in reverse 0 .. Initial_Length - 1 loop
         App.Snake.Append (Point_Type'(i, 0));
      end loop;
      Context.Get_Drawing_Context_2D (App.Display);

      loop
         Paint (Context, App, Game_Over);

         if Game_Over then
            exit;
         end if;

         delay 0.1;
      end loop;
   end Start_Game;

   procedure On_Key_Down (Object : in out Base_Type'Class;
                          Key    : in     Keyboard_Event_Record)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      if Key.Key_Code = 38 or Key.Key_Code = Character'Pos ('w') then
         App.Snake_Direction := Up;
      elsif Key.Key_Code = 37 or Key.Key_Code = Character'Pos ('a') then
         App.Snake_Direction := Left;
      elsif Key.Key_Code = 39 or Key.Key_Code = Character'Pos ('d') then
         App.Snake_Direction := Right;
      elsif Key.Key_Code = 40 or Key.Key_Code = Character'Pos ('s') then
         App.Snake_Direction := Down;
      end if;
   end On_Key_Down;

   procedure Paint (Context   : in out Context_2D_Type;
                    App       : in     App_Access;
                    Game_Over : out    Boolean)
   is
      Background_Rectangle : constant Rectangle_Type :=
        (0, 0, Display_Width, Display_Height);

      procedure Draw_Segment (Cell : Point_Type) is
         Cell_Rectangle : constant Rectangle_Type :=
           (Cell.X * Segment_Size, Cell.Y * Segment_Size,
            Segment_Size, Segment_Size);
      begin
         Context.Fill_Rectangle (Cell_Rectangle);
      end Draw_Segment;

      function Self_Collision (Cell : Point_Type) return Boolean is
      begin
         for i in App.Snake.First_Index .. App.Snake.Last_Index loop
            declare
               Current : Point_Type := App.Snake.Element (i);
            begin
               if Current.X = Cell.X and Current.Y = Cell.Y then
                  return True;
               end if;
            end;
         end loop;

         return False;
      end Self_Collision;

      function Food_Collision (Cell : Point_Type) return Boolean is
      begin
         if Cell.X = App.Food.X and Cell.Y = App.Food.Y then
            return True;
         else
            return False;
         end if;
      end Food_Collision;
   begin
      Game_Over := False;

      --  Snake Move Code
      declare
         Head_Cell : Point_Type := App.Snake.Element (0);
      begin
         case App.Snake_Direction is
            when Right =>
               Head_Cell.X := Head_Cell.X + 1;
            when Left =>
               Head_Cell.X := Head_Cell.X - 1;
            when Up =>
               Head_Cell.Y := Head_Cell.Y - 1;
            when Down =>
               Head_Cell.Y := Head_Cell.Y + 1;
         end case;

         if
           Head_Cell.X < 0 or Head_Cell.X * Segment_Size >= Display_Width or
           Head_Cell.Y < 0 or Head_Cell.Y * Segment_Size >= Display_Height or
           Self_Collision (Head_Cell)
         then
            Context.Fill_Color ("red");
            Context.Font (Height => "20px");
            Context.Fill_Text ("GAME OVER", 30, 30);
            Game_Over := True;
         else
            App.Snake.Prepend (Head_Cell);

            --  Snake Draw
            for i in App.Snake.First_Index .. App.Snake.Last_Index loop
               Context.Fill_Color ("purple");
               Draw_Segment (App.Snake.Element (i));
            end loop;

            if Food_Collision (Head_Cell) then
               -- clear old score
               Context.Fill_Color ("white");
               Context.Font (Height => "12px");
               Context.Fill_Text
                 ("Score :" & App.Score'Img, 5, Display_Height - 15);

               App.Score := App.Score + 10;

               App.Food := New_Food;
            else
               Context.Fill_Color ("white");
               Draw_Segment (App.Snake.Element (App.Snake.Last_Index));

               App.Snake.Delete_Last;
            end if;

            Context.Fill_Color ("brown");
            Draw_Segment (App.Food);
         end if;
      end;

      Context.Fill_Color ("green");
      Context.Font (Height => "12px");
      Context.Fill_Text ("Score :" & App.Score'Img, 5, Display_Height - 15);
   end Paint;

   ------------------------
   -- On_Connect_Default --
   ------------------------

   procedure On_Connect_Default
     (Main_Window : in out Window_Type'Class;
      Connection  : access Connection_Holder_Type)
   is
      App : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);

      App.Main_Window := Main_Window'Unchecked_Access;

      Display_Splash (Main_Window);

      Start_Game (Main_Window);
   end On_Connect_Default;

begin
   On_Connect_Handler
     (Event => Snake.Connection.On_Connect_Default'Unrestricted_Access,
      Path  => "default");
end Snake.Connection;
