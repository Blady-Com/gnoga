with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;

with Gnoga.Types;
with Gnoga.Window;
with Gnoga.Element.Common;
with Gnoga.Element.Canvas;

package body Snake.Connection is
   use Gnoga.Types;
   use Gnoga.Element.Common;
   use Gnoga.Element.Canvas;

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
           "<p>Use the arrow keys to move Sparky to pick up batteries.</p>" &
           "<i>Becareful...</i><br />" &
           "If sparky hits his tail he electrocutes himself to <b>death!!</b>" &
           "<br /><br />" &
           "Use the keys a,w,s,d for direction keys.<br/><br/>");
      Display.Style ("text-align", "center");
      Display.Style ("width", "100%");
      Display.Style ("height", "100%");
      Display.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);

      for i in 1 .. 15 loop
         declare
            Blip : Span_Type;
         begin
            Blip.Create (Display, "&nbsp;*&nbsp;");
            Blip.Place_Inside_Bottom_Of (Display);
            delay 0.5;
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
         Background  : DIV_Type;
         Display     : Canvas_Type;

         Score           : Integer := 0;
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
      App.Main_Window.On_Character_Handler (On_Key_Press'Access);

      App.Background.Create (Main_Window);
      App.Background.Style ("width", "100%");
      App.Background.Style ("height", "100%");
      App.Background.Background_Color ("orange");
      App.Background.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);

      App.Display.Create (App.Background, Display_Width, Display_Height);
      App.Display.Style ("border-width", 3);
      App.Display.Style ("border-style", "solid");
      App.Display.Background_Color ("white");
      App.Display.Place_Inside_Top_Of (App.Background);

      -- Initialize Snake
      for i in reverse 0 .. Initial_Length - 1 loop
         App.Snake.Append (Point_Type'(i, 0));
      end loop;
      App.Display.Get_Drawing_Context_2D (Context);

      loop
         Paint (Context, App, Game_Over);

         if Game_Over then
            exit;
         end if;

         delay 0.1;
      end loop;
   end Start_Game;

   procedure On_Key_Press (Object : in out Base_Type'Class;
                           Key    : in     Character)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      if Key = 'w' or Key = 'W' then
         App.Snake_Direction := Up;
      elsif Key = 'a' or Key = 'A' then
         App.Snake_Direction := Left;
      elsif Key = 'd' or Key = 'D' then
         App.Snake_Direction := Right;
      elsif Key = 's' or Key = 'S' then
         App.Snake_Direction := Down;
      end if;
   end On_Key_Press;

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
            Context.Font (Height_In_Pixels => 20);
            Context.Fill_Text ("GAME OVER", 20, 20);
            Game_Over := True;
         else
            App.Snake.Prepend (Head_Cell);

            --  Snake Draw
            for i in App.Snake.First_Index .. App.Snake.Last_Index loop
               Context.Fill_Color ("purple");
               Draw_Segment (App.Snake.Element (i));
            end loop;

            Context.Fill_Color ("brown");
            Draw_Segment (App.Food);

            if Food_Collision (Head_Cell) then
               App.Score := App.Score + 10;

               App.Food := New_Food;
            else
               Context.Fill_Color ("white");
               Draw_Segment (App.Snake.Element (App.Snake.Last_Index));

               App.Snake.Delete_Last;
            end if;
         end if;
      end;

      Context.Fill_Color ("green");
      Context.Font (Height_In_Pixels => 10);
      Context.Fill_Text ("Score :" & App.Score'Img, 5, Display_Height - 15);
   end Paint;

   ------------------------
   -- On_Connect_Default --
   ------------------------

   procedure On_Connect_Default
     (Main_Window : in out Window_Type'Class;
      Connection  : access Connection_Holder_Type)
   is
      App : aliased App_Data;
   begin
      App.Main_Window := Main_Window'Unchecked_Access;
      Connection_Data (Main_Window, App'Unchecked_Access);

      Display_Splash (Main_Window);

      Start_Game (Main_Window);

      Connection.Hold;
   end On_Connect_Default;

end Snake.Connection;
