with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Plugin.Pixi;
with Gnoga.Gui.Plugin.Pixi.Graphics;
with Gnoga.Gui.Plugin.Pixi.Sprite;
with Gnoga.Gui.Plugin.Pixi.Text;
with Gnoga.Types;
with Gnoga.Types.Colors;

procedure Pixi_Sprite_Test is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;
   use Gnoga.Gui.Element.Canvas;

   type App_Data is new Connection_Data_Type with record
      Main_Window : Window.Pointer_To_Window_Class;
      My_Canvas   : aliased Canvas.Canvas_Type;
      SP1, SP2    : Plugin.Pixi.Sprite.Sprite_Type;
   end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.SP1.Motion (App.SP1.Row_Velocity, App.SP1.Column_Velocity + 1.0);
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type)
   is
      App        : constant App_Access := new App_Data;
      R          : Plugin.Pixi.Renderer_Type;
      C          : Plugin.Pixi.Container_Type;
      G          : Plugin.Pixi.Graphics.Graphics_Type;
      T          : Plugin.Pixi.Texture_Type;
      S1, S2     : Plugin.Pixi.Style_Type;
      M0, M1, M2 : Plugin.Pixi.Text.Text_Type;

      Button1 : Common.Button_Type;

      X_Pos, Y_Pos : Integer;
      Coinc        : Boolean := False;
   begin
      Main_Window.Connection_Data (App);

      App.Main_Window := Main_Window'Unchecked_Access;

      Button1.Create
      (Parent                                           =>
         Main_Window.Document.Body_Element.all, Content =>
         "Go faster!");
      Button1.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);
      Button1.On_Click_Handler (On_Click'Unrestricted_Access);

      Plugin.Pixi.Load_PIXI (Main_Window);

      App.My_Canvas.Create (Main_Window, 600, 400);
      App.My_Canvas.Border;
      App.My_Canvas.Place_Inside_Bottom_Of
      (App.Main_Window.Document.Body_Element.all);

      R.Create (App.My_Canvas);
      C.Create (R);
      G.Create (C);

--        delay 30.1;

      G.Line_Color (Gnoga.Types.Colors.Yellow);
      G.Line_Width (8);
      G.Draw_Rect (50, 10, 90, 40);
      R.Render (C);

      S1.Create (G);
      S1.Font_Family ("Arial");
      S1.Font_Size ("36px");
      S1.Font_Style (Gnoga.Gui.Element.Italic);
      S1.Font_Weight (Gnoga.Gui.Element.Weight_Bold);
      S1.Fill (Gnoga.Types.Colors.Purple);
      S1.Stroke (Gnoga.Types.Colors.Orange);
      S1.Stroke_Thickness (5);
      S1.Drop_Shadow (True);
      S1.Drop_Shadow_Color (Gnoga.Types.Colors.Green);
      S1.Drop_Shadow_Distance (6);
      --  Create M0 in G so it won't be considered as sprite
      M0.Create (G, "Hello World!", 100, 100);
      M0.Set_Style (S1);

      S2.Create (G);
      S2.Fill (Gnoga.Types.Colors.Cyan);
      S2.Font_Size ("40px");
      --  Create M1 in C so it will be considered as sprite
      M1.Create (C, "with", 150, 150);
      M1.Set_Style (S2);
      M1.Rotation_Velocity (10.0);
      --  Create M2 in C so it will be considered as sprite
      M2.Create (C, "PIXI", 150, 250);
      M2.Set_Style (S2);
      M2.Rotation_Velocity (-10.0);

      App.SP1.Create (C, "img/E4a.png", 10, 10, 8.0, 16.0);
      --  C is set as the container of sprites
      R.Auto_Rendering (C, True);

      while not App.SP1.Coincidence (110, 210, 20) loop
         delay 0.1;
      end loop;

      App.SP1.Locate (150, 5);
      T.Create (R, "img/E6a.png");
      App.SP1.Put_Texture (T);
      App.SP1.Rotation_Velocity (10.0);
      App.SP1.Rotation_Acceleration (10.0);
      delay 5.0;
      G.Line_Color (Gnoga.Types.Colors.Green);
      G.Move_To (100, 140);
      G.Line_To (App.SP1.Column, App.SP1.Row);
      R.Render (C);
      Gnoga.Log ("Distance:" & App.SP1.Distance (100, 100)'Img);
      Gnoga.Log ("Rotation:" & App.SP1.Rotation'Img);
      delay 5.0;
      App.SP1.Rotation_Acceleration (-10.0);
      G.Line_Color (Gnoga.Types.Colors.Grey);
      G.Move_To (100, 140);
      G.Line_To (App.SP1.Column, App.SP1.Row);
      R.Render (C);
      Gnoga.Log ("Distance:" & App.SP1.Distance (100, 100)'Img);
      Gnoga.Log ("Rotation:" & App.SP1.Rotation'Img);
      delay 10.0;
      App.SP1.Delete (C);

      App.SP1.Create (C, "img/E11a.png", 250, 10);
      App.SP2.Create (C, "img/E17a.png", 250, 100);
      App.SP1.Motion (0.0, 5.0);
      App.SP1.Acceleration (0.0, 1.0);
      App.SP2.Motion (0.0, 5.0);
      App.SP2.Anchor (0.5, 0.5);
      App.SP2.Rotation_Velocity (-45.0);

      loop
         if not Gnoga.Gui.Plugin.Pixi.Sprite.Coincidence
             (App.SP1,
              App.SP2,
              60) and
           not Coinc
         then
            App.SP1.Position (Y_Pos, X_Pos);
            if X_Pos > 350 then
               App.SP1.Acceleration (0.0, -5.0);
               exit;
            end if;
         else
            if Coinc then
               if not Gnoga.Gui.Plugin.Pixi.Sprite.Coincidence
                   (App.SP1,
                    App.SP2,
                    65)
               then
                  Coinc := False;
                  App.SP1.Position (Y_Pos, X_Pos);
                  App.SP1.Locate (Y_Pos - 40, X_Pos);
               end if;
            else
               Coinc := True;
               App.SP1.Position (Y_Pos, X_Pos);
               App.SP1.Locate (Y_Pos + 40, X_Pos);
            end if;
         end if;
         delay 1.0;
         Gnoga.Log
           ("Distance:" &
            Gnoga.Gui.Plugin.Pixi.Sprite.Distance (App.SP1, App.SP2)'Img);
      end loop;

      delay 10.0;
      Gnoga.Gui.Plugin.Pixi.Sprite.Delete_All (C);
      R.Auto_Rendering (C, False);

      Connection.Hold;
   end On_Connect;

begin
   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access,
      Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end Pixi_Sprite_Test;
