with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Plugin.Pixi;
with Gnoga.Gui.Plugin.Pixi.Graphics;
with Gnoga.Gui.Plugin.Pixi.Sprite;
with Gnoga.Types;

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
      App : constant App_Access := new App_Data;
      R   : Plugin.Pixi.Renderer_Type;
      C   : Plugin.Pixi.Container_Type;
      G   : Plugin.Pixi.Graphics.Graphics_2D_Type;

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

      R.Get_Drawing_Context_2D (App.My_Canvas);
      C.Create (R);
      G.Create (C);

--        C.Begin_Path;
      G.Stroke_Color ("yellow");
      G.Line_Width (8);
      G.Rectangle ((100, 70, 90, 40));
      R.Render (C);

--        C.Font (Height => "40px");
      G.Stroke_Text (R, "Hello World!", 100, 100);

      App.SP1.Create (C, "img/E4a.png", 10, 10, 8.0, 16.0);
      R.Auto_Rendering (C, True);

      while not App.SP1.Coincidence (110, 210, 20) loop
         delay 0.1;
      end loop;
      App.SP1.Locate (150, 5);
      delay 5.0;
      G.Stroke_Color ("green");
--        C.Begin_Path;
      G.Move_To (100, 100);
      G.Line_To (App.SP1.Column, App.SP1.Row);
      R.Render (C);
      Gnoga.Log ("Distance:" & App.SP1.Distance (100, 100)'Img);
      delay 5.0;
      G.Stroke_Color ("green");
--        C.Begin_Path;
      G.Move_To (100, 100);
      G.Line_To (App.SP1.Column, App.SP1.Row);
      R.Render (C);
      Gnoga.Log ("Distance:" & App.SP1.Distance (100, 100)'Img);
      delay 5.0;
      App.SP1.Delete (C);

      App.SP1.Create (C, "img/E11a.png", 250, 10);
      App.SP2.Create (C, "img/E17a.png", 250, 100);
      App.SP1.Motion (0.0, 5.0);
      App.SP2.Motion (0.0, 5.0);

      loop
         if not Gnoga.Gui.Plugin.Pixi.Sprite.Coincidence
             (App.SP1,
              App.SP2,
              45) and
           not Coinc
         then
            App.SP1.Position (Y_Pos, X_Pos);
            if X_Pos > 50 then
               App.SP1.Motion (0.0, App.SP1.Column_Velocity + 1.0);
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
         exit when X_Pos > 300;
      end loop;

      App.SP1.Motion (0.0, 10.0);
      delay 3.0;
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
