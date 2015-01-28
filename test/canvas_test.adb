with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D;
with Gnoga.Types;

procedure Canvas_Test is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;
   use Gnoga.Gui.Element.Canvas;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         My_Canvas   : Canvas.Canvas_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
      C   : Context_2D.Context_2D_Type;
      Img : Gnoga.Gui.Element.Common.IMG_Type;
   begin
      C.Get_Drawing_Context_2D (App.My_Canvas);
      C.Rotate_Degrees (15.0);
      C.Fill_Color ("brown");
      C.Fill_Rectangle ((10,
                         10, 80, 80));
      Img.Create
        (Parent           => App.Main_Window.all,
         URL_Source       => "http://www.gnu.org/graphics/gplv3-127x51.png",
         Alternative_Text => "GNAT Modified GNU GPL 3");

      C.Draw_Image (Img, 100, 10);
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App     : App_Access := new App_Data;
      C       : Context_2D.Context_2D_Type;
      G       : Context_2D.Gradient_Type;
      P       : Context_2D.Pattern_Type;
      I       : Common.IMG_Type;

      Img_Dat : Context_2D.Image_Data_Type;

      Button1 : Common.Button_Type;
   begin
      Main_Window.Connection_Data (App);

      App.Main_Window := Main_Window'Unchecked_Access;

      Button1.Create (Parent  => Main_Window.Document.Body_Element.all,
                      Content => "Click Me");
      Button1.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);
      Button1.On_Click_Handler (On_Click'Unrestricted_Access);

      App.My_Canvas.Create (Main_Window, 400, 400);
      App.My_Canvas.Border;
      App.My_Canvas.Place_Inside_Bottom_Of
        (App.Main_Window.Document.Body_Element.all);

      C.Get_Drawing_Context_2D (App.My_Canvas);

      C.Stroke_Color ("yellow");
      C.Rectangle ((5, 5, 150, 100));
      C.Line_Width (8);
      C.Stroke;
      C.Fill_Color (RGBA_Type'(0, 255, 255, 1.0));
      C.Fill;
      C.Fill_Color (RGBA_Type'(0, 0, 255, 0.75));

      C.Fill_Rectangle ((20, 20, 150, 100));
      C.Fill_Color (RGBA_Type'(0, 0, 255, 0.50));
      C.Fill_Rectangle ((30, 30, 150, 100));

      G.Create_Linear_Gradient (Context => C,
                                X_1     => 0,
                                Y_1     => 0,
                                X_2     => 200,
                                Y_2     => 200);
      G.Add_Color_Stop (Position => 0.0,
                        Color    => RGBA_Type'(255, 255, 255, 1.0));
      G.Add_Color_Stop (Position => 1.0, Color => "purple");
      G.Add_Color_Stop (Position => 1.0, Color => "pink");
      C.Fill_Gradient (G);
      C.Fill_Rectangle ((40, 40, 150, 100));

      G.Create_Radial_Gradient (Context => C,
                                X_1     => 50,
                                Y_1     => 50,
                                R_1     => 0,
                                X_2     => 150,
                                Y_2     => 100,
                                R_2     => 360);
      G.Add_Color_Stop (Position => 0.0, Color => "orange");
      G.Add_Color_Stop (Position => 0.0, Color => "yellow");
      G.Add_Color_Stop (Position => 1.0, Color => "green");
      C.Fill_Gradient (G);

      C.Save;
      C.Shadow_Color ("black");
      C.Shadow_Blur (20);
      C.Fill_Rectangle ((50, 50, 150, 100));
      C.Restore;

      I.Create (App.My_Canvas,
                "/img/pattern.png");
      P.Create_Pattern (Context        => C,
                        Image          => I,
                        Repeat_Pattern => Context_2D.Repeat);
      C.Fill_Pattern (P);
      C.Fill_Rectangle ((80, 80, 150, 100));

      C.Fill_Color ("purple");
      C.Font (Height => "40px");
      C.Text_Alignment (Right);
      C.Fill_Text ("Hello World!", 200, 200);

      C.Get_Image_Data (Image_Data => Img_Dat,
                        Left       => 25,
                        Top        => 25,
                        Width      => 10,
                        Height     => 10);

      C.Put_Image_Data (Image_Data => Img_Dat,
                        Left       => 100,
                        Top        => 100);

      Gnoga.Log ("Dimensions of Image Data " & Img_Dat.Width'Img & " x" &
                   Img_Dat.Height'Img);
      Gnoga.Log ("Data : " & Img_Dat.Data);

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
end Canvas_Test;
