with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D;
with Gnoga.Gui.Element.Canvas.Context_2D.Sprite;
with Gnoga.Types;

procedure Sprite_Test is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;
   use Gnoga.Gui.Element.Canvas;
   use Gnoga.Gui.Element.Canvas.Context_2D.Sprite;

   type App_Data is new Connection_Data_Type with record
      Main_Window : Window.Pointer_To_Window_Class;
      My_Canvas   : Canvas.Canvas_Type;
      SP1         : Context_2D.Sprite.Sprite_Type;
   end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Motion (App.SP1, Row_Velocity (App.SP1), -Column_Velocity (App.SP1));
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
      C   : aliased Context_2D.Context_2D_Type;

      Img_Dat : Context_2D.Image_Data_Type;

      Button1 : Common.Button_Type;
   begin
      Main_Window.Connection_Data (App);

      App.Main_Window := Main_Window'Unchecked_Access;

      Button1.Create
      (Parent => Main_Window.Document.Body_Element.all, Content => "Click Me");
      Button1.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);
      Button1.On_Click_Handler (On_Click'Unrestricted_Access);

      App.My_Canvas.Create (Main_Window, 400, 400);
      App.My_Canvas.Border;
      App.My_Canvas.Place_Inside_Bottom_Of
      (App.Main_Window.Document.Body_Element.all);

      C.Get_Drawing_Context_2D (App.My_Canvas);

      C.Stroke_Color ("yellow");
      C.Rectangle ((100, 70, 90, 40));
      C.Line_Width (8);
      C.Stroke;

      C.Fill_Color ("purple");
      C.Font (Height => "40px");
      C.Fill_Text ("Hello World!", 100, 100);

      C.Get_Image_Data
      (Image_Data      =>
         Img_Dat, Left =>
         100, Top      =>
         70, Width     =>
         90, Height    =>
         40);

      Trigger (600);
      Create (App.SP1, C, Img_Dat, 10, 10, 5, 10);
      while not Coincidence (App.SP1, 110, 210, 20) loop
         delay 0.1;
      end loop;
      Locate (App.SP1, 150, 5);
      delay 5.0;
      Gnoga.Log ("Distance:" & Distance (App.SP1, 100, 100)'Img);
      delay 5.0;
      Gnoga.Log ("Distance:" & Distance (App.SP1, 100, 100)'Img);
      Delete (App.SP1);

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
end Sprite_Test;
