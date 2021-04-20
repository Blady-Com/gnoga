with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Canvas.Context_2D;

procedure Essai8 is
   use Gnoga;
   use Gnoga.Gui.View.Grid;
   use all type Gnoga.String;

   Main_Window : Gnoga.Gui.Window.Window_Type;

   Layout_View : Gnoga.Gui.View.Grid.Grid_View_Type;
   Text_View   : Gnoga.Gui.View.Console.Console_View_Type;
   Canvas_View : Gnoga.Gui.View.View_Type;
   My_Canvas   : Gnoga.Gui.Element.Canvas.Canvas_Type;
   Quit_Button : Gnoga.Gui.Element.Common.Button_Type;
   Cr          : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Application.Singleton.End_Application;
   end On_Quit;

   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      pragma Unreferenced (Object);
   begin
      Text_View.Put_Line (Image (Mouse_Event.X) & ',' & Image (Mouse_Event.Y));
   end Mouse_Move;

begin
   Gnoga.Application.Title ("Mouse position");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

--     Gnoga.Application.Open_URL ("http://127.0.0.1:8080");
   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080);

   Layout_View.Create (Parent => Main_Window,
                       Layout => ((1 => COL), (1 => COL), (1 => COL)));

   Quit_Button.Create (Layout_View.Panel (1, 1).all, "Quit");
   Quit_Button.On_Click_Handler (On_Quit'Unrestricted_Access);

   Canvas_View.Create (Layout_View.Panel (2, 1).all);
   Canvas_View.Overflow (Gnoga.Gui.Element.Auto);
   Canvas_View.Fill_Parent;
   My_Canvas.Create (Canvas_View, 640, 480);
   My_Canvas.Border;

   Text_View.Create (Layout_View.Panel (3, 1).all);
   Text_View.Fill_Parent;

   Text_View.Put_Line ("Mouse coordinates X, Y:");
   Cr.Get_Drawing_Context_2D (My_Canvas);
   Cr.Font (Height => "40px");
   for Ch in Character'('0') .. '9' loop
      Cr.Fill_Text (From_ASCII (Ch), 100, 30 + (Character'Pos (Ch) - 48) * 30);
   end loop;

   My_Canvas.On_Mouse_Move_Handler (Mouse_Move'Unrestricted_Access);

   Gnoga.Application.Singleton.Message_Loop;

exception
   when E : others =>
      Gnoga.Log (E);
end Essai8;
