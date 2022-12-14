with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Plugin.Message_Boxes;

with UXStrings.Conversions;

procedure Message_Boxes is
   use Gnoga;
   use all type Gnoga.String;

   function Image is new UXStrings.Conversions.Scalar_Image (Gnoga.Gui.Plugin.Message_Boxes.Message_Box_Result);

   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.Console.Console_View_Type;
   Result      : Gnoga.Gui.Plugin.Message_Boxes.Message_Box_Result;
begin
   Gnoga.Application.Title ("test01");
   Gnoga.Application.HTML_On_Close ("<b>Connection to Application has been terminated</b>");

--     Gnoga.Application.Open_URL ("http://127.0.0.1:8080");
   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8_080, Boot => "boot_jqueryui.html");

   Main_View.Create (Main_Window);
   Main_View.Put_Line ("Hello World1!");

   Gnoga.Gui.Plugin.Message_Boxes.Message_Box (Main_Window, "titre1", "texte1");

   Main_View.Put_Line ("Hello World2!");

   Result :=
     Gnoga.Gui.Plugin.Message_Boxes.Message_Box
       (Main_Window, "titre2", "texte2", Gnoga.Gui.Plugin.Message_Boxes.Yes_No_Def_Cancel_Box);
   Main_View.Put_Line (Image (Result));

   Main_View.Put_Line ("Hello World3!");

   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end Message_Boxes;
