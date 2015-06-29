with Ada.Exceptions;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Plugin.Message_Boxes;
with Gnoga.Gui.Plugin.jQueryUI;

procedure Message_Boxes is
   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.Console.Console_View_Type;
   Result      : Gnoga.Gui.Plugin.Message_Boxes.Message_Box_Result;
begin
   Gnoga.Application.Title ("test01");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Gnoga.Application.Open_URL ("http://127.0.0.1:8080");
   Gnoga.Application.Singleton.Initialize (Main_Window,
                                           Port => 8080,
                                           Boot => "boot_jqueryui.html");

   Main_View.Create (Main_Window);
   Main_View.Put_Line ("Hello World1!");

   Gnoga.Gui.Plugin.Message_Boxes.Message_Box (Main_Window,
                                               "titre1", "texte1");

   Main_View.Put_Line ("Hello World2!");

   Result := Gnoga.Gui.Plugin.Message_Boxes.Message_Box
     (Main_Window,
      "titre2",
      "texte2",
      Gnoga.Gui.Plugin.Message_Boxes.Yes_No_Def_Cancel_Box);
   Main_View.Put_Line (Result'Img);

   Main_View.Put_Line ("Hello World3!");

   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log
        (Ada.Exceptions.Exception_Name (E) &
         " - " &
         Ada.Exceptions.Exception_Message (E));
end Message_Boxes;
