with Ada.Exceptions;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;

with @@data.App_Name@@.Controller;

procedure @@data.App_Name@@.Main is
   Main_Window : Gnoga.Gui.Window.Window_Type;
begin
   Gnoga.Application.Title ("@@data.App_Name@@");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Gnoga.Application.Open_URL ("http://127.0.0.1:8080");   
   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080);   
   
   @@data.App_Name@@.Controller.Default (Main_Window);
   
   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end @@data.App_Name@@.Main;
