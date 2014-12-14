with Ada.Exceptions;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;

procedure @@data.App_Name@@ is
   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.Console.Console_View_Type;
begin
   Gnoga.Application.Title ("@@data.App_Name@@");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");
   
   -- Gnoga.Application.Open_URL_OSX ("http://127.0.0.1:8080");
   -- Gnoga.Application.Open_URL_Windows ("http://127.0.0.1:8080");
   -- Gnoga.Application.Open_URL_Linux ("http://127.0.0.1:8080");
   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080);

   Main_View.Create (Main_Window); 
   Main_View.Put_Line ("Hello World!");
   
   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end @@data.App_Name@@;
