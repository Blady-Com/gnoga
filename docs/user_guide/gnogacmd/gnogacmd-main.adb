with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;

with GnogaCMD.Controller;

procedure GnogaCMD.Main is
   Main_Window : Gnoga.Gui.Window.Window_Type;
begin
   Gnoga.Application.Title ("GnogaCMD");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Gnoga.Application.Open_URL("http://127.0.0.1:8080");   
   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080);   
   
   GnogaCMD.Controller.Default (Main_Window);
   
   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end GnogaCMD.Main;
