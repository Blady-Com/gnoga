with Ada.Exceptions;

with Gnoga.Application.Multi_Connect;

with @@data.App_Name@@.Controller;

procedure @@data.App_Name@@.Main is
begin
   Gnoga.Application.Title ("@@data.App_Name@@");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");
   
   Gnoga.Application.Multi_Connect.Initialize;
   
   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end @@data.App_Name@@.Main;
