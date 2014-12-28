with Ada.Exceptions;

with Gnoga.Application.Multi_Connect;

with GnogaBoard.Controller;

procedure GnogaBoard.Main is
begin
   Gnoga.Application.Title ("GnogaBoard");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");
   
   Gnoga.Application.Multi_Connect.Initialize;
   
   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end GnogaBoard.Main;
