with Snake;
with Gnoga.Application.Multi_Connect;

with Snake.Connection;

procedure Snake.Main is
   use Gnoga;
   use Gnoga.Application.Multi_Connect;
begin
   Application.Title (Snake.Title);

   Application.HTML_On_Close ("Application disconnected.");

   Initialize (Boot => "debug.html");

   On_Connect_Handler
     (Event => Snake.Connection.On_Connect_Default'Unrestricted_Access,
      Path  => "default");

   Message_Loop;
end Snake.Main;
