with Snake;
with Gnoga.Application.Multiuser;

with Snake.Connection;

procedure Snake.Main is
   use Gnoga;
   use Gnoga.Application.Multiuser;
begin
   Application.Title (Snake.Title);

   Application.HTML_On_Close ("Application disconnected.");

   Initialize (Boot => "debug.html");

   On_Connect_Handler
     (Event => Snake.Connection.On_Connect_Default'Unrestricted_Access,
      Path  => "default");

   Message_Loop;
end Snake.Main;
