with Snake;
with Gnoga.Application.Multi_Connect;

procedure Snake.Main is
   use Gnoga;
   use Gnoga.Application.Multi_Connect;
begin
   Application.Title (Snake.Title);

   Application.HTML_On_Close ("Application disconnected.");

   Initialize;

   Message_Loop;
end Snake.Main;
