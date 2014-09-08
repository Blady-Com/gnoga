with Gnoga.Connections;
with Gnoga.Types;

procedure Tasks is
   procedure On_Connect (ID         : in     Gnoga.Types.Connection_ID;
                         Connection : access Gnoga.Connections.Connection_Holder_Type)
   is
      use Gnoga.Connections;

      task Web_Counter is
         entry Start;
         entry Stop;
      end Web_Counter;

      task body Web_Counter is
         I : Integer := 0;
      begin
         accept Start;
         Gnoga.Log ("Start: Web_Counter Task");
         loop
            begin
               Gnoga.Log ("Start: Dispatching to socket ID " & ID'Img);

               I := I + 1;

               Gnoga.Connections.Execute_Script
                 (ID,
                  "$('#socket_message').html(""Message # " & I'Img & """);");

               Gnoga.Log ("End: Dispatching to socket ID " & ID'Img);
            exception
               when Gnoga.Connections.Connection_Error =>
                  Gnoga.Log ("End: Web_Counter Task");
                  exit;
            end;

            select
               accept Stop;
               exit;
            else
               delay 0.5;
            end select;
         end loop;
      end Web_Counter;
   begin
      Gnoga.Log ("Connection established on " & ID'img);
      Web_Counter.Start;
   end On_Connect;
begin
   Gnoga.Connections.Initialize (Boot => "tasks.html");

   Gnoga.Connections.On_Connect_Handler
     (Event => On_Connect'Unrestricted_Access);

   Gnoga.Connections.Run;
end Tasks;
