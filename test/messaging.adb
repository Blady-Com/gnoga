with Gnoga.Connections;
with Gnoga.Types;
with Gnoga.Base;

procedure Messaging is
   procedure On_Connect (ID         : in     Gnoga.Types.Connection_ID;
                         Connection : access Gnoga.Connections.Connection_Holder_Type)
   is
      use Gnoga.Connections;

      procedure On_Message (Object  : in out Gnoga.Base.Base_Type'Class;
                            Event   : in     String;
                            Message : in     String)
      is
      begin
         Gnoga.Log ("In on_message Event:" & Event & " Message: " & Message);
         Gnoga.Log ("Screen Height = " &
                      Gnoga.Connections.Execute_Script
                      (Object.Connection_ID, "screen.height"));
      end On_Message;

      T : Gnoga.Base.Base_Type;
      A : Gnoga.Base.Base_Type;
      B : Gnoga.Base.Base_Type;
   begin
      Gnoga.Log ("Connection established on " & ID'img);

      T.Create_Root (Connection_ID => ID,
                     ID            => "t",
                     HTML          => "<h3 id='t'>Hello world 2!</h3>");

      A.Create_After (Target => T,
                      ID     => "a",
                      HTML   => "<hr id='a' />");

      B.Create_Before (Target => T,
                       ID     => "b",
                       HTML   => "<h1 id='b'>Before</h1>");

      T.Bind_Event (Event   => "click",
                    Message => "Hello World!");
      T.On_Message_Handler (Handler => On_Message'Unrestricted_Access);

      for i in 1 .. 10 loop
         declare
            p : Gnoga.Base.Base_Type;
         begin
            P.Create_After (Target => A,
                            ID     => "",
                            HTML   => "<span>" & i'img & "</span>");
         end;
      end loop;

      Connection.Hold;
   exception
      when Gnoga.Connections.Connection_Error =>
         Gnoga.Log ("Connection Error on " & ID'img);
   end On_Connect;
begin
   Gnoga.Connections.Initialize;

   Gnoga.Connections.On_Connect_Handler
     (Event => On_Connect'Unrestricted_Access);

   Gnoga.Connections.Run;
end Messaging;
