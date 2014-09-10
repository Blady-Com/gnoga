with Gnoga.Application.Singleton;
with Gnoga.Types;
with Gnoga.Base;
with Gnoga.Element;
with Gnoga.Screen;
with Gnoga.Connections;
with Gnoga.Window;

procedure Demo is
   Main_Window : Gnoga.Window.Window_Type;

   T : Gnoga.Element.Element_Type;
   A : Gnoga.Element.Element_Type;
   B : Gnoga.Element.Element_Type;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class)
   is
   begin
      Main_Window.Alert ("Clicked.");
   end On_Click;

   procedure End_App (Object : in out Gnoga.Base.Base_Type'Class) is
   begin
      T.Visible (False);
      A.Visible (False);
      B.Visible (False);

      Gnoga.Log ("Ending application.");
      Gnoga.Application.Singleton.End_Application;
   end End_App;
begin
   Gnoga.Application.Singleton.Initialize (Main_Window, Boot => "debug.html");

   Gnoga.Log ("Connection established.");

   Main_Window.On_Click_Handler (On_Click'Unrestricted_Access);

   Main_Window.Move_To (10,10);

   Gnoga.Log (Integer'Image (Main_Window.Property("test")));

   Gnoga.Log ("Window width = " & Main_Window.Width'Img);

   T.Create_Inside_At_Top (Parent => Main_Window.Document.Body_Element.all,
                           ID     => "t",
                           HTML   => "<h3 id='t'>Hello world 2!</h3>");

   A.Create_After (Target => T,
                   ID     => "a",
                   HTML   => "<hr id='a' />");

   B.Create_Before (Target => T,
                    ID     => "b",
                    HTML   => "<h1 id='b'>Click when Done</h1>");

   B.On_Click_Handler (End_App'Unrestricted_Access);

   Gnoga.Application.Singleton.Message_Loop;

   Gnoga.Log ("Done.");
end Demo;
