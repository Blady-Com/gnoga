with Gnoga.Application.Singleton;
with Gnoga.Types;
with Gnoga.Base;

procedure Singleton is
   T : Gnoga.Base.Base_Type;
   A : Gnoga.Base.Base_Type;
   B : Gnoga.Base.Base_Type;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class)
   is
   begin
      Gnoga.Log ("Color = " & Object.Style ("color"));
      Gnoga.Log ("Height = " & Object.Style ("height"));
      Gnoga.Log ("Width = " & Object.Style ("width"));
      Object.Style ("color", "green");
      Object.Style ("background", "black");
      Object.Style ("height", "200px");
      Object.Width (300);
      Gnoga.Log ("Color = " & Object.Style ("color"));
      Gnoga.Log ("Height = " & Object.Height'Img);
      Gnoga.Log ("Width = " & Object.Width'Img);
   end On_Click;

   procedure End_App (Object : in out Gnoga.Base.Base_Type'Class) is
   begin
      T.Style ("visibility", "hidden");
      A.Style ("visibility", "hidden");
      B.Style ("visibility", "hidden");

      Gnoga.Log ("Ending application.");
      Gnoga.Application.Singleton.End_Application;
   end End_App;
begin
   Gnoga.Application.Singleton.Initialize;

   Gnoga.Log ("Connection established.");

   T.Create_Root (Connection_ID => Gnoga.Application.Singleton.Connection_ID,
                  ID            => "t",
                  HTML          => "<h3 id='t'>Hello world 2!</h3>");

   A.Create_After (Target => T,
                   ID     => "a",
                   HTML   => "<hr id='a' />");

   B.Create_Before (Target => T,
                    ID     => "b",
                    HTML   => "<h1 id='b'>Click when Done</h1>");

   T.On_Click_Handler (On_Click'Unrestricted_Access);
   B.On_Click_Handler (End_App'Unrestricted_Access);

   Gnoga.Application.Singleton.Message_Loop;

   Gnoga.Log ("Done.");
end Singleton;
