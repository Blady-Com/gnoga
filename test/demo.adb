with Gnoga.Application;
with Gnoga.Types;
with Gnoga.Base;
with Gnoga.Screen;
with Gnoga.Navigator;
with Gnoga.Connections;

procedure Demo is
   T : Gnoga.Base.Base_Type;
   A : Gnoga.Base.Base_Type;
   B : Gnoga.Base.Base_Type;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class)
   is
   begin
      Gnoga.Log ("Visible = " & Object.Visible'Img);
      Gnoga.Log ("Screen.Available_Height = " &
                   Gnoga.Screen.Available_Height (Object.Connection_ID)'Img);
      Gnoga.Log ("Screen.Height = " &
                   Gnoga.Screen.Height (Object.Connection_ID)'Img);
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
      T.Visible (False);
      A.Visible (False);
      B.Visible (False);

      Gnoga.Log ("Ending application.");
      Gnoga.Application.End_Application;
   end End_App;
begin
   Gnoga.Application.Initialize;

   Gnoga.Log ("Connection established.");

   Gnoga.Log ("?page_id = " &
                Gnoga.Connections.Search_Parameter
                (Gnoga.Application.Connection_ID, "page_id"));

   Gnoga.Log ("User Agent = " &
                Gnoga.Navigator.User_Agent (Gnoga.Application.Connection_ID));

   T.Create_Root (Connection_ID => Gnoga.Application.Connection_ID,
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

   Gnoga.Application.Message_Loop;

   Gnoga.Log ("Done.");
end Demo;
