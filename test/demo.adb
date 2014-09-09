with Gnoga.Application.Singleton;
with Gnoga.Types;
with Gnoga.Base;
with Gnoga.Element;
with Gnoga.Screen;
with Gnoga.Navigator;
with Gnoga.Connections;

procedure Demo is
   T : Gnoga.Element.Element_Type;
   A : Gnoga.Element.Element_Type;
   B : Gnoga.Element.Element_Type;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class)
   is
      use Gnoga.Element;
   begin
      Gnoga.Log ("Visible = " & Element_Type (Object).Visible'Img);
      Gnoga.Log ("Screen.Available_Height = " &
                   Gnoga.Screen.Available_Height (Element_Type (Object).Connection_ID)'Img);
      Gnoga.Log ("Screen.Height = " &
                   Gnoga.Screen.Height (Element_Type (Object).Connection_ID)'Img);
      Gnoga.Log ("Color = " & Element_Type (Object).Style ("color"));
      Gnoga.Log ("Height = " & Element_Type (Object).Style ("height"));
      Gnoga.Log ("Width = " & Element_Type (Object).Style ("width"));
      Element_Type (Object).Style ("color", "green");
      Element_Type (Object).Style ("background", "black");
      Element_Type (Object).Style ("height", "200px");
      Element_Type (Object).Width (300);
      Gnoga.Log ("Color = " & Element_Type (Object).Style ("color"));
      Gnoga.Log ("Height = " & Element_Type (Object).Height'Img);
      Gnoga.Log ("Width = " & Element_Type (Object).Width'Img);
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
   Gnoga.Application.Singleton.Initialize;

   Gnoga.Log ("Connection established.");

   Gnoga.Log ("page_id = " &
                Gnoga.Connections.Search_Parameter
                (Gnoga.Application.Singleton.Connection_ID, "page_id"));

   Gnoga.Log ("User Agent = " &
                Gnoga.Navigator.User_Agent
                (Gnoga.Application.Singleton.Connection_ID));

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
end Demo;
