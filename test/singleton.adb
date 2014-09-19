with Gnoga.Application.Singleton;
with Gnoga.Types;
with Gnoga.Base;
with Gnoga.Element;
with Gnoga.Element.Common;
with Gnoga.Screen;
with Gnoga.Navigator;
with Gnoga.Connections;
with Gnoga.Window;

procedure Singleton is
   task type Color_Me_Task (O : Gnoga.Element.Pointer_To_Element_Class) is
      entry start;
      entry stop;
   end Color_Me_Task;
   -- Strobe color Element O

   M : Gnoga.Window.Window_Type;
   T : aliased Gnoga.Element.Common.DIV_Type;
   A : Gnoga.Element.Common.HR_Type;
   B : Gnoga.Element.Common.DIV_Type;
   C : Color_Me_Task (T'Unchecked_Access);

   task body Color_Me_Task is
      type Colors is (red, green, blue, orange, black);

      Current_Color : Colors := Colors'First;
   begin
      accept start;

      loop
         begin
            if O.Valid then
               O.Style ("color", Current_Color'Img);

               if Current_Color = Colors'Last then
                  Current_Color := Colors'First;
               else
                  Current_Color := Colors'Succ (Current_Color);
               end if;
            end if;
         end;
         select
            accept stop;
            exit;
         or
            delay 0.1;
         end select;
      end loop;
   end Color_Me_Task;

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
      C.Stop;

      Gnoga.Log ("Ending application.");
      Gnoga.Application.Singleton.End_Application;
   end End_App;
begin
   Gnoga.Application.Title ("Test App for Gnoga");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Gnoga.Application.Singleton.Initialize (Main_Window => M);

   Gnoga.Log ("Connection established.");
   Gnoga.Log ("You can add to the path of app, e.g. http://url:8080/abc");
   Gnoga.Log ("if a file exists in server /html/ will be served if not");
   Gnoga.Log ("the app boot file will be served.");
   Gnoga.Log ("path on url : " & M.Location.Path_Name);
   Gnoga.Log ("page_id can be set using http://url:8080?page_id=xxx");
   Gnoga.Log ("page_id = " &
                Gnoga.Connections.Search_Parameter
                (M.Connection_ID, "page_id"));

   Gnoga.Log ("User Agent = " &
                Gnoga.Navigator.User_Agent
                (M.Connection_ID));

   T.Create (M, "<h1>Hello World!</h1>");
   T.Place_Inside_Top_Of (M.Document.Body_Element.all);

   A.Create (M);
   A.Place_After (T);

   B.Create (M, "<h3>Click when Done</h3>");
   B.Place_After (A);

   T.On_Click_Handler (On_Click'Unrestricted_Access);
   B.On_Click_Handler (End_App'Unrestricted_Access);

   C.Start;

   Gnoga.Application.Singleton.Message_Loop;

   Gnoga.Log ("Done.");
end Singleton;
