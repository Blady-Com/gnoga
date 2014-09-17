with Gnoga.Application.Multiuser;
with Gnoga.Window;
with Gnoga.Base;
with Gnoga.Element;
with Gnoga.Element.Common;
with Gnoga.Types;
with Gnoga.Connections;

procedure Popups is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Hello_World : Common.Button_Type;
         Click_Quit  : Common.Button_Type;
         Chld_Window : Window.Window_Type;
         New_Div     : Common.Div_Type;
         New_Div2    : Common.Div_Type;
      end record;
   type App_Access is access all App_Data;

   type App_Data2 is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         New_Div     : Common.DIV_Type;
      end record;
   type App_Access2 is access all App_Data2;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      Connections.Execute_Script (Object.Connection_ID,
                                 "newWin = window.open (""/demo"");");
      Connections.Execute_Script (Object.Connection_ID,
                                  "newWin2 = window.open (""/no_boot.html"");");
      App.Chld_Window.Launch (App.Main_Window.all, "/demo");
   end On_Click;


   procedure On_Click3 (Object : in out Gnoga.Base.Base_Type'Class)
   is
   begin
      Log ("Click 3 worked");
   end On_Click3;


   procedure On_Click2 (Object : in out Gnoga.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
      W   : Window.Window_Type;
      W2  : Window.Window_Type;
   begin
      W.Attach (App.Main_Window.all, "newWin");
      W.Document.Body_Element.Background_Color ("Red");

      declare
         App2 : App_Access2 := App_Access2 (W.Connection_Data);
         --  Note this would fail for W2 and App.Chld_Window since their
         --  connections are still on App.Main_Window's Connection.
      begin
         App2.New_Div.Create (W, "<H1>Where am I?</H1>");
         App2.New_Div.Place_Inside_Top_Of (W.Document.Body_Element.all);
         App2.New_Div.On_Click_Handler (On_Click3'Unrestricted_Access);
      end;

      W2.Attach (Connection_ID => Object.Connection_ID,
                 ID            => "newWin2");
      W2.Document.Body_Element.Background_Color ("Green");

      App.New_Div.Create (W2, "<H1>Where am I - parent connection_id?</H1>");
      App.New_Div.Place_Inside_Top_Of (W2.Document.Body_Element.all);
      App.New_Div.On_Click_Handler (On_Click3'Unrestricted_Access);

      App.New_Div2.Create (App.Chld_Window, "<H1>In Launched Window</H1>");
      App.New_Div2.Place_Inside_Top_Of
        (App.Chld_Window.Document.Body_Element.all);
      App.Chld_Window.On_Click_Handler (On_Click3'Unrestricted_Access);
   end On_Click2;

   procedure On_Connect
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      App : aliased App_Data;
      Hr1 : Gnoga.Element.Common.HR_Type;
      B   : Common.Button_Type;
   begin
      App.Main_Window := Main_Window'Unchecked_Access;

      App.Hello_World.Create (Main_Window, "Launch Window");
      App.Hello_World.Place_Inside_Top_Of
        (Main_Window.Document.Body_Element.all);
      App.Hello_World.On_Click_Handler (On_Click'Unrestricted_Access);

      Hr1.Create (Main_Window);
      Hr1.Place_After (App.Hello_World);

      App.Click_Quit.Create (Main_Window, "Attach Window");
      App.Click_Quit.Place_After (Hr1);
      App.Click_Quit.On_Click_Handler (On_Click2'Unrestricted_Access);

      B.Create (Main_Window, "click3");
      B.Place_Inside_Bottom_Of (Main_Window.Document.Body_Element.all);
      B.On_Click_Handler (On_Click3'Unrestricted_Access);

      Application.Multiuser.Connection_Data (Main_Window, App'Unchecked_Access);

      Connection.Hold;
   end On_Connect;

   procedure On_Connect_2
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      App : aliased App_Data2;
      D : Gnoga.Element.Common.DIV_Type;
      B : Common.Button_Type;
   begin
      Main_Window.Document.Body_Element.Background_Color ("blue");

      D.Create (Main_Window, "This is on another path in same application.");
      D.Color ("yellow");
      D.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);

      B.Create (Main_Window, "click3");
      B.Place_Inside_Bottom_Of (Main_Window.Document.Body_Element.all);
      B.On_Click_Handler (On_Click3'Unrestricted_Access);

      Application.Multiuser.Connection_Data (Main_Window, App'Unchecked_Access);

      Connection.Hold;
   end On_Connect_2;

begin
   Application.Multiuser.Initialize (Boot  => "debug.html");

   Application.Multiuser.On_Connect_Handler (On_Connect'Unrestricted_Access,
                                             "default");
   Application.Multiuser.On_Connect_Handler (On_Connect_2'Unrestricted_Access,
                                             "/demo");

   Application.Application_Name ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multiuser.Message_Loop;
end Popups;
