--  Since Gnoga controls the browser, it can tell it to do anything JavaScript
--  or HTML can. Including popping up additional windows or to use iFrames to
--  access other content from other sites or the same site. As long as there
--  is a connection back to the server Gnoga applications can control content
--  from regular HTML files in popups and iFrames if serverd out by the Gnoga
--  application. The limitation that you can not modify HTML from another
--  server is a security restriction imposed by the browsers, not Gnoga.

with Gnoga.Types;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Document;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.IFrame;

procedure Tutorial_06 is

   type App_Data is new Gnoga.Types.Connection_Data_Type with
      record
         My_View   : Gnoga.Gui.View.View_Type;
         My_Button : Gnoga.Gui.Element.Common.Button_Type;
         My_Exit   : Gnoga.Gui.Element.Common.Button_Type;
         My_Popup1 : Gnoga.Gui.Window.Window_Type;
         My_Popup2 : Gnoga.Gui.Window.Window_Type;
         My_PView  : Gnoga.Gui.View.View_Type;
         My_Frame  : Gnoga.Gui.Element.IFrame.IFrame_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Application event handlers

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_Popup1.Close;
      --  This will not close the popup since it is a cross-origin frame
      --  i.e. it has content from another server/domain.

      App.My_Popup2.Close;
      --  This will work since the popup is from this server/domain.
   end On_Click;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_View.New_Line;
      App.My_View.Put_Line ("Closing application and every connection!");

      App.My_Button.Disabled;
      App.My_Exit.Disabled;

      Gnoga.Application.Multi_Connect.End_Application;
   end On_Exit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup GUI for each connection.

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      use type Gnoga.Gui.Document.Ready_State_Type;

      App : constant App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);

      App.My_View.Create (Main_Window);

      App.My_Button.Create (App.My_View, "Close Popups");

      App.My_Exit.Create (App.My_View, "Exit Application");
      App.My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);

      App.My_View.Horizontal_Rule;

      App.My_Frame.Create (Parent   => App.My_View,
                           URL      => "http://www.gnoga.com",
                           Seamless => False);
      App.My_Frame.Width (App.My_View.Width);
      App.My_Frame.Height (500);
      App.My_Frame.Border;
      --  IFrame_Types are like any other element and can be styled, etc.

      App.My_Popup1.Launch (Parent   => Main_Window,
                            URL      => "http://google.com",
                            Width    => 800,
                            Height   => 500,
                            Left     => 10,
                            Top      => 10,
                            Location => True,
                            Menu     => True,
                            Status   => True,
                            Tool_Bar => True,
                            Title    => True);
      --  Most new browsers do not let you control the decorations,
      --  but we will set them anyways.

      App.My_Popup2.Launch (Parent   => Main_Window,
                            URL      => "/no_boot.html",
                            Width    => 500,
                            Height   => 500,
                            Left     => 50,
                            Top      => 50,
                            Location => False,
                            Menu     => False,
                            Status   => False,
                            Tool_Bar => False,
                            Title    => False);
      --  In order to avoid popup blocking issues on some browsers we need
      --  to make sure we pass in a URL on the same host.

      App.My_Button.On_Click_Handler (On_Click'Unrestricted_Access);
      --  We wait to add the On_Click_Handler to My_Button since if
      --  Popups are blocked My_Popup and My_Popup2 will have not been
      --  created and attempting to click them will raise an
      --  Object_Was_Not_Created exception when tying to close them.

      while
        App.My_Popup2.Document.Ready_State /= Gnoga.Gui.Document.Complete or
        App.My_Popup2.Width = 0
      loop
         delay 0.25;
      end loop;
      --  We need to make sure that popup2's contents is load in order to
      --  be interact with its contents and that My_Popup2 has been created
      --  by the browser. Although we could use My_Popup2 before the browser
      --  displayed the window as long as the Ready_State is Complete,
      --  My_PView will not be sized automatically to fit in to My_Popup2 since
      --  there is no knowldege yet have the width of the new window delivered
      --  by the browser.

      App.My_Popup2.Document.Title ("My Popup");
      App.My_PView.Create (App.My_Popup2);
      App.My_PView.Padding ("1em", "1em", "1em", "1em");
      App.My_PView.Put_Line
        ("Since this popup is not a cross-site frame " &
           "we have full control over it " &
           "even though this popup was blank and has no " &
           "websocket connection to our application. " &
           "We are piggy backing off the connection in the " &
           "main window.");
      App.My_PView.New_Line;
      App.My_PView.Put_Line
        ("Click the close popups buttons in the main window.");
   exception
      when Gnoga.Gui.Window.Popup_Blocked =>
         Gnoga.Log ("Popups are being blocked on the browser");
   end On_Connect;

begin
   Gnoga.Application.Title ("Tutorial 06");

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => On_Connect'Unrestricted_Access,
      Path  => "default");

   Gnoga.Application.Multi_Connect.Message_Loop;
end Tutorial_06;
