with Gnoga.Application.Multiuser;
with Gnoga.Window;
with Gnoga.Base;
with Gnoga.Element;
with Gnoga.Element.Common;
with Gnoga.Element.Canvas;
with Gnoga.Types;

procedure Demo is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Hello_World : Element_Type;
         Click_Quit  : Common.DIV_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Hello_World.Color ("green");
      App.Hello_World.Background_Color (RGBA_Type'(255,255,255,1.0));
      App.Main_Window.Log
        ("Color = " & Gnoga.Types.To_String (App.Hello_World.Color));
   end On_Click;


   procedure End_App (Object : in out Gnoga.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      Log ("Ending Application");
      Application.Multiuser.End_Application;
   end End_App;

   procedure On_Connect
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      App : aliased App_Data;
      Hr1 : Gnoga.Element.Common.HR_Type;
      Hr2 : Gnoga.Element.Common.HR_Type;
      Lnk : Gnoga.Element.Common.A_Type;
      Img : Gnoga.Element.Common.IMG_Type;
      Clr : Gnoga.Types.RGBA_Type;
   begin
      App.Main_Window := Main_Window'Unchecked_Access;

      Main_Window.Log (Main_Window.Location.Path_Name);

      Clr := Gnoga.Types.To_RGBA
        (App.Main_Window.Document.Body_Element.Style ("background-color"));

      App.Main_Window.Log
        ("Background Color was " & Gnoga.Types.To_String (Clr));

      App.Main_Window.Document.Body_Element.Style
        ("background-color",
         Gnoga.Types.To_String (RGBA_Type'(255,192,203,0.500)));

      Clr := Gnoga.Types.To_RGBA
        (App.Main_Window.Document.Body_Element.Style ("background-color"));
      App.Main_Window.Log
        ("Background Color now is " & Gnoga.Types.To_String (Clr));
      App.Main_Window.Log
        ("Value sent = " &
           Gnoga.Types.To_String (RGBA_Type'(255,192,203,0.500)));


      App.Hello_World.Create_From_HTML (Main_Window, "<h1 />");
      App.Hello_World.Text ("Hello World!");
      App.Hello_World.Place_Inside_Top_Of
        (Main_Window.Document.Body_Element.all);
      App.Hello_World.On_Click_Handler (On_Click'Unrestricted_Access);

      Hr1.Create (Main_Window);
      Hr1.Place_After (App.Hello_World);

      App.Click_Quit.Create (Main_Window, "Click to Quit");
      App.Click_Quit.Opacity (0.5);

      App.Main_Window.Log ("Click opacity = " &
                                     App.Click_Quit.Opacity'Img);

      App.Click_Quit.Place_After (Hr1);
      App.Click_Quit.On_Click_Handler (End_App'Unrestricted_Access);

      Lnk.Create (Parent  => Main_Window,
                  Link    => "http://www.gnoga.com",
                  Content => "Gnoga Home Page",
                  Target  => "_blank");
      Lnk.Place_After (App.Click_Quit);

      Hr2.Create (Main_Window);
      Hr2.Place_After (Lnk);

      Img.Create
        (Parent           => Main_Window,
         URL_Source       => "http://www.gnu.org/graphics/gplv3-127x51.png",
         Alternative_Text => "GNAT Modified GNU GPL 3");
      Img.Place_After (Hr2);

      Application.Multiuser.Connection_Data (Main_Window, App'Unchecked_Access);

      Connection.Hold;
   end On_Connect;

   procedure On_Connect_2
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      D : Gnoga.Element.Common.DIV_Type;
   begin
      Main_Window.Document.Body_Element.Background_Color ("blue");

      D.Create (Main_Window, "This is on another path in same application.");
      D.Color ("yellow");
      D.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);
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
end Demo;
