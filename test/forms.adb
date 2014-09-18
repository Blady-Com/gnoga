with Gnoga.Application.Multiuser;
with Gnoga.Window;
with Gnoga.Base;
with Gnoga.Element;
with Gnoga.Element.Form;
with Gnoga.Element.Common;
with Gnoga.Element.Canvas;
with Gnoga.Types;

procedure Forms is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         My_Form     : Form.Form_Type;
         Input       : Form.Form_Element_Type;
      end record;
   type App_Access is access all App_Data;

   type App_Data2 is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
      end record;
   type App_Access2 is access all App_Data2;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
      R   : Common.DIV_Type;
   begin
      App.Main_Window.Document.Body_Element.Background_Color ("Orange");

      R.Create (App.Main_Window.Document.Body_Element.all,
                "Value of Some_Text is " & App.Input.Value);
      R.Place_Inside_Bottom_Of (App.Main_Window.Document.Body_Element.all);
   end On_Click;

   procedure On_Submit (Object : in out Gnoga.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      Log ("On submit.");
      App.Main_Window.alert ("On Submit");
      if App.Input.Value /= "no" then
         App.My_Form.Submit;
      end if;
   end On_Submit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      App     : aliased App_Data;
      Button1 : Form.Form_Element_Type;
      Button2 : Form.Form_Element_Type;
   begin
      App.Main_Window := Main_Window'Unchecked_Access;

      App.My_Form.Create (Main_Window);
      App.My_Form.Action ("/demo");
      App.My_Form.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);
      App.My_Form.On_Submit_Handler (On_Submit'Unrestricted_Access);

      App.Input.Create (Form       => App.My_Form,
                        Input_Type => "text",
                        Name       => "Some_Text");
      App.Input.Place_After (App.My_Form);

      Button1.Create (Form       => App.My_Form,
                      Input_Type => "button",
                      Value      => "onclick button");
      Button1.Place_After (App.Input);
      Button1.On_Click_Handler (On_Click'Unrestricted_Access);

      Button2.Create (Form       => App.My_Form,
                      Input_Type => "submit",
                      Value      => "send to demo");
      Button2.Place_After (Button1);

      Application.Multiuser.Connection_Data (Main_Window, App'Unchecked_Access);

      Connection.Hold;
   end On_Connect;

   procedure On_Connect_2
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      App : aliased App_Data2;
      R   : Common.Div_Type;
   begin
      R.Create (Main_Window, "Results => " & Main_Window.Location.Search &
                  "<br />" &
                  "Some_Text => " & Main_Window.Search_Parameter ("Some_Text"));
      R.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);

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
end Forms;
