with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Types;

procedure Forms is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         My_Form     : Form.Form_Type;
         Input       : Form.Form_Element_Type;
         Pick        : Form.Color_Picker_Type;
      end record;
   type App_Access is access all App_Data;

   type App_Data2 is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
      end record;
   type App_Access2 is access all App_Data2;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
      R   : Common.DIV_Type;
   begin
      App.Main_Window.Document.Body_Element.Background_Color ("Orange");

      R.Create (App.Main_Window.Document.Body_Element.all,
                "Value of Some_Text is " & App.Input.Value);
      R.Place_Inside_Bottom_Of (App.Main_Window.Document.Body_Element.all);
   end On_Click;

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      Log ("On submit.");
      App.Main_Window.alert ("On Submit");
      if App.Input.Value /= "no" then
         App.My_Form.Submit;
      end if;
   end On_Submit;

   procedure On_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Input.Value (String'(App.Pick.Value));
   end On_Change;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App     : App_Access := new App_Data;
      Button1 : Form.Input_Button_Type;
      Button2 : Form.Submit_Button_Type;
      Label   : Form.Label_Type;
   begin
      Main_Window.Connection_Data (App);

      App.Main_Window := Main_Window'Unchecked_Access;

      App.My_Form.Create (Main_Window);
      App.My_Form.Method (Form.Post);
      App.My_Form.Action ("/demo");
      App.My_Form.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);
      App.My_Form.On_Submit_Handler (On_Submit'Unrestricted_Access);

      App.Input.Create_Element (Form       => App.My_Form,
                                Input_Type => "text",
                                Name       => "Some_Text");
      App.Input.Place_After (App.My_Form);

      Label.Create (Form       => App.My_Form,
                    Label_For  => App.Input,
                    Contents   => "Type Text:");

      Button1.Create (Form       => App.My_Form,
                      Value      => "onclick button");
      Button1.Place_After (App.Input);
      Button1.On_Click_Handler (On_Click'Unrestricted_Access);

      Button2.Create (Form       => App.My_Form,
                      Value      => "send to demo");
      Button2.Place_After (Button1);

      App.Pick.Create (Form       => App.My_Form,
                       Name       => "My_Color");
      App.Pick.Place_After (Button2);
      App.Pick.On_Change_Handler (On_Change'Unrestricted_Access);

      Connection.Hold;
      --  If any handlers are set on local variables they can't finalize
      --  before connection end.
   end On_Connect;

   procedure On_Connect_2
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access2 := new App_Data2;
      R   : Common.Div_Type;
   begin
      Main_Window.Connection_Data (App);

      R.Create (Main_Window, "Results => " & Main_Window.Location.Search &
                  "<br />" &
                  "Some_Text => " & Main_Window.Form_Parameter ("Some_Text"));
      R.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);
   end On_Connect_2;

begin
   Application.Multi_Connect.Initialize (Boot  => "debug.html");

   Application.Multi_Connect.On_Connect_Handler (On_Connect'Unrestricted_Access,
                                             "default");
   Application.Multi_Connect.On_Connect_Handler (On_Connect_2'Unrestricted_Access,
                                             "/demo");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end Forms;
