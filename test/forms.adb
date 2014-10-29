with Ada.Strings.Fixed;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.View.Console;
with Gnoga.Types;
with Gnoga.Server.Connection;

procedure Forms is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Console     : View.Console.Console_View_Type;
         My_Form     : Form.Form_Type;
         Input       : Form.Form_Element_Type;
         Pick        : Form.Color_Picker_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Window.Document.Body_Element.Background_Color ("Orange");

      App.Console.Put_Line ("Value of Some_Text is " & App.Input.Value);
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

      App.Console.Create (Main_Window);

      App.My_Form.Create (App.Console);
      App.My_Form.Method (Form.Post);
      App.My_Form.Action ("/demo");
      App.My_Form.On_Submit_Handler (On_Submit'Unrestricted_Access);

      App.Input.Create_Element (Form       => App.My_Form,
                                Input_Type => "text",
                                Name       => "Some_Text");

      Label.Create (Form       => App.My_Form,
                    Label_For  => App.Input,
                    Contents   => "Type Text:");

      Button1.Create (Form       => App.My_Form,
                      Value      => "onclick button");
      Button1.On_Click_Handler (On_Click'Unrestricted_Access);

      Button2.Create (Form       => App.My_Form,
                      Value      => "send to demo");
      App.Pick.Create (Form       => App.My_Form,
                       Name       => "My_Color");
      App.Pick.On_Change_Handler (On_Change'Unrestricted_Access);

      App.Console.New_Line;

      declare
         Form2   : Form.Form_Type;
         F_Input : Form.Form_Element_Type;
         S       : Form.Submit_Button_Type;
      begin
         Form2.Create (Parent => App.Console,
                       Action => "/demo",
                       Method => Form.POST);
         Form2.Encoding (Form.Multi_Part);
         Form2.Put ("File to upload: ");
         F_Input.Create_Element (Form       => Form2,
                                 Input_Type => "file",
                                 Name       => "fspec");
         S.Create (Form2, "Submit File");
      end;
      Connection.Hold;
      --  If any handlers are set on local variables they can't finalize
      --  before connection end.
   end On_Connect;

   procedure On_Connect_2
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      Console : View.Console.Console_View_Type;
   begin
      Console.Create (Main_Window);

      Console.Put_Line ("Get Results => " & Main_Window.Location.Search);
      Console.Put_Line ("Some_Text   => " &
                          Main_Window.Form_Parameter ("Some_Text"));

      Console.Put_Line ("File Name on Server  => " &
                          Main_Window.Form_Parameter ("File_Name_Server"));
      Console.Put_Line ("File Name => " &
                          Main_Window.Form_Parameter ("File_Name"));
   end On_Connect_2;

   procedure On_Post (URI        : String;
                      Parameters : in out Gnoga.Types.Data_Map_Type)
   is
   begin
      if Parameters.Contains ("fspec") then
         declare
            F : String  := Parameters.Element ("fspec");
            S : Natural := Ada.Strings.Fixed.Index (F, "|");
         begin
            Parameters.Insert ("File_Name_Server", F (F'First .. S - 1));
            Parameters.Insert ("File_Name", F (S + 1 .. F'Last));
         end;
      end if;
   end On_Post;
begin
   Application.Multi_Connect.Initialize (Boot  => "debug.html");

   Application.Multi_Connect.On_Connect_Handler (On_Connect'Unrestricted_Access,
                                             "default");
   Application.Multi_Connect.On_Connect_Handler (On_Connect_2'Unrestricted_Access,
                                             "/demo");

   Gnoga.Server.Connection.On_Post_Handler (On_Post'Unrestricted_Access);

   Application.Open_URL_OSX;

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end Forms;
