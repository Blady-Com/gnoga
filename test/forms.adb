with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

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

   Last_Parameters : Gnoga.Types.Data_Map_Type;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Console     : View.Console.Console_View_Type;
         My_Form     : Form.Form_Type;
         Input       : Form.Form_Element_Type;
         Pick        : Form.Color_Picker_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Submit_Get (Object : in out Gnoga.Gui.Base.Base_Type'Class);

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
      App.Main_Window.Alert ("On Submit");
      if App.Input.Value /= "no" then
         App.My_Form.Submit;
      end if;
   end On_Submit;

   procedure On_Submit_Get (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_Form.Method (Form.Get);
      App.My_Form.Submit;
   end On_Submit_Get;

   procedure On_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Input.Value (String'(App.Pick.Value));
   end On_Change;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App     : App_Access := new App_Data;
      Button1 : Form.Input_Button_Type;
      Button2 : Form.Submit_Button_Type;
      Button3 : Form.Input_Button_Type;
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

      Label.Create (Form      => App.My_Form,
                    Label_For => App.Input,
                    Content   => "Type Text:");

      Button1.Create (Form       => App.My_Form,
                      Value      => "onclick button");
      Button1.On_Click_Handler (On_Click'Unrestricted_Access);

      Button2.Create (Form       => App.My_Form,
                      Value      => "send to demo via post");

      Button3.Create (Form       => App.My_Form,
                      Value      => "send to demo via get");
      Button3.On_Click_Handler (On_Submit_Get'Unrestricted_Access);

      App.Pick.Create (Form       => App.My_Form,
                       Name       => "My_Color");
      App.Pick.On_Change_Handler (On_Change'Unrestricted_Access);

      App.Console.New_Line;

      declare
         Form2   : Form.Form_Type;
         F_Input : Form.Form_Element_Type;
         H_Input : Form.Hidden_Type;
         S       : Form.Submit_Button_Type;
      begin
         Form2.Create (Parent => App.Console,
                       Action => "/demo",
                       Method => Form.Post);
         Form2.Encoding (Form.Multi_Part);
         Form2.Put ("File to upload: ");
         F_Input.Create_Element (Form       => Form2,
                                 Input_Type => "file",
                                 Name       => "fspec");
         H_Input.Create (Form  => Form2,
                         Value => "test",
                         Name  => "Some_Text");

         S.Create (Form2, "Submit File");
      end;
      Connection.Hold;
      --  If any handlers are set on local variables they can't finalize
      --  before connection end.
   end On_Connect;

   procedure On_Connect_2
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Connect_2
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      Console : View.Console.Console_View_Type;
   begin
      Console.Create (Main_Window);

      --  Full Get string
      if Main_Window.Location.Search /= "" then
         Console.Put_Line ("Get Results => " & Main_Window.Location.Search);
      end if;

      --  Check if Some_Text is in the Get form parameters
      if Main_Window.Form_Parameter ("Some_Text") /= "undefined" then
         Last_Parameters.Include ("Some_Text",
                                  Main_Window.Form_Parameter ("Some_Text"));
      end if;

      --  If wa post was stored in On_Post
      if Last_Parameters.Contains ("Some_Text") then
         Console.Put_Line ("Some_Text   => " &
                             Last_Parameters.Element ("Some_Text"));
      else
         Console.Put_Line ("Some_Text   => No value sent");
      end if;

      if Last_Parameters.Contains ("file_name") then
         Console.Put_Line ("File Name on Server  => " &
                             Last_Parameters.Element ("file_name"));
         Console.Put_Line ("File Name => " &
                             Last_Parameters.Element ("temp_name"));
      end if;

      Last_Parameters.Clear;
   end On_Connect_2;

   procedure On_Post_Request
     (URI                 : in String;
      Accepted_Parameters : out Ada.Strings.Unbounded.Unbounded_String);

   procedure On_Post_Request
     (URI                 : in String;
      Accepted_Parameters : out Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Accepted_Parameters :=
        Ada.Strings.Unbounded.To_Unbounded_String ("Some_Text,fspec");
   end On_Post_Request;

   procedure On_Post (URI        : String;
                      Parameters : in out Gnoga.Types.Data_Map_Type);

   procedure On_Post (URI        : String;
                      Parameters : in out Gnoga.Types.Data_Map_Type)
   is
   begin
      Last_Parameters := Parameters;
      for C in Parameters.Iterate loop
         Gnoga.Log (Gnoga.Types.Data_Maps.Key (C) & " = " &
                      Gnoga.Types.Data_Maps.Element (C));
      end loop;
   end On_Post;

   procedure On_Post_File (URI       : in String;
                           File_Name : in String;
                           Temp_Name : in String);

   procedure On_Post_File (URI       : in String;
                           File_Name : in String;
                           Temp_Name : in String)
   is
   begin
      Last_Parameters.Include ("file_name", File_Name);
      Last_Parameters.Include ("temp_name", Temp_Name);

      Gnoga.Log ("File received : " & File_Name & " in " & Temp_Name);
   end On_Post_File;
begin
   Application.Multi_Connect.Initialize (Boot  => "debug.html");

   Application.Multi_Connect.On_Connect_Handler
     (On_Connect'Unrestricted_Access, "default");
   Application.Multi_Connect.On_Connect_Handler
     (On_Connect_2'Unrestricted_Access,  "/demo");

   Gnoga.Server.Connection.On_Post_Handler (On_Post'Unrestricted_Access);
   Gnoga.Server.Connection.On_Post_Request_Handler
     (On_Post_Request'Unrestricted_Access);
   Gnoga.Server.Connection.On_Post_File_Handler
     (On_Post_File'Unrestricted_Access);

   Application.Open_URL;

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end Forms;
