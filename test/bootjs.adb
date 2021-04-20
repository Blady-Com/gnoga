with Gnoga.Application.Multi_Connect;
with Gnoga.Types;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;

with Gnoga.Server;
with Gnoga.Client.Bind_Page;

procedure BootJS is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;

   type App_Data is new Connection_Data_Type with record
      Main_Window : Window.Pointer_To_Window_Class;
      Page        : aliased Gnoga.Gui.View.View_Type;
   end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   procedure On_BootJS_Demo_Page
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Window.Alert ("You clicked on me!");
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : constant App_Access := new App_Data;

      View : Gnoga.Gui.View.View_Type;

      My_Button : Gnoga.Gui.Element.Common.Button_Type;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;

      View.Create (Main_Window);
      View.Put_Line ("<a href='/bootjs_demo.html'>go to bootjs_demo.html</a>");
      View.Horizontal_Rule;
      View.Put_Line ("Test of French - Bienvenue.");
      View.Horizontal_Rule;
      View.Put_Line ("HTML Injection from bootjs_demo.html:");
      View.New_Line;
      View.Load_HTML (Gnoga.Server.HTML_Directory & "bootjs_demo.html");

      My_Button.Attach_Using_Parent (View, "my_button");

      My_Button.On_Click_Handler (On_Click'Unrestricted_Access);

      Connection.Hold;
   end On_Connect;

   procedure On_BootJS_Demo_Page
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App       : constant App_Access := new App_Data;
      Name_List : Gnoga.Types.Data_Array_Type;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;

      App.Main_Window.Disable_Auto_Set_View;
      App.Page.Create (Main_Window);

      Main_Window.Alert (Main_Window.Document.Title);

      Gnoga.Client.Bind_Page.Bind_Page (App.Page);

      Name_List := App.Page.Element_Names;
      for N of Name_List loop
         App.Page.Element (N).Color ("orange");
      end loop;

      App.Page.Element ("my_button").On_Click_Handler (On_Click'Unrestricted_Access);
   end On_BootJS_Demo_Page;

begin
   Application.Multi_Connect.Initialize;

   Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");

   Application.Multi_Connect.On_Connect_Handler
     (Event => On_BootJS_Demo_Page'Unrestricted_Access, Path => "bootjs_demo.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close ("<b>Connection to Application has been terminated</b>");

   Application.Open_URL;

   Application.Multi_Connect.Message_Loop;
end BootJS;
