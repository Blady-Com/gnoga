with Gnoga.Application.Multi_Connect;
with Gnoga.Types;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;

with Gnoga.Client.Bind_Page;
procedure BootJS is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Page        : aliased Gnoga.Gui.View.View_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      null;
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App       : App_Access := new App_Data;
      Name_List : Gnoga.Types.Data_Array_Type;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;
      App.Page.Create (Main_Window, Attach => False);

      Main_Window.Alert (Main_Window.Document.Title);

      Gnoga.Client.Bind_Page.Bind_Page (App.Page);

      Name_List := App.Page.Element_Names;
      for N of Name_List loop
         App.Page.Element (N).Color ("orange");
      end Loop;
   end On_Connect;

begin
   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access);

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Open_URL_OSX ("http://127.0.0.1:8080/bootjs_demo.html");

   Application.Multi_Connect.Message_Loop;
end BootJS;
