with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Types;

with Gnoga.Gui.Plugin.Boot_Strap;

procedure Boot_Strap_Demo is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;
   use Gnoga.Gui.Plugin;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Console     : View.Console.Console_View_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Console.Put_Line ("Clicked!");
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;

      Boot_Strap.Load_Boot_Strap (Main_Window);

      App.Console.Create (Main_Window);


   end On_Connect;

begin
   Application.Multi_Connect.Initialize (Event => On_Connect'Unrestricted_Access,
                                     Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Open_URL_OSX;

   Application.Multi_Connect.Message_Loop;
end Boot_Strap_Demo;
