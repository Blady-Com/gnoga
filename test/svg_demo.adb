with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.SVG;
with Gnoga.Types;

procedure SVG_Demo is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Console     : View.Console.Console_View_Type;
         B           : Common.Button_Type;
         S           : aliased SVG.SVG_Type;
         Box         : Element_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Console.Put_Line ("Clicked!");
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;

      App.Console.Create (Main_Window);

      App.S.Create (App.Console);
      App.S.Width (400);
      App.S.Height (400);
      App.S.Border;

      App.Box.Create_XML_Element (App.S, SVG_Namespace, "rect");
      App.Box.Attribute ("x", "10");
      App.Box.Attribute ("y", "10");
      App.Box.Attribute ("height", "50");
      App.Box.Attribute ("width", "50");
      App.Box.Style ("fill", "green");
      App.Box.Style ("stroke-width", "3");
      App.Box.Style ("stroke", "red");
      App.Box.On_Click_Handler (On_Click'Unrestricted_Access);

      App.B.Create (App.Console, "Click Me and Green Square");
      App.B.On_Click_Handler (On_Click'Unrestricted_Access);
   end On_Connect;

begin
   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access,
      Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

--     Application.Open_URL;

   Application.Multi_Connect.Message_Loop;
end SVG_Demo;
