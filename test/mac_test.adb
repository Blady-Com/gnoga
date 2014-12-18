with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Plugin.MacGap;

procedure Mac_Test is
   use Gnoga.Gui.Plugin;
   use Gnoga.Gui.Element;

   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.View_Type;

   procedure On_Destroy_Event (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Close Mac application on window destroyed.

   procedure On_Destroy_Event (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
   begin
      Gnoga.Application.Singleton.End_Application;
   end On_Destroy_Event;

begin
   Gnoga.Application.Singleton.Initialize (Main_Window, Verbose => True);
   Main_Window.On_Destroy_Handler (On_Destroy_Event'Unrestricted_Access);

   Main_View.Create (Main_Window);
   Main_View.Text_Alignment (Center);

   Main_View.Put_Line ("Hello World in a Mac App!");

   Gnoga.Application.Singleton.Message_Loop;
end Mac_Test;
