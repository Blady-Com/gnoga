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
      Gnoga.Log ("On Destroy");
      Gnoga.Application.Singleton.End_Application;
   end On_Destroy_Event;

begin
   Gnoga.Application.Singleton.Initialize (Main_Window, Verbose => True);
   Gnoga.Log_To_File (MacGap.Temp_Path (Main_Window) & "log.txt");

   Main_View.Create (Main_Window);
   Main_View.Text_Alignment (Center);

   Main_View.Put_Line ("Hello World in a Mac App!");
   Main_View.Put_Line ("Documents path = " &
                         MacGap.Documents_Path (Main_Window));
   Main_View.Put_Line ("Temp path = " &
                         MacGap.Temp_Path (Main_Window)  & "log.txt");

   MacGap.Open_URL (Main_Window, "http://www.google.com");

   MacGap.System_Beep (Main_Window);

   MacGap.Bounce_Dock_Icon (Main_Window);

   MacGap.Notify_User (Window  => Main_Window,
                       Title   => "Test message",
                       Message => "This is cool!",
                       Sound   => True);

   MacGap.Activate_Application (Main_Window);

   MacGap.Display_Sheet (Window  => Main_Window,
                         Title   => "Test sheet",
                         Message => "This is cool!",
                         Sound   => True);

   Gnoga.Application.Singleton.Message_Loop;
   Gnoga.Log ("Done");
end Mac_Test;
