with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Plugin.MacGap;

procedure Mac_Test is
   use Gnoga.Gui.Plugin;
   use Gnoga.Gui.Element;

   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.View_Type;
   Button      : Gnoga.Gui.Element.Common.Button_Type;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      pragma Unreferenced (Object);
   begin
      MacGap.System_Beep (Main_Window);
      MacGap.Open_URL (Main_Window, "http://www.google.com");
      MacGap.Toggle_Full_Screen (Main_Window);

--        if MacGap.Is_Maximized (Main_Window) then
--           MacGap.Restore (Main_Window);
--        else
--           MacGap.Maximize (Main_Window);
--        end if;
   end On_Click;

begin
   Gnoga.Application.Singleton.Initialize (Main_Window, Verbose => True);
   Gnoga.Log_To_File (MacGap.Temp_Path (Main_Window) & "log.txt");

   MacGap.Title (Main_Window, "Mac_Test");
   --  MacGap.Toggle_Full_Screen (Main_Window);
   --  MacGap.Move (Main_Window, 0, 0);
   --  MacGap.Resize (Main_Window, 400, 400);

   Main_View.Create (Main_Window);
   Main_View.Text_Alignment (Center);

   Button.Create (Main_View, "Click Me");
   Button.On_Click_Handler (On_Click'Unrestricted_Access);

   Main_View.Put_Line ("Hello World in a Mac App!");
   Main_View.Put_Line ("Documents path = " &
                         MacGap.Documents_Path (Main_Window));
   Main_View.Put_Line ("Temp path = " &
                         MacGap.Temp_Path (Main_Window)  & "log.txt");

--   MacGap.Open_URL (Main_Window, "http://www.google.com");

--   MacGap.System_Beep (Main_Window);

--   MacGap.Bounce_Dock_Icon (Main_Window);

--   MacGap.Notify_User (Window  => Main_Window,
--                         Title   => "Test message",
--                         Message => "This is cool!",
--                         Sound   => True);

--   MacGap.Activate_Application (Main_Window);

--     MacGap.Display_Sheet (Window  => Main_Window,
--                           Title   => "Test sheet",
--                           Message => "This is cool!",
--                           Sound   => True);

   Gnoga.Application.Singleton.Message_Loop;
   Gnoga.Log ("Done");
end Mac_Test;
