
--  In the previous tutorial we created a simple application that just
--  wrote in console fashion to the browser window. In this example
--  we introduce the idea of event handlers and how to interact live
--  with the GUI you create in a view.

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;

with Gnoga.Gui.Base;
--  The base type for all GUI elements is in Gnoga.Gui.Base. Most of the
--  event handlers and the prototypes used for handlers are found there
--  also.

with Gnoga.Gui.Element.Common;
--  Most interactive non-form specific GUI elements are found in Common.

procedure Tutorial_02 is
   My_Window : Gnoga.Gui.Window.Window_Type;

   My_View   : Gnoga.Gui.View.View_Type;

   My_Button : Gnoga.Gui.Element.Common.Button_Type;

   My_Exit   : Gnoga.Gui.Element.Common.Button_Type;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  The procedure matches the prototype for Action_Events
   --  This procedure can therefore be assigned to any event handler
   --  of that nature. It can even be assigned to multiple handlers at
   --  the same time. See Gnoga.Base in the event section of the spec
   --  for more details about available events.

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      My_View.New_Line;
      My_View.Put_Line ("I've been clicked!");
   end On_Click;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  When this action is fired it will end the application

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      My_View.New_Line;
      My_View.Put_Line ("Closing application");

      My_Button.Disabled;
      My_Exit.Disabled;

      Gnoga.Application.Singleton.End_Application;
   end On_Exit;
begin
   Gnoga.Application.Title ("Tutorial 02");

   Gnoga.Application.HTML_On_Close ("Application ended.");
   --  We can tell Gnoga that when the connection is severed to the browser
   --  that the browser should display this.

   Gnoga.Application.Singleton.Initialize (Main_Window => My_Window);

   My_View.Create (My_Window);

   My_Button.Create (My_View, "Click Me!");
   --  Since My_Button is created as a child of My_View a View_Type,
   --  View_Type is designed to automatically add newly created children
   --  in to the browser screen.

   My_Button.On_Click_Handler (On_Click'Unrestricted_Access);
   --  We assign the procedure On_Click to be called as a handler for
   --  On_Click events for My_Button. It would be preferable that
   --  On_Click be declared at library level so that Unrestricted_Access
   --  is not needed.

   My_Exit.Create (My_View, "End App");
   My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);

   My_View.Horizontal_Rule;

   Gnoga.Application.Singleton.Message_Loop;
   --  In the previous example we terminated the application when we were
   --  done writing to the browser. Since we wish to continue to interact
   --  with the browser, we tell Gnoga to go in to a message loop until
   --  End_Application is called.
end Tutorial_02;
