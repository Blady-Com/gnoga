--  With Gnoga it is possible to build rich interfaces with advanced features
--  like Drag and Drop.

with Gnoga.Types;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;

procedure Tutorial_07 is

   type App_Data is new Gnoga.Types.Connection_Data_Type with
      record
         My_View   : Gnoga.Gui.View.View_Type;
         My_Exit   : Gnoga.Gui.Element.Common.Button_Type;
         Source    : Gnoga.Gui.Element.Common.DIV_Type;
         Target    : Gnoga.Gui.Element.Common.DIV_Type;
         --  Source will be our drag source and Target our Drop Source
      end record;
   type App_Access is access all App_Data;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Application event handlers

   procedure Start_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure End_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  These two event handlers are for the drag Source. Using them we can
   --  set some visual cues, in this case fading out Source during the drag.

   procedure Enter_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure Leave_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure Drop (Object    : in out Gnoga.Gui.Base.Base_Type'Class;
                   Drag_Text : in     String);
   --  These three event handlers are used to show visual cues that Target
   --  is open to accept the drop, and to actually accept the drop.

   procedure Start_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Source.Opacity (0.4);
   end Start_Drag;

   procedure End_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Source.Opacity (1.0);
   end End_Drag;

   procedure Enter_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Target.Border (Color => "Red");
   end Enter_Drag;

   procedure Leave_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Target.Border (Color => "Black");
   end Leave_Drag;

   procedure Drop (Object    : in out Gnoga.Gui.Base.Base_Type'Class;
                   Drag_Text : in     String)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Target.Border (Color => "Green");
      App.Target.Text (Drag_Text);
   end Drop;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_View.New_Line;
      App.My_View.Put_Line ("Closing application and every connection!");

      App.My_Exit.Disabled;

      Gnoga.Application.Multi_Connect.End_Application;
   end On_Exit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup GUI for each connection.

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);

      App.My_View.Create (Main_Window);

      App.My_Exit.Create (App.My_View, "Exit Application");
      App.My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);

      App.My_View.Horizontal_Rule;

      App.Source.Create (App.My_View, "Drag Me");

      App.Source.Position (Gnoga.Gui.Element.Fixed);
      App.Source.Top (50);
      App.Source.Left (20);
      --  Place our Source DIV_Type at a fixed position in browser

      App.Source.Width (100);
      App.Source.Height (100);

      App.Source.Border;

      App.Source.Draggable;
      --  Tell the browser this is an Element that can be dragged.

      App.Source.On_Drag_Start_Handler
        (Handler   => Start_Drag'Unrestricted_Access,
         Drag_Text => "Got the Drop!");
      --  Setup drag start handler, which makes the element actually draggable
      --  The Drag_Text will be what is delivered to target when dropped on it.

      App.Source.On_Drag_End_Handler (End_Drag'Unrestricted_Access);

      App.Target.Create (App.My_View, "Drop on Me");

      App.Target.Position (Gnoga.Gui.Element.Fixed);
      App.Target.Top (50);
      App.Target.Left (300);

      App.Target.Width (100);
      App.Target.Height (100);
      --  Place our Target DIV_Type at a fixed position in browser

      App.Target.Border;

      App.Target.On_Drop_Handler (Drop'Unrestricted_Access);
      App.Target.On_Drag_Enter_Handler (Enter_Drag'Unrestricted_Access);
      App.Target.On_Drag_Leave_Handler (Leave_Drag'Unrestricted_Access);
   end On_Connect;

begin
   Gnoga.Application.Title ("Tutorial 07");

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => On_Connect'Unrestricted_Access,
      Path  => "default");

   Gnoga.Application.Multi_Connect.Message_Loop;
end Tutorial_07;
