--  The HTML5 Canvas offers a great way to create graphics programmatically.
--  SVG is another means offered by Gnoga. The key difference is that SVG
--  are scalable vector graphics, and the Canvas are raster images. Think
--  Canvas - Photoshop and SVG - Illustrator.

with Gnoga.Types;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D;

procedure Tutorial_05 is

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      My_View   : Gnoga.Gui.View.View_Type;
      My_Button : Gnoga.Gui.Element.Common.Button_Type;
      My_Exit   : Gnoga.Gui.Element.Common.Button_Type;
      My_Canvas : Gnoga.Gui.Element.Canvas.Canvas_Type;
         --  Canvas_Types are the container, but drawing is done with in a
         --  "Context" of the Canvas_Type. Currently only the 2D Context
         --  is standardized, but many browsers support WebGL as a 3D context.
   end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure Mouse_Down
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);
   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);
   procedure Mouse_Up
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);

   procedure Mouse_Down
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      App     : constant App_Access := App_Access (Object.Connection_Data);
      Context : Context_2D_Type;
   begin
      Context.Get_Drawing_Context_2D (App.My_Canvas);
      --  Before we can draw we need to get the 2D Drawing context from the
      --  canvas.

      Context.Begin_Path;
      --  This clears any previous setting of points on a path and readies the
      --  context to receive new path points.

      Context.Stroke_Color ("black");
      --  Settings in the context remain between one call to
      --  Get_Drawing_Context_2D and the next.

      Context.Move_To (Mouse_Event.X, Mouse_Event.Y);
      --  We set the first point on the path with out drawing.

      App.My_Canvas.On_Mouse_Move_Handler (Mouse_Move'Unrestricted_Access);
      App.My_Canvas.On_Mouse_Up_Handler (Mouse_Up'Unrestricted_Access);
      --  As you can see it is possible to add or remove event handlers at any
      --  time.
   end Mouse_Down;

   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      App     : constant App_Access := App_Access (Object.Connection_Data);
      Context : Context_2D_Type;
   begin
      Context.Get_Drawing_Context_2D (App.My_Canvas);

      Context.Line_To (Mouse_Event.X, Mouse_Event.Y);
      --  This adds a line on the path from the last point to this one.

      Context.Stroke;
      --  Stroke the point using the stroke style set. In this case the Color
      --  Black form Mouse_Down
   end Mouse_Move;

   procedure Mouse_Up
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      App     : constant App_Access := App_Access (Object.Connection_Data);
      Context : Context_2D_Type;
   begin
      Context.Get_Drawing_Context_2D (App.My_Canvas);

      Context.Line_To (Mouse_Event.X, Mouse_Event.Y);
      Context.Stroke;
      --  We draw up to the last point and then remove the handlers that
      --  perform drawing.

      App.My_Canvas.On_Mouse_Move_Handler (null);
      App.My_Canvas.On_Mouse_Up_Handler (null);
   end Mouse_Up;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      App     : constant App_Access := App_Access (Object.Connection_Data);
      Context : Context_2D_Type;
   begin
      Context.Get_Drawing_Context_2D (App.My_Canvas);
      Context.Save;
      --  This will push the state of the context on to the "stack",
      --  this way changes we make now can be discarded later by restoring
      --  from the stack what the state of the context was before we made
      --  changes.

      Context.Fill_Color ("white");
      Context.Stroke_Color ("white");
      Context.Fill_Rectangle ((0, 0, App.My_Canvas.Width, App.My_Canvas.Height));

      Context.Restore;
      --  Restore the state of the context.
   end On_Click;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_View.New_Line;
      App.My_View.Put_Line ("Closing application and every connection!");

      App.My_Button.Disabled;
      App.My_Exit.Disabled;

      Gnoga.Application.Multi_Connect.End_Application;
   end On_Exit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup GUI for each connection.

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);

      App.My_View.Create (Main_Window);

      App.My_Button.Create (App.My_View, "Clear Canvas");
      App.My_Button.On_Click_Handler (On_Click'Unrestricted_Access);

      App.My_Exit.Create (App.My_View, "Exit Application");
      App.My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);

      App.My_View.Horizontal_Rule;

      App.My_Canvas.Create (Parent => App.My_View, Width => 600, Height => 400);
      App.My_Canvas.Border;
      --  Every element inherits the ability to be styled from Element_Type

      App.My_Canvas.On_Mouse_Down_Handler (Mouse_Down'Unrestricted_Access);
   end On_Connect;

begin
   Gnoga.Application.Title ("Tutorial 05");

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");

   Gnoga.Application.Multi_Connect.Message_Loop;
end Tutorial_05;
