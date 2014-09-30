
--  Gnoga is Ada Inside, which means that it was designed to make use of all
--  that Ada has to offer including the ability to play along well with the
--  concurrency features offered by Ada.
--
--  In this tutorial we will start and stop an Ada task that will interact
--  with out Gnoga Gui.


with Gnoga.Types;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;


procedure Tutorial_04 is

   --  The Color_Me_Task_Type is passed in a GUI element for its discriminant.
   --  We give the task start and stop entry points in order to insure we
   --  have setup the Element it will be accessing first and to insure we
   --  stop the task before Element is finalized on close of the connection.

   task type Color_Me_Task_Type
     (O : Gnoga.Gui.Element.Pointer_To_Element_Class)
   is
      entry Start;
      entry Stop;
   end Color_Me_Task_Type;

   task body Color_Me_Task_Type is
      type Colors is (red, green, blue, orange, black);

      Current_Color : Colors := Colors'First;
   begin
      accept Start;

      loop
         begin
            if O.Valid then
               O.Color (Current_Color'Img);

               if Current_Color = Colors'Last then
                  Current_Color := Colors'First;
               else
                  Current_Color := Colors'Succ (Current_Color);
               end if;
            end if;
         end;
         select
            accept Stop;
            exit;
         or
            delay 0.1;
         end select;
      end loop;
   end Color_Me_Task_Type;

   type App_Data is new Gnoga.Types.Connection_Data_Type with
      record
         My_View   : Gnoga.Gui.View.View_Type;
         My_Button : Gnoga.Gui.Element.Common.Button_Type;
         My_Exit   : Gnoga.Gui.Element.Common.Button_Type;
         Flasher   : aliased Gnoga.Gui.Element.Common.DIV_Type;
         --  In Gnoga (as in HTML) the two main elements used for display are
         --  the DIV_Type and the Span_Type. The difference between the two is
         --  the a DIV_Type is a "block" element and a Span_Type is an inline
         --  element. In simple terms DIV_Types will take up an entire row
         --  and Span_Types will be placed on the same line as the last inline
         --  element. Think of Spans_Types as ways to mark text for
         --  interaction with the user.
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Events for our application.

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_View.Put ("I've been clicked! ");
      --  Put places its text in to a SPAN tag, Put_Line uses a DIV tag.
      --  This creats the familiar console like affect of Put and Put_Line
   end On_Click;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_View.New_Line;
      App.My_View.Put_Line ("Closing application and every connection!");

      App.My_Button.Disabled;
      App.My_Exit.Disabled;

      Gnoga.Application.Multi_Connect.End_Application;
   end On_Exit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup GUI for each connection.

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App          : App_Access := new App_Data;
      Flasher_Task : Color_Me_Task_Type (App.Flasher'Unchecked_Access);
      --  Here we pass in to the Color_Me_Task_Type the element that it will
      --  be coloring once started.
   begin
      Main_Window.Connection_Data (App.all);

      App.My_View.Create (Main_Window);

      App.Flasher.Create (App.My_View, "<H1>I love Gnoga!</H1>");

      App.My_Button.Create (App.My_View, "Click Me!");
      App.My_Button.On_Click_Handler (On_Click'Unrestricted_Access);

      App.My_View.Put ("<button id='my_exit'>End App</button>");
      App.My_Exit.Attach_Using_Parent (Parent  => App.My_View,
                                       ID      => "my_exit");
      --  It is possible to attach Gnoga elements that have already been
      --  created in HTML. (A later tutorial will show how to use popups
      --  and iFrames to load any HTML file and attach to controls in it.
      --  It will also show how to use custom "boot" files that already
      --  contain HTML or HTML files that include the Gnoga connection
      --  script.) For HTML tags not bound in Gnoga such as inline text
      --  styles, you can use Gnoga.Gui.Element.Element_Type to attach
      --  them and control them like bound controls, including adding
      --  event handlers!

      App.My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);
      --  Once a control from HTML is attached to a Gnoga object it can be
      --  used like any other.

      App.My_View.Put_Line ("<hr />");

      Flasher_Task.Start;

      Connection.Hold;
      --  Since we want to "clean up" (in this case stop the Flaster_Task)
      --  once the connection has been closed. We manually "hold" the
      --  connection until it is "released" from with in the Gnoga framework
      --  meaning the connection has been lost to the browser.

      Flasher_Task.Stop;
   end On_Connect;

begin
   Gnoga.Application.Title ("Tutorial 04");

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => On_Connect'Unrestricted_Access,
      Path  => "default");

   Gnoga.Application.Multi_Connect.Message_Loop;
end Tutorial_04;
