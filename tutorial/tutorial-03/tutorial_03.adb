--  In the previous tutorials we created Singletons applications where there
--  is no issue of concurrency outside of events from the same user. This
--  allowed for programming in the familiar sequential patterns of regular
--  console applications. In this tutorial we will create an application that
--  will allow for multiple connections on the same machine, network or across
--  the globe on the internet.
--
--  In order to help see the different approach to setting up the application
--  we will do something similar to the previous tutorial.

with Ada.Exceptions;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;

with Gnoga.Types;
--  Gnoga special types are found in Gnoga.Types.

procedure Tutorial_03 is

   --  Since this application will be used by multiple connections, we need to
   --  track and access that connection's specific data. To do this we create
   --  a derivative of Gnoga.Types.Connection_Data_Type that will be accessible
   --  to any object on a connection.

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      My_Window : Gnoga.Gui.Window.Pointer_To_Window_Class;
      My_View   : Gnoga.Gui.View.View_Type;
      My_Button : Gnoga.Gui.Element.Common.Button_Type;
      My_Exit   : Gnoga.Gui.Element.Common.Button_Type;
   end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Events for our application. The implementation is almost identical
   --  to our previous tutorial. However we access the elements through our
   --  App_Data data structure associated with the connection instead of
   --  globally.

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_View.New_Line;
      App.My_View.Put_Line ("I've been clicked!");
   end On_Click;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App  : constant App_Access := App_Access (Object.Connection_Data);
      View : Gnoga.Gui.View.View_Type;
   begin
      App.My_View.Remove;
      View.Create (App.My_Window.all);
      View.Put_Line ("Application exited.");
      App.My_Window.Close_Connection;
   exception
      when E : others =>
         Gnoga.Log (Message => "On_Exit: " & Ada.Exceptions.Exception_Information (E));
   end On_Exit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Instead of creating and setting up our GUI in the main body of the
   --  application, we now set up the GUI in a connection event handler.
   --  The implementation is almost identical to the last tutorial except we
   --  place our GUI element variables within the App_Data data structure
   --  that will be associated with the connection.

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      App.My_Window := Main_Window'Unchecked_Access;
      Main_Window.Connection_Data (App);
      --  This associates our application data to this connection. Now any
      --  object created on it has access to it using its Connection_Data
      --  property. When the connection is done it will deallocate the memory
      --  used.

      App.My_View.Create (Main_Window);

      App.My_Button.Create (App.My_View, "Click Me!");
      App.My_Button.On_Click_Handler (On_Click'Unrestricted_Access);

      App.My_Exit.Create (App.My_View, "Exit App");
      App.My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);

      App.My_View.Horizontal_Rule;
   end On_Connect;

begin
   Gnoga.Application.Title ("Tutorial 03");

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");
   --  With a Multi_Connect application it is possible to have different
   --  URL paths start different Connection Event Handlers. This allows
   --  for the creation of Web Apps that appear as larger web sites or as
   --  multiple applications to the user. Setting Path to "default" means
   --  that any unmatched URL path will start that event handler. A URL path
   --  is the path following the host and port. For example:
   --  http://localhost:8080/test/me
   --  The Path would be "/test/me". In Gnoga the path can even appear to be
   --  a file name "/test/me.html". However if you have a file of the same
   --  name in the html directory it will be served instead.

   Gnoga.Application.Multi_Connect.Message_Loop;
end Tutorial_03;
