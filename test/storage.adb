with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Types;
with Gnoga.Client.Storage;
with Gnoga.Server.Connection;

with Ada.Calendar;
with Ada.Calendar.Conversions;

procedure Storage is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;
   use all type Gnoga.String;

   type App_Data is new Connection_Data_Type with record
      Main_Window : Window.Pointer_To_Window_Class;
      Message     : Common.DIV_Type;
      View        : Gnoga.Gui.View.View_Type;
   end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Storage
     (Object        : in out Gnoga.Gui.Base.Base_Type'Class;
      Storage_Event : in     Gnoga.Gui.Window.Storage_Event_Record);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Message.Display ("none");
      Gnoga.Log ("Visible = " & Image (App.Message.Visible));
      Gnoga.Log ("Hidden = " & Image (App.Message.Hidden));
   end On_Click;

   procedure On_Storage
     (Object        : in out Gnoga.Gui.Base.Base_Type'Class;
      Storage_Event : in     Gnoga.Gui.Window.Storage_Event_Record)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Gnoga.Log ("On Storage");
      App.Main_Window.Alert
        (Storage_Event.Name & ": old = " & Storage_Event.Old_Value & ", now = " & Storage_Event.New_Value);
   end On_Storage;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App     : constant access App_Data                  := new App_Data;
      Local   : Gnoga.Client.Storage.Local_Storage_Type   := Gnoga.Client.Storage.Local_Storage (Main_Window);
      Session : Gnoga.Client.Storage.Session_Storage_Type := Gnoga.Client.Storage.Session_Storage (Main_Window);
   begin
      Main_Window.Connection_Data (App);

      App.Main_Window := Main_Window'Unchecked_Access;

      App.View.Create (Main_Window);
      App.View.Background_Color ("azure");
      App.View.Border;

      Main_Window.On_Storage_Handler (On_Storage'Unrestricted_Access);

      App.Message.Create (App.View, "Click me and I will hide.");
      App.Message.On_Click_Handler (On_Click'Unrestricted_Access);

      App.View.Put_Line ("Last access was at " & Local.Get ("Last_View"));
      Local.Set ("Last_View", Image (Natural (Ada.Calendar.Conversions.To_Unix_Time (Ada.Calendar.Clock))));

      if Session.Get ("ID") = "null" then
         Session.Set ("ID", Main_Window.Gnoga_Session_ID);
         App.View.Put_Line ("New session assigned.");
      end if;

      App.View.Put_Line ("Session id is " & Session.Get ("ID"));

      Gnoga.Log ("Hidden = " & Image (App.Message.Hidden));
      Gnoga.Log ("Visible = " & Image (App.Message.Visible));
      Gnoga.Log ("Session len = " & Image (Session.Length));
      Gnoga.Log ("Local len = " & Image (Local.Length));
      Gnoga.Log ("Session 1 = " & Session.Key (1) & " : " & Session.Get (Session.Key (1)));
      Gnoga.Log ("Local 1 = " & Local.Key (1) & " : " & Local.Get (Local.Key (1)));
      Gnoga.Log ("Number of active connections = " & Image (Gnoga.Server.Connection.Active_Connections));
      Gnoga.Log ("END");
   end On_Connect;

begin
   Application.Multi_Connect.Initialize (Event => On_Connect'Unrestricted_Access, Boot => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end Storage;
