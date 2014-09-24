with Gnoga.Application.Multiuser;
with Gnoga.Window;
with Gnoga.View;
with Gnoga.Base;
with Gnoga.Element;
with Gnoga.Element.Common;
with Gnoga.Types;

procedure Demo is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Message     : Common.DIV_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Message.Style ("display", "none");
      Gnoga.Log ("Visible = " & App.Message.Visible'Img);
      Gnoga.Log ("Hidden = " & App.Message.Hidden'Img);
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      App     : aliased App_Data;
      View    : Gnoga.View.View_Type;
   begin
      App.Main_Window := Main_Window'Unchecked_Access;

      View.Create (Main_Window);
      View.Background_Color ("azure");

      App.Message.Create (View, "Click me and I will hide.");
      App.Message.On_Click_Handler (On_Click'Unrestricted_Access);

      Gnoga.Log ("Hidden = " & App.Message.Hidden'Img);
      Gnoga.Log ("Visible = " & App.Message.Visible'Img);

      Application.Multiuser.Connection_Data (Main_Window, App'Unchecked_Access);

      Connection.Hold;
   end On_Connect;

begin
   Application.Multiuser.Initialize (Event => On_Connect'Unrestricted_Access,
                                     Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multiuser.Message_Loop;
end Demo;
