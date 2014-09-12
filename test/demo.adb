with Gnoga.Application.Multiuser;
with Gnoga.Window;
with Gnoga.Base;
with Gnoga.Element;
with Gnoga.Types;

procedure Demo is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Hello_World : Element_Type;
         Click_Quit  : Element_Type;
         X           : Element_Type;
         Y           : Element_Type;
         Key         : Element_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class;
                       Event  : in     Gnoga.Base.Mouse_Event_Record)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Hello_World.Style ("color", "green");
      App.Main_Window.Alert ("X = " & Event.X'Img & " Y = " & Event.Y'Img);
   end On_Click;

   procedure On_Key_Press (Object : in out Gnoga.Base.Base_Type'Class;
                           Event  : in     Gnoga.Base.Keyboard_Event_Record)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Key.Property ("textContent", Event.Key_Code'Img);
   end On_Key_Press;

   procedure On_Move (Object : in out Gnoga.Base.Base_Type'Class;
                      Event  : in     Gnoga.Base.Mouse_Event_Record)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.X.Property ("textContent", Event.X'Img);
      App.Y.Property ("textContent", Event.Y'Img);
   end On_Move;

   procedure On_Context (Object : in out Gnoga.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Hello_World.Style ("color", "red");
   end On_Context;

   procedure End_App (Object : in out Gnoga.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      Log ("Ending Application");
      Application.Multiuser.End_Application;
   end End_App;

   procedure On_Connect
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      App : aliased App_Data;
   begin
      App.Main_Window := Main_Window'Unchecked_Access;

      App.Hello_World.Create_Inside_At_Top
        (Parent => Main_Window.Document.Body_Element.all,
         ID     => "label1",
         HTML   => "<h1 />");

      App.Hello_World.Property ("textContent", "Hello World!");

      App.Hello_World.On_Context_Menu_Handler (On_Context'Unrestricted_Access);
      App.Hello_World.On_Mouse_Click_Handler (On_Click'Unrestricted_Access);
      App.Hello_World.On_Mouse_Move_Handler (On_Move'Unrestricted_Access);

      App.Click_Quit.Create_After (Target => App.Hello_World,
                                   ID     => "label2",
                                   HTML   => "<h3>Click to Quit</h3>");

      App.Click_Quit.On_Click_Handler (End_App'Unrestricted_Access);

      App.X.Create_After (Target => App.Hello_World,
                          ID     => "labelX",
                          HTML   => "<div />");

      App.Y.Create_After (Target => App.Hello_World,
                          ID     => "labelY",
                          HTML   => "<div />");

      App.Key.Create_After (Target => App.Hello_World,
                            ID     => "labelK",
                            HTML   => "<div />");

      App.Main_Window.On_Key_Down_Handler (On_Key_Press'Unrestricted_Access);

      Application.Multiuser.Connection_Data (Main_Window, App'Unchecked_Access);

      Connection.Hold;
   end On_Connect;

begin
   Application.Multiuser.Initialize (Boot => "debug.html");

   Application.Multiuser.On_Connect_Handler (On_Connect'Unrestricted_Access);

   Application.Multiuser.Message_Loop;
end Demo;
