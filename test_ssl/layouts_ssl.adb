with Gnoga.Types;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.View.Docker;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;

with Gnoga.Server.Connection.Secure;
with Gnoga.Types.Colors;

procedure Layouts_SSL is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Docks       : Gnoga.Gui.View.Docker.Docker_View_Type;
         View        : aliased Gnoga.Gui.View.Card.Card_View_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);

      Click_Count : Common.Span_Type;
      Count       : Natural;
   begin
      View.Card.Tab_Item_Type (Object).Tab_Select;

      Click_Count.Attach_Using_Parent (Object, "click_count");
      Count := Natural'Value (Click_Count.Text);
      Count := Count + 1;
      Click_Count.Text (Gnoga.Left_Trim (Count'Img));
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App    : App_Access := new App_Data;
      V      : View.Pointer_To_View_Class;
      Card_1 : View.Pointer_To_View_Class;
      Card_2 : View.Pointer_To_View_Base_Class;
      Tabs   : View.Card.Tab_Access;
      Tab    : View.Card.Tab_Item_Access;
      Dex    : View.Docker.Pointer_To_Docker_View_Class;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;

      --  Main_Window.Buffer_Connection;

      App.Docks.Create (Main_Window);

      Dex := new View.Docker.Docker_View_Type;
      Dex.Dynamic;
      Dex.Create (App.Docks);
      App.Docks.Fill_Dock (Dex);

      App.View.Create (Dex.all);
      App.View.Border;
      Dex.Fill_Dock (App.View'Access);

      Tabs := new View.Card.Tab_Type;
      Tabs.Dynamic;
      Tabs.Create (Parent       => Dex.all,
                   Card_View    => App.View,
                   Text_Color   => Gnoga.Types.Colors.Black,
                   Tab_Color    => Gnoga.Types.Colors.Blue,
                   Select_Color => Gnoga.Types.Colors.Light_Blue);

      Tab := new View.Card.Tab_Item_Type;
      Tab.Dynamic;
      Tab.Create (Tabs.all, "1", "Card 1");

      Tab := new View.Card.Tab_Item_Type;
      Tab.Dynamic;
      Tab.Create (Tabs.all, "2", "Card 2");
      Tab.On_Click_Handler (On_Click'Unrestricted_Access);

      Dex.Top_Dock (Tabs);

      V := new View.View_Type;
      V.Dynamic;
      V.Create (App.Docks);
      V.Background_Color ("Black");
      V.Put_Line ("Here");
      App.Docks.Top_Dock (V);

      V := new View.View_Type;
      V.Dynamic;
      V.Create (App.Docks);
      V.Background_Color ("Green");
      V.Put_Line ("Here");
      App.Docks.Bottom_Dock (V);

      V := new View.View_Type;
      V.Dynamic;
      V.Create (App.Docks);
      V.Background_Color ("Blue");
      V.Put_Line ("Here");
      App.Docks.Left_Dock (V);

      V := new View.View_Type;
      V.Dynamic;
      V.Create (App.Docks);
      V.Background_Color ("Yellow");
      V.Put_Line ("Here");
      App.Docks.Right_Dock (V);

      Card_1 := new View.Console.Console_View_Type;
      Card_1.Dynamic;
      Card_1.Create (App.View);

      App.View.Add_Card ("1", Card_1);
      App.View.Add_Card ("2", Show => False);

      Tab.Tab_Select;

      Card_2 := App.View.Card ("2");

      Card_1.Put (" Number of times clicked : ");
      Card_1.Put ("0", ID => "click_count");
   end On_Connect;

begin
   Gnoga.Server.Connection.Secure.Register_Secure_Server
     (Certificate_File => Gnoga.Server.Application_Directory &
        "/test_ssl/server.crt",
      Key_File         => Gnoga.Server.Application_Directory &
        "/test_ssl/server.key",
      Port             => 8443,
      Disable_Insecure => False);

   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access,
      Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Open_URL ("https://127.0.0.1:8443");

   Application.Multi_Connect.Message_Loop;
end Layouts_SSL;
