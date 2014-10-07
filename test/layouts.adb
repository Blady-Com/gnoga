with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.View.Docker;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Types;

procedure Layouts is
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

   procedure On_Click2 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.View.Show_Card ("1");
      App.View.Card ("1").Put_Line ("You have arrived on card 2");
   end On_Click2;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);

      Click_Count : Common.Span_Type;
      Count       : Natural;
   begin
      Click_Count.Attach_Using_Parent (Object, "click_count");
      Count := Natural'Value (Click_Count.Text);
      Count := Count + 1;
      Click_Count.Text (Gnoga.Left_Trim (Count'Img));

      App.View.Show_Card ("2");
      App.View.Card ("2").Put_Line ("You have arrived on card 1");
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App    : App_Access := new App_Data;
      V      : View.Pointer_To_View_Class;
      Card_1 : View.Pointer_To_View_Class;
      Card_2 : View.Pointer_To_View_Class;
   begin
      Main_Window.Connection_Data (App.all);
      App.Main_Window := Main_Window'Unchecked_Access;

      App.Docks.Create (Main_Window);

      App.View.Create (App.Docks);
      App.Docks.Fill_Dock (App.View'Access);

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

      Card_2 := App.View.Card ("2");

      declare
         use Gnoga.Gui.View;

         Button_1 : Common.Button_Access := Common.Button_Access
           (Card_1.New_Element ("next", new Common.Button_Type));
         Button_2 : Common.Button_Access := Common.Button_Access
           (Card_2.New_Element ("next", new Common.Button_Type));
      begin
         Button_1.Create (Card_1.all, "Click for Card 2");
         Button_2.Create (Card_2.all, "Click for Card 1");

         Card_1.Put (" Number of times clicked : ");
         Card_1.Put ("0", ID => "click_count");

         Button_1.On_Click_Handler (On_Click'Unrestricted_Access);
         Button_2.On_Click_Handler (On_Click2'Unrestricted_Access);
      end;
   end On_Connect;

begin
   Application.Multi_Connect.Initialize (Event => On_Connect'Unrestricted_Access,
                                     Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Open_URL_OSX;

   Application.Multi_Connect.Message_Loop;
end Layouts;
