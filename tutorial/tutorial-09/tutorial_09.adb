--  In the last tutorial we did a simple static form tutorial. In this tutorial
--  we will explore other ways to interact with a form and various ways to
--  layout views.

with Gnoga.Types;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.View.Docker;

with Tutorial_Widget; use Tutorial_Widget;
--  We are going to use the Widget we created in the last tutorial.
--  For clarity it has been moved out to its own package;

procedure Tutorial_09 is
   use all type Gnoga.String;

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      My_Window : Gnoga.Gui.Window.Pointer_To_Window_Class;

      My_Docker : Gnoga.Gui.View.Docker.Docker_View_Type;
         --  Main view, will dock a view on top (My_Panel) for the
         --  exit button and another taking up the rest of the window
         --  with another Docker (My_Deck)

      My_Panel : aliased Gnoga.Gui.View.View_Type;
      My_Deck  : aliased Gnoga.Gui.View.Docker.Docker_View_Type;
         --  My_Deck will dock a set of tabs at the top and the
         --  remainder will a card view My_Cards

      My_Tabs  : aliased Gnoga.Gui.View.Card.Tab_Type;
      My_Cards : aliased Gnoga.Gui.View.Card.Card_View_Type;

      My_Widget_1 : aliased My_Widget_Type;
      My_Widget_2 : aliased My_Widget_Type;
      My_Results  : aliased Gnoga.Gui.View.Console.Console_View_Type;
         --  These three views will be placed in to cards in My_Cards
   end record;
   type App_Access is access all App_Data;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Application event handlers

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Handle interactive submit

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup GUI for each connection.

   procedure On_Result_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup another path in to the application for submitting results
   --  /result, see On_Connect_Handler in body of this procedure.

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.My_Window := Main_Window'Unchecked_Access;

      App.My_Docker.Create (Main_Window);

      App.My_Panel.Create (App.My_Docker);
      App.My_Panel.Background_Color ("silver");

      Gnoga.Gui.Element.Common.Button_Access
        (App.My_Panel.New_Element ("Exit", new Gnoga.Gui.Element.Common.Button_Type))
        .Create
        (App.My_Panel, "Exit Application");
      --  Views have built in an Element Map that can be used for storing
      --  dynamic elements. When New_Element is used it marks the stored
      --  element as Dynamic for later garbage collection.

      App.My_Panel.Element ("Exit").On_Click_Handler (On_Exit'Unrestricted_Access);
      --  Once a dynamic element is stored in a view's map it can be easily
      --  accessed using the Element property.

      App.My_Docker.Top_Dock (App.My_Panel'Unchecked_Access);

      App.My_Deck.Create (App.My_Docker);
      App.My_Docker.Fill_Dock (App.My_Deck'Unchecked_Access);

      App.My_Cards.Create (App.My_Deck);
      App.My_Cards.Border;
      App.My_Deck.Fill_Dock (App.My_Cards'Unchecked_Access);

      App.My_Tabs.Create (Parent => App.My_Deck, Card_View => App.My_Cards);

      App.My_Widget_1.Create (App.My_Cards);
      App.My_Widget_1.Widget_Form.Action ("/result");
      App.My_Cards.Add_Card (Name => "Widget_1", Card => App.My_Widget_1'Access);
      App.My_Tabs.Add_Tab ("Widget_1", "Static Form", Selected => True);

      App.My_Widget_2.Create (App.My_Cards);
      App.My_Widget_2.Widget_Form.On_Submit_Handler (On_Submit'Unrestricted_Access);
      App.My_Cards.Add_Card (Name => "Widget_2", Card => App.My_Widget_2'Access);
      App.My_Tabs.Add_Tab ("Widget_2", "Interactive Form");

      App.My_Results.Create (App.My_Cards);
      App.My_Cards.Add_Card (Name => "Results", Card => App.My_Results'Access);
      App.My_Tabs.Add_Tab ("Results", "Interactive Results");

      App.My_Deck.Top_Dock (App.My_Tabs'Unchecked_Access);
      --  We wait to dock My_Tabs until after we have added the tabs
      --  this ensures that My_Tabs's Height with content is known.
   end On_Connect;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Gnoga.Gui.Element.Common.Button_Access (App.My_Panel.Element ("Exit")).Disabled;
      --  We can upcast to a Button_Type if Element ("Exit") was not a
      --  Button_Type then we would receive a runtime exception.

      Gnoga.Application.Multi_Connect.End_Application;
   end On_Exit;

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_Tabs.Select_Tab ("Results");

      App.My_Results.Put ("Name : ");
      App.My_Results.Put (App.My_Widget_2.Name_Input.Value);
      App.My_Results.New_Line;

      App.My_Results.Put ("Message : ");
      App.My_Results.Put (App.My_Widget_2.Message.Value);
      App.My_Results.New_Line;

      --  The normal static submit behavior will not take place unless
      --  a call the Form's Submit method is made.
   end On_Submit;

   procedure On_Result_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      Result_View : constant Gnoga.Gui.View.View_Access := new Gnoga.Gui.View.View_Type;
   begin
      Result_View.Dynamic;
      Result_View.Create (Main_Window);

      Result_View.Put_Line ("Name : " & Main_Window.Form_Parameter ("Name"));
      Result_View.Put_Line ("Message : " & Main_Window.Form_Parameter ("Message"));
   end On_Result_Connect;

begin
   Gnoga.Application.Title ("Tutorial 09");

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => On_Result_Connect'Unrestricted_Access, Path => "result");

   Gnoga.Application.Multi_Connect.Message_Loop;
end Tutorial_09;
