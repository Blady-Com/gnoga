with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.List;
with Gnoga.Types;

with Gnoga.Gui.Plugin.jQueryUI;
with Gnoga.Gui.Plugin.jQueryUI.Widget;

procedure jDemo is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;
   use Gnoga.Gui.Plugin;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Console     : aliased View.Console.Console_View_Type;
         Box         : Common.DIV_Type;
         Box2        : Common.DIV_Type;
         Sorter      : aliased List.Ordered_List_Type;
         Button      : Common.Button_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      declare
         C : List.List_Item_Access := new List.List_Item_Type;
         N : List.List_Item_Access;
      begin
         App.Sorter.First_Child (C.all);

         while C.ID /= "undefined" loop

            App.Console.Put_Line ("-->" & C.Text);
            App.Console.Put_Line (jQueryUI.Is_Selected (C.all)'Img);

            N := new List.List_Item_Type;
            C.Next_Sibling (N.all);
            C.Free;
            C := N;
         end loop;

         C.Free;
      end;

      jQueryUI.Toggle_With_Effect (Element            => App.Box,
                                   Effect_Name        => "explode");
   end On_Click;

   procedure On_Drop (Object    : in out Gnoga.Gui.Base.Base_Type'Class;
                      Event     : in     String;
                      Message   : in     String;
                      Continue  : out    Boolean)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      if Event = jQueryUI.jQuery_Dropped_Event_Name then
         App.Console.Put_Line ("Dropped on me! - " & Message);
         Continue := False;
      else
         Continue := True;
      end if;
   end On_Drop;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;

      jQueryUI.Load_jQueryUI (Main_Window);

      jQueryUI.Widget.Turn_On_Tool_Tips (Main_Window);

      App.Console.Create (Main_Window);

      App.Box.Create (App.Console);
      App.Box.Put_Line ("Drag me around!");
      App.Box.Width (200);
      App.Box.Border;
      jQueryUI.Position (App.Box, 10, 10, Using_My => "left top");


      App.Sorter.Create (App.Console);
      List.List_Item_Access
        (App.Sorter.New_Element
           ("1", new List.List_Item_Type)).Create (App.Sorter, "Item 1");
      List.List_Item_Access (App.Sorter.Element ("1")).Value ("4");

      List.List_Item_Access
        (App.Sorter.New_Element
           ("2", new List.List_Item_Type)).Create (App.Sorter, "Item 2");
      List.List_Item_Access
        (App.Sorter.New_Element
           ("3", new List.List_Item_Type)).Create (App.Sorter, "Item 3");

      jQueryUI.Make_Draggable (App.Box);
      jQueryUI.Make_Resizable (App.Box);

      -- jQueryUI.Make_Sortable (App.Sorter);
      jQueryUI.Make_Selectable (App.Sorter);

      App.Button.Create (App.Console, "Click Me");
      App.Button.On_Click_Handler (On_Click'Unrestricted_Access);
      jQueryUI.Widget.Add_Tool_Tip (App.Button, "I do all sorts of stuff");

      App.Box2.Create (App.Console, "Drop on me");
      App.Box2.Border;
      jQueryUI.Make_Droppable (App.Box2);
      App.Box2.On_Message_Handler (On_Drop'Unrestricted_Access);
   end On_Connect;

begin
   Application.Multi_Connect.Initialize (Event => On_Connect'Unrestricted_Access,
                                     Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Open_URL_OSX;

   Application.Multi_Connect.Message_Loop;
end jDemo;
