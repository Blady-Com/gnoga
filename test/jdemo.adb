with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.List;
with Gnoga.Gui.Element.Form;
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
         Menu        : aliased List.Unordered_List_Type;
         Sub_Menu    : aliased List.Unordered_List_Type;
         Tools       : aliased jQueryUI.Widget.Accordion_Type;
         Dialog      : aliased jQueryUI.Widget.Dialog_Type;
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
      App.Dialog.Open;
   end On_Click;

   procedure On_Menu_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Console.Put_Line ("Menu Clicked - " & Element_Type (Object).Text);
   end On_Menu_Click;

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

   procedure Close_Dialog (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      jQueryUI.Widget.Dialog_Access (Object.Parent).Close;
   end Close_Dialog;

   procedure On_Open (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      Log ("Dialog Opened");
   end On_Open;

   procedure On_Close (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      Log ("Dialog Closed");
   end On_Close;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;

      Main_Window.Document.Body_Element.Font
        (Family  => "Verdana, Arial",
         Height  => "10px");
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
      jQueryUI.Widget.Make_Button (App.Button);
      jQueryUI.Widget.Add_Tool_Tip (App.Button, "I do all sorts of stuff");

      App.Box2.Create (App.Console, "Drop on me");
      App.Box2.Border;
      jQueryUI.Make_Droppable (App.Box2);
      App.Box2.On_Message_Handler (On_Drop'Unrestricted_Access);

      App.Menu.Create (App.Console);

      List.List_Item_Access
        (App.Menu.New_Element
           ("1", new List.List_Item_Type)).Create (App.Menu, "Item 1");
      List.List_Item_Access (App.Menu.Element ("1")).Value ("4");

      App.Menu.Element ("1").On_Click_Handler
        (On_Menu_Click'Unrestricted_Access);

      List.List_Item_Access
        (App.Menu.New_Element
           ("2", new List.List_Item_Type)).Create (App.Menu, "Item 2");

      App.Menu.Element ("2").On_Click_Handler
        (On_Menu_Click'Unrestricted_Access);

      List.List_Item_Access
        (App.Menu.New_Element
           ("3", new List.List_Item_Type)).Create (App.Menu, "Item 3");

      App.Menu.Element ("3").On_Click_Handler
        (On_Menu_Click'Unrestricted_Access);

      App.Sub_Menu.Create (App.Menu.Element ("2").all);
      App.Sub_Menu.Place_Inside_Bottom_Of (App.Menu.Element ("2").all);

      List.List_Item_Access
        (App.Sub_Menu.New_Element
           ("1", new List.List_Item_Type)).Create (App.Sub_Menu, "SItem 1");
      List.List_Item_Access (App.Sub_Menu.Element ("1")).Value ("4");

      App.Sub_Menu.Element ("1").On_Click_Handler
        (On_Menu_Click'Unrestricted_Access);

      List.List_Item_Access
        (App.Sub_Menu.New_Element
           ("2", new List.List_Item_Type)).Create (App.Sub_Menu, "SItem 2");

      App.Sub_Menu.Element ("2").On_Click_Handler
        (On_Menu_Click'Unrestricted_Access);

      List.List_Item_Access
        (App.Sub_Menu.New_Element
           ("3", new List.List_Item_Type)).Create (App.Sub_Menu, "SItem 3");

      App.Sub_Menu.Element ("3").On_Click_Handler
        (On_Menu_Click'Unrestricted_Access);

      jQueryUI.Widget.Make_Menu (App.Menu);
      App.Menu.Width (100);
      App.Sub_Menu.Width (100);
      jQueryUI.Position (App.Menu, App.Console);

      App.Tools.Create (App.Console);
      App.Tools.Width (400);
      App.Tools.Height (400);

      App.Tools.Create_Section ("Section 1");
      Common.DIV_Access
        (App.Tools.New_Element
           ("S1", new Common.DIV_Type)).Create
          (App.Tools, "Section 1 View");

      App.Tools.Create_Section ("Section 2");
      Common.DIV_Access
        (App.Tools.New_Element
           ("S2", new Common.DIV_Type)).Create
          (App.Tools, "Section 2 View");

      App.Tools.Create_Section ("Section 3");
      Common.DIV_Access
        (App.Tools.New_Element
           ("S3", new Common.DIV_Type)).Create
          (App.Tools, "Section 3 View");

      App.Tools.Element ("S2").Font (Height => "10px");

      App.Tools.Render_Accordion;

      declare
         T  : jQueryUI.Widget.Tabs_Type;
         D1 : Common.DIV_Type;
         D2 : Common.DIV_Type;
         D3 : Common.DIV_Type;
      begin
         T.Create (App.Console);

         D1.Create (T, "This is tab 1");
         D2.Create (T, "This is Tab 2");
         D3.Create (T, "This is Tab 3");

         T.Add_Tab ("Tab 1", D1);
         T.Add_Tab ("Tab 2", D2);
         T.Add_Tab ("Tab 3", D3);

         T.Render_Tabs;

         T.Width (400);
      end;


      App.Dialog.Create (Parent          => App.Console,
                         Title           => "About jDemo",
                         Content         => "Hello World!",
                         Height          => 300,
                         Width           => 300,
                         Position_My     => "top",
                         Position_At     =>"center top+5%");
--                         Resizable       => True,
--                           Minimum_Height  => ,
--                           Minimum_Width   => ,
--                           Maximum_Height  => ,
--                           Maximum_Width   => ,
--                         Modal           => False,
--                           Close_On_Escape => ,
--                         Draggable       => False);
--                           ID              => );
      App.Dialog.On_Open_Handler (On_Open'Unrestricted_Access);
      App.Dialog.On_Close_Handler (On_Close'Unrestricted_Access);

      jQueryUI.Widget.Progress_Bar_Access
        (App.Dialog.New_Element
           ("progress", new jQueryUI.Widget.Progress_Bar_Type)). Create
          (Parent  => App.Dialog,
           Value   => 35,
           Maximum => 100);

      App.Dialog.Open;

      Common.Button_Access
        (App.Dialog.New_Element ("ok", new Common.Button_Type)).Create (App.Dialog, "Ok");
      Common.Button_Access
        (App.Dialog.Element ("ok")).Focus;
      Common.Button_Access
        (App.Dialog.Element ("ok")).On_Click_Handler
          (Close_Dialog'Unrestricted_Access);

      jQueryUI.Position
        (App.Dialog.Element ("ok").all,
         Target   => App.Dialog,
         Using_My => "bottom",
         At_Target => "center bottom-10");

      for i in 35 .. 100 loop
         jQueryUI.Widget.Progress_Bar_Access
           (App.Dialog.Element ("progress")).value (i);
         delay 0.10;
      end loop;
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
