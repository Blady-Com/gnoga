with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Section;
with Gnoga.Gui.Element.List;
with Gnoga.Types;
with Gnoga.Server.Template_Parser;

with Gnoga.Gui.Plugin.Boot_Strap;

procedure Boot_Strap_Demo is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;
   use Gnoga.Gui.Plugin;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Nav_Bar     : Section.Section_Type;
         M_Container : Boot_Strap.Fluid_Container_Type;
         J_Container : Boot_Strap.Container_Type;
         C_Container : Boot_Strap.Container_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      null;
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;

      Boot_Strap.Load_Boot_Strap (Main_Window);

      App.Nav_Bar.Create (Main_Window, Section.Nav,Attach => False);
      App.Nav_Bar.Add_Class ("navbar navbar-inverse");
      App.Nav_Bar.Place_Inside_Top_Of
        (Main_Window.Document.Body_Element.all);

      App.M_Container.Create (App.Nav_Bar);

      declare
         Nav_Head   : View.View_Type;
         Nav_Button : Common.Button_Type;
         Nav_Brand  : Common.A_Type;
         Nav_Links  : View.View_Type;
         Nav_Menu   : aliased List.Unordered_List_Type;
         Nav_Right  : aliased List.Unordered_List_Type;
      begin
         Nav_Head.Create (App.M_Container);
         Nav_Head.Add_Class ("navbar-header");

         Nav_Button.Create (Nav_Head);
         Nav_Button.Add_Class ("navbar-toggle collapsed");
         Nav_Button.Attribute ("data-toggle", "collapse");
         Nav_Button.Put ("", "icon-bar");
         Nav_Button.Put ("", "icon-bar");
         Nav_Button.Put ("", "icon-bar");

         Nav_Brand.Create (Nav_Head, "#", "Gnoga");
         Nav_Brand.Add_Class ("navbar-brand");

         Nav_Links.Create (App.M_Container);
         Nav_Links.Add_Class ("collapse navbar-collapse");
         Nav_Button.Attribute ("data-target", "#" & Nav_Links.ID);

         Nav_Menu.Create (Nav_Links);
         Nav_Menu.Add_Class ("nav navbar-nav");

         List.List_Item_Access
           (Nav_Menu.New_Element
              ("item1", new List.List_Item_Type)). Create
             (Nav_Menu, "<a href='#'>Item 1</a>");
         List.List_Item_Access
           (Nav_Menu.New_Element
              ("item2", new List.List_Item_Type)). Create
             (Nav_Menu, "<a href='#'>Item 1</a>");
         List.List_Item_Access
           (Nav_Menu.New_Element
              ("item3", new List.List_Item_Type)). Create
             (Nav_Menu, "<a href='#'>Item 1</a>");

         Nav_Right.Create (Nav_Links);
         Nav_Right.Add_Class ("nav navbar-nav navbar-right");

         List.List_Item_Access
           (Nav_Right.New_Element
              ("item1", new List.List_Item_Type)). Create
             (Nav_Right, "<a href='#'>Sign Up</a>");
         List.List_Item_Access
           (Nav_Right.New_Element
              ("item2", new List.List_Item_Type)). Create
             (Nav_Right, "<a href='#'>Login</a>");

      end;

      App.J_Container.Create (Main_Window);
      App.J_Container.Place_After (App.Nav_Bar);

      declare
         J : Boot_Strap.Jumbotron_Type;
      begin
         J.Create
           (App.J_Container,
            "<H1>Gnoga <small>The GNU Omnificent GUI for Ada</small></H1>" &
              "The Ada Open-Source Mission-Critical Cloud, Desktop and" &
              " Mobile Application Development Framework");
      end;

      App.C_Container.Create (Main_Window);
      App.C_Container.Place_After (App.J_Container);

      declare
         Row     : Boot_Strap.Row_Type;
         Panel_1 : View.Console.Console_View_Type;
         Panel_2 : View.Console.Console_View_Type;
         Panel_3 : View.Console.Console_View_Type;
      begin
         Row.Create (App.C_Container);

         Panel_1.Create (Row);
         Boot_Strap.Set_Columns (Panel_1, 3, Boot_Strap.Small);
         for i in 1 .. 50 loop
            Panel_1.Put ("Hello World!");
         end loop;

         Panel_2.Create (Row);
         Boot_Strap.Set_Columns (Panel_2, 6, Boot_Strap.Small);

         for i in 1 .. 50 loop
            Panel_2.Put ("Some World!");
         end loop;

         Panel_3.Create (Row);
         Boot_Strap.Set_Columns (Panel_3, 3, Boot_Strap.Small);

         for i in 1 .. 50 loop
            Panel_3.Put ("Bye World!");
         end loop;
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
end Boot_Strap_Demo;
