with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.List;
with Gnoga.Gui.Plugin.MNMenu;
with Gnoga.Application.Multi_Connect;

procedure Menu is
   use Gnoga;
   use Gnoga.Gui.View;
   use Gnoga.Gui.Element;
   use Gnoga.Gui.Element.List;
   use Gnoga.Gui.Plugin.MNMenu;

   --  Connections

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      E : Element_Type renames Element_Type (Object);
   begin
      Gnoga.Log (E.Text);
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type)
   is
      Main_View                       : constant View_Access := new View_Type;
      Main_Menu, Sub_Menu1, Sub_Menu2 : Gnoga.Gui.Plugin.MNMenu.MNMenu_Type;
      Menu_Item1, Sub_Menu_Item1_1 : Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type;
      Menu_Bar, Sub_Menu              : Gnoga.Gui.Plugin.MNMenu.MNMenu_Access;
      Menu_Item,
      Menu_Item_File,
      Menu_Item_Edit,
      Menu_Item_Format : Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access;
   begin
      Main_View.Dynamic;
      Main_View.Create (Main_Window);

      Load_MNMenu (Main_Window);
      --  Load javascript and css stuff for MNMenu

      Main_View.Put_Line ("Subprograms in line with dynamic memory creation:");

      New_Menu_Bar (Main_View, "MenuBar2", Menu_Bar, Main_View);
      --  Create main menu

      New_Item (Menu_Bar, "File2", Menu_Item_File);
      New_Item (Menu_Bar, "Edit2", Menu_Item_Edit);
      New_Item (Menu_Bar, "Format2", Menu_Item_Format);
      --  Create all menu items

      New_Sub_Menu (Menu_Bar, Menu_Item_File, Sub_Menu);
      --  Create File sub-menu

      New_Item (Sub_Menu, "Open2", Menu_Item);
      New_Item (Sub_Menu, "Close2", Menu_Item);
      Menu_Item.On_Click_Handler (On_Click'Unrestricted_Access);
      --  Create File sub-menu items

      New_Sub_Menu (Menu_Bar, Menu_Item_Edit, Sub_Menu);
      --  Create Edit sub-menu

      New_Item (Sub_Menu, "Copy2", Menu_Item);
      Menu_Item.On_Click_Handler (On_Click'Unrestricted_Access);
      New_Item (Sub_Menu, "Paste2", Menu_Item);
      Menu_Item.On_Click_Handler (On_Click'Unrestricted_Access);
      --  Create Edit sub-menu items

      New_Sub_Menu (Menu_Bar, Menu_Item_Format, Sub_Menu);
      --  Create Format sub-menu

      New_Item (Sub_Menu, "Bold2", Menu_Item);
      Menu_Item.On_Click_Handler (On_Click'Unrestricted_Access);
      New_Item (Sub_Menu, "Italic2", Menu_Item);
      --  Create Format sub-menu items

      Display_Menu (Menu_Bar);

      Main_View.Put_Line
      ("Subprograms in hierarchie with dynamic memory creation:");

      New_Menu_Bar (Main_View, "MenuBar3", Menu_Bar, Main_View);
      --  Create main menu

      New_Sub_Menu_Item (Menu_Bar, "File3", Sub_Menu);
      New_Item (Sub_Menu, "Open3", Menu_Item);
      Menu_Item.On_Click_Handler (On_Click'Unrestricted_Access);
      New_Item (Sub_Menu, "Close3", Menu_Item);
      --  Create File sub-menu and its items

      New_Sub_Menu_Item (Menu_Bar, "Edit3", Sub_Menu);
      New_Item (Sub_Menu, "Copy3", Menu_Item);
      Menu_Item.On_Click_Handler (On_Click'Unrestricted_Access);
      New_Item (Sub_Menu, "Paste3", Menu_Item);
      --  Create Edit sub-menu and its items

      New_Sub_Menu_Item (Menu_Bar, "Format3", Sub_Menu);
      New_Item (Sub_Menu, "Bold3", Menu_Item);
      Menu_Item.On_Click_Handler (On_Click'Unrestricted_Access);
      New_Item (Sub_Menu, "Italic3", Menu_Item);
      Menu_Item.On_Click_Handler (On_Click'Unrestricted_Access);
      --  Create Format sub-menu and its items

      Display_Menu (Menu_Bar);
      --  Display menu style

      Main_View.Put_Line ("Subprograms with plain type parameters:");

      Add_Menu_Bar
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.New_Element
            ("Menu5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Type)).all,
         Main_View.all);

      Add_Item
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("Menu5")).all,
         "File5",
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.New_Element
            ("File5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type)).all);
      Add_Sub_Menu
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("Menu5")).all,
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.Element ("File5")).all,
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.New_Element
            ("MenuFile5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Type)).all);
      Add_Item
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("MenuFile5")).all,
         "Open5",
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.New_Element
            ("Open5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type)).all);
      Main_View.Element ("Open5").On_Click_Handler
      (On_Click'Unrestricted_Access);

      Add_Item
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("Menu5")).all,
         "Edit5",
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.New_Element
            ("Edit5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type)).all);
      Add_Sub_Menu
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("Menu5")).all,
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.Element ("Edit5")).all,
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.New_Element
            ("MenuEdit5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Type)).all);
      Add_Item
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("MenuEdit5")).all,
         "Copy5",
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.New_Element
            ("Copy5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type)).all);
      Main_View.Element ("Copy5").On_Click_Handler
      (On_Click'Unrestricted_Access);
      Add_Item
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("MenuEdit5")).all,
         "Paste5",
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.New_Element
            ("Paste5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type)).all);

      Add_Sub_Menu_Item
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("Menu5")).all,
         "Format5",
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.New_Element
            ("Format5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type)).all,
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.New_Element
            ("MenuFormat5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Type)).all);
      Add_Item
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("MenuFormat5")).all,
         "Bold5",
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.New_Element
            ("Bold5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type)).all);
      Main_View.Element ("Bold5").On_Click_Handler
      (On_Click'Unrestricted_Access);
      Add_Item
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
           (Main_View.Element ("MenuFormat5")).all,
         "Italic5",
         Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
           (Main_View.New_Element
            ("Italic5", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type)).all);

      Display_Menu
        ((Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
            (Main_View.Element ("Menu5"))));
      --  Display menu style

      Main_View.Put_Line ("MNMenu types with original list subprograms:");

      Main_Menu.Create (Main_View.all);
      --  Create static main menu

      Menu_Item1.Create (Main_Menu, "Menu 1");
      --  Create static first item of main menu with name

      Main_Menu.Add_Element
      ("MenuItem2", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type);
      Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
        (Main_Menu.Element ("MenuItem2")).Create
      (Main_Menu, "Menu 2");
      --  Create dynamic second item of main menu with DOM ID

      Sub_Menu1.Create (Main_Menu);
      Sub_Menu1.Place_Inside_Bottom_Of (Menu_Item1);
      --  Create static submenu of first menu

      Sub_Menu_Item1_1.Create (Sub_Menu1, "Sub-Menu 1-1");
      --  Create static fisrt item of first submenu
      Sub_Menu_Item1_1.On_Click_Handler (On_Click'Unrestricted_Access);

      Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access'
        (new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type).Create
      (Sub_Menu1, "Sub-Menu 1-2");
      Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access'
        (new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type).Create
      (Sub_Menu1, "Sub-Menu 1-3");
      Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access'
        (new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type).Create
      (Sub_Menu1, "Sub-Menu 1-4");
      Gnoga.Gui.Plugin.MNMenu.Create
        (Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access'
           (new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type).all,
         Sub_Menu1,
         "Sub-Menu 1-5");
      --  Create dynamic other items of first submenu

      Main_Menu.Add_Element
      ("MenuItem3", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type);
      Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access
        (Main_Menu.Element ("MenuItem3")).Create
      (Main_Menu, "Menu 3");
      --  Create dynamic third item of main menu with DOM ID

      Sub_Menu2.Create (Main_Menu);
      Sub_Menu2.Place_Inside_Bottom_Of (Main_Menu.Element ("MenuItem2").all);
      --  Create static submenu of second menu

      Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access'
        (new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type).Create
      (Sub_Menu2, "Sub-Menu 2-1");
      --  Create dynamic first item of second submenu

      Main_Menu.Add_Element
      ("SubMenu3", new Gnoga.Gui.Plugin.MNMenu.MNMenu_Type);
      Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
        (Main_Menu.Element ("SubMenu3")).Create
      (Main_Menu);
      Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
        (Main_Menu.Element ("SubMenu3")).Place_Inside_Bottom_Of
      (Main_Menu.Element ("MenuItem3").all);
      --  Create dynamic submenu of third menu

      Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access'
        (new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type).Create
      (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
         (Main_Menu.Element ("SubMenu3")).all, "Sub-Menu 3-1");
      Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Access'
        (new Gnoga.Gui.Plugin.MNMenu.MNMenu_Item_Type).Create
      (Gnoga.Gui.Plugin.MNMenu.MNMenu_Access
         (Main_Menu.Element ("SubMenu3")).all, "Sub-Menu 3-2");
      --  Create dynamic items of third submenu

      Display_Menu (Main_Menu);
      --  Display menu style

      Main_View.Put_Line ("Original types and list subprograms:");

      Unordered_List_Access
        (Main_View.New_Element ("Menu", new Unordered_List_Type)).Create
      (Main_View.all);
      List.List_Item_Access
        (Main_View.New_Element ("Item1", new List_Item_Type)).Create
      (Unordered_List_Type'Class (Main_View.Element ("Menu").all), "Item1");
      List.List_Item_Access
        (Main_View.New_Element ("Item2", new List_Item_Type)).Create
      (Unordered_List_Type'Class (Main_View.Element ("Menu").all), "Item2");
      Main_View.Element ("Item2").On_Click_Handler
      (On_Click'Unrestricted_Access);

      Unordered_List_Access
        (Main_View.New_Element ("SubMenu1", new Unordered_List_Type)).Create
      (Main_View.all);
      Main_View.Element ("SubMenu1").Place_Inside_Bottom_Of
      (Main_View.Element ("Item1").all);
      List.List_Item_Access
        (Main_View.New_Element ("SubItem1", new List_Item_Type)).Create
      (Unordered_List_Type'Class
         (Main_View.Element ("SubMenu1").all), "SubItem1");
      Main_View.Element ("SubItem1").On_Click_Handler
      (On_Click'Unrestricted_Access);
      List.List_Item_Access
        (Main_View.New_Element ("SubItem2", new List_Item_Type)).Create
      (Unordered_List_Type'Class
         (Main_View.Element ("SubMenu1").all), "SubItem2");
      Main_View.Element ("SubItem2").On_Click_Handler
      (On_Click'Unrestricted_Access);

      Main_View.Element ("Menu").jQuery_Execute ("mnmenu()");

      Main_View.Put_Line ("Some more text");

      Connection.Hold;
   end On_Connect;
begin
   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access,
      Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end Menu;
