with Gnoga.Application.Multi_Connect;

with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.List;
with Gnoga.Gui.Plugin.JSTree;

procedure Tree is
   use Gnoga;
   use Gnoga.Gui.View;
   use Gnoga.Gui.Element;
   use Gnoga.Gui.Element.List;
   use Gnoga.Gui.Plugin.JSTree;

   --  Views

   type Tree_Test_View_Type is new Gnoga.Gui.View.View_Type with record
      null;
   end record;
   type Pointer_To_Tree_Text_View is access all Tree_Test_View_Type'Class;

   overriding procedure Create
     (View   : in out Tree_Test_View_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String := "");
   --  Create tree

   overriding procedure On_Message
     (Object  : in out Tree_Test_View_Type;
      Event   : in     String;
      Message : in     String);
   --  Capture tree events

   overriding procedure Create
     (View   : in out Tree_Test_View_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String := "")
   is
   begin
      View_Type (View).Create (Parent, ID);
      View.Width (200);

      Unordered_List_Access (View.New_Element ("root", new Unordered_List_Type)).Create (View);

      List.List_Item_Access (View.New_Element ("node1", new List_Item_Type)).Create
        (Unordered_List_Type'Class (View.Element ("root").all), "node1");
      View.Element ("node1").Attribute ("data-jstree", "{""icon"":""/img/gnoga_tiny.png""," & """disabled"":true}");
      List.List_Item_Access (View.New_Element ("node2", new List_Item_Type)).Create
        (Unordered_List_Type'Class (View.Element ("root").all), "node2");
      List.List_Item_Access (View.New_Element ("node3", new List_Item_Type)).Create
        (Unordered_List_Type'Class (View.Element ("root").all), "node3");

      Unordered_List_Access (View.New_Element ("branch1", new Unordered_List_Type)).Create (View);
      View.Element ("branch1").Place_Inside_Bottom_Of (View.Element ("node3").all);
      View.Element ("node3").Add_Class ("jstree-open");

      List.List_Item_Access (View.New_Element ("b1-node1", new List_Item_Type)).Create
        (Unordered_List_Type'Class (View.Element ("branch1").all), "b1-node1");
      List.List_Item_Access (View.New_Element ("b1-node2", new List_Item_Type)).Create
        (Unordered_List_Type'Class (View.Element ("branch1").all), "b1-node2");

      View.jQuery_Execute ("jstree()");

      View.Bind_Event
        (Event => "select_node.jstree", Message => "",
         Eval  =>
           "var i, j, r = [];" & "for(i = 0, j = data.selected.length; i < j; i++) {" &
           "r.push(data.instance.get_node(data.selected[i]).text);" & "}",
         Script => "r.join(', ')");
   end Create;

   overriding procedure On_Message
     (Object  : in out Tree_Test_View_Type;
      Event   : in     String;
      Message : in     String)
   is
   begin
      if Event = "select_node.jstree" then
         Gnoga.Log ("Node selected - " & Message);
      else
         View_Type (Object).On_Message (Event, Message);
      end if;
   end On_Message;

   --  Connections

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Open
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Node   : in     String);

   procedure On_Open
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Node   : in     String)
   is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log ("Open/Close: " & Node);
   end On_Open;

   procedure On_Select
     (Object   : in out Gnoga.Gui.Base.Base_Type'Class;
      Selected : in     String);

   procedure On_Select
     (Object   : in out Gnoga.Gui.Base.Base_Type'Class;
      Selected : in     String)
   is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log ("Selected: " & Selected);
   end On_Select;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      Main_View                      : constant Pointer_To_View_Class     := new View_Type;
      Tree_View                      : constant Pointer_To_Tree_Text_View := new Tree_Test_View_Type;
      Tree, Sub_Tree                 : Gnoga.Gui.Plugin.JSTree.JSTree_Access;
      Node1, Node2, Node3, Node_Item : Gnoga.Gui.Plugin.JSTree.JSTree_Item_Access;
   begin
      Main_View.Dynamic;
      Main_View.Create (Main_Window);

      Load_JSTree (Main_Window);
      --  Load javascript and css stuff for JSTree

      Main_View.Put_Line ("Subprograms in line with dynamic memory creation:");

      New_Root_Tree (Tree, "Root1", Main_View.all);
      -- Create the main tree

      New_Item (Tree, "Node1", Node1, Icon => "/img/gnoga_tiny.png");
      New_Item (Tree, "Node2", Node2, Selected => True);
      New_Item (Tree, "Node3", Node3, Opened => True);

      New_Sub_Tree (Tree, Node3, Sub_Tree);

      New_Item (Sub_Tree, "B3-Node1", Node_Item, Disabled => True);
      New_Item (Sub_Tree, "B3-Node2", Node_Item);

      Display_Tree (Tree);

      Main_View.Put_Line ("Subprograms in hierarchie with dynamic memory creation:");

      New_Root_Tree (Tree, "Root2", Main_View.all);
      -- Create the main tree

      New_Item (Tree, "Node21", Node1);
      New_Item (Tree, "Node22", Node2);
      New_Sub_Tree_Item (Tree, "Node23", Sub_Tree);
      New_Item (Sub_Tree, "B3-Node1", Node_Item);
      New_Item (Sub_Tree, "B3-Node2", Node_Item);

      Tree.On_Open_Node_Handler (On_Open'Unrestricted_Access);
      Tree.On_Close_Node_Handler (On_Open'Unrestricted_Access);
      Tree.On_Select_Node_Handler (On_Select'Unrestricted_Access);
      Tree.On_Deselect_Node_Handler (On_Select'Unrestricted_Access);

      Display_Tree (Tree, (Core => (Themes => (Dots => False, others => <>), others => <>), others => <>));

      Main_View.Put_Line ("Subprograms with plain type parameters:");

      Add_Root_Tree
        (Gnoga.Gui.Plugin.JSTree.JSTree_Access
           (Main_View.New_Element ("Root3", new Gnoga.Gui.Plugin.JSTree.JSTree_Type)).all,
         Main_View.all);

      Add_Item
        (Gnoga.Gui.Plugin.JSTree.JSTree_Access (Main_View.Element ("Root3")).all, "Node31",
         Gnoga.Gui.Plugin.JSTree.JSTree_Item_Access
           (Main_View.New_Element ("Node31", new Gnoga.Gui.Plugin.JSTree.JSTree_Item_Type)).all);

      Add_Item
        (Gnoga.Gui.Plugin.JSTree.JSTree_Access (Main_View.Element ("Root3")).all, "Node32",
         Gnoga.Gui.Plugin.JSTree.JSTree_Item_Access
           (Main_View.New_Element ("Node32", new Gnoga.Gui.Plugin.JSTree.JSTree_Item_Type)).all);

      Add_Sub_Tree_Item
        (Gnoga.Gui.Plugin.JSTree.JSTree_Access (Main_View.Element ("Root3")).all, "Node33",
         Gnoga.Gui.Plugin.JSTree.JSTree_Item_Access
           (Main_View.New_Element ("Node33", new Gnoga.Gui.Plugin.JSTree.JSTree_Item_Type)).all,
         Gnoga.Gui.Plugin.JSTree.JSTree_Access
           (Main_View.New_Element ("Node33", new Gnoga.Gui.Plugin.JSTree.JSTree_Type)).all);

      Add_Item
        (Gnoga.Gui.Plugin.JSTree.JSTree_Access (Main_View.Element ("Node33")).all, "B3-Node1",
         Gnoga.Gui.Plugin.JSTree.JSTree_Item_Access
           (Main_View.New_Element ("B3-Node1", new Gnoga.Gui.Plugin.JSTree.JSTree_Item_Type)).all);

      Add_Item
        (Gnoga.Gui.Plugin.JSTree.JSTree_Access (Main_View.Element ("Node33")).all, "B3-Node2",
         Gnoga.Gui.Plugin.JSTree.JSTree_Item_Access
           (Main_View.New_Element ("B3-Node2", new Gnoga.Gui.Plugin.JSTree.JSTree_Item_Type)).all);

      Add_Sub_Tree
        (Gnoga.Gui.Plugin.JSTree.JSTree_Access (Main_View.Element ("Root3")).all,
         Gnoga.Gui.Plugin.JSTree.JSTree_Item_Access (Main_View.Element ("B3-Node2")).all,
         Gnoga.Gui.Plugin.JSTree.JSTree_Access
           (Main_View.New_Element ("Node34", new Gnoga.Gui.Plugin.JSTree.JSTree_Type)).all);

      Add_Item
        (Gnoga.Gui.Plugin.JSTree.JSTree_Access (Main_View.Element ("Node34")).all, "B4-Node1",
         Gnoga.Gui.Plugin.JSTree.JSTree_Item_Access
           (Main_View.New_Element ("B4-Node1", new Gnoga.Gui.Plugin.JSTree.JSTree_Item_Type)).all);

      Display_Tree
        (Gnoga.Gui.Plugin.JSTree.JSTree_Access (Main_View.Element ("Root3")).all,
         (Plugins => (ContextMenu => True, DragAndDrop => True, others => <>), others => <>));

      Main_View.Put_Line ("Original types and list subprograms:");

      Tree_View.Dynamic;
      Tree_View.Create (Main_View.all);
      Tree_View.Border;

      Main_View.Put_Line ("Some more text");
   end On_Connect;
begin
   Application.Multi_Connect.Initialize (Event => On_Connect'Unrestricted_Access, Boot => "debug.html");

   Application.Title ("Test App for Gnoga");
--     Application.HTML_On_Close
--       ("<b>Connection to Application has been terminated</b>");

--     Application.Open_URL;

   Application.Multi_Connect.Message_Loop;
end Tree;
