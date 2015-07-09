with Gnoga.Application.Multi_Connect;

with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.List;

procedure Tree is
   use Gnoga;
   use Gnoga.Gui.Element;

   --  Views

   type Tree_Test_View is new Gnoga.Gui.View.View_Type with
      record
         null;
      end record;
   type Pointer_To_Tree_Text_View is access all Tree_Test_View'Class;

   overriding
   procedure Create
     (View   : in out Tree_Test_View;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "");
   --  Create tree

   overriding
   procedure On_Message (Object  : in out Tree_Test_View;
                         Event   : in     String;
                         Message : in     String);
   --  Capture tree events

   overriding
   procedure Create
     (View   : in out Tree_Test_View;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "")
   is
      use Gnoga.Gui.View;
      use Gnoga.Gui.Element.List;
   begin
      View_Type (View).Create (Parent, ID);
      Unordered_List_Access
        (View.New_Element ("root", new Unordered_List_Type)).Create (View);

      List.List_Item_Access
        (View.New_Element ("node1", new List_Item_Type)).Create
          (Unordered_List_Type'Class (View.Element ("root").all), "node1");
      View.Element ("node1").Attribute
        ("data-jstree", "{""icon"":""/img/gnoga_tiny.png""," &
           """disabled"":true}");
      List.List_Item_Access
        (View.New_Element ("node2", new List_Item_Type)).Create
          (Unordered_List_Type'Class (View.Element ("root").all), "node2");
      List.List_Item_Access
        (View.New_Element ("node3", new List_Item_Type)).Create
          (Unordered_List_Type'Class (View.Element ("root").all), "node3");

      Unordered_List_Access
        (View.New_Element ("branch1", new Unordered_List_Type)).Create (View);
      View.Element ("branch1").Place_Inside_Bottom_Of
        (View.Element ("node3").all);
      View.Element ("node3").Add_Class ("jstree-open");

      List.List_Item_Access
        (View.New_Element ("b1-node1", new List_Item_Type)).Create
          (Unordered_List_Type'Class (View.Element ("branch1").all),
           "b1-node1");
      List.List_Item_Access
        (View.New_Element ("b1-node2", new List_Item_Type)).Create
          (Unordered_List_Type'Class (View.Element ("branch1").all),
           "b1-node2");

      View.jQuery_Execute ("jstree()");

      View.Bind_Event
        (Event   => "select_node.jstree",
         Message => "",
         Eval  => "var i, j, r = [];" &
           "for(i = 0, j = data.selected.length; i < j; i++) {" &
           "r.push(data.instance.get_node(data.selected[i]).text);" &
           "}",
         Script => "r.join(', ')");
   end Create;

   overriding
   procedure On_Message (Object  : in out Tree_Test_View;
                         Event   : in     String;
                         Message : in     String)
   is
      use Gnoga.Gui.View;
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
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      Main_View : Pointer_To_Tree_Text_View := new Tree_Test_View;
   begin
      Main_View.Dynamic;
      Main_View.Create (Main_Window);
   end On_Connect;
begin
   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access,
      Boot  => "boot_jstree.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Open_URL;

   Application.Multi_Connect.Message_Loop;
end Tree;
