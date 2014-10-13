--  With Gnoga it is possible to build your own custom widgets by composting
--  elements in to views and build web forms. In the next turorial we will
--  see how to interat with forms in a more application centric way instead of
--  as static forms.


with Gnoga.Types;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;

procedure Tutorial_08 is

   -------------------------------------------------------------------------
   -- My_Widget_Type
   -------------------------------------------------------------------------
   --  In this tutorial we are going to create a custom Gnoga View that is a
   --  composite of other elements so that we can reuse the view in the
   --  future.

   type My_Widget_Type is new Gnoga.Gui.View.View_Type with
      record
         Widget_Form : Gnoga.Gui.Element.Form.Form_Type;
         Name_Input  : Gnoga.Gui.Element.Form.Text_Type;
         Message     : Gnoga.Gui.Element.Form.Text_Area_Type;
         My_Submit   : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

   overriding
   procedure Create  (View    : in out My_Widget_Type;
                      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                      Attach  : in     Boolean := True;
                      ID      : in     String  := "");
   --  Used to create our custom view

   procedure Create (View    : in out My_Widget_Type;
                     Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                     Attach  : in     Boolean := True;
                     ID      : in     String  := "")
   is
      use Gnoga.Gui.Element.Table;
      Layout_Table : Table_Access := new Table_Type;
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, Attach, ID);

      View.Widget_Form.Create (View);

      Layout_Table.Dynamic;
      --  Marking an element Dynamic before creation tells Gnoga to garbage
      --  collect, i.e. deallocate the element when it is parent is finalized.
      Layout_Table.Create (View);

      declare
         row  : Table_Row_Access := new Table_Row_Type;
         col1 : Table_Column_Access := new Table_Column_Type;
         col2 : Table_Column_Access := new Table_Column_Type;
      begin
         row.Dynamic;
         col1.Dynamic;
         col2.Dynamic;

         row.Create (Layout_Table.all);
         col1.Create (row.all, "Name");
         col2.Create (row.all);
         View.Name_Input.Create (Form  => View.Widget_Form,
                                 Size  => 20,
                                 Name  => "Name");
         -- The Name of the element is its variable name when submitted.

         View.Name_Input.Place_Inside_Top_Of (col2.all);
         -- Since forms are auto placed in side the Form. We need to move the
         -- element where we would like it to display.
      end;

      declare
         row  : Table_Row_Access := new Table_Row_Type;
         col1 : Table_Column_Access := new Table_Column_Type;
         col2 : Table_Column_Access := new Table_Column_Type;
      begin
         row.Dynamic;
         col1.Dynamic;
         col2.Dynamic;

         row.Create (Layout_Table.all);
         row.Style ("vertical-align", "top");
         col1.Create (row.all, "Message");
         col2.Create (row.all);
         View.Message.Create (Form    => View.Widget_Form,
                              Columns => 20,
                              Rows    => 10,
                              Name    => "Message");
         View.Message.Place_Inside_Top_Of (col2.all);
      end;

      View.My_Submit.Create (Form => View.Widget_Form, Value => "Submit");
      View.My_Submit.Place_After (Layout_Table.all);
   end Create;

   type App_Data is new Gnoga.Types.Connection_Data_Type with
      record
         My_View   : Gnoga.Gui.View.View_Type;
         My_Widget : My_Widget_Type;
         My_Exit   : Gnoga.Gui.Element.Common.Button_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Application event handlers

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_View.New_Line;
      App.My_View.Put_Line ("Closing application and every connection!");

      App.My_Exit.Disabled;

      Gnoga.Application.Multi_Connect.End_Application;
   end On_Exit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup GUI for each connection.

   procedure On_Result_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup another path in to the application for submitting results
   --  /result, see On_Connect_Handler in body of this procedure.

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.My_View.Create (Main_Window);

      App.My_Exit.Create (App.My_View, "Exit Application");
      App.My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);

      App.My_View.Put_HTML ("<hr>");

      App.My_Widget.Create (App.My_View);
      App.My_Widget.Widget_Form.Action ("/result");
   end On_Connect;

   procedure On_Result_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      --  Since there will be no interactions with page once displayed there
      --  is no need to setup any data to associate with the main window.

      Result_View : Gnoga.Gui.View.View_Access := new Gnoga.Gui.View.View_Type;
   begin
      Result_View.Dynamic;
      --  By marking the View dynamic it will be deallocated by Main_Window
      --  when it finalizes.
      Result_View.Create (Main_Window);

      Result_View.Put_Line ("Name : " & Main_Window.Search_Parameter ("Name"));
      Result_View.Put_Line ("Message : " &
                              Main_Window.Search_Parameter ("Message"));
   end On_Result_Connect;

begin
   Gnoga.Application.Title ("Tutorial 08");

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => On_Connect'Unrestricted_Access,
      Path  => "default");

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => On_Result_Connect'Unrestricted_Access,
      Path  => "result");

   Gnoga.Application.Multi_Connect.Message_Loop;
end Tutorial_08;
