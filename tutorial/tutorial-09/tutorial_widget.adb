with Gnoga.Gui.Element.Table;

package body Tutorial_Widget is

   overriding procedure Create
     (View   : in out My_Widget_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String := "")
   is
      use Gnoga.Gui.Element.Table;
      Layout_Table : constant Table_Access := new Table_Type;
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, ID);

      View.Widget_Form.Create (View);

      Layout_Table.Dynamic;
      Layout_Table.Create (View);

      declare
         row  : constant Table_Row_Access    := new Table_Row_Type;
         col1 : constant Table_Column_Access := new Table_Column_Type;
         col2 : constant Table_Column_Access := new Table_Column_Type;
      begin
         row.Dynamic;
         col1.Dynamic;
         col2.Dynamic;

         row.Create (Layout_Table.all);
         col1.Create (row.all, "Name");
         col2.Create (row.all);
         View.Name_Input.Create (Form => View.Widget_Form, Size => 40, Name => "Name");
         View.Name_Input.Required;
         View.Name_Input.Place_Holder ("(Only letters and spaces permitted)");
         View.Name_Input.Pattern ("[a-zA-Z\ ]+");

         View.Name_Input.Place_Inside_Top_Of (col2.all);
      end;

      declare
         row  : constant Table_Row_Access    := new Table_Row_Type;
         col1 : constant Table_Column_Access := new Table_Column_Type;
         col2 : constant Table_Column_Access := new Table_Column_Type;
      begin
         row.Dynamic;
         col1.Dynamic;
         col2.Dynamic;

         row.Create (Layout_Table.all);
         row.Style ("vertical-align", "top");
         col1.Create (row.all, "Message");
         col2.Create (row.all);
         View.Message.Create (Form => View.Widget_Form, Columns => 20, Rows => 10, Name => "Message");
         View.Message.Place_Inside_Top_Of (col2.all);
      end;

      View.My_Submit.Create (Form => View.Widget_Form, Value => "Submit");
      View.My_Submit.Place_After (Layout_Table.all);
   end Create;

end Tutorial_Widget;
