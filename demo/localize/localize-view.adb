-------------------------------------------------------------------------------
-- NAME (body)                  : localize-view.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : User interface display unit.
-- NOTES                        : Ada 2012, GNOGA 1.6 alpha
--
-- COPYRIGHT                    : (c) Pascal Pignard 2020
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

package body Localize.View is

   ------------
   -- Create --
   ------------

   overriding procedure Create
     (Grid        : in out Default_View_Type;
      Parent      : in out Gnoga.Gui.Base.Base_Type'Class;
      Layout      : in     Gnoga.Gui.View.Grid.Grid_Rows_Type;
      Fill_Parent : in     Boolean := True; Set_Sizes : in Boolean := True;
      ID          : in     String  := "")
   is
   begin
      Gnoga.Gui.View.Grid.Grid_View_Type (Grid).Create
        (Parent, Layout, Fill_Parent, Set_Sizes, ID);
      Grid.Panel (1, 1).Border;

      Grid.List_Form.Create (Grid.Panel (1, 1).all);
      Grid.List_Form.Put ("Master strings file path:");
      Grid.Master_Path.Create (Grid.List_Form, 40);
      Grid.List_Form.New_Line;
      Grid.List_Form.Put ("Locale strings file path:");
      Grid.Locale_Path.Create (Grid.List_Form, 40);
      Grid.Load_Button.Create (Grid.List_Form, "Load both strings files");
      Grid.H1.Create (Grid.List_Form);
      Grid.Keys_Label.Create (Grid.List_Form, "Keys (?):");
      Grid.Key_List.Create (Grid.List_Form, Visible_Lines => 20);
      Grid.List_Form.New_Line;
      Grid.List_Form.Put_Line
        ("(# Master key only, @ Locale key only, * Locale key modified)");
      Grid.Save_Button.Create (Grid.List_Form, "Save locale strings file");
      Grid.List_Form.New_Line;
      Grid.Exit_Button.Create (Grid.List_Form, "Disconnect");
      Grid.List_Form.New_Line;
      Grid.Quit_Button.Create (Grid.List_Form, "End Localize server");

      Grid.Panel (1, 2).Border;
      Grid.Text_Form.Create (Grid.Panel (1, 2).all);
      Grid.Text_Form.Put ("Select keys: ");
      Grid.Select_Pattern.Create (Grid.Text_Form);
      Grid.Text_Form.New_Line;
      Grid.Text_Form.Put ("Key: ");
      Grid.Key_Input.Create (Grid.Text_Form);
      Grid.Text_Form.New_Line;
      Grid.Duplicate_Button.Create (Grid.Text_Form, "Duplicate");
      Grid.Duplicate_Button.Advisory_Title ("Duplicate from master to locale");
      Grid.Insert_Button.Create (Grid.Text_Form, "Insert");
      Grid.Delete_Button.Create (Grid.Text_Form, "Delete");
      Grid.Rename_Button.Create (Grid.Text_Form, "Rename");
      Grid.Text_Form.New_Line;
      Grid.Error_Label.Create (Grid.Text_Form, "No error.");
      Grid.H2.Create (Grid.Text_Form);
      Grid.Text_Form.Put_Line ("Master text:");
      Grid.Master_Text.Create (Grid.Text_Form, 40, 4);
      Grid.Master_Text.Read_Only;
      Grid.Text_Form.Put_Line ("Locale text:");
      Grid.Locale_Text.Create (Grid.Text_Form, 40, 4);
      Grid.H3.Create (Grid.Text_Form);
      Grid.Text_Form.Put_Line ("Master comment:");
      Grid.Master_Comment.Create (Grid.Text_Form, 40, 4);
      Grid.Master_Comment.Read_Only;
      Grid.Text_Form.Put_Line ("Locale comment:");
      Grid.Locale_Comment.Create (Grid.Text_Form, 40, 4);
   end Create;

end Localize.View;
