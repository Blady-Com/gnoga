-------------------------------------------------------------------------------
-- NAME (specification)         : localize-view.ads
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : User interface display unit.
-- NOTES                        : Ada 2012, GNOGA 1.6 alpha
--
-- COPYRIGHT                    : (c) Pascal Pignard 2020
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Gnoga.Gui.Base;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Window;

package Localize.View is

   type Default_View_Type is new Gnoga.Gui.View.Grid.Grid_View_Type with record
      Main_Window      : Gnoga.Gui.Window.Pointer_To_Window_Class;
      List_Form        : Gnoga.Gui.Element.Form.Form_Type;
      Locale_List      : Gnoga.Gui.Element.Form.Selection_Type;
      Master_Path      : Gnoga.Gui.Element.Form.Text_Type;
      Locale_Path      : Gnoga.Gui.Element.Form.Text_Type;
      Load_Button      : Gnoga.Gui.Element.Form.Input_Button_Type;
      H1, H2, H3       : Gnoga.Gui.Element.Common.HR_Type;
      Keys_Label       : Gnoga.Gui.Element.Common.DIV_Type;
      Key_List         : Gnoga.Gui.Element.Form.Selection_Type;
      Save_Button      : Gnoga.Gui.Element.Form.Input_Button_Type;
      Text_Form        : Gnoga.Gui.Element.Form.Form_Type;
      Select_Pattern   : Gnoga.Gui.Element.Form.Search_Type;
      Key_Input        : Gnoga.Gui.Element.Form.Text_Type;
      Duplicate_Button : Gnoga.Gui.Element.Form.Input_Button_Type;
      Insert_Button    : Gnoga.Gui.Element.Form.Input_Button_Type;
      Delete_Button    : Gnoga.Gui.Element.Form.Input_Button_Type;
      Rename_Button    : Gnoga.Gui.Element.Form.Input_Button_Type;
      Error_Label      : Gnoga.Gui.Element.Common.DIV_Type;
      Master_Text      : Gnoga.Gui.Element.Form.Text_Area_Type;
      Master_Comment   : Gnoga.Gui.Element.Form.Text_Area_Type;
      Use_Button       : Gnoga.Gui.Element.Form.Input_Button_Type;
      Locale_Text      : Gnoga.Gui.Element.Form.Text_Area_Type;
      Locale_Comment   : Gnoga.Gui.Element.Form.Text_Area_Type;
      Exit_Button      : Gnoga.Gui.Element.Form.Input_Button_Type;
      Quit_Button      : Gnoga.Gui.Element.Form.Input_Button_Type;
      Old_Key_Index    : Natural;
   end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_to_Default_View_Class is access all Default_View_Type'Class;

   overriding procedure Create
     (Grid        : in out Default_View_Type;
      Parent      : in out Gnoga.Gui.Base.Base_Type'Class;
      Layout      : in     Gnoga.Gui.View.Grid.Grid_Rows_Type;
      Fill_Parent : in     Boolean := True; Set_Sizes : in Boolean := True;
      ID          : in     String  := "");

end Localize.View;
