with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View;

package Tutorial_Widget is
   -------------------------------------------------------------------------
   --  My_Widget_Type
   -------------------------------------------------------------------------

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
end Tutorial_Widget;
