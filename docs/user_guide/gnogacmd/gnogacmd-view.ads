with Gnoga.Gui.Base;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Element.Form;

package GnogaCMD.View is
   
   type Default_View_Type is new Gnoga.Gui.View.Console.Console_View_Type with
      record
         Entry_Form : Gnoga.Gui.Element.Form.Form_Type;
         Prompt     : Gnoga.Gui.Element.Form.Label_Type;
         Cmd_Line   : Gnoga.Gui.Element.Form.Text_Type;
         Go_Button  : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_to_Default_View_Class is access all Default_View_Type'Class;

   overriding
   procedure Create
     (View    : in out Default_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach  : in     Boolean := True;
      ID      : in     String  := "");

end GnogaCMD.View;
