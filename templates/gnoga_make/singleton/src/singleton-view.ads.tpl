with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;

package @@data.App_Name@@.View is
   
   type Default_View_Type is new Gnoga.Gui.View.View_Type with
      record
         Label_Text   : Gnoga.Gui.View.View_Type;
         Click_Button : Gnoga.Gui.Element.Common.Button_Type;
      end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_to_Default_View_Class is access all Default_View_Type'Class;

   overriding
   procedure Create
     (View    : in out Default_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach  : in     Boolean := True;
      ID      : in     String  := "");     
   
end @@data.App_Name@@.View;
