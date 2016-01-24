with Gnoga.Gui.View;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Form;

package Gnoga_Extras is

   type Labeled_Range_Type is new Gnoga.Gui.Element.Form.Range_Type with record
      Label : Gnoga.Gui.Element.Form.Label_Type;
   end record;

   type Labeled_Check_Box_Type is new Gnoga.Gui.Element.Form
     .Check_Box_Type with
   record
      Label : Gnoga.Gui.Element.Form.Label_Type;
   end record;

   type View_Type is new Gnoga.Gui.View.View_Type with private;

   overriding procedure Hidden
     (Element : in out View_Type;
      Value   : in     Boolean := True);

   procedure On_Show_Handler
     (Object  : in out View_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event);
   procedure On_Hide_Handler
     (Object  : in out View_Type;
      Handler : in     Gnoga.Gui.Base.Action_Event);

private
   type View_Type is new Gnoga.Gui.View.View_Type with record
      Show_Handler : Gnoga.Gui.Base.Action_Event;
      Hide_Handler : Gnoga.Gui.Base.Action_Event;
   end record;
end Gnoga_Extras;
