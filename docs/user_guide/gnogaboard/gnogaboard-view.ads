with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D;

package GnogaBoard.View is

   type Default_View_Type is new Gnoga.Gui.View.View_Type with
      record
         On_Change : Gnoga.Gui.Base.Action_Event := null;
         --  Access to the controlers On_Change event to request broadcast
         --  to all children.
         X1, Y1    : Integer;
         X2, Y2    : Integer;
         Canvas    : Gnoga.Gui.Element.Canvas.Canvas_Type;
         Context   : Gnoga.Gui.Element.Canvas.Context_2D.Context_2D_Type;
      end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_to_Default_View_Class is access all Default_View_Type'Class;

   overriding
   procedure Create
     (View    : in out Default_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     String  := "");

   procedure Draw (View : in out Default_View_Type; X1, Y1, X2, Y2 : Integer);
   --  Draw a line from X1,Y1 to X2, Y2

end GnogaBoard.View;
