package body GnogaBoard.View is

   procedure Mouse_Down
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);
   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);
   procedure Mouse_Up
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record);

   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (View    : in out Default_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Attach  : in     Boolean := True;
      ID      : in     String  := "")
   is
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, Attach, ID);
      
      View.Canvas.Create (Parent => View,
                          Width  => 400,
                          Height => 400);
      View.Canvas.Border;

      View.Canvas.On_Mouse_Down_Handler (Mouse_Down'Access);
      
      View.Context.Get_Drawing_Context_2D (View.Canvas);
   end Create;

   ----------
   -- Draw --
   ----------
   
   procedure Draw (View : in out Default_View_Type; X1, Y1, X2, Y2 : Integer)
   is
   begin
      View.Context.Begin_Path;
      View.Context.Stroke_Color ("Black");
      View.Context.Move_To (X1, Y1);
      View.Context.Line_To (X2, Y2);
      View.Context.Stroke;
   end Draw;
   
   ----------------
   -- Mouse_Down --
   ----------------
   
   procedure Mouse_Down
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      View : Default_View_Type renames Default_View_Type (Object.Parent.all);
   begin      
      View.X1 := Mouse_Event.X;
      View.Y1 := Mouse_Event.Y;
      View.X2 := Mouse_Event.X;
      View.Y2 := Mouse_Event.Y;

      View.Canvas.On_Mouse_Move_Handler (Mouse_Move'Unrestricted_Access);
      View.Canvas.On_Mouse_Up_Handler (Mouse_Up'Unrestricted_Access);
   end Mouse_Down;

   ----------------
   -- Mouse_Move --
   ----------------
   
   procedure Mouse_Move
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      View : Default_View_Type renames Default_View_Type (Object.Parent.all);
   begin
      View.X1 := View.X2;
      View.Y1 := View.Y2;
      View.X2 := Mouse_Event.X;
      View.Y2 := Mouse_Event.Y;
      
      View.On_Change (View);      
   end Mouse_Move;

   --------------
   -- Mouse_Up --
   --------------
   
   procedure Mouse_Up
     (Object      : in out Gnoga.Gui.Base.Base_Type'Class;
      Mouse_Event : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      use Gnoga.Gui.Element.Canvas.Context_2D;

      View : Default_View_Type renames Default_View_Type (Object.Parent.all);
   begin
      View.X1 := View.X2;
      View.Y1 := View.Y2;
      View.X2 := Mouse_Event.X;
      View.Y2 := Mouse_Event.Y;
      
      View.On_Change (View);

      View.Canvas.On_Mouse_Move_Handler (null);
      View.Canvas.On_Mouse_Up_Handler (null);
   end Mouse_Up; 
end GnogaBoard.View;
