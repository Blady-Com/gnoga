with Gnoga.Gui.Base;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Plugin.Pixi.Sprite;
with Gnoga.Gui.Plugin.Ace_Editor.Console_IO;

package Logo.View is

   type Default_View_Type is new Gnoga.Gui.View.Grid.Grid_View_Type with record
      Label_Text   : Gnoga.Gui.View.View_Type;
      Click_Button : Gnoga.Gui.Element.Common.Button_Type;
      Canvas       : Gnoga.Gui.Element.Canvas.Canvas_Type;
      Renderer     : Gnoga.Gui.Plugin.Pixi.Renderer_Type;
      Container    : Gnoga.Gui.Plugin.Pixi.Container_Type;
      Turtle       : Gnoga.Gui.Plugin.Pixi.Sprite.Sprite_Type;
      Console      : Gnoga.Gui.Plugin.Ace_Editor.Console_IO.Console_IO_Type;
   end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_to_Default_View_Class is access all Default_View_Type'Class;

   overriding procedure Create
     (Grid        : in out Default_View_Type;
      Parent      : in out Gnoga.Gui.Base.Base_Type'Class;
      Layout      : in     Gnoga.Gui.View.Grid.Grid_Rows_Type;
      Fill_Parent : in     Boolean := True;
      Set_Sizes   : in     Boolean := True;
      ID          : in     String  := "");

end Logo.View;
