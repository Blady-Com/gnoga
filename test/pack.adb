with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Grid;
with Gnoga.Types.Colors;

procedure Pack is
   use Gnoga.Gui.Element;
   use Gnoga.Gui.View.Grid;
   use Gnoga.Types.Colors;

   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.Grid.Grid_View_Type;
   Split_View  : Gnoga.Gui.View.Grid.Grid_View_Type;

   A_View   : Gnoga.Gui.View.View_Type;
   A_Button : Common.Button_Type;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      Main_View.Panel (2, 1).Box_Width ("10%");
      Split_View.Panel (1, 1).Box_Height ("90%");
      Split_View.Panel (2, 1).Background_Color (Yellow_Green);
   end On_Click;
begin
   Gnoga.Application.Open_URL;

   Gnoga.Application.Singleton.Initialize (Main_Window);

   Main_View.Create (Main_Window,
                     ((COL, SPN),
                      (COL, COL),
                      (COL, SPN)),
                     Set_Sizes => True);

   Main_View.Panel (1, 1).Box_Height ("10%");
   Main_View.Panel (1, 1).Background_Color (Pink);

   Main_View.Panel (2, 1).Box_Height ("80%");
   Main_View.Panel (2, 1).Background_Color (Orange);
   Main_View.Panel (2, 1).Overflow (Auto);
   Main_View.Panel (2, 1).Box_Width ("30%");
   Main_View.Panel (2, 1).Resizable (Horizontal);

   Main_View.Panel (3, 1).Box_Height ("10%");
   Main_View.Panel (3, 1).Background_Color (Blue);

   Split_View.Create (Main_View.Panel (2, 2).all, Vertical_Split);

   Split_View.Panel (1, 1).Background_Color (Yellow);
   Split_View.Panel (2, 1).Background_Color (Brown);

   A_View.Create (Split_View.Panel (1, 1).all);
   A_View.Position (Absolute);
   --  By setting a child view to Absolute it's "world" is the next Element
   --  above it in the DOM that is not position:static. In the case of grids
   --  the cells are set to position:relative to insure that position:absolute
   --  is in relation to the cell. The benefit of this is that now setting
   --  a height of 100% will work since the layout engine knows the height
   --  bounds of the cell. Normally on an HTML page setting height to 100%
   --  does nothing.
   A_View.Box_Height ("100%");
   A_View.Box_Width ("100%");
   A_View.Box_Sizing (Border_Box);
   A_View.Border;
   --  Note that border extends beyond the regular box if box model is not
   --  set to Border_Box since then border is outside "Content Box"

   A_View.Text_Alignment (Center);

   A_View.New_Line;

   A_Button.Create (A_View, "Press Me");
   A_Button.On_Click_Handler (On_Click'Unrestricted_Access);

   Gnoga.Application.Singleton.Message_Loop;
end Pack;
