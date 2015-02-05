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

   Form_Grid   : Gnoga.Gui.View.Grid.Grid_View_Type;
   L1, L2, L3  : Common.Span_Type;
   B1, B2, B3  : Common.Button_Type;

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

   Gnoga.Application.Singleton.Initialize (Main_Window, Boot => "ajax.html");

   Main_View.Create (Main_Window,
                     ((COL, SPN),
                      (COL, COL),
                      (COL, SPN)),
                     Set_Sizes => True);

   Main_View.Panel (1, 1).Box_Height ("10%");
   Main_View.Panel (1, 1).Background_Color (Pink);

   --  Demonstrate packing left and right

   Common.Button_Access
     (Main_View.Panel (1, 1).New_Element
      ("Menu_Item_1", new Common.Button_Type)).Create
       (Main_View.Panel (1, 1).all, "Item 1");

   Common.Button_Access
     (Main_View.Panel (1, 1).New_Element
      ("Menu_Item_2", new Common.Button_Type)).Create
       (Main_View.Panel (1, 1).all, "Item 2");

   Common.Button_Access
     (Main_View.Panel (1, 1).New_Element
      ("Menu_Item_3", new Common.Button_Type)).Create
       (Main_View.Panel (1, 1).all, "Item 3");

   Common.Button_Access
     (Main_View.Panel (1, 1).New_Element
      ("Menu_Item_4", new Common.Button_Type)).Create
       (Main_View.Panel (1, 1).all, "Item 4");

   Common.Button_Access
     (Main_View.Panel (1, 1).New_Element
      ("Menu_Item_5", new Common.Button_Type)).Create
       (Main_View.Panel (1, 1).all, "Item 5");

   Main_View.Panel (1, 1).Element ("Menu_Item_4").Layout_Float (Right);
   Main_View.Panel (1, 1).Element ("Menu_Item_5").Layout_Float (Right);

   Main_View.Panel (2, 1).Box_Height ("80%");
   Main_View.Panel (2, 1).Background_Color (Orange);
   Main_View.Panel (2, 1).Overflow (Auto);
   Main_View.Panel (2, 1).Box_Width ("30%");
   Main_View.Panel (2, 1).Resizable (Horizontal);

   Form_Grid.Create (Main_View.Panel (2, 1).all,
                     ((COL, COL),
                      (COL, COL),
                      (COL, COL)),
                     Fill_Parent => False,
                     Set_Sizes => False);

   Form_Grid.Panel (1, 1).Padding ("5px", "5px", "5px", "5px");
   Form_Grid.Panel (2, 1).Padding ("5px", "5px", "5px", "5px");
   Form_Grid.Panel (3, 1).Padding ("5px", "5px", "5px", "5px");

   Form_Grid.Panel (1, 2).Vertical_Align (Middle);
   Form_Grid.Panel (2, 2).Vertical_Align (Middle);
   Form_Grid.Panel (3, 2).Vertical_Align (Middle);

   L1.Create (Form_Grid.Panel (1, 1).all, "Label 1");
   L2.Create (Form_Grid.Panel (2, 1).all, "Label 2");
   L3.Create (Form_Grid.Panel (3, 1).all, "Label 3");

   B1.Create (Form_Grid.Panel (1, 2).all, "Button 1");
   B2.Create (Form_Grid.Panel (2, 2).all, "Button 2");
   B3.Create (Form_Grid.Panel (3, 2).all, "Button 3");

   Main_View.Panel (3, 1).Box_Height ("10%");
   Main_View.Panel (3, 1).Background_Color (Blue);

   Split_View.Create (Main_View.Panel (2, 2).all, Vertical_Split);

   Split_View.Panel (1, 1).Background_Color (Aqua);
   Split_View.Panel (2, 1).Background_Color (Brown);

   A_View.Create (Split_View.Panel (1, 1).all);
   A_View.Fill_Parent;
   Gnoga.Log (A_View.Position'Img);

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
