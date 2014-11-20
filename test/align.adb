with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.View.Docker;

procedure Align is
   use Gnoga.Gui.Element;

   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.View_Type;

   F             : Form.Form_Type;

   Number_Choice : Form.Text_Type;
   Choice_Label  : Form.Label_Type;
   Factor_Button : Form.Submit_Button_Type;

   Calc_Dec       : Form.Check_Box_Type;
   Calc_Dec_Label : Form.Label_Type;

   Dec_Range      : Form.Range_Type;
   Range_Value    : Form.Label_Type;

   Game_View   : Gnoga.Gui.View.Docker.Docker_View_Type;
   Grid_Box    : aliased Gnoga.Gui.View.View_Type;
   Control_Box : aliased Gnoga.Gui.View.View_Type;

   type Button_Column is array (1 .. 30) of Common.Button_Type;
   type Button_Grid is array (1 .. 15) of Button_Column;

   Buttons : Button_Grid;

   procedure Dec_Change (Element : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      Range_Value.Text (Dec_Range.Value);
   end Dec_Change;

   procedure Calculate (Element : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      Main_View.Put_Line (Number_Choice.Value);
   end Calculate;
begin
   Gnoga.Application.Open_URL_OSX;
   Gnoga.Application.Singleton.Initialize (Main_Window);

   Main_View.Create (Main_Window);
   Main_View.Text_Alignment (Center);

   Main_View.Put_Line ("Calculate");

   F.Create (Main_View);
   F.On_Submit_Handler (Calculate'Unrestricted_Access);

   Number_Choice.Create (Form  => F,
                         Size  => 20,
                         Value => "");
   Choice_Label.Create (Form       => F,
                        Label_For  => Number_Choice,
                        Contents   => "Enter a number:");
   Factor_Button.Create (Form  => F,
                         Value => "Factorial");

   F.New_Line;

   Calc_Dec.Create (Form => F);
   Calc_Dec_Label.Create (Form       => F,
                          Label_For  => Calc_Dec,
                          Contents   => "Calculate Decimal",
                          Auto_Place => False);

   F.New_Line;

   Dec_Range.Create (Form  => F);
   Dec_Range.Minimum ("0");
   Dec_Range.Maximum ("10");
   Dec_Range.Value (0);
   Range_Value.Create (Form       => F,
                       Label_For  => Dec_Range,
                       Contents   => "0",
                       Auto_Place => False);
   F.Put (" Decimal(s)");

   Dec_Range.On_Change_Handler (Dec_Change'Unrestricted_Access);

   F.Horizontal_Rule;

   Game_View.Create (Main_View);
   Game_View.Position (Relative);
   --  Docked views use absolute positioning which means they are relatively
   --  positioned X,Y to the first non-static position control above them
   --  in the DOM. Relative position means position X,Y from where it would
   --  be static, so this has no affect on where Game_View would be located
   --  but now that it's position is not statict docked views will be relative
   --  to it.

   Grid_Box.Create (Game_View);

   for i in Buttons'First .. Buttons'Last loop
      for n in Buttons (i)'First .. Buttons (i)'Last loop
         Buttons (i) (n).Create (Grid_Box, "1");
         Buttons (i) (n).Font (Family => "monospace", Height => "11px");
         Buttons (i) (n).Background_Color ("lightgray");
         Buttons (i) (n).Vertical_Align (Middle);
         Buttons (i) (n).Minimum_Width (25);
         Buttons (i) (n).Maximum_Width (25);
         Buttons (i) (n).Minimum_Height (25);
         Buttons (i) (n).Maximum_Height (25);
         Buttons (i) (n).Text_Alignment (Center);
         Buttons (i) (n).Overflow (Hidden);
         Buttons (i) (n).Margin ("1px", "1px", "1px", "1px");
      end loop;
      Grid_Box.New_Line;
   end loop;

   Control_Box.Create (Game_View);
   Control_Box.Put ("<button>Push Me</button>"); Control_Box.New_Line;
   Control_Box.Put ("<button>Push Me</button>"); Control_Box.New_Line;
   Control_Box.Put ("<button>Push Me</button>"); Control_Box.New_Line;

   Game_View.Left_Dock (Grid_Box'Unchecked_Access);
   Game_View.Right_Dock (Control_Box'Unchecked_Access);


   Gnoga.Application.Singleton.Message_Loop;
end Align;
