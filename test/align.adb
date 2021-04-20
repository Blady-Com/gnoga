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

   procedure On_Move
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : in     Gnoga.Gui.Base.Mouse_Event_Record);
   procedure Dec_Change (Element : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure Calculate (Element : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure Radio_Select (Element : in out Gnoga.Gui.Base.Base_Type'Class);

   Main_Window : Gnoga.Gui.Window.Window_Type;
   Main_View   : Gnoga.Gui.View.View_Type;

   F : Form.Form_Type;

   Number_Choice : Form.Text_Type;
   Choice_Label  : Form.Label_Type;
   Factor_Button : Form.Submit_Button_Type;

   Calc_Dec       : Form.Check_Box_Type;
   Calc_Dec_Label : Form.Label_Type;

   Dec_Range   : Form.Range_Type;
   Range_Value : Form.Label_Type;

   Game_View   : Gnoga.Gui.View.Docker.Docker_View_Type;
   Grid_Box    : aliased Gnoga.Gui.View.View_Type;
   Control_Box : aliased Gnoga.Gui.View.View_Type;
   Mouse_Box   : Gnoga.Gui.View.View_Type;

   type Radio_Array is array (1 .. 5) of Form.Radio_Button_Type;

   Radios      : Radio_Array;
   Radio_Group : Radio_Array;

   type Button_Column is array (1 .. 15) of Common.Button_Type;
   type Button_Grid is array (1 .. 15) of Button_Column;

   Buttons : Button_Grid;

   procedure On_Move
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Event  : in     Gnoga.Gui.Base.Mouse_Event_Record)
   is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Event.X'Img & " x " & Event.Y'Img);
   end On_Move;

   procedure Dec_Change (Element : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Element);
   begin
      Range_Value.Text (Dec_Range.Value);
   end Dec_Change;

   procedure Calculate (Element : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Element);
   begin
      Main_View.Put_Line (Number_Choice.Value);
   end Calculate;

   procedure Radio_Select (Element : in out Gnoga.Gui.Base.Base_Type'Class) is
      Radio : Form.Radio_Button_Type renames Form.Radio_Button_Type (Element);
   begin
      --  On_Change for radios is only evoked when a radio is selected not
      --  deselected, which makes checking for Radio.Checked unnecessary.

      Gnoga.Log ("Radio Group #" & Radio.Value & " is now the checked radio");
   end Radio_Select;

begin
--     Gnoga.Application.Open_URL;

   Gnoga.Application.Singleton.Initialize (Main_Window, "localhost");

   Main_View.Create (Main_Window);
   Main_View.Text_Alignment (Center);

   Main_View.Put_Line ("Calculate");

   F.Create (Main_View);
   F.On_Submit_Handler (Calculate'Unrestricted_Access);

   Number_Choice.Create (Form => F, Size => 20, Value => "");
   Choice_Label.Create (Form => F, Label_For => Number_Choice, Content => "Enter a number:");
   Factor_Button.Create (Form => F, Value => "Factorial");

   F.New_Line;

   Calc_Dec.Create (Form => F);
   Calc_Dec_Label.Create (Form => F, Label_For => Calc_Dec, Content => "Calculate Decimal", Auto_Place => False);

   F.New_Line;

   Dec_Range.Create (Form => F);
   Dec_Range.Minimum (0);
   Dec_Range.Maximum (10);
   Dec_Range.Value (0);
   Range_Value.Create (Form => F, Label_For => Dec_Range, Content => "0", Auto_Place => False);
   F.Put (" Decimal(s)");

   Dec_Range.On_Change_Handler (Dec_Change'Unrestricted_Access);

   F.Horizontal_Rule;

   --  These radio buttons will operate individually since Radios function
   --  as groups based on the Name attribute

   for i in Radios'Range loop
      Radios (i).Create (F);
      F.Put ("Radio #" & i'Img);
      F.New_Line;
   end loop;

   F.Horizontal_Rule;

   --  These radio buttons will operate together since Radios function
   --  as groups based on the Name attribute, in this case "Group1"

   for i in Radio_Group'Range loop
      Radio_Group (i).Create (F, Name => "Group1", Value => i'Img);
      Radio_Group (i).On_Change_Handler (Radio_Select'Unrestricted_Access);
      F.Put ("Radio Group #" & i'Img);
      F.New_Line;
   end loop;

   F.Horizontal_Rule;

   Game_View.Create (Main_View);
   Game_View.Position (Relative);
   --  Docked views use absolute positioning which means they are relatively
   --  positioned X,Y to the first non-static position control above them
   --  in the DOM. Relative position means position X,Y from where it would
   --  be static, so this has no affect on where Game_View would be located
   --  but now that it's position is not static, docked views will be relative
   --  to it.

   Grid_Box.Create (Game_View);

   Main_Window.Buffer_Connection;

   for i in Buttons'Range loop
      for n in Buttons (i)'Range loop
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

   Main_Window.Buffer_Connection (False);

   Control_Box.Create (Game_View);
   Control_Box.Put ("<button>Push Me</button>");
   Control_Box.New_Line;
   Control_Box.Put ("<button>Push Me</button>");
   Control_Box.New_Line;
   Control_Box.Put ("<button>Push Me</button>");
   Control_Box.New_Line;

   Game_View.Left_Dock (Grid_Box'Unchecked_Access);
   Game_View.Right_Dock (Control_Box'Unchecked_Access);

   Mouse_Box.Create (Main_View);
   Mouse_Box.Position (Absolute);
   Mouse_Box.Width (200);
   Mouse_Box.Height (200);
   Mouse_Box.Top (500);
   Mouse_Box.Left (500);
   Mouse_Box.Border;
   Mouse_Box.Background_Color ("orange");
   Mouse_Box.On_Mouse_Move_Handler (On_Move'Unrestricted_Access);

   Gnoga.Application.Singleton.Message_Loop;
end Align;
