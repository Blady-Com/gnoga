with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;

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
   Range_Label    : Form.Label_Type;
   Range_Value    : Form.Label_Type;

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
   F.Put (" ");

   Range_Label.Create (Form       => F,
                       Label_For  => Dec_Range,
                       Contents   => "Decimal(s)",
                       Auto_Place => False);

   Dec_Range.On_Change_Handler (Dec_Change'Unrestricted_Access);


   Gnoga.Application.Singleton.Message_Loop;
end Align;
