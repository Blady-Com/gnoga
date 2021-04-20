with Ada.Numerics.Discrete_Random;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Types;
with Gnoga.Gui.View;
with Gnoga.Gui.Window;

package body Random_Int.UI is
   use Gnoga;
   use all type Gnoga.String;

   type App_Info is new Gnoga.Types.Connection_Data_Type with record
      Window    : Gnoga.Gui.Window.Pointer_To_Window_Class;
      View      : Gnoga.Gui.View.View_Type;
      Min_Label : Gnoga.Gui.Element.Form.Label_Type;
      Max_Label : Gnoga.Gui.Element.Form.Label_Type;
      Input     : Gnoga.Gui.Element.Form.Form_Type;
      Min_Entry : Gnoga.Gui.Element.Form.Number_Type;
      Max_Entry : Gnoga.Gui.Element.Form.Number_Type;
      Result    : Gnoga.Gui.Element.Form.Text_Type;
      Generate  : Gnoga.Gui.Element.Form.Submit_Button_Type;
      Quit      : Gnoga.Gui.Element.Common.Button_Type;
   end record;

   type App_Ptr is access all App_Info;

   procedure On_Generate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      Low    : Integer;
      High   : Integer;
      Result : Integer;
   begin -- On_Generate
      Min_Error :
      begin
         Low := Value (App.Min_Entry.Value);
      exception -- Min_Error
         when others =>
            App.Min_Entry.Value (Value => "Error");

            return;
      end Min_Error;

      Max_Error :
      begin
         High := Value (App.Max_Entry.Value);
      exception -- Max_Error
         when others =>
            App.Max_Entry.Value (Value => "Error");

            return;
      end Max_Error;

      if High < Low then
         Result := High;
         High   := Low;
         Low    := Result;
      end if;

      App.Min_Entry.Value (Value => Low);
      App.Max_Entry.Value (Value => High);

      Get_Value :
      declare
         subtype Desired is Integer range Low .. High;

         package Random is new Ada.Numerics.Discrete_Random (Result_Subtype => Desired);

         Gen : Random.Generator;
      begin -- Get_Value
         Random.Reset (Gen => Gen);
         Result := Random.Random (Gen);
         App.Result.Value (Value => Image (Result));
      end Get_Value;
   exception -- On_Generate
      when E : others =>
         Gnoga.Log (Message => "On_Generate: ", Occurrence => E);
   end On_Generate;

   End_Message : constant Gnoga.String := "Random Integers ended.";

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Ptr := App_Ptr (Object.Connection_Data);

      View : Gnoga.Gui.View.View_Type;
   begin -- On_Quit
      App.View.Remove;
      View.Create (Parent => App.Window.all);
      View.Put_Line (Message => End_Message);
      App.Window.Close;
      App.Window.Close_Connection;
   exception -- On_Quit
      when E : others =>
         Gnoga.Log (Message => "On_Quit: ", Occurrence => E);
   end On_Quit;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      Placeholder : constant Gnoga.String := "Enter an integer";

      App : constant App_Ptr := new App_Info;
   begin -- On_Connect
      Main_Window.Connection_Data (Data => App);
      App.Window := Main_Window'Unchecked_Access;
      App.View.Create (Parent => Main_Window);
      App.View.Box_Width (Value => 500);
      App.Input.Create (Parent => App.View);
      App.Input.Text_Alignment (Value => Gnoga.Gui.Element.Right);
      App.Min_Entry.Create (Form => App.Input);
      App.Min_Entry.Required;
      App.Min_Entry.Place_Holder (Value => Placeholder);
      App.Min_Label.Create (Form => App.Input, Label_For => App.Min_Entry, Content => "Minimum value");
      App.Input.New_Line;
      App.Max_Entry.Create (Form => App.Input);
      App.Max_Entry.Required;
      App.Max_Entry.Place_Holder (Value => Placeholder);
      App.Max_Label.Create (Form => App.Input, Label_For => App.Max_Entry, Content => "Maximum value");
      App.Input.New_Line;
      App.Result.Create (Form => App.Input);
      App.Generate.Create (Form => App.Input, Value => "Generate");
      App.Input.On_Submit_Handler (Handler => On_Generate'Access);
      App.Input.New_Line;
      App.Input.New_Line;
      App.Quit.Create (Parent => App.Input, Content => "Quit");
      App.Quit.On_Click_Handler (Handler => On_Quit'Access);
   exception -- On_Connect
      when E : others =>
         Gnoga.Log (Message => "On_Connect: ", Occurrence => E);
   end On_Connect;
begin -- Random_Int.UI
   Gnoga.Application.Title (Name => "Random Integers");
   Gnoga.Application.HTML_On_Close (HTML => End_Message);
   Gnoga.Application.Multi_Connect.Initialize;
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Access);
   Gnoga.Application.Multi_Connect.Message_Loop;
exception -- Random_Int.UI
   when E : others =>
      Gnoga.Log (E);
end Random_Int.UI;
