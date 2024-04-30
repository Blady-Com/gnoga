with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Base;  use Gnoga.Gui.Base;
with UxStrings;

procedure Test_Keyboard is
   Window : Gnoga.Gui.Window.Window_Type;
   View   : Gnoga.Gui.View.View_Type;

   use type Gnoga.String;

   function "&"
      (L, R : Standard.String)
       return Gnoga.String
   is (UxStrings.From_Latin_1(L & R));

   procedure On_Key_Down
     (Object         : in out Base_Type'Class;
      Keyboard_Event : in     Keyboard_Event_Record)
   is begin
      Window.On_Key_Down_Handler(null);
      View.Put("Key: """ & Keyboard_Event.Key & """");
      View.Put(", ");
      View.Put("Key Code: " & Keyboard_Event.Key_Code'Image);
      View.Put(", ");
      View.Put("Key Char: " & Keyboard_Event.Key_Char'Image);
      View.New_Line;
   end On_Key_Down;

   procedure On_Key_Up
     (Object         : in out Base_Type'Class;
      Keyboard_Event : in     Keyboard_Event_Record)
   is begin
      Window.On_Key_Down_Handler(On_Key_Down'Unrestricted_Access);
   end On_Key_Up;

begin
   Gnoga.Application.Title ("Keyboard Event Test");
   --     Gnoga.Application.Open_URL;
   Gnoga.Application.Singleton.Initialize(Window);

   View.Create(Window);

   View.Put_Line("Hello Keyboard Event Test");

   Window.On_Key_Up_Handler(On_Key_Up'Unrestricted_Access);
   Window.On_Key_Down_Handler(On_Key_Down'Unrestricted_Access);

   Gnoga.Application.Singleton.Message_Loop;
end Test_Keyboard;
