with Gnoga.Application.Multiuser;
with Gnoga.Window;
with Gnoga.Base;
with Gnoga.Element;
with Gnoga.Element.Common;
with Gnoga.Types;

procedure Multiuser is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Element;

   task type Color_Me_Task (O : Element.Pointer_To_Element_Class) is
      entry Start;
      entry Stop;
   end Color_Me_Task;

   task body Color_Me_Task is
      type Colors is (red, green, blue, orange, black);

      Current_Color : Colors := Colors'First;
   begin
      accept Start;

      loop
         begin
            if O.Valid then
               O.Color (Current_Color'Img);

               if Current_Color = Colors'Last then
                  Current_Color := Colors'First;
               else
                  Current_Color := Colors'Succ (Current_Color);
               end if;
            end if;
         end;
         select
            accept Stop;
            exit;
         or
            delay 0.1;
         end select;
      end loop;
end Color_Me_Task;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Hello_World : aliased Common.DIV_Type;
         Click_Quit  : Common.DIV_Type;
         X           : Common.DIV_Type;
         Y           : Common.DIV_Type;
         Key         : Common.DIV_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Base.Base_Type'Class;
                       Event  : in     Gnoga.Base.Mouse_Event_Record)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Hello_World.Color ("green");
      App.Hello_World.Background_Color (RGBA_Type'(255,255,255,1.0));
      App.Main_Window.Log
        ("Color = " & Gnoga.Types.To_String (App.Hello_World.Color));
      App.Main_Window.Alert ("X = " & Event.X'Img & " Y = " & Event.Y'Img);
   end On_Click;

   procedure On_Key_Press (Object : in out Gnoga.Base.Base_Type'Class;
                           Event  : in     Gnoga.Base.Keyboard_Event_Record)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Key.Text (Event.Key_Code'Img);
   end On_Key_Press;

   procedure On_Move (Object : in out Gnoga.Base.Base_Type'Class;
                      Event  : in     Gnoga.Base.Mouse_Event_Record)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.X.Text (Event.X'Img);
      App.Y.Text (Event.Y'Img);
   end On_Move;

   procedure On_Context (Object : in out Gnoga.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Hello_World.Color ("red");
   end On_Context;

   procedure End_App (Object : in out Gnoga.Base.Base_Type'Class) is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Window.Document.Body_Element.Inner_HTML ("Application closed.");

      Log ("Ending Application");
      Application.Multiuser.End_Application;
   end End_App;

   procedure On_Connect
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      App : aliased App_Data;

      Color_Me : Color_Me_Task (App.Hello_World'Unchecked_Access);

      Hr1 : Gnoga.Element.Common.HR_Type;
      Hr2 : Gnoga.Element.Common.HR_Type;
      Lnk : Gnoga.Element.Common.A_Type;
      Img : Gnoga.Element.Common.IMG_Type;
   begin
      App.Main_Window := Main_Window'Unchecked_Access;

      App.Hello_World.Create (Main_Window);
      App.Hello_World.Inner_HTML ("<h1>Multiuser App Demo</h1>");
      App.Hello_World.Place_Inside_Top_Of
        (Main_Window.Document.Body_Element.all);

      App.Hello_World.On_Context_Menu_Handler (On_Context'Unrestricted_Access);
      App.Hello_World.On_Mouse_Click_Handler (On_Click'Unrestricted_Access);
      App.Hello_World.On_Mouse_Move_Handler (On_Move'Unrestricted_Access);

      Hr1.Create (Main_Window);
      Hr1.Place_After (App.Hello_World);

      App.Click_Quit.Create (Main_Window, "Click to close down server");
      App.Click_Quit.Place_After (Hr1);
      App.Click_Quit.On_Click_Handler (End_App'Unrestricted_Access);

      App.X.Create (Main_Window);
      App.Y.Create (Main_Window);
      App.Key.Create (Main_Window);

      App.X.Place_After (App.Hello_World);
      App.Y.Place_After (App.X);
      App.Key.Place_After (App.Y);

      Lnk.Create (Parent  => Main_Window,
                  Link    => "http://www.gnoga.com",
                  Content => "Gnoga Home Page",
                  Target  => "_blank");
      Lnk.Place_After (App.Click_Quit);

      Hr2.Create (Main_Window);
      Hr2.Place_After (Lnk);

      Img.Create
        (Parent           => Main_Window,
         URL_Source       => "http://www.gnu.org/graphics/gplv3-127x51.png",
         Alternative_Text => "GNAT Modified GNU GPL 3");
      Img.Place_After (Hr2);

      App.Main_Window.On_Key_Down_Handler (On_Key_Press'Unrestricted_Access);

      Application.Multiuser.Connection_Data (Main_Window, App'Unchecked_Access);

      Color_Me.Start;

      Connection.Hold;

      Color_Me.Stop;
   end On_Connect;

   procedure On_Connect_2
     (Main_Window : in out Gnoga.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      D : Gnoga.Element.Common.DIV_Type;
   begin
      Main_Window.Document.Body_Element.Background_Color ("blue");

      D.Create (Main_Window, "This is on another path in same application.");
      D.Color ("yellow");
      D.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);
   end On_Connect_2;

begin
   Application.Multiuser.Initialize (Boot => "debug.html");

   Application.Multiuser.On_Connect_Handler (On_Connect'Unrestricted_Access,
                                             "default");
   Application.Multiuser.On_Connect_Handler (On_Connect_2'Unrestricted_Access,
                                             "/demo");

   Application.Multiuser.Message_Loop;
end Multiuser;
