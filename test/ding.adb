with Gnoga.Application.Singleton;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Multimedia;
with Gnoga.Gui.View;
with Gnoga.Gui.Window;

procedure Ding is
   use Gnoga;
   use all type Gnoga.String;

   Window : Gnoga.Gui.Window.Window_Type;
   View   : Gnoga.Gui.View.View_Type;
   Player : Gnoga.Gui.Element.Multimedia.Audio_Type;
   Play   : Gnoga.Gui.Element.Common.Button_Type;
   Quit   : Gnoga.Gui.Element.Common.Button_Type;

   procedure Sound_Duration (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   --  Empty;
   begin -- Sound_Duration
      Gnoga.Log (Message => "Sound_Duration: " & Player.Media_Source & Image (Player.Media_Duration));
   exception -- Sound_Duration
      when E : others =>
         Gnoga.Log (Message => "Sound_Duration: ", Occurrence => E);
   end Sound_Duration;

   procedure Play_Sound (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   --  Empty;
   begin -- Play_Sound
      Player.Media_Source (Source => "glass.ogg");

      Wait_For_Ready :
      loop
         Gnoga.Log (Message => Image (Integer'(Player.Property ("readyState"))) & ' ' & Image (Player.Ready_To_Play));

         exit Wait_For_Ready when Player.Ready_To_Play;

         delay 0.01;
      end loop Wait_For_Ready;

      Gnoga.Log (Message => "Play_Sound: " & Player.Media_Source & Image (Player.Media_Duration));
      Player.Play;
   exception -- Play_Sound
      when E : others =>
         Gnoga.Log (Message => "Play_Sound: ", Occurrence => E);
   end Play_Sound;

   procedure Quit_Now (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   --  Empty;
   begin -- Quit_Now
      Gnoga.Application.Singleton.End_Application;
   exception -- Quit_Now
      when E : others =>
         Gnoga.Log (Message => "Quit_Now: ", Occurrence => E);
   end Quit_Now;
begin -- Ding
   Gnoga.Application.Title ("Ding");
   Gnoga.Application.HTML_On_Close ("Ding ended.");
   Gnoga.Application.Singleton.Initialize (Main_Window => Window);
   View.Create (Parent => Window);
   View.Text_Alignment (Value => Gnoga.Gui.Element.Center);
   Player.Create (Parent => View, Preload => True);
   Player.On_Duration_Change_Handler (Handler => Sound_Duration'Unrestricted_Access);
   View.New_Line;
   Play.Create (Parent => View, Content => "Play");
   Play.On_Click_Handler (Handler => Play_Sound'Unrestricted_Access);
   Quit.Create (Parent => View, Content => "Quit");
   Quit.On_Click_Handler (Handler => Quit_Now'Unrestricted_Access);
   Gnoga.Application.Singleton.Message_Loop;
exception -- Ding
   when E : others =>
      Gnoga.Log (E);
end Ding;
