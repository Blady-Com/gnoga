with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Multimedia;
with Gnoga.Types;

procedure Media is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;
   use all type Gnoga.String;

   type App_Data is new Connection_Data_Type with record
      Main_Window : Window.Pointer_To_Window_Class;
      Audio1      : Multimedia.Audio_Type;
      Video1      : Multimedia.Video_Type;
   end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Progress (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Audio1.Play;
      Gnoga.Log ("Current position : " & Image (App.Audio1.Media_Position));
      Gnoga.Log ("Duration : " & Image (App.Audio1.Media_Duration));
      Gnoga.Log ("Playback rate : " & Image (App.Audio1.Playback_Rate));
      App.Audio1.Media_Position (0.0);
      App.Audio1.Playback_Rate (App.Audio1.Playback_Rate * 1.25);
   end On_Click;

   procedure On_Progress (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Gnoga.Log ("Progress position : " & Image (App.Audio1.Media_Position));
   end On_Progress;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App  : constant App_Access := new App_Data;
      Play : Common.Button_Type;
      View : Gnoga.Gui.View.View_Type;
   begin
      Main_Window.Connection_Data (App);

      App.Main_Window := Main_Window'Unchecked_Access;

      View.Create (Main_Window);

      Play.Create (View, "Play");
      Play.On_Click_Handler (On_Click'Unrestricted_Access);

      View.New_Line;

      App.Audio1.Create
        (Parent  => View, Source => "http://www.teachittome.com/misc/IgeretHaramban.mp3", Controls => False,
         Preload => True);
      App.Audio1.On_Progress_Handler (On_Progress'Unrestricted_Access);

      Gnoga.Log ("The browser can play MP3s? - " & Image (App.Audio1.Can_Play ("audio/mp3")));
      Gnoga.Log ("Audio source is : " & App.Audio1.Media_Source);

      View.New_Line;

      App.Video1.Create (Parent => View, Source => "http://www.mindbodycures.com/archive/Ankle.mp4", Controls => True);

      Connection.Hold;
   end On_Connect;

begin
   Application.Multi_Connect.Initialize (Event => On_Connect'Unrestricted_Access);

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close ("<b>Connection to Application has been terminated</b>");

--     Application.Open_URL;

   Application.Multi_Connect.Message_Loop;
end Media;
