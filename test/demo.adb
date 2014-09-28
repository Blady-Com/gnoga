with Gnoga.Application.Multiuser;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Multimedia;
with Gnoga.Types;


procedure Demo is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Audio1      : Multimedia.Audio_Type;
         Video1      : Multimedia.Video_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Audio1.Play;
   end On_Click;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type)
   is
      App     : aliased App_Data;
      Play    : Common.Button_Type;
      View    : Gnoga.Gui.View.View_Type;
   begin
      App.Main_Window := Main_Window'Unchecked_Access;

      View.Create (Main_Window);

      Play.Create (View, "Play");
      Play.On_Click_Handler (On_Click'Unrestricted_Access);

      View.New_Line;

      App.Audio1.Create (Parent   => View,
                         Source   => "http://www.teachittome.com/misc/IgeretHaramban.mp3",
                         Controls => False,
                         Preload  => True);

      View.New_Line;

      App.Video1.Create (Parent   => View,
                         Source   => "http://www.mindbodycures.com/archive/Ankle.mp4",
                         Controls => True);

      Application.Multiuser.Connection_Data (Main_Window, App'Unchecked_Access);

      Connection.Hold;
   end On_Connect;

begin
   Application.Multiuser.Initialize (Event => On_Connect'Unrestricted_Access,
                                     Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multiuser.Message_Loop;
end Demo;
