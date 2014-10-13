with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.Base;
with Gnoga.Types;
with Gnoga.Gui.Plugin.Ace_Editor;

procedure AdaEdit is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window : Window.Pointer_To_Window_Class;
         Editor      : aliased Gnoga.Gui.Plugin.Ace_Editor.Ace_Editor_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App    : App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;
      Gnoga.Gui.Plugin.Ace_Editor.Load_Ace_Editor (Main_Window);

      App.Editor.Create (Main_Window);
      App.Editor.Text
        ("with Ada.Text_IO; use Ada.Text_IO;" & Character'Val (10) &
           "procedure Hello is" & Character'Val (10) &
           "begin" & Character'Val (10) &
           "Put_Line (""Hello, world!"");" & Character'Val (10) &
           "end Hello;" & Character'Val (10));
      App.Editor.Set_Theme ("monokai");
      App.Editor.Set_Language_Mode ("ada");
   end On_Connect;

begin
   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access);

   Application.Title ("AdaEdit");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end AdaEdit;
