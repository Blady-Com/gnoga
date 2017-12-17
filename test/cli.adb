with Gnoga.Application.Multi_Connect;
with Gnoga.Types;
with Gnoga.Gui.Window;
with Gnoga.Gui.Plugin.Ace_Editor.Console_IO;

procedure CLI is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;

   type App_Data is new Connection_Data_Type with record
      Main_Window : Window.Pointer_To_Window_Class;
      CLI         : Plugin.Ace_Editor.Console_IO.Console_IO_Type;
   end record;
   type App_Access is access all App_Data;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);

      App : constant App_Access := new App_Data;
      S   : String (1 .. 20);
      L   : Natural;

   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;

      --  If not using a custom boot loader like boot_ace.html use:
      Gnoga.Gui.Plugin.Ace_Editor.Load_Ace_Editor (Main_Window);

      App.CLI.Create (Main_Window);
      App.CLI.Show_Invisibles;
      App.CLI.Word_Wrap;
      App.CLI.Wrap_Limit (40);

      loop
         App.CLI.Put ("Enter: ");
         App.CLI.Put ("Line :@" & App.CLI.Get_Line);
         App.CLI.Put ("@");
         App.CLI.New_Line;
         App.CLI.Put ("Enter exit to end: ");
         App.CLI.Get_Line (S, L);
         App.CLI.Put ("Line (20 max):@" & S (S'First .. L));
         App.CLI.Put_Line ("@");
         exit when S (S'First .. L) = "exit";
      end loop;
      App.CLI.Put_Line ("End.");
   end On_Connect;

begin
   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access,
      Boot  => "debug.html");

   Application.Title ("Editor");
--     Application.HTML_On_Close
--       ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end CLI;
