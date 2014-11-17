with Gnoga.Application.Multi_Connect;
with Gnoga.Types;
with Gnoga.Gui.Window;
with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Plugin.Ace_Editor;
with Gnoga.Server.Template_Parser.Simple;

with Ada.Directories;


procedure AdaEdit is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;

   type App_Data is new Connection_Data_Type with
      record
         Main_Window  : Window.Pointer_To_Window_Class;
         Main_View    : View.View_Type;
         Control_Form : Form.Form_Type;
         File_List    : Form.Selection_Type;
         Editor       : Plugin.Ace_Editor.Ace_Editor_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : App_Access := App_Access (Object.Connection_Data);
   begin
      App.Editor.Text
        (Gnoga.Server.Template_Parser.Simple.Load_View (App.File_List.Value));
      delay 0.2;
      App.Editor.Current_Line (1);
   end On_Change;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      use Ada.Directories;

      App : App_Access := new App_Data;

      procedure Fill_List (Directory_Entry : Directory_Entry_Type) is
      begin
         if Kind (Directory_Entry) = Ordinary_File then
            App.File_List.Add_Option (Simple_Name (Directory_Entry),
                                      Simple_Name (Directory_Entry));
         end if;
      end Fill_List;

   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;
      Gnoga.Gui.Plugin.Ace_Editor.Load_Ace_Editor (Main_Window);

      App.Main_View.Create (Main_Window);
      App.Control_Form.Create (App.Main_View);
      App.File_List.Create (Form            => App.Control_Form,
                            Visible_Lines   => 20);
      App.File_List.Layout_Float (Left);
      App.File_List.Minimum_Width ("20%");
      App.File_List.Minimum_Height ("100%");
      App.File_List.On_Change_Handler (On_Change'Unrestricted_Access);

      Gnoga.Server.Template_Parser.Set_Template_Directory
        (Gnoga.Server.Application_Directory &
           Gnoga.Server.Directory_Separator & "src");

      Search (Directory => Gnoga.Server.Application_Directory &
                Gnoga.Server.Directory_Separator & "src",
              Pattern   => "*.ad?",
              Process   => Fill_List'Access);

      App.Editor.Create (App.Main_View);
      App.Editor.Layout_Float (Left);
      App.Editor.Minimum_Width ("80%");
      App.Editor.Minimum_Height ("100%");
      App.Editor.Set_Theme ("monokai");
      App.Editor.Set_Language_Mode ("ada");
      App.Editor.Default_Tab_Size (3);
      App.Editor.Soft_Tabs;
   end On_Connect;

begin
   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access);

   Application.Open_URL_OSX;

   Application.Title ("AdaEdit");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end AdaEdit;
