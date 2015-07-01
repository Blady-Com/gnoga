with Ada.Exceptions;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Base;
with Gnoga.Gui.Plugin.Ace_Editor;
with Gnoga.Server.Connection;

procedure Essai10b is
   use Gnoga.Gui.View.Grid;

   Main_Window : Gnoga.Gui.Window.Window_Type;

   Layout_View   : Gnoga.Gui.View.Grid.Grid_View_Type;
   Text_View     : Gnoga.Gui.View.Console.Console_View_Type;
   Quit_Button   : Gnoga.Gui.Element.Common.Button_Type;
   Action_Button : Gnoga.Gui.Element.Common.Button_Type;
   Ace_View      : Gnoga.Gui.Plugin.Ace_Editor.Ace_Editor_Type;
   RO            : Boolean := True;

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Action (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Application.Singleton.End_Application;
   end On_Quit;

   procedure On_Action (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Ace_View.Editor_Execute ("getHighlightGutterLine()"));
      Gnoga.Log (Ace_View.Editor_Execute ("setHighlightGutterLine(false)"));
      Gnoga.Log (Ace_View.Editor_Execute
                 ("getSession().getDocument().getLength()"));
      Gnoga.Log
        (Gnoga.Server.Connection.Execute_Script
           (Ace_View.Connection_ID,
            "var p=" & Ace_View.Editor_Var & ".getCursorPosition(); p.row"));
      Gnoga.Log
        (Gnoga.Server.Connection.Execute_Script
           (Ace_View.Connection_ID,
            "var p=" & Ace_View.Editor_Var &
              ".getCursorPosition(); p.column"));
      Gnoga.Log (Ace_View.ID & ',' & Ace_View.Editor_Var);
      Ace_View.Read_Only (RO);
      RO := not RO;
   end On_Action;

begin
   Gnoga.Application.Title ("ACE tester");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   --  Gnoga.Application.Open_URL ("http://127.0.0.1:8080");
   Gnoga.Application.Singleton.Initialize (Main_Window,
                                           Boot => "boot_ace.html",
                                           Port => 8080);

   --  Using boot file instead
   --  Gnoga.Gui.Plugin.Ace_Editor.Load_Ace_Editor (Main_Window);

   Layout_View.Create (Parent => Main_Window,
                       Layout => ((1 => COL), (1 => COL), (1 => COL)));

   Quit_Button.Create (Layout_View.Panel (1, 1).all, "Quit");
   Quit_Button.On_Click_Handler (On_Quit'Unrestricted_Access);

   Action_Button.Create (Layout_View.Panel (1, 1).all, "Action");
   Action_Button.On_Click_Handler (On_Action'Unrestricted_Access);

   Text_View.Create (Layout_View.Panel (2, 1).all);
   Text_View.Fill_Parent;

   Text_View.Put_Line ("Keyboard characters:");

   Ace_View.Create (Layout_View.Panel (3, 1).all);
   Ace_View.Fill_Parent;
   --  Ace_View.Set_Theme ("monokai");
   Ace_View.Set_Language_Mode ("ada");
   Ace_View.Default_Tab_Size (3);
   Ace_View.Soft_Tabs;
   Ace_View.Insert_Text_At_Cursor ("Keyboard characters 2:");

   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end Essai10b;
