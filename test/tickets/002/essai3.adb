with Ada.Exceptions;
with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Base;
procedure Essai3 is
   use Gnoga.Gui.View.Grid;

   Main_Window  : Gnoga.Gui.Window.Window_Type;
   Popup_Window : Gnoga.Gui.Window.Window_Type;
   Layout_View       : Gnoga.Gui.View.Grid.Grid_View_Type;
   Popup_Layout_View : Gnoga.Gui.View.Grid.Grid_View_Type;
   Text_View         : Gnoga.Gui.View.Console.Console_View_Type;
   Quit_Button       : Gnoga.Gui.Element.Common.Button_Type;


   C : Gnoga.Gui.View.Console.Console_View_Type;

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Application.Singleton.End_Application;
   end On_Quit;

begin
   Gnoga.Application.Title ("Essai3");
   Gnoga.Application.HTML_On_Close ("<b>Connection to Application has been terminated</b>");

   --  Gnoga.Application.Open_URL ("http://127.0.0.1:8080");
   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080,Boot => "debug.html");

   Layout_View.Create
   (Parent                =>
      Main_Window, Layout =>
      ((Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
       (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
       (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL)));

   Quit_Button.Create (Layout_View.Panel (1, 1).all, "Quit");
   Quit_Button.On_Click_Handler (On_Quit'Unrestricted_Access);

   Layout_View.Panel (2, 1).Text ("tototot");
   Layout_View.Panel (3, 2).Text ("tititititit");
   Text_View.Create (Layout_View.Panel (3, 1).all);
   Text_View.Fill_Parent;
   Text_View.Put_Line ("Mouse coordinates X, Y:");

   -- same with popup window
   Popup_Window.Launch (Parent => Main_Window, URL => "/no_boot.html", Width => 400, Height => 200);
   Popup_Layout_View.Create
   (Parent                 =>
      Popup_Window, Layout =>
      ((Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
       (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL),
       (Gnoga.Gui.View.Grid.COL, Gnoga.Gui.View.Grid.COL)),ID => "test1");
   Popup_Layout_View.Border;
   Popup_Layout_View.Panel (1, 1).Width (100);
   Popup_Layout_View.Panel (1, 1).Height (100);

   Popup_Layout_View.Panel (2, 1).Text ("tototot");
   Popup_Layout_View.Panel (3, 2).Text ("tititititit");
   -- nothing appear in popup window

   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " & Ada.Exceptions.Exception_Message (E));
end Essai3;
