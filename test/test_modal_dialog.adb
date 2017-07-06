with Gnoga.Types;
with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Modal_Dialog;
--  with Gnoga.Gui.View.Modal_Dialog;
with Gnoga.Application.Multi_Connect;

with Gnoga.Gui.Window;

procedure Test_Modal_Dialog is
   use Gnoga;

   --  This type define the page layout
   type App_Data (Main_Window : access Gnoga.Gui.Window.Window_Type'Class) is
      new Gnoga.Types.Connection_Data_Type
   with
      record
         Top_Level_View : Gnoga.Gui.View.View_Type;
         Modal          : Gnoga.Gui.Modal_Dialog.Dialog_Type;
--           Modal          : Gnoga.Gui.View.Modal_Dialog.Dialog_Type;
         Modal_View     : Gnoga.Gui.View.View_Type;
         Show_Dlg_Bttn  : Gnoga.Gui.Element.Common.Button_Type;
         Hide_Dlg_Bttn  : Gnoga.Gui.Element.Common.Button_Type;

         Modal2          : Gnoga.Gui.Modal_Dialog.Dialog_Type;
--           Modal2          : Gnoga.Gui.View.Modal_Dialog.Dialog_Type;
         Modal_View2     : Gnoga.Gui.View.View_Type;
         Show_Dlg_Bttn2  : Gnoga.Gui.Element.Common.Button_Type;
         Hide_Dlg_Bttn2  : Gnoga.Gui.Element.Common.Button_Type;

         Grid            : Gnoga.Gui.View.Grid.Grid_View_Type;
         Grid_Top        : Gnoga.Gui.View.View_Type;
         Grid_Left       : Gnoga.Gui.View.View_Type;
         Grid_Right      : Gnoga.Gui.View.View_Type;

         Modal3          : Gnoga.Gui.Modal_Dialog.Dialog_Type;
--           Modal3          : Gnoga.Gui.View.Modal_Dialog.Dialog_Type;
         Modal_View3     : Gnoga.Gui.View.View_Type;
         Show_Dlg_Bttn3  : Gnoga.Gui.Element.Common.Button_Type;
         Hide_Dlg_Bttn3  : Gnoga.Gui.Element.Common.Button_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Show_Dlg_Click
      (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Modal.Show;
   end On_Show_Dlg_Click;

   procedure On_Hide_Dlg_Click
      (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Modal.Show (False);
   end On_Hide_Dlg_Click;

   procedure On_Show_Dlg_Click2
      (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Modal2.Show;
   end On_Show_Dlg_Click2;

   procedure On_Hide_Dlg_Click2
      (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Modal2.Show (False);
   end On_Hide_Dlg_Click2;

   procedure On_Show_Dlg_Click3
      (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Modal3.Show;
   end On_Show_Dlg_Click3;

   procedure On_Hide_Dlg_Click3
      (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Modal3.Show (False);
   end On_Hide_Dlg_Click3;

   --  This procedure is called each time a connection is made.
   procedure On_Connect
      (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
       Connection  : access
          Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      --  This is deallocated when the connection closes
      App : constant App_Access := new App_Data (Main_Window'Unchecked_Access);
      use Gnoga.Gui.View.Grid;
   begin
      --  Associate App with this connection
      App.Main_Window.Connection_Data (App);

      --  Initialize Application Layout
      App.Top_Level_View.Create (Main_Window);

      App.Top_Level_View.Hidden;
      App.Top_Level_View.Put_Line ("Hello World");
      App.Grid.Create
         (Parent      => App.Top_Level_View,
          Layout      =>
             (1 => (COL, SPN),
              2 => (COL, COL)),
          Fill_Parent => True,
          Set_Sizes   => True);

      App.Grid_Top.Create (App.Grid.Panel (1, 1).all);
      App.Grid_Left.Create (App.Grid.Panel (2, 1).all);
      App.Grid_Right.Create (App.Grid.Panel (2, 2).all);

      App.Grid.Panel (1, 1).Border;
      App.Grid.Panel (2, 1).Border;
      App.Grid.Panel (2, 2).Border;

      App.Show_Dlg_Bttn.Create (App.Grid_Top, "Show Dialog 1");
      App.Show_Dlg_Bttn.On_Click_Handler (On_Show_Dlg_Click'Unrestricted_Access);

      App.Show_Dlg_Bttn2.Create (App.Grid_Top, "Show Dialog 2");
      App.Show_Dlg_Bttn2.On_Click_Handler (On_Show_Dlg_Click2'Unrestricted_Access);

      App.Show_Dlg_Bttn3.Create (App.Grid_Right, "Show Dialog 3");
      App.Show_Dlg_Bttn3.On_Click_Handler (On_Show_Dlg_Click3'Unrestricted_Access);

      --  Modal dialog #1
      App.Modal.Create (App.Main_Window.all);
--        App.Modal_View.Create(App.Modal);
      App.Modal.Create_Main_View (App.Modal_View);
      App.Modal_View.Width (400);
      App.Modal_View.Height (300);
      App.Modal_View.Put_Line ("Modal Dialog");
      App.Hide_Dlg_Bttn.Create (App.Modal_View, "Hide Dialog 1");
      App.Hide_Dlg_Bttn.On_Click_Handler (On_Hide_Dlg_Click'Unrestricted_Access);

      --  Modal dialog #2
      App.Modal2.Create (App.Main_Window.all);
--        App.Modal_View2.Create(App.Modal2);
      App.Modal2.Create_Main_View (App.Modal_View2);
      App.Modal_View2.Width (800);
      App.Modal_View2.Height (600);
      App.Modal_View2.Put_Line ("Modal Dialog 2");
      App.Hide_Dlg_Bttn2.Create (App.Modal_View2, "Hide Dialog 2");
      App.Hide_Dlg_Bttn2.On_Click_Handler (On_Hide_Dlg_Click2'Unrestricted_Access);

      --  Modal dialog #3
      App.Modal3.Create (App.Grid_Right);
--        App.Modal_View3.Create(App.Modal3);
      App.Modal3.Create_Main_View (App.Modal_View3);
      App.Hide_Dlg_Bttn3.Create (App.Modal_View3, "Hide Dialog 3");
      App.Hide_Dlg_Bttn3.On_Click_Handler (On_Hide_Dlg_Click3'Unrestricted_Access);
      App.Modal_View3.Width (400);
      App.Modal_View3.Height (200);

      App.Top_Level_View.Hidden (False);

      --  Do something

   end On_Connect;
begin
   Application.Multi_Connect.Initialize
     (Event => On_Connect'Unrestricted_Access,
      Boot  => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   Application.Multi_Connect.Message_Loop;
end Test_Modal_Dialog;
