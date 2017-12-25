with Gnoga.Gui.Base;

with Logo.View;
with Logo.Engine;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Plugin.Pixi;
with Gnoga.Gui.Plugin.Ace_Editor;

package body Logo.Controller is

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : Logo.View.Default_View_Access :=
        Logo.View.Default_View_Access (Object.Parent.Parent);
   begin
      View.Label_Text.Put_Line ("Click");
--        gnoga.log(ada.Tags.Expanded_Name(Object.Parent.Parent'tag));
   end On_Click;

   task type Interpreter (View : Logo.View.Default_View_Access) is
      entry Start;
   end Interpreter;

   task body Interpreter is
   begin
      accept Start;
      loop
         View.Console.Put ("Action : ");
         Logo.Engine.Action (Logo.Engine.Decode (View.Console.Get_Line), View);
      end loop;
   end Interpreter;

   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type)
   is
      View : Logo.View.Default_View_Access := new Logo.View.Default_View_Type;
      Worker : Interpreter (View);
   begin
      View.Dynamic;
      Gnoga.Gui.Plugin.Pixi.Load_PIXI (Main_Window);
      Gnoga.Gui.Plugin.Ace_Editor.Load_Ace_Editor (Main_Window);
      View.Create (Main_Window, Gnoga.Gui.View.Grid.Horizontal_Split);
      --        View.Click_Button.On_Click_Handler (On_Click'Access);
      Gnoga.Activate_Exception_Handler (Worker'Identity);
      Worker.Start;
   end Default;

begin
   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Default'Access,
      "default");
end Logo.Controller;
