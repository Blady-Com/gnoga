with Gnoga.Gui.Base;

with @@data.App_Name@@.View;

package body @@data.App_Name@@.Controller is

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   
   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : @@data.App_Name@@.View.Default_View_Access := 
               @@data.App_Name@@.View.Default_View_Access (Object.Parent);
   begin
      View.Label_Text.Put_Line ("Click");
   end On_Click;
   
   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class)
   is
      View : @@data.App_Name@@.View.Default_View_Access :=
               new @@data.App_Name@@.View.Default_View_Type;
   begin
      View.Dynamic;
      View.Create (Main_Window);
      View.Click_Button.On_Click_Handler (On_Click'Access);
   end Default;

end @@data.App_Name@@.Controller;
