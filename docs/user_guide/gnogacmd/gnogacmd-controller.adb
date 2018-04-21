with GnogaCMD.View;

package body GnogaCMD.Controller is

   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class)
   is
      View : constant GnogaCMD.View.Default_View_Access :=
               new GnogaCMD.View.Default_View_Type;
   begin
      View.Dynamic;
      View.Create (Main_Window);
   end Default;

end GnogaCMD.Controller;
