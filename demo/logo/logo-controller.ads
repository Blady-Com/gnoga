with Gnoga.Gui.Window;
with Gnoga.Application.Multi_Connect;

package Logo.Controller is
   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type);
end Logo.Controller;
