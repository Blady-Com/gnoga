with Gnoga.Gui.Window;
with Gnoga.Application.Multiuser;

package AdaBlog.Controller is
   procedure Index
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type);

   procedure New_Entry
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type);

   procedure Log_Out
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access Gnoga.Application.Multiuser.Connection_Holder_Type);
end AdaBlog.Controller;
