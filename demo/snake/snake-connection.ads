with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Application.Multiuser;

package Snake.Connection is
   use Gnoga.Gui.Base;
   use Gnoga.Gui.Window;
   use Gnoga.Application.Multiuser;

   procedure Display_Splash (Main_Window : in out Window_Type'Class);

   procedure Start_Game (Main_Window : in out Window_Type'Class);

   procedure On_Key_Press (Object : in out Base_Type'Class;
                           Key    : in     Character);

   procedure On_Connect_Default
     (Main_Window : in out Window_Type'Class;
      Connection  : access Connection_Holder_Type);
   --  Handle user connection to default page

end Snake.Connection;
