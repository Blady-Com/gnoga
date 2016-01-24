-------------------------------------------------------------------------------
-- Main procedure for Connect Four web server
-- COPYRIGHT : 20150511 by Pascal Pignard
-- LICENCE   : CeCILL V2.1 (http://www.cecill.info)
-- CONTACT   : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Gnoga.Application;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with ConnectFour; use ConnectFour;
with connectfour_messages.connectfour_Strings;
use connectfour_messages.connectfour_Strings;
with ZanyBlue.Text.Locales;
with Ada.Characters.Conversions;
with Ada.Strings.UTF_Encoding.Strings; use Ada.Strings.UTF_Encoding.Strings;
with Gnoga.Gui.Navigator;

procedure Connect_Four is

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type);
   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type)
   is
      Page : aliased Typ;
   begin
      Page.Create (Main_Window);
      Page.Main_Window :=
        Gnoga.Gui.Window.Window_Type (Main_Window)'Unchecked_Access;
      Page.Locale :=
        ZanyBlue.Text.Locales.Make_Locale
          (Ada.Characters.Conversions.To_Wide_String
             (Gnoga.Gui.Navigator.Language (Main_Window)));
      Init (Page'Access);
      Connection.Hold;
   end On_Connect;

begin
   Gnoga.Application.Title (Name => "Connect Four");
   Gnoga.Application.HTML_On_Close (HTML => Decode (Format_APPE));
   Gnoga.Application.Multi_Connect.Initialize;
   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => On_Connect'Unrestricted_Access);
   Gnoga.Application.Multi_Connect.Message_Loop;
end Connect_Four;
