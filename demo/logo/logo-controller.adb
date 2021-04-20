-------------------------------------------------------------------------------
-- NAME (body)                  : logo-controller.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : User interface control unit.
-- NOTES                        : Ada 2012, GNOGA 1.4 beta
--
-- COPYRIGHT                    : (c) Pascal Pignard 2018
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Gnoga.Gui.Base;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Plugin.Pixi;
with Gnoga.Gui.Plugin.Ace_Editor;
with Gnoga.Gui.Navigator;
with Gnoga.Server.Connection;
with ZanyBlue.Text.Locales;

with Logo.View;
with Logo.Parser;
with Logo.Engine;
with logo_messages.logo_Strings;

package body Logo.Controller is

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Logo.View.Default_View_Access := Logo.View.Default_View_Access (Object.Parent.Parent);
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
         View.Console.Put (logo_messages.logo_Strings.Format_ACTN (View.Locale));
         Logo.Engine.Action (Logo.Engine.Decode (View.Console.Get_Line, View.Primitives), View);
      end loop;
   end Interpreter;

   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      View   : constant Logo.View.Default_View_Access := new Logo.View.Default_View_Type;
      Worker : Interpreter (View);
   begin
      View.Dynamic;
      Gnoga.Gui.Plugin.Pixi.Load_PIXI (Main_Window);
      Gnoga.Gui.Plugin.Ace_Editor.Load_Ace_Editor (Main_Window);

      View.Locale :=
        ZanyBlue.Text.Locales.Make_Locale_Narrow (Gnoga.Gui.Navigator.Language (Main_Window) & ".ISO8859-1");
      Main_Window.Document.Title (logo_messages.logo_Strings.Format_TITL (View.Locale));
--        Gnoga.Server.Connection.HTML_On_Close
--          (Main_Window.Connection_ID,
--           logo_messages.logo_Strings.Format_APPE (View.Locale));

      Logo.Parser.Fill (View.Primitives, View.Locale);
      View.Create (Main_Window, Gnoga.Gui.View.Grid.Horizontal_Split);
      --        View.Click_Button.On_Click_Handler (On_Click'Access);
      Gnoga.Activate_Exception_Handler (Worker'Identity);
      Worker.Start;
   end Default;

begin
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Default'Access, "default");
end Logo.Controller;
