-------------------------------------------------------------------------------
-- NAME (specification)         : logo-controller.ads
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : User interface control unit.
-- NOTES                        : Ada 2012, GNOGA 1.4 beta
--
-- COPYRIGHT                    : (c) Pascal Pignard 2018
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Gnoga.Gui.Window;
with Gnoga.Application.Multi_Connect;

package Logo.Controller is
   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect
        .Connection_Holder_Type);
end Logo.Controller;
