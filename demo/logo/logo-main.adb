-------------------------------------------------------------------------------
-- NAME (body)                  : logo-main.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Main unit.
-- NOTES                        : Ada 2012, GNOGA 1.4 beta
--
-- COPYRIGHT                    : (c) Pascal Pignard 2018
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Gnoga.Application.Multi_Connect;

with Logo.Controller; --  Needed to register connection in body initialization

procedure Logo.Main is
begin
   Gnoga.Application.Multi_Connect.Initialize;
   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end Logo.Main;
