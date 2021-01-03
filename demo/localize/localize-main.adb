-------------------------------------------------------------------------------
-- NAME (body)                  : localize-main.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : Main unit.
-- NOTES                        : Ada 2012, GNOGA 2.1 alpha
--
-- COPYRIGHT                    : (c) Pascal Pignard 2021
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Ada.Exceptions;

with Gnoga.Application.Multi_Connect;

--  Needed to register connection in body initialization
with Localize.Controller;
pragma Unreferenced (Localize.Controller);

procedure Localize.Main is
begin
   Gnoga.Application.Title (Name => "Localize (V1.1-alpha)");
   Gnoga.Application.Multi_Connect.Initialize;
   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (From_Latin_1 (Ada.Exceptions.Exception_Name (E) & " - " & Ada.Exceptions.Exception_Message (E)));
end Localize.Main;
