with Ada.Exceptions;

with GNAT.OS_Lib;

with Gnoga.Application.Multi_Connect;
with Gnoga.Server.Migration;

with AdaBlog.Migrations;
with AdaBlog.Controller;

procedure AdaBlog.Main is
   use Gnoga;

   pragma Linker_Options ("-lsqlite3");
begin
   if
     Gnoga.Server.Migration.Migrations_Handled_Command_Line
       (Connection, Migrations'Unrestricted_Access)
   then
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   Application.Title ("AdaBlog - Gnoga Demo");
   Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");
   Application.Multi_Connect.Initialize (Boot  => "debug.html");

   Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Log (Ada.Exceptions.Exception_Name (E) & " - " &
             Ada.Exceptions.Exception_Message (E));

end AdaBlog.Main;
