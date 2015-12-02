with Ada.Text_IO;                  use Ada.Text_IO;
with Gnoga.Server.Database.SQLite; use Gnoga.Server.Database.SQLite;
procedure Pragma_SQLite is
   pragma Linker_Options ("-lsqlite3");
   DB : Connection;
begin
   DB.Connect ("test_prama.db");
   Put_Line ("At creation Short_Column_Names: " & DB.Short_Column_Names'Img);
   DB.Short_Column_Names (not DB.Short_Column_Names);
   Put_Line ("After swap Short_Column_Names: " & DB.Short_Column_Names'Img);
   Put_Line ("At creation Full_Column_Names: " & DB.Full_Column_Names'Img);
   DB.Full_Column_Names (not DB.Full_Column_Names);
   Put_Line ("After swap Full_Column_Names: " & DB.Full_Column_Names'Img);
   DB.Disconnect;
end Pragma_SQLite;
