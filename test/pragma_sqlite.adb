with Ada.Text_IO;                  use Ada.Text_IO;
with Gnoga.Server.Database.SQLite; use Gnoga.Server.Database.SQLite;
procedure Pragma_SQLite is
   pragma Linker_Options ("-lsqlite3");
   DB : Connection;
begin
   DB.Connect ("test_pragma.db");
   Put_Line ("At creation Short_Column_Names: " & DB.Short_Column_Names'Img);
   DB.Short_Column_Names (not DB.Short_Column_Names);
   Put_Line ("After swap Short_Column_Names: " & DB.Short_Column_Names'Img);
   Put_Line ("At creation Full_Column_Names: " & DB.Full_Column_Names'Img);
   DB.Full_Column_Names (not DB.Full_Column_Names);
   Put_Line ("After swap Full_Column_Names: " & DB.Full_Column_Names'Img);
   Put_Line ("At creation Encoding: " & DB.Encoding);
   DB.Execute_Query
     ("CREATE TABLE `users`" & " (" & DB.ID_Field_String & "," & " lastname VARCHAR(80)," & " firstname VARCHAR(80))");
   Put_Line ("At creation UTF8_STring: " & DB.UTF8_String'Img);
   Put_Line ("Insert Blansec Adèle");
   DB.Execute_Query ("INSERT INTO users (`lastname`, `firstname`) " & "VALUES ('Blansec','Adèle')");
   declare
      RS : Gnoga.Server.Database.Recordset'Class := DB.Query ("select * from users");
   begin
      while RS.Next loop
         for J in 1 .. RS.Number_Of_Fields loop
            Put_Line (RS.Field_Name (J) & " => " & RS.Field_Value (J));
         end loop;
      end loop;
   end;
   Put_Line
     ("With Ada Latin-1 strings the result is ok because Latin-1 text is" &
      " stored in database, then is read as Latin-1." & " But UTF-8 text is expected by database.");
   DB.UTF8_String (not DB.UTF8_String);
   Put_Line ("After swap UTF8_STring: " & DB.UTF8_String'Img);
   Put_Line ("Insert Lupin Arsène");
   DB.Execute_Query ("INSERT INTO users (`lastname`, `firstname`) " & "VALUES ('Lupin','Arsène')");
   declare
      RS : Gnoga.Server.Database.Recordset'Class := DB.Query ("select * from users");
   begin
      while RS.Next loop
         for J in 1 .. RS.Number_Of_Fields loop
            Put_Line (RS.Field_Name (J) & " => " & RS.Field_Value (J));
         end loop;
      end loop;
   end;
   Put_Line ("The result 1 provokes an exception because Latin-1 stored in " & "database is read as UTF-8.");
   Put_Line
     ("The result 2 is ok because Latin-1 translated into UTF-8 is stored" &
      " in database then is read as UTF-8 and translated into latin-1.");
   DB.Execute_Query ("DROP TABLE `users`");
   DB.Disconnect;
end Pragma_SQLite;
