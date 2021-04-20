with Gnoga.Server.Database.SQLite; use Gnoga.Server.Database.SQLite;
with UXStrings.Text_IO;
procedure Pragma_SQLite is
   use Gnoga;
   use all type Gnoga.String;
   use UXStrings.Text_IO;
   pragma Linker_Options ("-lsqlite3");
   DB : Connection;
begin
   DB.Connect ("test_pragma.db");
   Put_Line ("At creation Short_Column_Names: " & Image (DB.Short_Column_Names));
   DB.Short_Column_Names (not DB.Short_Column_Names);
   Put_Line ("After swap Short_Column_Names: " & Image (DB.Short_Column_Names));
   Put_Line ("At creation Full_Column_Names: " & Image (DB.Full_Column_Names));
   DB.Full_Column_Names (not DB.Full_Column_Names);
   Put_Line ("After swap Full_Column_Names: " & Image (DB.Full_Column_Names));
   Put_Line ("At creation Encoding: " & DB.Encoding);
   DB.Execute_Query
     ("CREATE TABLE `users`" & " (" & DB.ID_Field_String & "," & " lastname VARCHAR(80)," & " firstname VARCHAR(80))");
   Put_Line ("At creation UTF8_STring: " & Image (DB.UTF8_String));
   Put_Line ("Insert and read Blansec Adèle");
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
     ("With UXStrings the result is ok because the text is" &
      " stored in database following UTF8_String attribut, then is read in the same way." &
      " Here UTF-8 text is expected by database regarding Encoding attribut.");
   DB.UTF8_String (not DB.UTF8_String);
   Put_Line ("After swap UTF8_STring: " & Image (DB.UTF8_String));
   Put_Line ("Insert and read Lupin Arsène");
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
   Put_Line ("The result 1 is incorrect because the value stored in " & "database as UTF-8 is read as Latin-1.");
   Put_Line ("The result 2 is ok because the value is stored" & " in database as Latin 1 then is read as latin-1.");
   DB.Execute_Query ("DROP TABLE `users`");
   DB.Disconnect;
end Pragma_SQLite;
