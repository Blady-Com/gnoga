--  You need to setup a MySQL database and grant access to the user and pass
--  you configure below.
--
--  then, run: db_mysql setup
--  then run again: db_mysql

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;

with GNAT.OS_Lib;

with Gnoga.Types;
with Gnoga.Server.Database.MySQL;
with Gnoga.Server.Migration;

with Gnoga.Application.Singleton;
with Gnoga.Window;

procedure DB_MySQL is
   pragma Linker_Options ("-lmysqlclient");

   Connection : aliased Gnoga.Server.Database.MySQL.Connection;

   procedure Migrations
     (M : in out Gnoga.Server.Migration.Migration_Collection)
   is
   begin
      M.Add_Migration_Up
        ("CREATE TABLE `users`" &
           " (" & Connection.ID_Field_String & "," &
           "  lastname VARCHAR(80)," &
           "  firstname VARCHAR(80))");
      M.Add_Migration_Down
        ("DROP TABLE `users`");

      M.Add_Migration_Up
        ("INSERT INTO users (`lastname`, `firstname`) VALUES ('Taft','Tucker')");
      M.Add_Migration_Down
        ("delete from users");
      M.Add_Migration_Up
        ("INSERT INTO users (`lastname`, `firstname`) VALUES ('Dewar','Robert')");
      M.Add_Migration_Down
        ("delete from users");
      M.Add_Migration_Up
        ("INSERT INTO users (`lastname`, `firstname`) VALUES ('Botton','David')");
      M.Add_Migration_Down
        ("delete from users");
   end Migrations;

   Tables       : Gnoga.Types.Data_Array.Vector;
   Fields       : Gnoga.Types.Data_Array.Vector;
   Descriptions : Gnoga.Server.Database.Field_Description_Array.Vector;
   Description  : Gnoga.Server.Database.Field_Description;

   M : Gnoga.Window.Window_Type;
begin
   Gnoga.Log ("Openning connection to MySQL database.");

   Connection.Connect (Database => "xxx",
                       Host     => "xxx",
                       User     => "xxx",
                       Password => "xxx");

   if
     Gnoga.Server.Migration.Migrations_Handled_Command_Line
       (Connection'Unchecked_Access, Migrations'Unrestricted_Access)
   then
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   Gnoga.Application.Title ("Database test for Gnoga");
   Gnoga.Application.HTML_On_Close ("Application closed.");
   Gnoga.Application.Singleton.Initialize (Main_Window => M);

   M.Document.Put_Line ("Obtain list of tables from: test");
   Tables := Connection.List_Of_Tables;

   for I in 1 .. Natural (Tables.Length) loop
      M.Document.Put_Line ("Table Name in database : " & Tables.Element (I));
   end loop;

   Fields := Connection.List_Fields_Of_Table ("users");
   for I in 1 .. Natural (Fields.Length) loop
      M.Document.Put_Line ("Field Name in users : " & Fields.Element (I));
   end loop;

   Descriptions := Connection.Field_Descriptions ("users");
   for I in Descriptions.First_Index .. Descriptions.Last_Index loop
      Description := Descriptions.Element (I);
      M.Document.Put_Line ("Column Name : " &
                             To_String (Description.Column_Name));
      M.Document.Put_Line ("Data Type   : " &
                             To_String (Description.Data_Type));
      M.Document.Put_Line ("Field Type  : " &
                  Gnoga.Server.Database.Field_Type (Description));
      M.Document.Put_Line ("Field Opts  : " &
                  Gnoga.Server.Database.Field_Options (Description));
      M.Document.Put_Line ("Field Size  : " &
                  Gnoga.Server.Database.Field_Size (Description)'Img);
      M.Document.Put_Line ("Decimals    : " &
         Gnoga.Server.Database.Field_Decimals (Description)'Img);
      M.Document.Put_Line ("Can Be Null : " & Description.Can_Be_Null'Img);
      M.Document.Put_Line ("Default     : " &
                             To_String (Description.Default_Value));
   end loop;

   M.Document.Put_Line
     (Connection.Escape_String ("I've been thinking.. ""escaped"" \ is it?"));

   declare
      RS : Gnoga.Server.Database.Recordset'Class :=
        Connection.Query ("select * from users");
   begin
      while RS.Next loop
         for J in 1 .. RS.Number_Of_Fields loop
            M.Document.Put_Line (RS.Field_Name (J) & " => " &
                      RS.Field_Value (J));
         end loop;
      end loop;

      RS.Close;
   end;

   Connection.Disconnect;

   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end DB_MySQL;
