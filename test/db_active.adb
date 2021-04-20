--  To use this app, run: db_active setup
--  then run again: db_active

with GNAT.OS_Lib;

with Gnoga.Server.Database.SQLite;
with Gnoga.Server.Migration;
with Gnoga.Server.Model;
with Gnoga.Server.Model.Queries;
with Gnoga.Server.Model.Table;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element;

procedure DB_Active is
   use Gnoga;
   use Gnoga.Server;
   use all type Gnoga.String;

   pragma Linker_Options ("-lsqlite3");

   Connection : Gnoga.Server.Database.Connection_Access;

   procedure Migrations (M : in out Gnoga.Server.Migration.Migration_Collection);

   procedure Migrations (M : in out Gnoga.Server.Migration.Migration_Collection) is
   begin
      M.Add_Migration_Up
        ("CREATE TABLE `users`" & " (" & Connection.ID_Field_String & "," & "  lastname VARCHAR(80)," &
         "  firstname VARCHAR(80))");
      M.Add_Migration_Down ("DROP TABLE `users`");

      M.Add_Migration_Up ("INSERT INTO users (`lastname`, `firstname`) " & "VALUES ('Taft','Tucker')");
      M.Add_Migration_Down ("delete from users");
      M.Add_Migration_Up ("INSERT INTO users (`lastname`, `firstname`) " & "VALUES ('Dewar','Robert')");
      M.Add_Migration_Down ("delete from users");
      M.Add_Migration_Up ("INSERT INTO users (`lastname`, `firstname`) " & "VALUES ('Botton','David')");
      M.Add_Migration_Down ("delete from users");

      M.Add_Migration_Up
        ("CREATE TABLE `foods`" & " (" & Connection.ID_Field_String & "," & "  user_id Integer," &
         "  food VARCHAR(80))");
      M.Add_Migration_Down ("DROP TABLE `foods`");

   end Migrations;

   M : Gnoga.Gui.Window.Window_Type;
   V : Gnoga.Gui.View.View_Type;
begin
   Gnoga.Log ("Openning database file: ./active_test.db");

   Connection := Gnoga.Server.Database.SQLite.Connect ("active_test.db");

   if Gnoga.Server.Migration.Migrations_Handled_Command_Line (Connection, Migrations'Unrestricted_Access) then
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   Gnoga.Application.Title ("Active Data test for Gnoga");
   Gnoga.Application.HTML_On_Close ("Application closed.");
   Gnoga.Application.Singleton.Initialize (Main_Window => M);

   V.Create (M);
   V.Overflow (Gnoga.Gui.Element.Scroll);

   V.Put_Line ("Using Gnoga.Server.Model.Table");

   declare
      package Users is new Model.Table ("users", Connection);

      package Foods is new Model.Table ("foods", Connection);

      Records : Model.Queries.Active_Record_Array.Vector;

      A_User : Users.Active_Record;
      A_Food : Foods.Active_Record;
   begin
      A_User.Find_Where ("lastname='Botton'");
      A_Food.Value ("user_id", A_User.Value ("id"));
      A_Food.Value ("food", "Apples");
      A_Food.Save;

      A_Food.Clear;
      A_Food.Value ("user_id", A_User.Value ("id"));
      A_Food.Value ("food", "Oranges");
      A_Food.Save;

      A_User.Find_Where ("lastname='Dewar'");
      A_Food.Clear;
      A_Food.Value ("user_id", A_User.Value ("id"));
      A_Food.Value ("food", "Pears");
      A_Food.Save;

      Records := Users.Find_All;

      V.Put_Line ("Records in table : " & Image (Natural (Records.Length)));

      for i in Records.First_Index .. Records.Last_Index loop
         declare
            F  : Model.Queries.Active_Record_Array.Vector;
            F2 : Foods.Active_Record;
         begin
            V.Put_Line ("Record : " & Image (i));
            V.Put_Line ("First Name : " & Records.Element (i).Value ("firstname"));
            V.Put_Line ("Last Name : " & Records.Element (i).Value ("lastname"));

            --  One to Many Users -> Foods
            F := Foods.Find_Items (Parent => Records.Element (i));
            for j in F.First_Index .. F.Last_Index loop
               V.Put_Line ("He Likes : " & F.Element (j).Value ("food"));
            end loop;

            --  One to One Users -> Foods
            F2.Find_Item (Parent => Records.Element (i));
            if F2.Value ("id") /= "" then
               V.Put_Line ("The first thing he liked was " & F2.Value ("food"));
            end if;

         end;
      end loop;
   end;

   Connection.Disconnect;

   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end DB_Active;
