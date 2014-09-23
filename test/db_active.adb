
--  To use this app, run: db_active setup
--  then run again: db_active

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Containers;

with GNAT.OS_Lib;

with Gnoga.Types;
with Gnoga.Server.Database.SQLite;
with Gnoga.Server.Migration;
with Gnoga.Server.Model;
with Gnoga.Server.Model.Queries;
with Gnoga.Server.Model.Specific;

with Gnoga.Application.Singleton;
with Gnoga.Window;

procedure DB_Active is
   use Gnoga.Server;

   pragma Linker_Options ("-lsqlite3");

   Connection : Gnoga.Server.Database.Connection_Access;

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

      M.Add_Migration_Up
        ("CREATE TABLE `foods`" &
           " (" & Connection.ID_Field_String & "," &
           "  user_id Integer," &
           "  food VARCHAR(80))");
      M.Add_Migration_Down
        ("DROP TABLE `foods`");

   end Migrations;

   M : Gnoga.Window.Window_Type;
begin
   Gnoga.Log ("Openning database file: ./active_test.db");

   Connection := Gnoga.Server.Database.SQLite.Connect ("active_test.db");

   if
     Gnoga.Server.Migration.Migrations_Handled_Command_Line
       (Connection, Migrations'Unrestricted_Access)
   then
      GNAT.OS_Lib.OS_Exit (0);
   end if;

   Gnoga.Application.Title ("Active Data test for Gnoga");
   Gnoga.Application.HTML_On_Close ("Application closed.");
   Gnoga.Application.Singleton.Initialize (Main_Window => M);

   M.Document.Put_Line ("Using Gnoga.Server.Model.Specific");

   declare
      package Users is new Model.Specific
        ("users", Connection);

      package Foods is new Model.Specific
        ("foods", Connection);

      Records : Model.Queries.Active_Record_Array.Vector;

      A_User  : Users.Active_Record;
      A_Food  : Foods.Active_Record;
   begin
      A_User.Find_Where ("lastname='Botton'");
      A_Food.Set_Value ("user_id", A_User.Value ("id"));
      A_Food.Set_Value ("food", "Apples");
      A_Food.Save;

      A_User.Find_Where ("lastname='Dewar'");
      A_Food.Clear;
      A_Food.Set_Value ("user_id", A_User.Value ("id"));
      A_Food.Set_Value ("food", "Pears");
      A_Food.Save;

      Records := Users.Find_All;

      M.Document.Put_Line ("Records in table : " & Records.Length'Img);

      for i in Records.First_Index .. Records.Last_Index loop
         declare
            use type Ada.Containers.Count_Type;

            R  : Model.Active_Record'Class := Records.Element (i);
            F  : Model.Queries.Active_Record_Array.Vector;
            F2 : Foods.Active_Record;
         begin
            M.Document.Put_Line ("Record : " & i'Img);
            M.Document.Put_Line ("First Name : " & R.Value ("firstname"));
            M.Document.Put_Line ("Last Name : " & R.Value ("lastname"));

            -- One to Many Users -> Foods
            F := Foods.Find_Items (R);
            if F.Length > 0 then
               for j in F.First_Index .. F.Last_Index loop
                  M.Document.Put_Line ("He Likes : " &
                                         F.Element (j).Value ("food"));
               end loop;
            end if;

            -- One to One Users -> Foods

            F2.Find_Item (R, True);
            if F2.Value ("id") /= "" then
               M.Document.Put_Line ("The first thing he liked was " &
                                      F2.Value ("food"));
            end if;

         end;
      end loop;
   end;

   Connection.Disconnect;

   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end DB_Active;
