--  This tutorial demonstrates using the Gnoga's built in data modeling
--  facilities using the Active Record paradigm.
--
--  Important: Before running tutorial_11 make sure to run:
--     ./tutorial_11 setup
--  This will run as described below the database migrations and setup the
--  needed database schema and sample data.

with Gnoga.Server.Database.SQLite;
--  As before we will be using SQLite for this tutorial but changing to a
--  different database enginge is as simple as changing this package and
--  the Connection.Open to match the appropriat enginge.

with Gnoga.Server.Migration;

with Gnoga.Server.Model;
with Gnoga.Server.Model.Queries;
with Gnoga.Server.Model.Table;
--  These packages will be used to work with our database based data modeling

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;

with Ada.Containers;

procedure Tutorial_11 is
   pragma Linker_Options ("-lsqlite3");

   Connection : Gnoga.Server.Database.Connection_Access;
   --  Unlike the previous tutorial we will use an access type to point
   --  to the connection.

   procedure Migrations
     (M : in out Gnoga.Server.Migration.Migration_Collection);

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
      --  For Gnoga's automatic database modeling, table names end with a
      --  plural 's'.

      M.Add_Migration_Up
        ("INSERT INTO users (`lastname`, `firstname`) " &
           "VALUES ('Taft','Tucker')");
      M.Add_Migration_Down
        ("delete from users");
      M.Add_Migration_Up
        ("INSERT INTO users (`lastname`, `firstname`) " &
           "VALUES ('Dewar','Robert')");
      M.Add_Migration_Down
        ("delete from users");
      M.Add_Migration_Up
        ("INSERT INTO users (`lastname`, `firstname`) " &
           "VALUES ('Botton','David')");
      M.Add_Migration_Down
        ("delete from users");

      M.Add_Migration_Up
        ("CREATE TABLE `foods`" &
           " (" & Connection.ID_Field_String & "," &
           "  user_id Integer," &
           "  food VARCHAR(80))");
      M.Add_Migration_Down
        ("DROP TABLE `foods`");
      --  In this tutorial our database is a bit more involved and has a one to
      --  many relationship. The "many" references the "one" using a standard
      --  of tablename(minus plural) + _id, so in our tutorial to reference the
      --  users database, the id of the user would be user_id
   end Migrations;

begin
   Connection := Gnoga.Server.Database.SQLite.Connect ("tutorial_11.db");
   --  This will both create the connection object and connect to the SqlLite
   --  database.

   if not
     Gnoga.Server.Migration.Migrations_Handled_Command_Line
       (Connection, Migrations'Unrestricted_Access)
     --  If the command line was handled by the Migration package we don't
     --  run our regular application code.
   then
      declare
         My_Window : Gnoga.Gui.Window.Window_Type;
         My_View   : Gnoga.Gui.View.Console.Console_View_Type;
      begin
         Gnoga.Application.Title ("Tutorial 11");
         Gnoga.Application.Singleton.Initialize (Main_Window => My_Window);
         My_View.Create (My_Window);
         --  This could be a command line only tutorial as there is no
         --  dependancy on the GUI elements of Gnoga in the database bindings.

         My_View.Put_Line ("Using Gnoga.Server.Model.Table");

         declare
            package Users is new Gnoga.Server.Model.Table
              ("users", Connection);

            package Foods is new Gnoga.Server.Model.Table
              ("foods", Connection);

            Records : Gnoga.Server.Model.Queries.Active_Record_Array.Vector;

            A_User  : Users.Active_Record;
            A_Food  : Foods.Active_Record;
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

            My_View.Put_Line ("Records in table : " & Records.Length'Img);

            for i in Records.First_Index .. Records.Last_Index loop
               declare
                  use type Ada.Containers.Count_Type;

                  F  : Gnoga.Server.Model.Queries.Active_Record_Array.Vector;
                  F2 : Foods.Active_Record;
               begin
                  My_View.Put_Line ("Record : " & i'Img);
                  My_View.Put_Line ("First Name : " &
                                Records.Element (i).Value ("firstname"));
                  My_View.Put_Line ("Last Name : " &
                                Records.Element (i).Value ("lastname"));

                  --  One to Many Users -> Foods
                  F := Foods.Find_Items (Parent => Records.Element (i));
                  for j in F.First_Index .. F.Last_Index loop
                     My_View.Put_Line ("He Likes : " &
                                   F.Element (j).Value ("food"));
                  end loop;

                  --  One to One Users -> Foods
                  F2.Find_Item (Parent => Records.Element (i));
                  if F2.Value ("id") /= "" then
                     My_View.Put_Line ("The first thing he liked was " &
                                   F2.Value ("food"));
                  end if;

               end;
            end loop;
         end;

         Gnoga.Application.Singleton.End_Application;
      end;
   end if;

   Connection.Disconnect;
end Tutorial_11;
