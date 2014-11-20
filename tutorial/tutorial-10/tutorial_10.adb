--  Gnoga comes with various bindings beyond GUI development but use of Gnoga
--  does not require using those over other available Ada bindings and
--  libraries. This tutorial demonstrates using the Gnoga's built in database
--  bindings.
--
--  Important: Before running tutorial_10 make sure to run:
--     ./tutorial_10 setup
--  This will run as described below the database migrations and setup the
--  needed database schema and sample data.

with Gnoga.Server.Database;
--  This package contains the abstract interface to all database engines.

with Gnoga.Server.Database.SQLite;
--  This package contains the SQLite3 specific implementation

with Gnoga.Server.Migration;
--  This package is to allow for schema migrations which is discussed below.

with Gnoga.Types;
with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Tutorial_10 is
   pragma Linker_Options ("-lsqlite3");
   --  This linker option is needed to link to the database engine's library.
   --  For example if using the MySQL implementation of the datbase bindings
   --  the option would be -lmysqlclient instead. The option can be specified
   --  as part of the build instead.

   Connection : aliased Gnoga.Server.Database.SQLite.Connection;
   --  Upon application start up, Connection will be opened using the SQLite
   --  database engine.

   procedure Migrations
     (M : in out Gnoga.Server.Migration.Migration_Collection);
   --  Migrations are a graceful way of dealing with changing database schema.
   --  For every step of a migration we add what is needed to bring "up" the
   --  change in the database schema and to bring "down", undo that change.
   --
   --  The first time a migration is run, a table called gnogaparams is created
   --  that will store information about the current migration state.
   --
   --  From the command line the application can be run at any time using the
   --  command line parameter "setup" and the database will be migrated up
   --  to include all new (if there are new) migrations.
   --  Example:
   --     ./tutorial_10 setup
   --  Instead of running the normal Tutorial_10 application the migration
   --  procedure will be executed instead and return.
   --
   --  An additional command line parameter "migrate" following by the
   --  migration level will upgrade or downgrade automatically to that
   --  specific migration level.
   --  Example:
   --     ./tutorial_10 migrate 0
   --   In this case the entire schema will be undone.
   --
   --  Migration and migration levels make it possible to deliver new versions
   --  or roll back to older versions of applications.

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
      --  Our first migration level is to set up the users table.

      M.Add_Migration_Up
        ("INSERT INTO users (`lastname`, `firstname`) " &
           "VALUES ('Taft','Tucker')");
      M.Add_Migration_Down
        ("delete from users");
      --  It is possible to also us migration levels to not just modify the
      --  schema but to also run any query that would be needed for the
      --  application. All migrations though must include the needed queries
      --  to bring down and changes the migration brings up.

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
   end Migrations;

   Tables       : Gnoga.Types.Data_Array_Type;
   Fields       : Gnoga.Types.Data_Array_Type;
   Descriptions : Gnoga.Server.Database.Field_Description_Array_Type;
   Description  : Gnoga.Server.Database.Field_Description;
   --  Variables for use in our tutorial

begin
   Connection.Connect (Database => "tutorial_10.db");
   --  This will connect to (and if needed create) the tutorial_10.db in the
   --  current directory using Sqlite3.

   if not
     Gnoga.Server.Migration.Migrations_Handled_Command_Line
       (Connection'Unchecked_Access, Migrations'Unrestricted_Access)
     --  If the command line was handled by the Migration package we don't
     --  run our regular application code.
   then
      declare
         My_Window : Gnoga.Gui.Window.Window_Type;
         My_View   : Gnoga.Gui.View.Console.Console_View_Type;
      begin
         Gnoga.Application.Title ("Tutorial 10");
         Gnoga.Application.Singleton.Initialize (Main_Window => My_Window);
         My_View.Create (My_Window);
         --  This could be a command line only tutorial as there is no
         --  dependancy on the GUI elements of Gnoga in the database bindings.

         My_View.Put_Line ("Obtain list of tables from: test");
         Tables := Connection.List_Of_Tables;

         My_View.Put_Line ("Display list of tables: test");
         for I in 1 .. Natural (Tables.Length) loop
            My_View.Put_Line ("Table Name in database : " &
                                Tables.Element (I));
         end loop;

         Fields := Connection.List_Fields_Of_Table ("users");
         for I in 1 .. Natural (Fields.Length) loop
            My_View.Put_Line ("Field Name in users : " & Fields.Element (I));
         end loop;

         Descriptions := Connection.Field_Descriptions ("users");
         for I in Descriptions.First_Index .. Descriptions.Last_Index loop
            Description := Descriptions.Element (I);
            My_View.Put_Line ("Column Name : " &
                                To_String (Description.Column_Name));
            My_View.Put_Line ("Data Type   : " &
                                To_String (Description.Data_Type));
            My_View.Put_Line
              ("Field Type  : " &
                 Gnoga.Server.Database.Field_Type (Description));
            My_View.Put_Line
              ("Field Opts  : " &
                 Gnoga.Server.Database.Field_Options (Description));
            My_View.Put_Line
              ("Field Size  : " &
                 Gnoga.Server.Database.Field_Size (Description)'Img);
            My_View.Put_Line
              ("Decimals    : " &
                 Gnoga.Server.Database.Field_Decimals (Description)'Img);
            My_View.Put_Line ("Can Be Null : " & Description.Can_Be_Null'Img);
            My_View.Put_Line ("Default     : " &
                                To_String (Description.Default_Value));
         end loop;

         My_View.Put_Line
           (Connection.Escape_String
              ("This will escape things like a ' or "" or " &
                 " \ or whatever may be needed like a ` based on the " &
                 "particular database enginge."));

         declare
            RS : Gnoga.Server.Database.Recordset'Class :=
                   Connection.Query ("select * from users");
         begin
            while RS.Next loop
               for J in 1 .. RS.Number_Of_Fields loop
                  My_View.Put_Line (RS.Field_Name (J) & " => " &
                                      RS.Field_Value (J));
               end loop;
            end loop;

            RS.Close;
         end;

         Gnoga.Application.Singleton.End_Application;
      end;
   end if;

   Connection.Disconnect;
end Tutorial_10;
