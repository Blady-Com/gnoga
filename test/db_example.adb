with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gnoga.Server.Database.SQLite;
--  with Gnoga.Server.Database.MySQL;

procedure DB_Example is
   pragma Linker_Options ("-L/usr/lib/sqlite3");
   pragma Linker_Options ("-lsqlite3");


   Connection : Gnoga.Server.Database.SQLite.Connection;
--   Connection : Gnoga.Server.Database.MySQL.Connection;

   Tables       : Gnoga.Server.Data_Array.Vector;
   Fields       : Gnoga.Server.Data_Array.Vector;
   Descriptions : Gnoga.Server.Database.Field_Description_Array.Vector;
   Description  : Gnoga.Server.Database.Field_Description;
begin
   Put_Line ("Open Database: test");
   Connection.Connect (Database => "../test/test.db");

--    For MySQL would be:
--    Connection.Connect (Database => "test",
--                        Host     => "xxx",
--                        User     => "xx",
--                        Password => "xxx");

   Put_Line ("Obtain list of tables from: test");
   Tables := Connection.List_Of_Tables;

   Put_Line ("Display list of tables: test");
   for I in 1 .. Natural (Tables.Length) loop
      Put_Line ("Table Name in database : " & Tables.Element (I));
   end loop;

   Fields := Connection.List_Fields_Of_Table ("users");
   for I in 1 .. Natural (Fields.Length) loop
      Put_Line ("Field Name in users : " & Fields.Element (I));
   end loop;

   Descriptions := Connection.Field_Descriptions ("users");
   for I in Descriptions.First_Index .. Descriptions.Last_Index loop
      Description := Descriptions.Element (I);
      Put_Line ("Column Name : " & To_String (Description.Column_Name));
      Put_Line ("Data Type   : " & To_String (Description.Data_Type));
      Put_Line ("Field Type  : " &
                  Gnoga.Server.Database.Field_Type (Description));
      Put_Line ("Field Opts  : " &
                  Gnoga.Server.Database.Field_Options (Description));
      Put_Line ("Field Size  : " &
                  Gnoga.Server.Database.Field_Size (Description)'Img);
      Put_Line ("Decimals    : " &
         Gnoga.Server.Database.Field_Decimals (Description)'Img);
      Put_Line ("Can Be Null : " & Description.Can_Be_Null'Img);
      Put_Line ("Default     : " & To_String (Description.Default_Value));
   end loop;

   Put_Line (Connection.Escape_String ("I've been thinking.. escaped"));

   declare
      RS : Gnoga.Server.Database.Recordset'Class :=
        Connection.Query ("select * from users");
   begin
      while RS.Next loop
         for J in 1 .. RS.Number_Of_Fields loop
            Put_Line (RS.Field_Name (J) & " => " &
                      RS.Field_Value (J));
         end loop;
      end loop;

      RS.Close;
   end;

   Connection.Disconnect;
end DB_Example;
