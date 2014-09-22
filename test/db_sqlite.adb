with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with GNAT.Traceback.Symbolic;

with Gnoga.Types;
with Gnoga.Server.Database.SQLite;

with Gnoga.Application.Singleton;
with Gnoga.Window;

procedure DB_SQLite is
   pragma Linker_Options ("-lsqlite3");


   Connection : Gnoga.Server.Database.SQLite.Connection;

   Tables       : Gnoga.Types.Data_Array.Vector;
   Fields       : Gnoga.Types.Data_Array.Vector;
   Descriptions : Gnoga.Server.Database.Field_Description_Array.Vector;
   Description  : Gnoga.Server.Database.Field_Description;

   M : Gnoga.Window.Window_Type;
begin
   Gnoga.Application.Title ("Database test for Gnoga");
   Gnoga.Application.HTML_On_Close ("Application closed.");
   Gnoga.Application.Singleton.Initialize (Main_Window => M);

   M.Document.Put_Line ("Open Database: test");
   Connection.Connect (Database => "../test/test.db");

   M.Document.Put_Line ("Obtain list of tables from: test");
   Tables := Connection.List_Of_Tables;

   M.Document.Put_Line ("Display list of tables: test");
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
      Gnoga.Log (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end DB_SQLite;
