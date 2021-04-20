with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Table;
with Gnoga.Types;

procedure TTables is
   use Gnoga;
   use Gnoga.Types;
   use Gnoga.Gui;
   use Gnoga.Gui.Element;

   type App_Data is new Connection_Data_Type with record
      Main_Window : Window.Pointer_To_Window_Class;
      Console     : aliased View.Console.Console_View_Type;
      My_Table    : Table.Table_Type;
   end record;
   type App_Access is access all App_Data;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.Main_Window := Main_Window'Unchecked_Access;
      App.Console.Create (Main_Window);

      App.My_Table.Create (App.Console);

      declare
         head : constant Table.Table_Header_Access := new Table.Table_Header_Type;
         row  : constant Table.Table_Row_Access    := new Table.Table_Row_Type;
      begin
         head.Dynamic;
         head.Create (App.My_Table);
         row.Create (head.all);

         for cols in 1 .. 5 loop
            declare
               col : constant Table.Table_Heading_Access := new Table.Table_Heading_Type;
            begin
               col.Dynamic;
               col.Create (row.all, cols'Img);
            end;
         end loop;
      end;

      for rows in 1 .. 5 loop
         declare
            row : constant Table.Table_Row_Access := new Table.Table_Row_Type;
         begin
            row.Dynamic;
            row.Create (App.My_Table);

            for cols in 1 .. 5 loop
               declare
                  col : constant Table.Table_Column_Access := new Table.Table_Column_Type;
               begin
                  col.Dynamic;
                  col.Create (row.all, cols'Img);
               end;
            end loop;
         end;
      end loop;

      App.My_Table.Add_Caption ("My First Table");
   end On_Connect;

begin
   Application.Multi_Connect.Initialize (Event => On_Connect'Unrestricted_Access, Boot => "debug.html");

   Application.Title ("Test App for Gnoga");
   Application.HTML_On_Close ("<b>Connection to Application has been terminated</b>");

--     Application.Open_URL;

   Application.Multi_Connect.Message_Loop;
end TTables;
