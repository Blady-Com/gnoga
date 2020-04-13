with Ada.Exceptions;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Element.Form;
with Gnoga.Types;
with Gnoga.Gui.Base;
with Gnoga.Client.Files;

procedure Files_Ops is
   Main_Window : Gnoga.Gui.Window.Window_Type;

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      Main_View        : Gnoga.Gui.View.Console.Console_View_Type;
      Mon_Formulaire   : Gnoga.Gui.Element.Form.Form_Type;
      Mon_Texte_Multi  : Gnoga.Gui.Element.Form.Text_Area_Type;
      Mon_Bouton_Envoi : Gnoga.Gui.Element.Form.Submit_Button_Type;
      Mon_Bouton_RAZ   : Gnoga.Gui.Element.Form.Reset_Button_Type;
      Mon_Fichier      : Gnoga.Gui.Element.Form.File_Type;
      Reader           : Gnoga.Client.Files.File_Reader_Type;
      Mon_Fichier_Mult : Gnoga.Gui.Element.Form.File_Type;
   end record;
   type App_Access is access all App_Data;

   procedure On_Load
     (Object : in out Gnoga.Gui.Base.Base_Type'Class; Event : String)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Mon_Texte_Multi.Value (App.Reader.Content);
      App.Main_View.Put_Line ("Reader Event = " & Event);
      App.Main_View.Put_Line ("Reader State = " & App.Reader.State'Image);
      App.Main_View.Put_Line ("Reader Error = " & App.Reader.Error_Name);
   end On_Load;

   procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Reader.Transfert_As_Text (App.Mon_Fichier);
--        App.Reader.Transfert_As_Binary (App.Mon_Fichier);
      App.Main_View.Put_Line ("Reader State = " & App.Reader.State'Image);
      App.Main_View.Put_Line ("Reader Error = " & App.Reader.Error_Name);
   end On_Submit;

   procedure On_Change (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_View.Put_Line
        ("Files count = " & App.Mon_Fichier_Mult.File_Count'Image);
      for I in 1 .. App.Mon_Fichier_Mult.File_Count loop
         App.Main_View.Put_Line
           ("File (" & I'image & ") = " &
            App.Mon_Fichier_Mult.File_Name (Index => I));
         App.Main_View.Put_Line
           ("File (" & I'image & ") = " &
            App.Mon_Fichier_Mult.File_WebkitRelativePath (Index => I));
      end loop;
   end On_Change;

   procedure Formulaires (App : App_Access) is
   begin
      Main_Window.Connection_Data (App);
      App.Main_View.Create (Main_Window);
      App.Mon_Formulaire.Create (App.Main_View);
      App.Mon_Formulaire.Put_Line ("Single file selection:");
      App.Mon_Fichier.Create (App.Mon_Formulaire);
      App.Mon_Formulaire.New_Line;
      App.Mon_Formulaire.On_Submit_Handler (On_Submit'Unrestricted_Access);
      App.Mon_Bouton_Envoi.Create (App.Mon_Formulaire, "Upload");
      App.Mon_Bouton_RAZ.Create (App.Mon_Formulaire, "Reset");
      App.Mon_Formulaire.New_Line;
      App.Mon_Texte_Multi.Create
        (App.Mon_Formulaire, 40, 8, Value => "Text...");
      App.Reader.Create (Main_Window);
      App.Reader.On_Load_Handler (On_Load'Unrestricted_Access);
      App.Mon_Formulaire.New_Line;
      App.Mon_Formulaire.Put_Line ("Multiple file and folder selection:");
      App.Mon_Fichier_Mult.Create (App.Mon_Formulaire);
      App.Mon_Fichier_Mult.Multiple;
      App.Mon_Fichier_Mult.WebkitDirectory;
      App.Mon_Fichier_Mult.On_Change_Handler (On_Change'Unrestricted_Access);
   end Formulaires;

begin
   Gnoga.Application.Title ("Files Operations");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");

   --     Gnoga.Application.Open_URL ("http://127.0.0.1:8080");
   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080);

   Formulaires (new App_Data);

   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log
        (Ada.Exceptions.Exception_Name (E) & " - " &
         Ada.Exceptions.Exception_Message (E));
end Files_Ops;
