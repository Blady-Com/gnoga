-------------------------------------------------------------------------------
-- NAME (body)                  : localize-controller.adb
-- AUTHOR                       : Pascal Pignard
-- ROLE                         : User interface control unit.
-- NOTES                        : Ada 2012, GNOGA 2.1 alpha
--
-- COPYRIGHT                    : (c) Pascal Pignard 2021
-- LICENCE                      : CeCILL V2 (http://www.cecill.info)
-- CONTACT                      : http://blady.pagesperso-orange.fr
-------------------------------------------------------------------------------

with Gnoga.Gui.Base;
with Gnoga.Gui.View.Grid;

with Localize.View;
with Localize.Parser;

package body Localize.Controller is

   --  Handlers
   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Load (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Save (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Change_Key (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Change_Text (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Change_Comment (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Select (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Enter (Object : in out Gnoga.Gui.Base.Base_Type'Class) is null;
   procedure On_Duplicate (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Insert (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Delete (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Rename (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   function Modified
     (Properties : Localize.Parser.Property_List;
      Key        : String)
      return Character is (if Localize.Parser.Modified (Properties, Key) then '*' else ' ');
   function Not_In
     (Properties : Localize.Parser.Property_List;
      Key        : String;
      Tag        : Character)
      return Character is (if not Localize.Parser.Contains (Properties, Key) then Tag else ' ');

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Dummy_Last_View : Gnoga.Gui.View.View_Type;
   begin
      View.Remove;
      Dummy_Last_View.Create (View.Main_Window.all);
      Dummy_Last_View.Put_Line ("Disconnected!");
      View.Main_Window.Close;
      View.Main_Window.Close_Connection;
   end On_Exit;

   procedure On_Quit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Dummy_Last_View : Gnoga.Gui.View.View_Type;
   begin
      View.Remove;
      Dummy_Last_View.Create (View.Main_Window.all);
      Dummy_Last_View.Put_Line ("Localize server ended!");
      Gnoga.Application.Multi_Connect.End_Application;
   end On_Quit;

   procedure On_Change_Key (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Key : constant String := View.Key_List.Value;
   begin
      View.Error_Label.Text ("No error.");
      View.Key_Input.Value (Key);
      View.Master_Text.Value (Localize.Parser.Text (View.Master, Key));
      View.Master_Comment.Value (Localize.Parser.Comment (View.Master, Key));
      View.Locale_Text.Value (Localize.Parser.Text (View.Locale, Key));
      View.Locale_Comment.Value (Localize.Parser.Comment (View.Locale, Key));
      View.Old_Key_Index := View.Key_List.Selected_Index;
   exception
      when others =>
         View.Error_Label.Text ("Error CK with key: " & Key);
   end On_Change_Key;

   procedure On_Change_Text (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Key_Index : constant Natural := View.Old_Key_Index;
      Key       : constant String  := (if Key_Index > 0 then View.Key_List.Value (Key_Index) else "");
   begin
      View.Error_Label.Text ("No error.");
      Localize.Parser.Text (View.Locale, Key, View.Locale_Text.Value);
      if Localize.Parser.Modified (View.Locale, Key) then
         View.Key_List.Text
           (Key_Index,
            From_Latin_1
              (Not_In (View.Master, Key, '@') & Not_In (View.Locale, Key, '#') & Modified (View.Locale, Key)) &
            Key);
      end if;
   exception
      when others =>
         View.Error_Label.Text ("Error CT with key: " & Key);
   end On_Change_Text;

   procedure On_Change_Comment (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Key_Index : constant Natural := View.Old_Key_Index;
      Key       : constant String  := (if Key_Index > 0 then View.Key_List.Value (Key_Index) else "");
   begin
      View.Error_Label.Text ("No error.");
      Localize.Parser.Comment (View.Locale, Key, View.Locale_Comment.Value);
      if Localize.Parser.Modified (View.Locale, Key) then
         View.Key_List.Text
           (Key_Index,
            From_Latin_1
              (Not_In (View.Master, Key, '@') & Not_In (View.Locale, Key, '#') & Modified (View.Locale, Key)) &
            Key);
      end if;
   exception
      when others =>
         View.Error_Label.Text ("Error CC with key: " & Key);
   end On_Change_Comment;

   procedure On_Load (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
   begin
      View.Error_Label.Text ("No error.");
      Localize.Parser.Read (View.Master, View.Master_Path.Value);
      Localize.Parser.Read (View.Locale, View.Locale_Path.Value);
      On_Select (Object);
   exception
      when others =>
         View.Error_Label.Text ("Error with files.");
   end On_Load;

   procedure On_Save (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Key : constant String := View.Key_List.Value;
   begin
      View.Error_Label.Text ("No error.");
      Localize.Parser.Write (View.Locale, View.Locale_Path.Value);
      Localize.Parser.Reset_Modified_Indicators (View.Locale);
      On_Select (Object);
   exception
      when others =>
         View.Error_Label.Text ("Error with file or key: " & Key);
   end On_Save;

   procedure On_Select (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Old_Key : constant String := View.Key_List.Value;
   begin
      View.Error_Label.Text ("No error.");
      View.Old_Key_Index := 0;
      View.Key_List.Empty_Options;
      for Key of Localize.Parser.Selected_Keys (View.Master, View.Locale, View.Select_Pattern.Value) loop
         View.Key_List.Add_Option
           (Key,
            From_Latin_1
              (Not_In (View.Master, Key, '@') & Not_In (View.Locale, Key, '#') & Modified (View.Locale, Key)) &
            Key);
         if Key = Old_Key then
            View.Key_List.Selected (View.Key_List.Length);
         end if;
      end loop;
      View.Keys_Label.Text ("Keys (" & Gnoga.Image (View.Key_List.Length) & "):");
      if View.Key_List.Length > 0 then
         if View.Key_List.Selected_Index = 0 then
            View.Key_List.Selected (1);
         end if;
         View.Key_List.Focus;
      end if;
      On_Change_Key (Object);
   exception
      when others =>
         View.Error_Label.Text ("Error S with key: " & Old_Key);
   end On_Select;

   procedure On_Duplicate (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Key : constant String := View.Key_Input.Value;
   begin
      if Key /= "" then
         View.Error_Label.Text ("No error.");
         Localize.Parser.Text (View.Locale, Key, Localize.Parser.Text (View.Master, Key));
         Localize.Parser.Comment (View.Locale, Key, Localize.Parser.Comment (View.Master, Key));
         View.Locale_Text.Value (Localize.Parser.Text (View.Master, Key));
         View.Locale_Comment.Value (Localize.Parser.Comment (View.Master, Key));
         On_Select (Object);
      else
         View.Error_Label.Text ("Empty key.");
      end if;
   exception
      when others =>
         View.Error_Label.Text ("Error with key: " & Key);
   end On_Duplicate;

   procedure On_Insert (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Key : constant String := View.Key_Input.Value;
   begin
      if Key /= "" then
         View.Error_Label.Text ("No error.");
         Localize.Parser.Insert (View.Locale, Key);
         On_Select (Object);
      else
         View.Error_Label.Text ("Empty key.");
      end if;
   exception
      when others =>
         View.Error_Label.Text ("Error with key: " & Key);
   end On_Insert;

   procedure On_Delete (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      Key : constant String := View.Key_Input.Value;
   begin
      if Key /= "" then
         View.Error_Label.Text ("No error.");
         Localize.Parser.Delete (View.Locale, Key);
         On_Select (Object);
      else
         View.Error_Label.Text ("Empty key.");
      end if;
   exception
      when others =>
         View.Error_Label.Text ("Error with key: " & Key);
   end On_Delete;

   procedure On_Rename (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : constant Localize.View.Default_View_Access :=
        Localize.View.Default_View_Access (Object.Parent.Parent.Parent);
      From : constant String := View.Key_List.Value;
      To   : constant String := View.Key_Input.Value;
   begin
      if From /= "" and To /= "" then
         View.Error_Label.Text ("No error.");
         Localize.Parser.Rename (View.Locale, From, To);
         On_Select (Object);
      else
         View.Error_Label.Text ("Empty key.");
      end if;
   exception
      when others =>
         View.Error_Label.Text ("Error with keys: " & From & " and " & To);
   end On_Rename;

   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      View : constant Localize.View.Default_View_Access := new Localize.View.Default_View_Type;
   begin
      View.Dynamic;
      View.Main_Window   := Main_Window'Unchecked_Access;
      View.Old_Key_Index := 0;
      View.Create (Main_Window, Gnoga.Gui.View.Grid.Horizontal_Split);
      --  Avoid Enter to bubble up a new connection
      View.On_Submit_Handler (On_Enter'Access);
      View.Load_Button.On_Click_Handler (On_Load'Access);
      View.Key_List.On_Change_Handler (On_Change_Key'Access);
      View.Key_List.On_Click_Handler (On_Change_Key'Access);
      View.Save_Button.On_Click_Handler (On_Save'Access);
      View.Exit_Button.On_Click_Handler (On_Exit'Access);
      View.Quit_Button.On_Click_Handler (On_Quit'Access);
      View.Select_Pattern.On_Change_Handler (On_Select'Access);
      View.Duplicate_Button.On_Click_Handler (On_Duplicate'Access);
      View.Insert_Button.On_Click_Handler (On_Insert'Access);
      View.Delete_Button.On_Click_Handler (On_Delete'Access);
      View.Rename_Button.On_Click_Handler (On_Rename'Access);
      View.Locale_Text.On_Change_Handler (On_Change_Text'Access);
      View.Locale_Comment.On_Change_Handler (On_Change_Comment'Access);
   end Default;

begin
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Default'Access, "default");
end Localize.Controller;
