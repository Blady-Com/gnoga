with Ada.Calendar;

with Gnoga.Types;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;

with AdaBlog.View;
with AdaBlog.Model;

package body AdaBlog.Controller is

   procedure On_Create_User (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  The create user button was clicked

   procedure On_User_Login (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  User clicked the submit button for his user name and password

   procedure On_Submit_Entry (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  User clicked the submit button to an a blog entry.

   procedure On_Create_User (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Main_Window  : aliased Gui.Window.Window_Type;
      Message_Area : Gui.Element.Common.DIV_Type;
      User_Login   : Gui.Element.Common.Button_Type;

      username : Gui.Element.Form.Text_Type;
      password : Gui.Element.Form.Text_Type;
      pass2    : Gui.Element.Form.Text_Type;
      verify   : Gui.Element.Common.DIV_Type;

      User : AdaBlog.Model.Users.Active_Record;
   begin
      Main_Window.Attach (Object.Connection_ID);
      Message_Area.Attach_Using_Parent (Main_Window, "message");

      User_Login.Attach_Using_Parent (Main_Window, "login-button");
      User_Login.Disabled;

      username.Attach_Using_Parent (Main_Window, "username");
      password.Attach_Using_Parent (Main_Window, "pass");
      pass2.Attach_Using_Parent (Main_Window, "pass2");

      verify.Attach_Using_Parent (Main_Window, "verify-pass");
      verify.Display ("inline");

      if Gnoga.String'(password.Value) /= Gnoga.String'(pass2.Value) or pass2.Value = "" then
         Message_Area.Text ("Please verify password.");
         return;
      end if;

      if username.Value = "" then
         Message_Area.Text ("Username is required.");
         return;
      end if;

      User.Find_Where ("username='" & username.Value & "'");

      if User.Value ("id") /= "" then
         Message_Area.Text ("Username is already taken.");
         return;
      end if;

      User.Value ("username", Value => username.Value);
      User.Value ("pass", Value => password.Value);
      User.Value ("last_session", Value => Main_Window.Gnoga_Session_ID);
      User.Save;

      Main_Window.Location.URL ("/");
   end On_Create_User;

   procedure On_User_Login (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Main_Window  : aliased Gui.Window.Window_Type;
      Message_Area : Gui.Element.Common.DIV_Type;

      username : Gui.Element.Form.Text_Type;
      password : Gui.Element.Form.Text_Type;

      User : AdaBlog.Model.Users.Active_Record;
   begin
      Main_Window.Attach (Object.Connection_ID);
      Message_Area.Attach_Using_Parent (Main_Window, "message");

      username.Attach_Using_Parent (Main_Window, "username");
      password.Attach_Using_Parent (Main_Window, "pass");

      User.Find_Where
        (Where      => "username ='" & username.Value & "'" & " AND " & "pass ='" & password.Value & "'",
         Create_New => False);
      User.Value ("last_session", Value => Main_Window.Gnoga_Session_ID);
      User.Save;

      Main_Window.Location.URL ("/");
   exception
      when others =>
         Message_Area.Text ("Invalid Login");
   end On_User_Login;

   procedure On_Submit_Entry (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      User    : AdaBlog.Model.Users.Active_Record;
      Entries : AdaBlog.Model.Blog_Entries.Active_Record;

      Main_Window : aliased Gui.Window.Window_Type;
      Text        : Gnoga.Gui.Element.Form.Text_Area_Type;
   begin
      Main_Window.Attach (Object.Connection_ID);

      Text.Attach_Using_Parent (Main_Window, "entry_text");

      User.Find_Where ("last_session ='" & Main_Window.Gnoga_Session_ID & "'");
      Entries.Value ("user_id", Value => User.Value ("id"));
      Entries.Value ("entry_date", Date_Value => Ada.Calendar.Clock);
      Entries.Value ("entry_text", Value => Text.Value);
      Entries.Save;

      Main_Window.Location.URL ("/");
   end On_Submit_Entry;

   procedure Index
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      Content_Area : Gui.Element.Common.DIV_Type;
      Left_Area    : Gui.Element.Common.DIV_Type;
      User_Login   : Gui.Element.Common.Button_Type;
      Create_User  : Gui.Element.Common.Button_Type;

      User : AdaBlog.Model.Users.Active_Record;

      procedure Display_Row (Row : Gnoga.Types.Data_Map_Type);

      procedure Display_Row (Row : Gnoga.Types.Data_Map_Type) is
      begin
         AdaBlog.View.Display_Blog_Entry (Content_Area, Row);
      end Display_Row;
   begin
      User.Find_Where ("last_session ='" & Main_Window.Gnoga_Session_ID & "'");

      Main_Window.Disable_Auto_Set_View;

      Content_Area.Create (Parent => Main_Window, ID => "main-body");

      AdaBlog.Connection.Iterate
        (SQL =>
           "select username, entry_date, entry_text " & "from users, blog_entries " &
           "where users.id=blog_entries.user_id",
         Process => Display_Row'Unrestricted_Access);

      View.User_Panel (Main_Window => Main_Window, Panel => Left_Area, User_Record => User.Values);

      View.Template (Main_Window => Main_Window, Content => Content_Area, Left_Panel => Left_Area);

      User_Login.Attach_Using_Parent (Main_Window, "login-button");
      User_Login.On_Click_Handler (On_User_Login'Access);

      Create_User.Attach_Using_Parent (Main_Window, "create-button");
      Create_User.On_Click_Handler (On_Create_User'Access);

      Connection.Hold;
   end Index;

   procedure New_Entry
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      User : AdaBlog.Model.Users.Active_Record;

      Content_Area : Gui.Element.Common.DIV_Type;
      Left_Area    : Gui.Element.Common.DIV_Type;

      Submit : Gui.Element.Common.Button_Type;
   begin
      User.Find_Where ("last_session ='" & Main_Window.Gnoga_Session_ID & "'");

      if User.Value ("id") = "" then
         Main_Window.Location.URL ("/");
         return;
      end if;

      Main_Window.Disable_Auto_Set_View;

      View.New_Entry_Form (Main_Window => Main_Window, Content => Content_Area);

      View.User_Panel (Main_Window => Main_Window, Panel => Left_Area, User_Record => User.Values);

      View.Template (Main_Window => Main_Window, Content => Content_Area, Left_Panel => Left_Area);

      Submit.Attach_Using_Parent (Main_Window, "submit_entry");
      Submit.On_Click_Handler (On_Submit_Entry'Access);

      Connection.Hold;
   end New_Entry;

   procedure Log_Out
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      User : AdaBlog.Model.Users.Active_Record;
   begin
      User.Find_Where ("last_session ='" & Main_Window.Gnoga_Session_ID & "'");

      if User.Value ("id") /= "" then
         User.Value ("last_session", "");
         User.Save;
      end if;

      Main_Window.Location.URL ("/");
   end Log_Out;
begin
   Application.Multi_Connect.On_Connect_Handler (Controller.Index'Unrestricted_Access, "default");
   Application.Multi_Connect.On_Connect_Handler (Controller.Index'Unrestricted_Access, "main");
   Application.Multi_Connect.On_Connect_Handler (New_Entry'Unrestricted_Access, "new_entry");
   Application.Multi_Connect.On_Connect_Handler (Log_Out'Unrestricted_Access, "logout");
end AdaBlog.Controller;
