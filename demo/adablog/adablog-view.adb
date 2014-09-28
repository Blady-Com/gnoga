package body AdaBlog.View is
   use Gnoga;
   use Gnoga.Gui;

   procedure Display_Blog_Entry
     (Parent : in out Gnoga.Gui.Element.Common.DIV_Type'Class;
      Data   : in     Gnoga.Types.Data_Map_Type)
   is
      Entry_Div : Gnoga.Gui.Element.Common.DIV_Type;
   begin
      Entry_Div.Create
        (Parent,
         "<div class='blog_entry'>" &
           "<div class='blog_user'>" & Data.Element ("username") & "</div>" &
           "<div class='blog_date'>&nbsp;said on " &
           Data.Element ("entry_date") & ":</div><br />" &
           "<div class='blog_text'>" & Data.Element ("entry_text") &
           "</div></div>");
      Entry_Div.Place_Inside_Bottom_Of (Parent);
   end Display_Blog_Entry;

   procedure New_Entry_Form
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Content     : in out Gnoga.Gui.Element.Common.DIV_Type'Class)
   is
   begin
      Content.Create (Parent  => Main_Window,
                      Content =>
                        "<div><form>" &
                        "Blog Entry: <br />" &
                        "<textarea cols=60 rows=10 id='entry_text' " &
                        "name=""entry_text""></textarea>" &
                        "<input type='button' id='submit_entry' " &
                        "value='Submit'></form></div>",
                      ID      => "main-body");
   end New_Entry_Form;

   procedure User_Panel
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Panel       : in out Gnoga.Gui.Element.Common.DIV_Type'Class;
      User_Record : in     Gnoga.Types.Data_Map_Type)
   is
   begin
      if not User_Record.Contains ("username") then
         Panel.Create
           (Parent  => Main_Window,
            Content =>
              "<form>" &
              "Username: <input type=text name=""username"" " &
              "id='username' size='20'><br />" &
              "Password : <input type=password name=""pass"" " &
              "id='pass' size='20'><br />" &
              "<div id='verify-pass' style='display:none'> " &
              "Verify Password : <input type=password name=""pass2"" " &
              "id='pass2' size='20'><br /></div>" &
              "<input id=""login-button"" type=""button""" &
              " value=""Submit""> " &
              "<input id=""create-button"" type=""button""" &
              " value=""Create""> <br/>" &
              "</form>",
            ID => "left-panel");
      else
         Panel.Create
           (Parent  => Main_Window,
            Content =>
              " Username: <b>" & User_Record.Element ("username") &
              "</b><br /><br />" &
              "<a href=""/new_entry" &
              """>Add Entry</a><br /><br />" &
              "<a href=""/logout"">Logout</a ><br/>",
            ID      => "left-panel");
      end if;
   end User_Panel;

   procedure Template
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Content     : in out Gnoga.Gui.Element.Common.DIV_Type'Class;
      Left_Panel  : in out Gnoga.Gui.Element.Common.DIV_Type'Class)
   is
      Title_Div    : Element.Common.DIV_Type;
      Message_Area : Element.Common.DIV_Type;
   begin
      Main_Window.Document.Load_CSS ("/css/adablog.css");

      Title_Div.Create (Parent  => Main_Window,
                        Content => "AdaBlog",
                        ID      => "title");
      Title_Div.Place_Inside_Top_Of (Main_Window.Document.Body_Element.all);

      Left_Panel.Place_After (Title_Div);

      Content.Place_After (Left_Panel);

      Message_Area.Create (Main_Window, ID => "message");
      Message_Area.Place_Inside_Top_Of (Content);
   end Template;
end AdaBlog.View;
