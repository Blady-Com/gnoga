with Gnoga.Server.Migration;

procedure AdaBlog.Migrations (M : in out Gnoga.Server.Migration.Migration_Collection) is
begin
   --  Add migrations here
   M.Add_Migration_Up
     ("CREATE TABLE `users`" & " (" & AdaBlog.Connection.ID_Field_String & "," & "  username VARCHAR(80)," &
      "  pass VARCHAR(80)," & "  last_session VARCHAR(80))");
   M.Add_Migration_Down ("DROP TABLE `users`");

   M.Add_Migration_Up
     ("CREATE TABLE `blog_entries`" & " (" & AdaBlog.Connection.ID_Field_String & "," & "  user_id INTEGER," &
      "  entry_date DATE," & "  entry_text TEXT)");
   M.Add_Migration_Down ("DROP TABLE `blog_entries`");
end AdaBlog.Migrations;
