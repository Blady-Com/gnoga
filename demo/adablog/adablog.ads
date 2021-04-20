with Gnoga.Server.Database.SQLite;

package AdaBlog is
   use Gnoga;
   use all type Gnoga.String;

   Connection : Gnoga.Server.Database.Connection_Access := Gnoga.Server.Database.SQLite.Connect ("adablog.db");
end AdaBlog;
