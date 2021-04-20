with Gnoga.Server.Model.Table;

package AdaBlog.Model is

   package Users is new Gnoga.Server.Model.Table ("users", Connection);

   package Blog_Entries is new Gnoga.Server.Model.Table ("blog_entries", Connection);

end AdaBlog.Model;
