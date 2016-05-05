--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_HTTP_SQLite_Servers                    Luebeck            --
--  Test server to browse SQLite3 Database         Winter, 2014       --
--  Implementation                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

package body Test_HTTP_SQLite_Servers is

   CRLF : constant String := Character'Val (13) & Character'Val (10);

   Forms_Page : constant String :=
                (  "<form action=""input.htm"" method=""post"">" & CRLF
                &  "Select an SQLite3 database file: "
                &  "<input type=""file"" name=""database"">" & CRLF
                &  "<input type=""submit"" name=""submit"" "
                &  "value=""Show tables"">" & CRLF
                &  "<input type=""submit"" name=""submit"" "
                &  "value=""Make query"">" & CRLF
                &  "</form>" & CRLF
                &  "<p>Note that the file path must be absolute or "
                &  "else refer to the directory where this test "
                &  "server is running. That is because of security "
                &  "reasons.</p>"
                );

   function Create
            (  Factory  : access Test_HTTP_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      if Get_Clients_Count (Listener.all) < Factory.Max_Connections then
         Result :=
            new Test_Client
                (  Listener       => Listener.all'Unchecked_Access,
                   Request_Length => Factory.Request_Length,
                   Input_Size     => Factory.Input_Size,
                   Output_Size    => Factory.Output_Size
                );
         Receive_Body_Tracing (Test_Client (Result.all), True);
         return Result;
      else
         return null;
      end if;
   end Create;

   procedure Do_Body (Client : in out Test_Client) is
      Status : Status_Line renames Get_Status_Line (Client);
   begin
      if Status.Kind = File then
         if Status.File = "input.htm" then
            Receive_Body (Client, "file,database,submit");
         elsif Status.File = Get_Query_Action (Client.Query) then
            Receive_Body (Client, "statement,submit");
         end if;
      end if;
   end Do_Body;

   procedure Do_Get_Head
             (  Client : in out Test_Client;
                Get    : Boolean
             )  is
      procedure Do_Content_Page (Table : String) is
      begin
         Set_Database (Client.Content, Client.Root);
         Set_Table (Client.Content, Table);
         Send_Status_Line (Client, 200, "OK");
         Send_Date (Client);
         Send_Server (Client);
         Send_Content_Type (Client, "text/html");
         Send_Body (Client, Client.Content'Access);
      end Do_Content_Page;

      procedure Do_Root_Page is
      begin
         Send_Status_Line (Client, 200, "OK");
         Send_Date (Client);
         Send_Server (Client);
         Send_Content_Type (Client, "text/html");
         Accumulate_Body
         (  Client,
            (  "<html><head><title>Select database</title>"
            &  "</head><body>" & CRLF
            &  Forms_Page
            &  "</body></html>"
         )  );
         Send_Body (Client, Get);
      end Do_Root_Page;

      procedure Do_Schema_Page (Table : String) is
      begin
         Set_Database (Client.Schema, Client.Root);
         Set_Table (Client.Schema, Table);
         Send_Status_Line (Client, 200, "OK");
         Send_Date (Client);
         Send_Server (Client);
         Send_Content_Type (Client, "text/html");
         Send_Body (Client, Client.Schema'Access);
      end Do_Schema_Page;

      Status : Status_Line renames Get_Status_Line (Client);
   begin
      case Status.Kind is
         when None =>
            Do_Root_Page;
         when File =>
            if Status.File = "" then
               Do_Root_Page;
            elsif Is_Prefix
                  (  Get_Query_Action (Client.Query),
                     Status.File
                  )  then
               Send_Status_Line (Client, 200, "OK");
               Send_Date (Client);
               Send_Server (Client);
               Send_Content_Type (Client, "text/html");
               Set_Query (Client);
            else
               declare
                  DB_Path : constant String :=
                            Get_Database_Path (Client.Root);
                  Pointer : Integer := Status.File'First;
                  Start   : Integer;
               begin
                  if Is_Prefix (DB_Path & '/', Status.File) then
                     Pointer := Pointer + DB_Path'Length + 1;
                     Start   := Pointer;
                     while Pointer <= Status.File'Last loop
                        if Status.File (Pointer) = '/' then
                           if (  "content.htm"
                              =  Status.File
                                 (  Pointer + 1
                                 .. Status.File'Last
                              )  )  then
                              Do_Content_Page
                              (  Status.File (Start..Pointer - 1)
                              );
                              return;
                           elsif (  "schema.htm"
                                 =  Status.File
                                    (  Pointer + 1
                                    .. Status.File'Last
                                 )  )  then
                              Do_Schema_Page
                              (  Status.File (Start..Pointer - 1)
                              );
                              return;
                           else
                              exit;
                           end if;
                        end if;
                        Pointer := Pointer + 1;
                     end loop;
                  end if;
                  Reply_Text
                  (  Client,
                     404,
                     "Not found",
                     (  "No file "
                     &  Quote (Status.File)
                     &  " found"
                  )  );
               exception
                  when Error : others =>
                     Send_Status_Line (Client, 200, "OK");
                     Send_Date (Client);
                     Send_Server (Client);
                     Send_Content_Type (Client, "text/html");
                     Accumulate_Body
                     (  Client,
                        (  "<html><head><title>Database fault</title>"
                        &  "</head><body>" & CRLF
                        &  "<p>Database file "
                        &  To_HTML (Quote (DB_Path))
                        &  " access fault:</p>"
                        &  To_HTML (Exception_Information (Error))
                        &  "</body></html>"
                     )  );
                     Send_Body (Client);
               end;
            end if;
         when URI =>
            Reply_Text
            (  Client,
               404,
               "Not found",
               "No URI " & Quote (Status.File) & " found"
            );
      end case;
   end Do_Get_Head;

   procedure Do_Get (Client : in out Test_Client) is
   begin
      Do_Get_Head (Client, True);
   end Do_Get;

   procedure Do_Head (Client : in out Test_Client) is
   begin
      Do_Get_Head (Client, False);
   end Do_Head;

   procedure Do_Post (Client : in out Test_Client) is
      Status : Status_Line renames Get_Status_Line (Client);
   begin
      if Status.Kind = File then
         if Status.File = "input.htm" then
            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            Send_Server (Client);
            Send_Content_Type (Client, "text/html");
            declare
               File : constant String :=
                               Get_CGI_Value (Client, "database");
            begin
               if File = "" then
                  Accumulate_Body
                  (  Client,
                     (  "<html><head><title>Database error</title>"
                     &  "</head><body>" & CRLF
                     &  "<p>No database specified</p>" & CRLF
                     &  Forms_Page & CRLF
                     &  "</body></html>"
                  )  );
                  Send_Body (Client);
                  return;
               end if;
               if Get_CGI_Value (Client, "submit") = "Show tables" then
                  Set_Database_Path (Client.Root, File);
                  Send_Body (Client, Client.Root'Access);
               else
                  Set_Database_Path (Client.Query, File);
                  Set_Query (Client);
               end if;
            exception
               when Error : Data_Error =>
                  Accumulate_Body
                  (  Client,
                     (  "<html><head><title>Database error</title>"
                     &  "</head><body>" & CRLF
                     &  "<p>Database "
                     &  To_HTML (Quote (File))
                     &  " error:</p>"
                     &  To_HTML (Exception_Message (Error)) & CRLF
                     &  Forms_Page & CRLF
                     &  "</body></html>"
                  )  );
                  Send_Body (Client);
               when Error : Use_Error =>
                  Accumulate_Body
                  (  Client,
                     (  "<html><head><title>File open error</title>"
                     &  "</head><body>" & CRLF
                     &  "<p>Database file "
                     &  To_HTML (Quote (File))
                     &  " open error:</p>"
                     &  To_HTML (Exception_Message (Error)) & CRLF
                     &  Forms_Page & CRLF
                     &  "</body></html>"
                  )  );
                  Send_Body (Client);
               when Error : others =>
                  Accumulate_Body
                  (  Client,
                     (  "<html><head><title>Database fault</title>"
                     &  "</head><body>" & CRLF
                     &  "<p>Database file "
                     &  To_HTML (Quote (File))
                     &  " access fault:</p>"
                     &  To_HTML (Exception_Information (Error)) & CRLF
                     &  Forms_Page & CRLF
                     &  "</body></html>"
                  )  );
                  Send_Body (Client);
            end;
         elsif Status.File = Get_Query_Action (Client.Query) then
            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            Send_Server (Client);
            Send_Content_Type (Client, "text/html");
            Set_Query (Client);
         end if;
         return;
      end if;
      Reply_Text (Client, 404, "Not found", "Not found");
   end Do_Post;

   procedure Set_Query (Client : in out Test_Client) is
      Command : constant String := Get_CGI_Value (Client, "statement");
   begin
      Set_Statement (Client.Query, Command);
      Send_Body (Client, Client.Query'Access);
   exception
      when Error : others =>
         Accumulate_Body
         (  Client,
            (  "<html><head><title>Database fault"
            &  "</title>"
            &  "</head><body>" & CRLF
            &  "<p>Database file "
            &  To_HTML
               (  Quote
                  (  Get_Database_Path (Client.Query)
               )  )
            &  " access fault:</p>"
            &  To_HTML (Exception_Information (Error))
            &  "</body></html>"
         )  );
         Send_Body (Client);
   end Set_Query;
end Test_HTTP_SQLite_Servers;
