--                                                                    --
--  package Test_HTTP_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Test server                                    Luebeck            --
--  Implementation                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  12:25 15 May 2015  --
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

with Ada.Streams.Stream_IO;  use Ada.Streams.Stream_IO;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

package body Test_HTTP_Servers is

   procedure Body_Error
             (  Client : in out Test_Client;
                Stream : in out Root_Stream_Type'Class;
                Error  : Exception_Occurrence
             )  is
   begin
      Save_Occurrence (Client, Error);
      Client.Content.Failed := True;
   end Body_Error;

   procedure Body_Received
             (  Client : in out Test_Client;
                Stream : in out Root_Stream_Type'Class
             )  is
   begin
      Close (Client.Content.File);
   exception
      when Error : others =>
         Save_Occurrence (Client, Error);
         Client.Content.Failed := True;
   end Body_Received;

   procedure Body_Sent
             (  Client : in out Test_Client;
                Stream : access Root_Stream_Type'Class;
                Get    : Boolean
             )  is
   begin
      if Is_Open (Client.Content.File) then
         Close (Client.Content.File);
      end if;
   end Body_Sent;

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
         Receive_Body_Tracing   (Test_Client (Result.all), True);
         Receive_Header_Tracing (Test_Client (Result.all), True);
         return Result;
      else
         return null;
      end if;
   end Create;

   procedure Do_Body (Client : in out Test_Client) is
      Status : Status_Line renames Get_Status_Line (Client);
   begin
      if Status.Kind = File then
         if Status.File = "test_forms.htm" then
            Receive_Body (Client, "text,submit,a,b");
         elsif Status.File = "test_forms_2.htm" then
            Receive_Body (Client, Client.Content.Keys'Access);
         elsif Status.File = "test_forms_4.htm" then
            Receive_Body (Client);
         else
            declare
               Disposition : String renames
                             Get_Multipart_Header
                             (  Client,
                                Content_Disposition_Header
                             );
               Pointer : aliased Integer := Disposition'First;
            begin
               if not Client.Content.Failed then
                  while Pointer < Disposition'Last loop
                     if Is_Prefix
                        (  "filename=",
                           Disposition,
                           Pointer
                        )
                     then
                        Pointer := Pointer + 9;
                        declare
                           Name : String :=
                                  Get_Quoted
                                  (  Disposition,
                                     Pointer'Access
                                  );
                        begin
                           Create
                           (  Client.Content.File,
                              Out_File,
                              Name,
                              "Text_Translation=No"
                           );
                           Receive_Body
                           (  Client,
                              Stream (Client.Content.File)
                           );
                           return;
                        exception
                           when Error : others =>
                              Save_Occurrence (Client, Error);
                              Client.Content.Failed := True;
                        end;
                     else
                        Pointer := Pointer + 1;
                     end if;
                  end loop;
               end if;
            exception
               when Error : others =>
                  Save_Occurrence (Client, Error);
                  Client.Content.Failed := True;
            end;
         end if;
      end if;
   end Do_Body;

   procedure Do_Get_Head
             (  Client : in out Test_Client;
                Get    : Boolean
             )  is
      Status : Status_Line renames Get_Status_Line (Client);
      procedure Do_Directory (Name : String) is
      begin
         Client.Content.Path (1..Name'Length) := Name;
         Client.Content.Last := Name'Length;
         Open (Client.Content.Directory, Name);
         Client.Content.Start := True;
         Send_Status_Line (Client, 200, "OK");
         Send_Date (Client);
         Send_Server (Client);
         Send_Content_Type (Client, "text/html");
         Send_Body (Client, Client.Content'Access, Get);
      end Do_Directory;
   begin
      case Status.Kind is
         when None =>
            Do_Directory (Get_Current_Dir);
         when File =>
            if Status.File = "" then
               Do_Directory (Get_Current_Dir);
            elsif Status.File = "test_forms.htm" then
               Send_Status_Line (Client, 200, "OK");
               Send_Date (Client);
               Send_Server (Client);
               Send_Content_Type (Client, "text/html");
               Accumulate_Body
               (  Client,
                  "<html><head><title>Forms</title></head><body>"
               );
               Accumulate_Body
               (  Client,
                  "<form action=""test_forms.htm"" method=""post"">"
               );
               Accumulate_Body
               (  Client,
                  "<input type=""text"" name=""text"">"
               );
               Accumulate_Body
               (  Client,
                  "<input type=""submit"" name=""submit"" " &
                  "value=""submit"">"
               );
               Accumulate_Body
               (  Client,
                  "<input type=""checkbox"" name=""a"" value=""a a"">"
               );
               Accumulate_Body
               (  Client,
                  "<input type=""checkbox"" name=""b"" value=""b+b"">"
               );
               Accumulate_Body
               (  Client,
                  "</form></body></html>"
               );
               Send_Body (Client, Get);
            elsif (  Status.File = "test_forms_2.htm"
                  or else
                     Status.File = "test_forms_4.htm"
                  )  then
               Send_Status_Line (Client, 200, "OK");
               Send_Date (Client);
               Send_Server (Client);
               Send_Content_Type (Client, "text/html");
               Accumulate_Body
               (  Client,
                  "<html><head><title>Forms</title></head><body>"
               );
               Accumulate_Body
               (  Client,
                  "<form action=""test_forms_2.htm"" method=""post"">"
               );
               Accumulate_Body
               (  Client,
                  "<input type=""text"" name=""text"">"
               );
               Accumulate_Body
               (  Client,
                  "<input type=""submit"" name=""submit"" " &
                  "value=""submit"">"
               );
               Accumulate_Body
               (  Client,
                  "<input type=""checkbox"" name=""a"" value=""a a"">"
               );
               Accumulate_Body
               (  Client,
                  "<input type=""checkbox"" name=""b"" value=""b+b"">"
               );
               Accumulate_Body
               (  Client,
                  "</form></body></html>"
               );
               Send_Body (Client, Get);
            elsif Status.File = "test_forms_3.htm" then
               Send_Status_Line (Client, 200, "OK");
               Send_Date (Client);
               Send_Server (Client);
               Send_Content_Type (Client, "text/html");
               Accumulate_Body
               (  Client,
                  "<html><head><title>Forms</title></head><body>"
               );
               Accumulate_Body
               (  Client,
                  (  "<form action=""/demo"" method=""POST"""
                  &  "target=""_self"" id=""g27"""
                  &  " enctype=""multipart/form-data"">"
                  &  "<span id=""g29"">File to upload: </span>"
                  &  "<input type=""file"" form=""g27"" value="""""
                  &  " name=""fspec"" id=""g30"">"
                  &  "<input type=""hidden"""
                  &  " form=""g27"" value=""test"" name=""Some_Text"""
                  &  " id=""g31"">"
                  &  "<input type=""submit"" "
                  &  "form=""g27"" value=""Submit File"" "
                  &  "id=""g32""></form>"
               )  );
               Accumulate_Body
               (  Client,
                  "</body></html>"
               );
               Send_Body (Client, Get);
            elsif Is_Directory (Status.File) then
               Do_Directory (Status.File);
            elsif Is_Regular_File (Status.File) then
               if Is_Open (Client.Content.File) then
                  Close (Client.Content.File);
               end if;
               Open (Client.Content.File, In_File, Status.File);
               Send_Status_Line (Client, 200, "OK");
               Send_Date (Client);
               Send_Server (Client);
               Send_Content_Type (Client, "text/plain");
               Send_Body
               (  Client,
                  Stream (Client.Content.File),
                  Stream_Element_Count (Size (Client.Content.File)),
                  Get
               );
            elsif Is_Readable_File (Status.File) then
               Send_Status_Line (Client, 200, "OK");
               Send_Date (Client);
               Send_Server (Client);
               Send_Body
               (  Client,
                  (  "File "
                  &  Quote (Status.File)
                  & " is not regular or directory"
                  ),
                  Get
               );
            else
               Reply_Text
               (  Client,
                  404,
                  "Not found",
                  "No file " & Quote (Status.File) & " found"
               );
            end if;
         when URI =>
            if Status.Path = "" then
               Do_Directory (Get_Current_Dir);
            else
               Reply_Text
               (  Client,
                  404,
                  "Not found",
                  "No URI " & Quote (Status.Path) & " found"
               );
            end if;
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
         if (  Status.File = "test_forms.htm"
            or else
               Status.File = "test_forms_2.htm"
            )
         then
            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            Send_Server (Client);
            Send_Content_Type (Client, "text/html");
            Accumulate_Body
            (  Client,
               "<html><head><title>Forms</title></head><body>"
            );
            Accumulate_Body (Client, "<p>Form values</p>");
            for Index in 1..Get_CGI_Size (Client) loop
               Accumulate_Body
               (  Client,
                  (  To_HTML (Get_CGI_Key (Client, Index))
                  &  " = "
                  &  To_HTML (Get_CGI_Value (Client, Index))
                  &  "<br>"
               )  );
            end loop;
            Accumulate_Body (Client, "</body></html>");
            Send_Body (Client);
         else
            if Client.Content.Failed then
               declare
                  Error : Exception_Occurrence;
               begin
                  Client.Content.Failed := False;
                  Get_Occurrence (Client, Error);
                  Reply_Text
                  (  Client,
                     403,
                     "Error",
                     Exception_Message (Error)
                  );
               end;
            else
               Reply_Text (Client, 200, "OK", "Created");
            end if;
         end if;
      end if;
   end Do_Post;

   procedure Finalize (Source : in out Directory_Content) is
   begin
      Finalize (Content_Source (Source));
      if Is_Open (Source.Directory) then
         Close (Source.Directory);
      end if;
      if Is_Open (Source.File) then
         Close (Source.File);
      end if;
   end Finalize;

   function Get (Source : access Directory_Content) return String is
      Prefix : constant String :=
               (  "<html><body><big>Directory "
               &  To_HTML_Escaped (Source.Path (1..Source.Last))
               &  ":</big><hr>"
               );
      Suffix : constant String :=
               (  "<hr>"
               &  "<form method='POST' enctype='multipart/form-data'>"
               &  "File to upload: <input type=file name=upfile>"
               &  "<input type=submit value=Press> to upload the file!"
               &  "</form><br>"
               &  "<a href=""test_forms.htm"">test forms</a><br>"
               &  "<a href=""test_forms_2.htm"">another test forms</a>"
               &  "<a href=""test_forms_4.htm"">yet another one</a>"
               &  "</body></html>"
               );
      function Item (Name : String) return String is
      begin
         if Name = ".." then  -- Parent
            declare
               Parent : String := Normalize_Pathname (Name);
            begin
               return
               (  "<a href="""
               &  To_Escaped (Parent)
               &  """>..</a><br>"
               &  Character'Val (13)
               &  Character'Val (10)
               );
            end;
         else
            return
            (  "<a href="""
            &  To_Escaped (Name)
            &  """>"
            &  To_HTML_Escaped (Name)
            &  "</a><br>"
            &  Character'Val (13)
            &  Character'Val (10)
            );
         end if;
      end Item;
   begin
      if Is_Open (Source.Directory) then
         loop
            Read (Source.Directory, Source.Path, Source.Last);
            if Source.Last = 0 then -- No more files in the directory
               Close (Source.Directory);
               if Source.Start then
                  Source.Start := False;
                  return Prefix & Suffix;
               else
                  return Suffix;
               end if;
            elsif Source.Path (1..Source.Last) = "." then  -- Ignore
               null;
            elsif Source.Path (1..Source.Last) = ".." then  -- Ignore
               null;
            else -- A file found
               if Source.Start then
                  Source.Start := False;
                  return Prefix & Item (Source.Path (1..Source.Last));
               else
                  return Item (Source.Path (1..Source.Last));
               end if;
            end if;
         end loop;
      else
         return "";
      end if;
   end Get;

   procedure Initialize (Client : in out Test_Client) is
      use CGI_Keys;
   begin
      Initialize (HTTP_Client (Client));
      Add (Client.Content.Keys, "text",   null);
      Add (Client.Content.Keys, "submit", null);
      Add (Client.Content.Keys, "a",      null);
      Add (Client.Content.Keys, "b",      null);
   end Initialize;

   function To_HTML_Escaped (Text : String) return String is
      Length : Natural := 0;
   begin
      for Index in Text'Range loop
         case Text (Index) is
            when '"' => Length := Length + 6;
            when '&' => Length := Length + 5;
            when '<' => Length := Length + 4;
            when '>' => Length := Length + 4;
            when others => Length := Length + 1;
         end case;
      end loop;
      declare
         Pointer : Integer := 1;
         Result  : String (1..Length);
      begin
         for Index in Text'Range loop
            case Text (Index) is
               when '"' =>
                  Put (Result, Pointer, "&quot;");
                  Pointer := Pointer + 6;
               when '&' =>
                  Put (Result, Pointer, "&amp;");
                  Pointer := Pointer + 5;
               when '<' =>
                  Put (Result, Pointer, "&lt;");
                  Pointer := Pointer + 4;
               when '>' =>
                  Put (Result, Pointer, "&gt;");
                  Pointer := Pointer + 4;
               when others =>
                  Result (Pointer) := Text (Index);
                  Pointer := Pointer + 1;
            end case;
         end loop;
         return Result (1..Pointer - 1);
      end;
   end To_HTML_Escaped;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Directory_Content
             )  is
   begin
      null;
   end Write;

end Test_HTTP_Servers;
