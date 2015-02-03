--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_WebSocket_Duplex_Servers               Luebeck            --
--  Test WebSocket                                 Winter, 2014       --
--  Implementation                                                    --
--                                                                    --
--                                Last revision :  21:26 01 Feb 2015  --
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

with Ada.Calendar;           use Ada.Calendar;
with Ada.Text_IO;            use Ada.Text_IO;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;

with Ada.Unchecked_Deallocation;

package body Test_WebSocket_Duplex_Servers is

   CRLF : constant String := Character'Val (13) & Character'Val (10);
   Page : constant String :=
      "<!DOCTYPE html>" & CRLF &
      "<html lang=""en"">" & CRLF &
      "<head>" & CRLF &
      "   <meta charset=""utf-8"">" & CRLF &
      "   <title>WebSockets Duplex Test</title>" & CRLF &
      "</head>" & CRLF &
      "<body>" & CRLF &
      "   <div id=""page-wrapper"">" & CRLF &
      "      <h1>WebSockets Test</h1>" & CRLF &
      "      <div id=""status"">Connecting...</div>" & CRLF &
      "      <ul id=""messages""></ul>" & CRLF &
      "      <form id=""message-form"" action=""#""" &
      "            method=""post"">" & CRLF &
      "         <textarea id=""message""" &
      "                   placeholder=""Write file name here...""" &
      "                   required></textarea>" & CRLF &
      "         <button type=""submit"">Show file</button>" & CRLF &
      "         <button type=""button""" &
      "                 id=""close"">Disconnect</button>" & CRLF &
      "      </form>" & CRLF &
      "   </div>" & CRLF &
      "   <script 'text/javascript'>" & CRLF &
      "      window.onload = function ()" & CRLF &
      "      {" & CRLF &
      "            // Elements on the page" & CRLF &
      "         var Form =" & CRLF &
      "             document.getElementById ('message-form');" & CRLF &
      "         var Message =" & CRLF &
      "             document.getElementById ('message');" & CRLF &
      "         var List =" & CRLF &
      "             document.getElementById ('messages');" & CRLF &
      "         var Status =" & CRLF &
      "             document.getElementById ('status');" & CRLF &
      "         var Close =" & CRLF &
      "             document.getElementById ('close');" & CRLF &
      "            // Create a new WebSocket" & CRLF &
      "         var Socket = new WebSocket ('ws://localhost');" & CRLF &
      "         Socket.onerror = function (error)" & CRLF &
      "         {  // Socket error" & CRLF &
      "            console.log ('Error: ' + error);" & CRLF &
      "         };" & CRLF &
      "         Socket.onopen = function (event)" & CRLF &
      "         {  // Opened" & CRLF &
      "            Status.innerHTML = 'Connected to: '" &
      "                            + event.currentTarget.URL;" & CRLF &
      "            Status.className = 'open';" & CRLF &
      "         };" & CRLF &
      "         Socket.onmessage = function (event)" & CRLF &
      "         {  // Incoming message" & CRLF &
      "            var message = event.data;" & CRLF &
      "            List.innerHTML +=" & CRLF &
      "               '<li class=""received""><span>Received:</span>'" &
      "             + message + '</li>';" & CRLF &
      "         };" & CRLF &
      "         Socket.onclose = function (event)" & CRLF &
      "         {  // Disconnected" & CRLF &
      "            Status.innerHTML = 'Disconnected';" & CRLF &
      "            Status.className = 'closed';" & CRLF &
      "         };" & CRLF &
      "         Form.onsubmit = function (e)" & CRLF &
      "         {  // Send when the form is submitted" & CRLF &
      "            e.preventDefault ();" & CRLF &
      "            var Text = Message.value; // Text" & CRLF &
      "            Socket.send (Text); // Send the message" & CRLF &
      "            List.innerHTML += // To the list" & CRLF &
      "               '<li class=""sent""><span>Sent:</span>'" &
      "             + Text + '</li>';" & CRLF &
      "            Message.value = ''; // Clear the field" & CRLF &
      "            return false;" & CRLF &
      "         };" & CRLF &
      "         Close.onclick = function (e)" & CRLF &
      "         {  // When the close button is clicked" & CRLF &
      "            e.preventDefault ();" & CRLF &
      "            Socket.close(); // Close the socket" & CRLF &
      "            return false;" & CRLF &
      "         };" & CRLF &
      "      };" & CRLF &
      "   </script>" & CRLF &
      "</body>" & CRLF &
      "</html>";

   function Create
            (  Factory  : access Chat_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      Result :=
         new Chat_Client
             (  Listener       => Listener.all'Unchecked_Access,
                Request_Length => Factory.Request_Length,
                Input_Size     => Factory.Input_Size,
                Output_Size    => Factory.Output_Size
             );
      Receive_Body_Tracing (Chat_Client (Result.all), True);
      return Result;
   end Create;

   procedure Do_Get_Head
             (  Client : in out Chat_Client;
                Get    : Boolean
             )  is
      Status : Status_Line renames Get_Status_Line (Client);
   begin
      case Status.Kind is
         when None =>
            Reply_Text (Client, 404, "Not found", "Not found");
         when File =>
            if Status.File = "" or else Status.File = "index.htm" then
               Send_Status_Line (Client, 200, "OK");
               Send_Date (Client);
               Send_Server (Client);
               Send_Content_Type (Client, "text/html");
               Accumulate_Body (Client, Page);
               Send_Body (Client, Get);
            else
               Reply_Text
               (  Client,
                  404,
                  "Not found",
                  "No file " & Quote (Status.File) & " found"
               );
            end if;
         when URI =>
            Reply_Text
            (  Client,
               404,
               "Not found",
               "No URI " & Quote (Status.Path) & " found"
            );
      end case;
   end Do_Get_Head;

   procedure Do_Get (Client : in out Chat_Client) is
   begin
      Do_Get_Head (Client, True);
   end Do_Get;

   procedure Do_Head (Client : in out Chat_Client) is
   begin
      Do_Get_Head (Client, False);
   end Do_Head;

   procedure WebSocket_Finalize (Client : in out Chat_Client) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Chat_Task, Chat_Task_Ptr);
   begin
      if Client.Chatter /= null then
         if not Client.Chatter'Terminated then
            Client.Chatter.Stop;
         end if;
         while not Client.Chatter'Terminated loop
            delay 0.010;
         end loop;
         Free (Client.Chatter);
      end if;
   end WebSocket_Finalize;

   procedure WebSocket_Initialize (Client : in out Chat_Client) is
   begin
      Client.Chatter := new Chat_Task (Client'Unchecked_Access);
   end WebSocket_Initialize;

   function WebSocket_Open
            (  Client : access Chat_Client
            )  return WebSocket_Accept is
   begin
      return (True, 0, 1024, True, False, "");
   end WebSocket_Open;

   procedure WebSocket_Received
             (  Client  : in out Chat_Client;
                Message : String
             ) is
   begin
      Client.Chatter.Show (Message);
   end WebSocket_Received;

   function Fill (Length : Positive) return String is
      Result : String (1..Length);
   begin
      for Index in Result'Range loop
         Result (Index) :=
            Character'Val (Character'Pos ('0') + Index mod 10);
      end loop;
      return Result;
   end Fill;

   task body Chat_Task is
      Buffer  : String (1..1024*10);
      Exiting : Boolean := False;
      Last    : Integer;
   begin
      Trace (Client.all, "Task started");
      WebSocket_Send (Client.all, "Random length strings");
      for Length in 250..1_000 loop
         select
            accept Stop;
            Exiting := True;
            exit;
         else
            null;
         end select;
         Trace
         (  Client.all,
            "Sending length" & Integer'Image (Length)
         );
         declare
            Header : String := "Length" & Integer'Image (Length) & ' ';
         begin
            WebSocket_Send
            (  Client.all,
               Header & Fill (Length - Header'Length)
            );
         end;
      end loop;
      while not Exiting loop
         select
            accept Stop;
            Exiting := True;
            exit;
         or accept Show (Name : String) do
               Buffer (1..Name'Length) := Name;
               Last := Name'Length;
            end Show;
            declare
               File : File_Type;
            begin
               Open (File, In_File, Buffer (1..Last));
               declare
                  Pointer : Integer;
               begin
                  for Index in Positive'Range loop
                     Pointer := 1;
                     Put
                     (  Destination => Buffer,
                        Pointer     => Pointer,
                        Value       => Index,
                        Field       => 3,
                        Justify     => Strings_Edit.Right,
                        Fill        => ' '
                     );
                     Buffer (4) := ':';
                     Get_Line (File, Buffer (4..Buffer'Last), Last);
                     begin
                        WebSocket_Send (Client.all, Buffer (1..Last));
                     exception
                        when End_Error =>
                           Close (File);
                           exit;
                     end;
                  end loop;
               exception
                  when End_Error =>
                     Close (File);
                  when Error : others =>
                      WebSocket_Send
                      (  Client.all,
                         "Read error:" & Exception_Message (Error)
                      );
                     Close (File);
               end;
            exception
               when Error : others =>
                  WebSocket_Send
                  (  Client.all,
                     "Open error:" & Exception_Message (Error)
                  );
            end;
         or delay 1.0;
         end select;
      end loop;
      Trace (Client.all, "Task stopped");
   exception
      when Error : others =>
         Trace
         (  Client.all,
            "Task failed: " & Exception_Information (Error)
         );
         accept Stop;
   end Chat_Task;

end Test_WebSocket_Duplex_Servers;
