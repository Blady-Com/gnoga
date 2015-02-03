--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_WebSocket_Servers                      Luebeck            --
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

with Ada.Calendar;         use Ada.Calendar;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;

package body Test_WebSocket_Servers is

   CRLF : constant String := Character'Val (13) & Character'Val (10);
   Page : constant String :=
      "<!DOCTYPE html>" & CRLF &
      "<html lang=""en"">" & CRLF &
      "<head>" & CRLF &
      "   <meta charset=""utf-8"">" & CRLF &
      "   <title>WebSockets Test</title>" & CRLF &
      "</head>" & CRLF &
      "<body>" & CRLF &
      "   <div id=""page-wrapper"">" & CRLF &
      "      <h1>WebSockets Test</h1>" & CRLF &
      "      <div id=""status"">Connecting...</div>" & CRLF &
      "      <ul id=""messages""></ul>" & CRLF &
      "      <form id=""message-form"" action=""#""" &
      "            method=""post"">" & CRLF &
      "         <textarea id=""message""" &
      "                   placeholder=""Write message here...""" &
      "                   required></textarea>" & CRLF &
      "         <button type=""submit"">Send Message</button>" & CRLF &
      "         <button type=""button""" &
      "                 id=""close"">Close Connection</button>" & CRLF &
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

   function WebSocket_Open
            (  Client : access Chat_Client
            )  return WebSocket_Accept is
   begin
      return
      (  Accepted  => True,
         Length    => 0,
         Size      => 125,
         Duplex    => False,
         Chunked   => True,
         Protocols => ""
      );
   end WebSocket_Open;

   procedure WebSocket_Received
             (  Client  : in out Chat_Client;
                Message : String
             ) is
   begin
      WebSocket_Send
      (  Client,
         (  To_HTTP (Clock)
         &  " last chunk:"
         &  Quote (Message)
      )  );
      Client.Chunk := 0;
   end WebSocket_Received;

   procedure WebSocket_Received_Part
             (  Client  : in out Chat_Client;
                Message : String
             ) is
   begin
      Client.Chunk := Client.Chunk + 1;
      WebSocket_Send
      (  Client,
         (  To_HTTP (Clock)
         &  Integer'Image (Client.Chunk)
         &  " chunk:"
         &  Quote (Message)
      )  );
   end WebSocket_Received_Part;

end Test_WebSocket_Servers;
