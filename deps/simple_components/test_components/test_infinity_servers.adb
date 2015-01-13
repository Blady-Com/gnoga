--                                                                    --
--  procedure Test_HTTP_Server      Copyright (c)  Dmitry A. Kazakov  --
--     Test_Infinity_Servers                       Luebeck            --
--  Test HTTP content flood                        Winter, 2013       --
--                                                                    --
--                                Last revision :  08:20 11 Jan 2015  --
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

package body Test_Infinity_Servers is

   CRLF : constant String := Character'Val (13) & Character'Val (10);
   Page : constant String :=
          "<!DOCTYPE html>" & CRLF &
          "<html lang=""en"">" & CRLF &
          "<head>" & CRLF &
          "   <meta charset=""utf-8"">" & CRLF &
          "   <title>Infinite flood test</title>" & CRLF &
          "</head>" & CRLF &
          "<body>" & CRLF &
          "   <p>Now flood of paragraphs to follow</p>" & CRLF;

   function Create
            (  Factory  : access Flood_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      Result :=
         new Flood_Client
             (  Listener       => Listener.all'Unchecked_Access,
                Request_Length => Factory.Request_Length,
                Input_Size     => Factory.Input_Size,
                Output_Size    => Factory.Output_Size
             );
      Receive_Body_Tracing (Flood_Client (Result.all), True);
      return Result;
   end Create;

   procedure Do_Get_Head
             (  Client : in out Flood_Client;
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
               Send_Body (Client, Client.Content'Access);
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

   procedure Do_Get (Client : in out Flood_Client) is
   begin
      Do_Get_Head (Client, True);
   end Do_Get;

   procedure Do_Head (Client : in out Flood_Client) is
   begin
      Do_Get_Head (Client, False);
   end Do_Head;

   function Get (Source : access Flood_Content) return String is
   begin
      if Source.Head then
         Source.Head := False;
         return Page;
      else
         declare
            Now : Time := Clock;
         begin
            if Now > Source.Last + 1.0 then
               Source.Last := Now;
               return "   <p>" & To_HTTP (Now) & "</p>" & CRLF;
            else
               raise Content_Not_Ready;
            end if;
         end;
      end if;
   end Get;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Flood_Content
             )  is
   begin
      null;
   end Write;

end Test_Infinity_Servers;
