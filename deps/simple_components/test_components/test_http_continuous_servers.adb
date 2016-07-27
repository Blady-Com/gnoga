--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_HTTP_Continuous_Server                 Luebeck            --
--  Implementation                                 Winter, 2015       --
--                                                                    --
--                                Last revision :  12:48 19 Jun 2016  --
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
with Strings_Edit;         use Strings_Edit;
with Strings_Edit.Quoted;  use Strings_Edit.Quoted;

with Ada.Unchecked_Deallocation;

package body Test_HTTP_Continuous_Servers is

   Content_Timeout    : constant Duration := 1.0;
   Send_Block_Timeout : constant Duration := 120.0;

   CRLF : constant String := Character'Val (13) & Character'Val (10);
   Head : constant String :=
      "<!DOCTYPE html>" & CRLF &
      "<html lang=""en"">" & CRLF &
      "<head>" & CRLF &
      "   <meta charset=""utf-8"">" & CRLF &
      "   <title>Test continuous page</title>" & CRLF &
      "</head>" & CRLF &
      "<body>" & CRLF &
      "<p>Page starts</p>" & CRLF;

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

   procedure Do_Get_Head
             (  Client : in out Test_Client;
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
               Send_Body (Client, Client.Content'Access, Get);
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

   procedure Do_Get (Client : in out Test_Client) is
   begin
      Do_Get_Head (Client, True);
   end Do_Get;

   procedure Do_Head (Client : in out Test_Client) is
   begin
      Do_Get_Head (Client, False);
   end Do_Head;

   procedure Finalize (Client : in out Test_Client) is
     procedure Free is
        new Ada.Unchecked_Deallocation
            (  Content_Generator,
               Content_Generator_Ptr
            );
     Source : Infinite_Content renames Client.Content;
   begin
      if Source.Generator /= null then
         Source.Generator.Stop;
         while not Source.Generator'Terminated loop
            delay 0.001;
         end loop;
         Free (Source.Generator);
      end if;
   end Finalize;

   function Get (Source : access Infinite_Content) return String is
   begin
      if Source.State = 0 then
         Source.State := Source.State + 1;
         return Head;
      else
         if not Source.Ready then
            raise Content_Not_Ready;
         end if;
         Source.State := Source.State + 1;
         return "<li>Part" & Integer'Image (Source.State - 1) &
                "</li>" & CRLF;
      end if;
   end Get;

   function Get_IO_Timeout (Factory : Test_HTTP_Factory)
      return Duration is
   begin
      return Send_Block_Timeout;
   end Get_IO_Timeout;

   procedure Initialize (Source : in out Infinite_Content) is
   begin
      Source.Generator := new Content_Generator
                              (  Source.Client.all'Unchecked_Access
                              );
   end Initialize;

   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Infinite_Content
             )  is
   begin
      null;
   end Write;

   task body Content_Generator is
      Last : Time := Clock;
   begin
      loop
         select
            accept Stop;
            exit;
         else
            null;
         end select;
         delay 0.001;
         if Clock - Last > Content_Timeout then
            Last := Clock;
            Client.Content.Ready := True;
            Unblock_Send (Client.all);
         else
            Client.Content.Ready := False;
         end if;
      end loop;
   end Content_Generator;

end Test_HTTP_Continuous_Servers;
