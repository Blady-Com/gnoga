--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_WebSocket_Duplex_Servers               Luebeck            --
--  Test WebSocket                                 Winter, 2014       --
--  Interface                                                         --
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

with Ada.Exceptions;       use Ada.Exceptions;
with GNAT.Sockets;         use GNAT.Sockets;
with GNAT.Sockets.Server;  use GNAT.Sockets.Server;

with GNAT.Sockets.Connection_State_Machine.HTTP_Server;
use  GNAT.Sockets.Connection_State_Machine.HTTP_Server;

package Test_WebSocket_Duplex_Servers is
--
-- Chat_Factory -- Creates chat connection objects
--
   type Chat_Factory
        (  Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Max_Connections : Positive
        )  is new Connections_Factory with null record;
   function Create
            (  Factory  : access Chat_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
--
-- Chat_Client -- Chat HTTP site
--
   type Chat_Client is new HTTP_Client with private;
   procedure Do_Get  (Client : in out Chat_Client);
   procedure Do_Head (Client : in out Chat_Client);
   procedure WebSocket_Finalize (Client : in out Chat_Client);
   procedure WebSocket_Initialize (Client : in out Chat_Client);
   function WebSocket_Open
            (  Client : access Chat_Client
            )  return WebSocket_Accept;
   procedure WebSocket_Received
             (  Client  : in out Chat_Client;
                Message : String
             );
private
   task type Chat_Task (Client : access Chat_Client'Class) is
      entry Show (Name : String);
      entry Stop;
   end Chat_Task;
   type Chat_Task_Ptr is access Chat_Task;

   type Chat_Client is new HTTP_Client with record
      Chatter : Chat_Task_Ptr;
   end record;

end Test_WebSocket_Duplex_Servers;
