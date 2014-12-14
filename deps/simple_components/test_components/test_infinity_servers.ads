--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Infinity_Servers                       Luebeck            --
--  Test HTTP content flood                        Winter, 2014       --
--  Interface                                                         --
--                                Last revision :  13:01 14 Dec 2014  --
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
with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Streams;          use Ada.Streams;
with GNAT.Sockets;         use GNAT.Sockets;
with GNAT.Sockets.Server;  use GNAT.Sockets.Server;

with GNAT.Sockets.Connection_State_Machine.HTTP_Server;
use  GNAT.Sockets.Connection_State_Machine.HTTP_Server;

package Test_Infinity_Servers is
--
-- Flood_Factory -- Creates flood connection objects
--
   type Flood_Factory
        (  Request_Length  : Positive;
           Output_Size     : Buffer_Length;
           Max_Connections : Positive
        )  is new Connections_Factory with null record;
   function Create
            (  Factory  : access Flood_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
--
-- Flood_Client -- Chat HTTP site
--
   type Flood_Client is new HTTP_Client with private;
   procedure Do_Get  (Client : in out Flood_Client);
   procedure Do_Head (Client : in out Flood_Client);

private
   type Flood_Content is new Content_Source with record
      Head : Boolean := True;
      Last : Time := Clock;
   end record;
   function Get (Source : access Flood_Content) return String;
   procedure Write
             (  Stream : access Root_Stream_Type'Class;
                Item   : Flood_Content
             );
   for Flood_Content'Write use Write;

   type Flood_Client is new HTTP_Client with record
      Content : aliased Flood_Content;
   end record;

end Test_Infinity_Servers;
