--                                                                    --
--  package Test_Echo_Clients       Copyright (c)  Dmitry A. Kazakov  --
--  Test echo client                               Luebeck            --
--  Interface                                      Winter, 2015       --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Streams;          use Ada.Streams;
with GNAT.Sockets;         use GNAT.Sockets;
with GNAT.Sockets.Server;  use GNAT.Sockets.Server;

package Test_Echo_Clients is
   Session_Length : constant := 500;

   type Echo_Factory is new Connections_Factory with private;
   procedure Trace
             (  Factory    : in out Echo_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             );

   type Echo_Connection is new Connection with private;
   procedure Connected (Client : in out Echo_Connection);
   procedure Received
             (  Client  : in out Echo_Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Sent (Client : in out Echo_Connection);

private
   type Echo_Factory is new Connections_Factory with null record;
   type Echo_Connection is new Connection with record
      Messages_Count : Natural := Session_Length;
      Data_Count     : Stream_Element_Offset := 0;
   end record;

end Test_Echo_Clients;
