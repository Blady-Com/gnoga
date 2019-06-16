--                                                                    --
--  package Test_MQTT_Serials       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2018       --
--                                                                    --
--                                Last revision :  14:04 26 Dec 2018  --
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

with Ada.Streams;                   use Ada.Streams;
with GNAT.Serial_Communications;    use GNAT.Serial_Communications;
with GNAT.Sockets.Server;           use GNAT.Sockets.Server;
with GNAT.Sockets.Server.Blocking;  use GNAT.Sockets.Server.Blocking;

package Test_MQTT_Serials is

   type MQTT_Serial_Server
        (  Port          : access Serial_Port;
           Factory       : access Connections_Factory'Class;
           Input_Stream  : access Root_Stream_Type'Class;
           Output_Stream : access Root_Stream_Type'Class;
           Input_Size    : Positive
        )  is new Blocking_Server
                  (  Factory       => Factory,
                     Input_Stream  => Input_Stream,
                     Output_Stream => Output_Stream,
                     Input_Size    => Input_Size
                  )  with null record;
   procedure Cancel_IO
             (  Server : in out MQTT_Serial_Server;
                Client : in out Connection'Class
             );

end Test_MQTT_Serials;
