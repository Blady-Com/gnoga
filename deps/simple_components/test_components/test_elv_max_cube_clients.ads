--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_ELV_MAX_Cube_Clients                   Luebeck            --
--  ELV MAX! Cube test client                      Summer, 2015       --
--                                                                    --
--                                Last revision :  10:33 11 May 2019  --
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

with GNAT.Sockets.Server;  use GNAT.Sockets.Server;

with GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;
use  GNAT.Sockets.Connection_State_Machine.ELV_MAX_Cube_Client;

package Test_ELV_MAX_Cube_Clients is

   type Test_Type is
        (  Mode_Set,
           Delete_Device,
           NTP_Servers_Set,
           Device_Pairing,
           Device_Pair_And_Add
        );
   type Test_Client
        (  Listener : access Connections_Server'Class
        )  is new ELV_MAX_Cube_Client
                  (  Listener    => Listener,
                     Line_Length => 4000,
                     Input_Size  => 80,
                     Output_Size => 200
                  )  with
   record
      Test_Mode : Test_Type := Mode_Set;
   end record;

   procedure Configuration_Updated
             (  Client : in out Test_Client;
                Update : Update_Data
             );

end Test_ELV_MAX_Cube_Clients;
