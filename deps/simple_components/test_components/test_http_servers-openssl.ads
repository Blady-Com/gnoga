--                                                                    --
--  package Test_HTTP_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Test server OpenSSL factory                    Luebeck            --
--  Interface                                      Winter, 2019       --
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

with OpenSSL;  use OpenSSL;

with GNAT.Sockets.Server.OpenSSL;

package Test_HTTP_Servers.OpenSSL is
   use GNAT.Sockets.Server.OpenSSL;

   type HTTPS_OpenSSL_Factory
        (  Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Decoded_Size    : Buffer_Length;
           Max_Connections : Positive
        )  is new Abstract_OpenSSL_Factory with private;

   function Create
            (  Factory  : access HTTPS_OpenSSL_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
   procedure Handshake_Completed
             (  Factory : in out HTTPS_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             );
   procedure Prepare
             (  Factory : in out HTTPS_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             );
private
   type HTTPS_OpenSSL_Factory
        (  Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Decoded_Size    : Buffer_Length;
           Max_Connections : Positive
        )  is new Abstract_OpenSSL_Factory
                  (  Decoded_Size => Decoded_Size
                  )  with null record;
--     procedure Trace_Service_Loop
--               (  Factory : in out HTTPS_OpenSSL_Factory;
--                  Stage   : Service_Loop_Stage;
--                  Server  : in out Connections_Server'Class
--               );

end Test_HTTP_Servers.OpenSSL;
