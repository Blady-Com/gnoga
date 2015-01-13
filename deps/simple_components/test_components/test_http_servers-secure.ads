--                                                                    --
--  package Test_HTTP_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Test server secure factory                     Luebeck            --
--  Interface                                      Winter, 2015       --
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

with GNAT.Sockets.Server.Secure;
with GNAT.Sockets.Server.Secure.Anonymous;
with GNAT.Sockets.Server.Secure.X509;

package Test_HTTP_Servers.Secure is
   use GNAT.Sockets.Server.Secure;
   use GNAT.Sockets.Server.Secure.Anonymous;
   use GNAT.Sockets.Server.Secure.X509;

   type Anonymous_HTTPS_Factory
        (  Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Decoded_Size    : Buffer_Length;
           Max_Connections : Positive
        )  is new Anonymous_Authentication_Factory with private;

   function Create
            (  Factory  : access Anonymous_HTTPS_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;

   type X509_HTTPS_Factory
        (  Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Decoded_Size    : Buffer_Length;
           Max_Connections : Positive
        )  is new X509_Authentication_Factory with private;

   function Create
            (  Factory  : access X509_HTTPS_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
private
   type Anonymous_HTTPS_Factory
        (  Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Decoded_Size    : Buffer_Length;
           Max_Connections : Positive
        )  is new Anonymous_Authentication_Factory
                  (  Decoded_Size => Decoded_Size
                  )  with null record;

   type X509_HTTPS_Factory
        (  Request_Length  : Positive;
           Input_Size      : Buffer_Length;
           Output_Size     : Buffer_Length;
           Decoded_Size    : Buffer_Length;
           Max_Connections : Positive
        )  is new X509_Authentication_Factory
                  (  Decoded_Size => Decoded_Size
                  )  with null record;

end Test_HTTP_Servers.Secure;
