--                                                                    --
--  package Test_HTTP_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Test server secure factory                     Luebeck            --
--  Implementation                                 Winter, 2015       --
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

package body Test_HTTP_Servers.Secure is

   function Create
            (  Factory  : access Anonymous_HTTPS_Factory;
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

   function Create
            (  Factory  : access X509_HTTPS_Factory;
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

end Test_HTTP_Servers.Secure;
