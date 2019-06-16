--                                                                    --
--  package Test_HTTP_Servers       Copyright (c)  Dmitry A. Kazakov  --
--  Test server OpenSSL factory                    Luebeck            --
--  Implementation                                 Winter, 2019       --
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

package body Test_HTTP_Servers.OpenSSL is

   function Create
            (  Factory  : access HTTPS_OpenSSL_Factory;
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

   procedure Handshake_Completed
             (  Factory : in out HTTPS_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             )  is
   begin -- Just dumping some information here
      Trace
      (  Factory,
         "Shared ciphers : " & Get_Shared_Ciphers (Session)
      );
      Trace (Factory, "Client supported ciphers:");
      for Index in 1..Get_Number_Of_Client_Ciphers (Session) loop
         Trace
         (  Factory,
            "   " & Get_Name_Of_Client_Cipher (Session, Index)
         );
      end loop;
   end Handshake_Completed;

   procedure Prepare
             (  Factory : in out HTTPS_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             )  is
   begin
      if Is_Incoming (Client) then -- We are servers
         --
         -- Setting up the certificate and the key.  The certificate and
         -- key for testing purposes could be created like this:
         --
         --    openssl req -x509
         --                -newkey rsa:4096
         --                -sha256
         --                -days 3650
         --                -nodes
         --                -keyout example.key
         --                -out example.crt
         --                -subj /CN=example.com
         --                -addext subjectAltName=DNS:example.com,
         --                        DNS:example.net,
         --                        IP:127.0.0.1
         --
         Use_Certificate_PEM_File (Session, "example.crt");
         Use_RSA_Key_PEM_File     (Session, "example.key");
         Check_Private_Key        (Session);
--
--           Some informational output
--
--           Trace
--           (  Factory,
--              "Ciphers : " & Get_Cipher_List (Session, 0)
--           );
--           Trace
--           (  Factory,
--              "Ciphers available for the session:"
--           );
--           for Index in 1..Get_Number_Of_Ciphers (Session) loop
--              Trace
--              (  Factory,
--                 "   " & Get_Name_Of_Cipher (Session, Index)
--              );
--           end loop;
--           Trace (Factory, "Ciphers advertised to the client:");
--           for Index in 1
--                     .. Get_Number_Of_Supported_Ciphers (Session) loop
--              Trace
--              (  Factory,
--                 "   " & Get_Name_Of_Supported_Cipher (Session, Index)
--              );
--           end loop;
      end if;
   exception
      when Error : others =>
         Trace_Error (Factory, "Session start fault", Error);
   end Prepare;

--     procedure Trace_Service_Loop
--               (  Factory : in out HTTPS_OpenSSL_Factory;
--                  Stage   : Service_Loop_Stage;
--                  Server  : in out Connections_Server'Class
--               )  is
--     begin
--        Trace (Factory, "Loop " & Service_Loop_Stage'Image (Stage));
--     end Trace_Service_Loop;

end Test_HTTP_Servers.OpenSSL;
