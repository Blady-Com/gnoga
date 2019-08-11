--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_HTTPS_OpenSSL_Server                   Luebeck            --
--  HTTPS OpenSSL server test                      Winter, 2019       --
--                                                                    --
--                                Last revision :  16:05 08 Jun 2019  --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Streams;                  use Ada.Streams;
--with GNAT.Exception_Traces;      use GNAT.Exception_Traces;
with GNAT.Sockets.Server.OpenSSL;  use GNAT.Sockets.Server.OpenSSL;
with OpenSSL;                      use OpenSSL;
with Strings_Edit.Integers;        use Strings_Edit.Integers;
with Test_HTTP_Servers.OpenSSL;    use Test_HTTP_Servers.OpenSSL;

with GNAT.Sockets.Server.Pooled;

procedure Test_HTTPS_OpenSSL_Server is
   Minutes : constant := 3.0; -- * 60.0 * 24.0 * 10.0;
   Port    : constant := 443;
   Tasks   : constant := 5;
begin
-- Trace_On (Every_Raise);
   declare
      use GNAT.Sockets.Server;
      Buffer : Stream_Element_Array (1..20);
      Salt   : Stream_Element_Array (1..4);
   begin
      Put_Line ("HEre");
      RAND_Bytes (Buffer);
      Put_Line ("RAND_Bytes: " & Image (Buffer));
      RAND_Bytes (Buffer);
      Put_Line ("RAND_Bytes: " & Image (Buffer));
      RAND_Bytes (Salt);
      PKCS5_PBKDF2_HMAC_SHA1 ("My password", Salt, 1_000, Buffer);
      Put_Line ("Hash: " & Image (Buffer));
      PKCS5_PBKDF2_HMAC ("My password", Salt, 1_000, SHA512, Buffer);
      Put_Line ("Hash SHA512: " & Image (Buffer));
   end;
   declare
      Factory : aliased HTTPS_OpenSSL_Factory
                        (  Request_Length  => 200,
                           Input_Size      => 1024,
                           Output_Size     => 1024,
                           Decoded_Size    => 40,
                           Max_Connections => 100
                        );
      Max, Min : SSL_Version_No;
   begin
      Set_TLS_Tracing
      (  Factory => Factory,
         Session => True,
         Decoded => True
      );
--        Set_Default_Verify_Paths
--        (  Factory,
--           Server_Context
--        );
--        Put_Line ("Available ciphers:");
--        for Index in 1..Get_Number_Of_Ciphers
--                        (  Factory,
--                           Server_Context
--                        )  loop
--           Put_Line
--           (  "   "
--           &  Get_Name_Of_Cipher (Factory, Server_Context, Index)
--           );
--        end loop;
      Set_Proto_Version
      (  Factory,
         Server_Context,
         TLS1_2_VERSION,
         TLS1_3_VERSION
      );
      Get_Proto_Version
      (  Factory,
         Server_Context,
         Min,
         Max
      );
      Put_Line
      (  "Versions range: "
      &  Image (Integer (Min), Base => 16)
      &  ".."
      &  Image (Integer (Max), Base => 16)
      );
      Load_Verify_Locations
      (  Factory => Factory,
         Context => Server_Context
      );
--
-- Note this does not work in openssl1.1.1a for unclear reasons. Luckily
-- the same can be done in the session
--
--        Use_Certificate_PEM_File
--        (  Factory => Factory,
--           Context => Server_Context,
--           File    => "example.crt"
--        );
--        Use_RSA_Key_PEM_File
--        (  Factory => Factory,
--           Context => Server_Context,
--           File    => "example.key"
--        );
--        Check_Private_Key
--        (  Factory => Factory,
--           Context => Server_Context
--        );
      Trace_On
      (  Factory  => Factory,
         Received => GNAT.Sockets.Server.Trace_Any,
         Sent     => GNAT.Sockets.Server.Trace_Any
      );
      declare
--           Server  : GNAT.Sockets.Server.Pooled.Pooled_Server
--                     (  Factory'Access,
--                        Port,
--                        Tasks
--                     );
         Server  : GNAT.Sockets.Server.
                   Connections_Server (Factory'Access, Port);
      begin
         Put_Line ("HTTP server started");
         delay 60.0 * Minutes; -- Service
         Put_Line ("HTTP server stopping");
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_HTTPS_OpenSSL_Server;
