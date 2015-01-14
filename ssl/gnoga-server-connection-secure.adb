------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--         G N O G A . S E R V E R . C O N N E C I O N . S E C U R E        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
--                                                                          --
-- For more information please go to http://www.gnoga.com                   --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Gnoga.Server.Connection.Common; use Gnoga.Server.Connection.Common;

with GNAT.Sockets.Server; use GNAT.Sockets.Server;
with GNAT.Sockets.Connection_State_Machine.HTTP_Server;
use  GNAT.Sockets.Connection_State_Machine.HTTP_Server;
with GNAT.Sockets.Server.Secure;
with GNAT.Sockets.Server.Secure.Anonymous;
with GNAT.Sockets.Server.Secure.X509;
with GNUTLS;

package body Gnoga.Server.Connection.Secure is
   function Secure_Server_Factory return Pointer_To_Connections_Factory_Class;

   type X509_HTTPS_Factory (Request_Length  : Positive;
                            Input_Size      : Buffer_Length;
                            Output_Size     : Buffer_Length;
                            Decoded_Size    : Buffer_Length;
                            Max_Connections : Positive)
   is new GNAT.Sockets.Server.Secure.X509.X509_Authentication_Factory
     (Decoded_Size => Decoded_Size)
   with null record;

   overriding
   function Create (Factory  : access X509_HTTPS_Factory;
                    Listener : access Connections_Server'Class;
                    From     : GNAT.Sockets.Sock_Addr_Type)
                    return Connection_Ptr;

   overriding
   function Create (Factory  : access X509_HTTPS_Factory;
                    Listener : access Connections_Server'Class;
                    From     : GNAT.Sockets.Sock_Addr_Type)
                    return Connection_Ptr
   is
   begin
      return Gnoga.Server.Connection.Common.Gnoga_Client_Factory
        (Listener       => Listener.all'Unchecked_Access,
         Request_Length => Factory.Request_Length,
         Input_Size     => Factory.Input_Size,
         Output_Size    => Factory.Output_Size);
   end Create;

   procedure Register_Secure_Server
     (Certificate_File : String;
      Key_File         : String;
      Port             : Integer := 443;
      Disable_Insecure : Boolean := False)
   is
   begin
      Secure_Server := True;

      Secure_Port := GNAT.Sockets.Port_Type (Port);
      Secure_Only := Disable_Insecure;

      Secure_Crt := Ada.Strings.Unbounded.To_Unbounded_String
        (Certificate_File);
      Secure_Key := Ada.Strings.Unbounded.To_Unbounded_String (Key_File);
   end Register_Secure_Server;

   Factory : aliased  X509_HTTPS_Factory
     (Request_Length  => Max_HTTP_Request_Length,
      Input_Size      => Max_HTTP_Input_Chunk,
      Output_Size     => Max_HTTP_Output_Chunk,
      Decoded_Size    => Max_HTTP_Input_Chunk,
      Max_Connections => Max_HTTP_Connections);

   function Secure_Server_Factory return Pointer_To_Connections_Factory_Class
   is
   begin
      Add_System_Trust (Factory);
      Add_Key_From_PEM_File
        (Factory          => Factory,
         Certificate_File => Ada.Strings.Unbounded.To_String (Secure_Crt),
         Key_File         => Ada.Strings.Unbounded.To_String (Secure_Key));
      Generate_Diffie_Hellman_Parameters (Factory);

      --           Trace_On (Factory  => Factory,
      --                     Received => GNAT.Sockets.Server.Trace_Any,
      --                     Sent     => GNAT.Sockets.Server.Trace_Any);
      --           Set_TLS_Tracing (Factory => Factory,
      --                            Session => True,
      --                            Decoded => True);

      return Factory'Access;
   end Secure_Server_Factory;

begin
   Gnoga.Server.Connection.Common.Gnoga_Secure_Factory :=
     Secure_Server_Factory'Access;
end Gnoga.Server.Connection.Secure;
