--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Secure                  Luebeck            --
--  Interface                                      Winter, 2015       --
--                                                                    --
--                                Last revision :  10:32 11 May 2019  --
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

with GNUTLS;        use GNUTLS;
with Interfaces.C;  use Interfaces.C;

package GNAT.Sockets.Server.Secure is
--
-- Abstract_GNUTLS_Factory -- Abstract factory for TLS connections
--
--    Decoded_Size - Of the buffer containing decoded input
--
-- The  factories creating  servers  accepting  TLS  connections must be
-- derived from this type.  The implementation  uses  GNUTLS  for  hand-
-- shaking and encoding data.  The  authentification is handled from the
-- Prepare operation,  which is called when a connection is established
-- and a TLSsession is created. Typically it would set some certificates
-- into the session.
--
   type Abstract_GNUTLS_Factory
        (  Decoded_Size : Buffer_Length
        )  is abstract new Connections_Factory with private;
--
-- Create_Transport -- Overriding GNAT.Sockets.Server...
--
   function Create_Transport
            (  Factory  : access Abstract_GNUTLS_Factory;
               Listener : access Connections_Server'Class;
               Client   : access Connection'Class
            )  return Encoder_Ptr;
--
-- Handshake_Completed -- Handshake completion call back
--
--    Factory - The TLS connection factory
--    Client  - The client
--    Session - The TLS session
--
-- This   procedure   is  called  upon  TLS  handshake  completion.  The
-- implementation  may  use  it  in  order  to  check  the session, e.g.
-- verifying client's  certificates.  The  default  implementation  does
-- nothing.
--
-- Exceptions :
--
--    Connection_Error - Silently drop connection
--    others           - Drops connection
--
   procedure Handshake_Completed
             (  Factory : in out Abstract_GNUTLS_Factory;
                Client  : in out Connection'Class;
                Session : in out Session_Type
             );
--
-- Overriding GNAT.Sockets.Server...
--
   function Is_TLS_Capable
            (  Factory : Abstract_GNUTLS_Factory
            )  return Boolean;
--
-- Is_Trace_Decoded -- Check tracing
--
--    Factory - The TLS connection factory
--
-- Returns :
--
--    True if tracing decoded content is enabled
--
   function Is_Trace_Decoded
            (  Factory : Abstract_GNUTLS_Factory
            )  return Boolean;
--
-- Is_Trace_Session -- Check tracing
--
--    Factory - The TLS connection factory
--
-- Returns :
--
--    True if session state tracing is enabled
--
   function Is_Trace_Session
            (  Factory : Abstract_GNUTLS_Factory
            )  return Boolean;
--
-- Prepare -- TLS session
--
--    Factory - The TLS connection factory
--    Client  - The client
--    Session - The TLS session to add certificates to
--
-- This procedure  is called in order  to add server certificates to the
-- specified  session.  This happens  when  a connection  to  client  is
-- established. It can be more than one certificate.
--
   procedure Prepare
             (  Factory : in out Abstract_GNUTLS_Factory;
                Client  : in out Connection'Class;
                Session : in out Session_Type
             )  is abstract;
--
-- Set_TLS_Tracing -- Enable or disable TLS tracing
--
--   Factory - The TLS connection factory
--   Session - True if tracing of session state
--   Decoded - True if tracing decoded content must be enabled
--
-- This  procedure  is used  to enable  or disable  tracing  of security
-- actions.
--
   procedure Set_TLS_Tracing
             (  Factory : in out Abstract_GNUTLS_Factory;
                Session : Boolean;
                Decoded : Boolean
             );
private
   type Abstract_GNUTLS_Factory
        (  Decoded_Size : Buffer_Length
        )  is abstract new Connections_Factory with
   record
      Trace_Session : Boolean := False;
      Trace_Decoded : Boolean := False;
   end record;

   type TLS_Session_State is (TLS_Handshake, TLS_Exchange);

   type TLS_Session
        (  Client : access Connection'Class;
           Size   : Buffer_Length;
           Flags  : Init_Flags
        )  is new Encoder (Size) with
   record
      Session : aliased Session_Type (Flags);
      State   : TLS_Session_State := TLS_Handshake;
   end record;
--
-- Encode -- Overriding GNAT.Sockets.Server...
--
   procedure Encode
             (  Transport : in out TLS_Session;
                Client    : in out Connection'Class;
                Data      : Stream_Element_Array;
                Last      : out Stream_Element_Offset
             );
   function Get_Session
            (  Client : Connection'Class
            )  return Session_Type_Ptr;
   procedure Process
             (  Transport : in out TLS_Session;
                Listener  : in out Connections_Server'Class;
                Client    : in out Connection'Class;
                Data_Left : out Boolean
             );
   procedure Read
             (  Client  : in out Connection'Class;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure Write
             (  Client  : in out Connection'Class;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );

end GNAT.Sockets.Server.Secure;
