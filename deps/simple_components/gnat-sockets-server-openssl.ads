--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.OpenSSL                 Luebeck            --
--  Interface                                      Winter, 2019       --
--                                                                    --
--                                Last revision :  09:42 12 Dec 2020  --
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

with OpenSSL;       use OpenSSL;
with Interfaces.C;  use Interfaces.C;

package GNAT.Sockets.Server.OpenSSL is
--
-- Context_Type -- The context where settings apply
--
   type Context_Type is (Client_Context, Any_Context, Server_Context);
--
-- Abstract_OpenSSL_Factory -- Factory for OpenSSL connections
--
--    Decoded_Size - Of the buffer containing decoded input
--
-- The  factories creating  servers  accepting  TLS  connections must be
-- derived from this type.  The implementation  uses  OpenSSL for  hand-
-- shaking and encoding data.  The  authentification is handled from the
-- Prepare operation,  which is called  when a connection is established
-- and a session is created.  Typically  it would set  some certificates
-- into the session.  Alternatively  a certificate can  be set  into the
-- factory and will be inherited by all new sessions.
--
   type Abstract_OpenSSL_Factory
        (  Decoded_Size : Buffer_Length
        )  is abstract new Connections_Factory with private;
--
-- Check_Private_Key -- Check the key and certificate
--
--    Session - The OpenSSL session
--
-- This procedure  checks  the consistency  of a private  key  with  the
-- corresponding certificate.
--
-- Exceptions :
--
--    Constraint_Error - Consistency check error
--
   procedure Check_Private_Key (Session : SSL);
--
-- Check_Private_Key -- Check the key and certificate
--
--    Factory - The OpenSSL connection factory
--    Context - Where the cipher is set
--
-- This procedure  checks  the consistency  of a private  key  with  the
-- corresponding certificate.
--
-- Exceptions :
--
--    Constraint_Error - Consistency check error
--
   procedure Check_Private_Key
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             );
--
-- Clear_Options -- Of a session
--
--    Session - The OpenSSL session
--    Options - Options to clear
--
   procedure Clear_Options (Session : SSL; Options : SSL_OP);
--
-- Clear_Options -- Of a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    Options - Options inherited by new connections to clear
--
   procedure Clear_Options
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Options : SSL_OP
             );
--
-- Create_Transport -- Overriding GNAT.Sockets.Server...
--
   function Create_Transport
            (  Factory  : access Abstract_OpenSSL_Factory;
               Listener : access Connections_Server'Class;
               Client   : access Connection'Class
            )  return Encoder_Ptr;
--
-- Get_Cipher_List -- Get list of ciphers
--
--    Session  - The OpenSSL session
--    Priority - The returned chipers must have equal or greater
--
-- Returns :
--
--    The list
--
   function Get_Cipher_List (Session : SSL; Priority : int)
      return String;
--
-- Get_Name_Of_Cipher -- Name of an available cipher for the session
--
--    Session - The OpenSSL session
--    Index   - Of the cipher 1..Get_Number_Of_Ciphers
--
-- Returns :
--
--    The cipher name
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--
   function Get_Name_Of_Cipher
            (  Session : SSL;
               Index   : Positive
            )  return String;
--
-- Get_Name_Of_Cipher -- Name of an available cipher
--
--    Factory - The OpenSSL connection factory
--    Context - Where the cipher is set
--    Index   - Of the cipher 1..Get_Number_Of_Ciphers
--
-- When both client and server contexts apply the client context ciphers
-- are enumerated first.
--
-- Returns :
--
--    The cipher name
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--
   function Get_Name_Of_Cipher
            (  Factory : Abstract_OpenSSL_Factory;
               Context : Context_Type;
               Index   : Positive
            )  return String;
--
-- Get_Name_Of_Client_Cipher -- Name of a client's cipher
--
--    Session - The OpenSSL session
--    Index   - Of the cipher 1..Get_Number_Of_Client_Ciphers
--
-- Returns :
--
--    The cipher name
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--
   function Get_Name_Of_Client_Cipher
            (  Session : SSL;
               Index   : Positive
            )  return String;
--
-- Get_Name_Of_Supported_Cipher -- Name of an advertised cipher
--
--    Session - The OpenSSL session
--    Index   - Of the cipher 1..Get_Number_Of_Supported_Ciphers
--
-- Returns :
--
--    The cipher name
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--
   function Get_Name_Of_Supported_Cipher
            (  Session : SSL;
               Index   : Positive
            )  return String;
--
-- Get_Number_Of_Ciphers -- Number of available ciphers for the session
--
--    Session - The OpenSSL session
--
-- Returns :
--
--    The cipher count
--
   function Get_Number_Of_Ciphers (Session : SSL) return Natural;
--
-- Get_Number_Of_Ciphers -- Number of available ciphers
--
--    Factory - The OpenSSL connection factory
--    Context - Where the cipher is set
--
-- When both client and server contexts apply the client context ciphers
-- are enumerated first.
--
-- Returns :
--
--    The cipher count
--
   function Get_Number_Of_Ciphers
            (  Factory : Abstract_OpenSSL_Factory;
               Context : Context_Type
            )  return Natural;
--
-- Get_Number_Of_Client_Ciphers -- Number of ciphers supported by client
--
--    Session - The OpenSSL session
--
-- The result is alwaus 0 if the session is a client session.
--
-- Returns :
--
--    The cipher count
--
   function Get_Number_Of_Client_Ciphers (Session : SSL) return Natural;
--
-- Get_Number_Of_Supported_Ciphers -- Number of ciphers supported
--
--    Session - The OpenSSL session
--
-- The result  is the number  of ciphers  as will  be advertised  to the
-- client.
--
-- Returns :
--
--    The cipher count
--
   function Get_Number_Of_Supported_Ciphers (Session : SSL)
      return Natural;
--
-- Get_Options -- Of a session
--
--    Session - The OpenSSL session
--
-- Returns :
--
--    The options used by the connection owning the session
--
   function Get_Options (Session : SSL) return SSL_OP;
--
-- Get_Options -- Of a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--
-- When Context  is  Any_Context  the  result  is  or-combination of the
-- options of a contexts.
--
-- Returns :
--
--    The options inherited by new connections
--
   function Get_Options
            (  Factory : Abstract_OpenSSL_Factory;
               Context : Context_Type
            )  return SSL_OP;
--
-- Get_Proto_Version -- Get proto version of a session
--
--    Session - The OpenSSL session
--    Minimal - The minimal supported version
--    Maximal - The maximal supported version
--
   procedure Get_Proto_Version
             (  Session : SSL;
                Minimal : out SSL_Version_No;
                Maximal : out SSL_Version_No
             );
--
-- Get_Proto_Version -- Get proto version of a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    Minimal - The minimal supported version
--    Maximal - The maximal supported version
--
-- When Context  specifies several  contexts Minimal  is the maximum and
-- Maximal is the minimum of the corresponding versions.
--
   procedure Get_Proto_Version
             (  Factory : Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Minimal : out SSL_Version_No;
                Maximal : out SSL_Version_No
             );
--
-- Get_Shared_Ciphers -- Get list of ciphers
--
--    Session - The OpenSSL session
--
-- This function returns a colon separated of SSL_CIPHER names  that are
-- available in both the client and the server.
--
-- Returns :
--
--    The list
--
   function Get_Shared_Ciphers (Session : SSL) return String;
--
-- Handshake_Completed -- Handshake completion call back
--
--    Factory - The OpenSSL connection factory
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
             (  Factory : in out Abstract_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             );
--
-- Overriding GNAT.Sockets.Server...
--
   function Is_TLS_Capable
            (  Factory : Abstract_OpenSSL_Factory
            )  return Boolean;
--
-- Is_Trace_Decoded -- Check tracing
--
--    Factory - The OpenSSL connection factory
--
-- Returns :
--
--    True if tracing decoded content is enabled
--
   function Is_Trace_Decoded
            (  Factory : Abstract_OpenSSL_Factory
            )  return Boolean;
--
-- Is_Trace_Session -- Check tracing
--
--    Factory - The OpenSSL connection factory
--
-- Returns :
--
--    True if session state tracing is enabled
--
   function Is_Trace_Session
            (  Factory : Abstract_OpenSSL_Factory
            )  return Boolean;
--
-- Load_Verify_Locations -- Of a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    CA_File - The file in PEM format
--    CA_Path - The directory containing files in PEM format
--
-- This procedure  specifies  the locations at which CA certificates for
-- verification  purposes are located.  The certificates  available  via
-- CA_File and CA_Path are trusted.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Load_Verify_Locations
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                CA_File : String := "";
                CA_Path : String := ""
             );
--
-- Prepare -- OpenSSL session
--
--    Factory - The TLS connection factory
--    Client  - The client
--    Session - The OpenSSL session to add certificates to
--
-- This procedure  is called in order  to add server certificates to the
-- specified  session in case it is not inherited from the factory. This
-- happens when  a connection  to client  is  established.  The  default
-- implementation does nothing.
--
   procedure Prepare
             (  Factory : in out Abstract_OpenSSL_Factory;
                Client  : in out Connection'Class;
                Session : SSL
             );
--
-- Set_Cipher_List -- Of a session
--
--    Session - The OpenSSL session
--    List    - The list of of available ciphers
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Set_Cipher_List (Session : SSL; List : String);
--
-- Set_Cipher_List -- Of a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    List    - The list of of available ciphers for future connections
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Set_Cipher_List
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                List    : String
             );
--
-- Set_Cipher_Suites -- Of a session
--
--    Session - The OpenSSL session
--    List    - The list of of available cipher suites
--
-- List is colon separated
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Set_Cipher_Suites (Session : SSL; List : String);
--
-- Set_Cipher_Suites -- Of a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    List    - The list of of available suites for future connections
--
-- This procedure  is used  to configure  the available  TLS 1.3  cipher
-- suites  for the contexts  specified  by the  parameter  Context.  The
-- format of the string List is a simple colon separated list of TLS 1.3
-- cipher suite names  in order of preference.  The valid TLS 1.3 cipher
-- suite names are:
--
--    TLS_AES_128_GCM_SHA256
--    TLS_AES_256_GCM_SHA384
--    TLS_CHACHA20_POLY1305_SHA256
--    TLS_AES_128_CCM_SHA256
--    TLS_AES_128_CCM_8_SHA256
--
-- An empty list is permissible. The default is:
--
--    TLS_AES_256_GCM_SHA384:
--    TLS_CHACHA20_POLY1305_SHA256:
--    TLS_AES_128_GCM_SHA256
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Set_Cipher_Suites
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                List    : String
             );
--
-- Set_Default_Verify_Dir -- Of a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--
-- This  procedure  specifies   the  default  directory  from  which  CA
-- certificates  are loaded.  There  is  one  default  directory and one
-- default file. This procedure sets the directory.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Set_Default_Verify_Dir
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             );
--
-- Set_Default_Verify_File -- Of a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--
-- This  procedure  specifies  the  default  directoryed from  which  CA
-- certificates  are loaded.  There  is  one  default  directory and one
-- default file. These procedure sets the file.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Set_Default_Verify_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             );
--
-- Set_Default_Verify_Paths -- Of a session
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--
-- This procedure specifies  that the  default  locations  from which CA
-- certificates  are  loaded  should  be  used.  There  is  one  default
-- directory and one default file. The default CA certificates directory
-- is called "certs" in the default OpenSSL directory. Alternatively the
-- SSL_CERT_DIR  environment  variable  can be  defined to override this
-- location.  The default  CA certificates file is called "cert.pem"  in
-- the  default  OpenSSL  directory.   Alternatively  the  SSL_CERT_FILE
-- environment variable can be defined to override this location.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Set_Default_Verify_Paths
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             );
--
-- Set_Options -- Of a session
--
--    Session - The OpenSSL session
--    Options - Options to set
--
   procedure Set_Options (Session : SSL; Options : SSL_OP);
--
-- Set_Options -- Of a session
--
--    Factory - The OpenSSL connection factory
--    Options - Options inherited by new connections to set
--
   procedure Set_Options
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Options : SSL_OP
             );
--
-- Set_Proto_Version -- Set proto version of a session
--
--    Session - The OpenSSL session
--    Minimal - The minimal supported version
--    Maximal - The maximal supported version
--
   procedure Set_Proto_Version
             (  Session : SSL;
                Minimal : SSL_Version_No;
                Maximal : SSL_Version_No
             );
--
-- Set_Proto_Version -- Get proto version of a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    Minimal - The minimal supported version
--    Maximal - The maximal supported version
--
   procedure Set_Proto_Version
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Minimal : SSL_Version_No;
                Maximal : SSL_Version_No
             );
--
-- Set_TLS_Tracing -- Enable or disable TLS tracing
--
--   Factory - The OpenSSL connection factory
--   Session - True if tracing of session state
--   Decoded - True if tracing decoded content must be enabled
--
-- This  procedure  is used  to enable  or disable  tracing  of security
-- actions.
--
   procedure Set_TLS_Tracing
             (  Factory : in out Abstract_OpenSSL_Factory;
                Session : Boolean;
                Decoded : Boolean
             );
--
-- Use_Certificate_ASN1 -- Of a context
--
--    Session     - The OpenSSL session
--    Certificate - The certificate in ASN1 format
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Certificate_ASN1
             (  Session     : SSL;
                Certificate : Stream_Element_Array
             );
--
-- Use_Certificate_ASN1 -- For a context
--
--    Factory     - The OpenSSL connection factory
--    Context     - Where the settings apply
--    Certificate - The certificate in ASN1 format
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Certificate_ASN1
             (  Factory     : in out Abstract_OpenSSL_Factory;
                Context     : Context_Type;
                Certificate : Stream_Element_Array
             );
--
-- Use_Certificate_Chain_File -- For the session
--
--    Session - The OpenSSL session
--    File    - The file containing the certificates
--
-- This procedure  loads a certificate chain from file. The certificates
-- must be in PEM format and must be sorted starting with  the subject's
-- certificate  (actual  client  or  server  certificate),  followed  by
-- intermediate CA certificates if applicable, and ending at the highest
-- level (root) CA.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Certificate_Chain_File
             (  Session : SSL;
                File    : String
             );
--
-- Use_Certificate_Chain_File -- For a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    File    - The file containing the certificates
--
-- This procedure  loads a certificate chain from file. The certificates
-- must be in PEM format and must be sorted starting with  the subject's
-- certificate  (actual  client  or  server  certificate),  followed  by
-- intermediate CA certificates if applicable, and ending at the highest
-- level (root) CA.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Certificate_Chain_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             );
--
-- Use_Certificate_{ASN1|PEM}_File -- For the session
--
--    Session - The OpenSSL session
--    File    - The file containing the certificate
--
-- These  procedures  specify  the  certificate  from  a file.  The file
-- format is ASN1 or PEM according to the name.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Certificate_ASN1_File
             (  Session : SSL;
                File    : String
             );
   procedure Use_Certificate_PEM_File
             (  Session : SSL;
                File    : String
             );
--
-- Use_Certificate_{ASN1|PEM}_File -- For a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    File    - The file containing the certificate
--
-- These  procedures  specify  the  certificate  from  a file.  The file
-- format is ASN1 or PEM according to the name.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Certificate_ASN1_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             );
   procedure Use_Certificate_PEM_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             );
--
-- Use_[RSA_]Key_ASN1 -- For the session
--
--    Session - The OpenSSL session
--    Key     - The key in ASN1 format
--
-- These  procedures  specify the key from memory.  The format  is ASN1.
-- The  variant  with  RSA  in  the  name  deals  with  RSA  keys.  If a
-- certificate has  already been set  and the private does not belong to
-- the  certificate  an error  is  returned.  To change  a  certificate,
-- private key pair  the new certificate  needs to be set before setting
-- the private key.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Key_ASN1
             (  Session : SSL;
                Key     : Stream_Element_Array
             );
   procedure Use_RSA_Key_ASN1
             (  Session : SSL;
                Key     : Stream_Element_Array
             );
--
-- Use_[RSA_]Key_{ASN1|PEM}_File -- For a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    Key     - The key in ASN1 format
--
-- These procedures specify the key from a file. The file format is ASN1
-- or PEM according to the name.  The variant with RSA in the name deals
-- with RSA keys. If a certificate has already been set  and the private
-- key  does  not belong  to the  certificate  an error is returned.  To
-- change a  certificate,  private key pair  the new  certificate  needs
-- to be set before setting the private key.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Key_ASN1
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Key     : Stream_Element_Array
             );
   procedure Use_RSA_Key_ASN1
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                Key     : Stream_Element_Array
             );
--
-- Use_[RSA_]Key_{ASN1|PEM}_File -- For the session
--
--    Session - The OpenSSL session
--    File    - The file containing the key
--
-- These procedures specify the key from a file. The file format is ASN1
-- or PEM according to the name.  The variant with RSA in the name deals
-- with RSA keys. If a certificate has already been set  and the private
-- key  does  not belong  to the  certificate  an error is returned.  To
-- change a  certificate,  private key pair  the new  certificate  needs
-- to be set before setting the private key.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Key_ASN1_File
             (  Session : SSL;
                File    : String
             );
   procedure Use_Key_PEM_File
             (  Session : SSL;
                File    : String
             );
   procedure Use_RSA_Key_ASN1_File
             (  Session : SSL;
                File    : String
             );
   procedure Use_RSA_Key_PEM_File
             (  Session : SSL;
                File    : String
             );
--
-- Use_[RSA_]Key_{ASN1|PEM}_File -- For a context
--
--    Factory - The OpenSSL connection factory
--    Context - Where the settings apply
--    File    - The file containing the key
--
-- These procedures specify the key from a file. The file format is ASN1
-- or PEM according to the name.
--
-- Exceptions :
--
--    Use_Error - An error
--
   procedure Use_Key_ASN1_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             );
   procedure Use_Key_PEM_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             );
   procedure Use_RSA_Key_ASN1_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             );
   procedure Use_RSA_Key_PEM_File
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type;
                File    : String
             );

private
   type Abstract_OpenSSL_Factory
        (  Decoded_Size : Buffer_Length
        )  is new Connections_Factory with
   record
      Trace_Session  : Boolean := False;
      Trace_Decoded  : Boolean := False;
      Client_Context : SSL_CTX := No_SSL_CTX;
      Server_Context : SSL_CTX := No_SSL_CTX;
   end record;
   procedure Create_Context
             (  Factory : in out Abstract_OpenSSL_Factory;
                Context : Context_Type
             );
   procedure Finalize (Factory : in out Abstract_OpenSSL_Factory);

   type OpenSSL_Session_State is (TLS_Handshake, TLS_Exchange);

   type OpenSSL_Session
        (  Client : access Connection'Class;
           Size   : Buffer_Length
        )  is new Encoder (Size) with
   record
      SSL_Session : SSL := No_SSL;
      Input       : BIO := No_BIO;
      Output      : BIO := No_BIO;
      State       : OpenSSL_Session_State := TLS_Handshake;
   end record;
   procedure Finalize (Session : in out OpenSSL_Session);
--
-- Encode -- Overriding GNAT.Sockets.Server...
--
   procedure Encode
             (  Transport : in out OpenSSL_Session;
                Client    : in out Connection'Class;
                Data      : Stream_Element_Array;
                Last      : out Stream_Element_Offset
             );
   function Get_Session
            (  Client : Connection'Class
            )  return SSL;
   procedure Process
             (  Transport : in out OpenSSL_Session;
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
--
-- OpenSSL BIO operations
--
   function BIO_Control
            (  b   : BIO;
               cmd : BIO_CTRL;
               num : long;
               ptr : System.Address
            )  return long;
   pragma Convention (C, BIO_Control);

   function BIO_Gets
            (  b    : BIO;
               data : Stream_Element_Pointers.Pointer;
               dlen : int
            )  return int;
   pragma Convention (C, BIO_Gets);

   function BIO_Puts
            (  b    : BIO;
               data : Stream_Element_Pointers.Pointer
            )  return int;
   pragma Convention (C, BIO_Puts);

   function BIO_Read
            (  b         : BIO;
               data      : System.Address;
               dlen      : int
            )  return int;
   function BIO_Read
            (  b         : BIO;
               data      : System.Address;
               dlen      : size_t;
               readbytes : access size_t
            )  return int;
   pragma Convention (C, BIO_Read);

   function BIO_Write
            (  b       : BIO;
               data    : System.Address;
               dlen    : int
            )  return int;
   function BIO_Write
            (  b       : BIO;
               data    : System.Address;
               dlen    : size_t;
               written : access size_t
            )  return int;
   pragma Convention (C, BIO_Write);

end GNAT.Sockets.Server.OpenSSL;
