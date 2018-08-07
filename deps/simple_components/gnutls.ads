--                                                                    --
--  package GNUTLS                  Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2015       --
--                                                                    --
--                                Last revision :  17:44 21 Jul 2018  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either Version  2  of  --
--  the License, or (at your option) any later Version. This library  --
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

with Ada.Calendar;    use Ada.Calendar;
with Ada.Exceptions;  use Ada.Exceptions;
with Ada.Streams;     use Ada.Streams;
with Interfaces.C;    use Interfaces.C;

with Ada.Finalization;
with System;

package GNUTLS is
--
-- TLS_Error -- The exception used on GNUTLS errors
--
   TLS_Error : exception;
--
-- Enumeration of different symmetric encryption algorithms.
--
   type Cipher_Algorithm is
        (  Cipher_Unknown,
           Cipher_Null,
           Cipher_ARCFOUR_128,
           Cipher_3DES_CBC,
           Cipher_AES_128_CBC,
           Cipher_AES_256_CBC,
           Cipher_ARCFOUR_40,
           Cipher_CAMELLIA_128_CBC,
           Cipher_CAMELLIA_256_CBC,
           Cipher_RC2_40_CBC,
           Cipher_DES_CBC,
           Cipher_AES_192_CBC,
           Cipher_AES_128_GCM,
           Cipher_AES_256_GCM,
           Cipher_CAMELLIA_192_CBC,
           Cipher_SALSA20_256,
           Cipher_ESTREAM_SALSA20_256,
           Cipher_CAMELLIA_128_GCM,
           Cipher_CAMELLIA_256_GCM,
           --
           -- Used only for PGP internals. Ignored in TLS/SSL
           --
           Cipher_IDEA_PGP_CFB,
           Cipher_3DES_PGP_CFB,
           Cipher_CAST5_PGP_CFB,
           Cipher_BLOWFISH_PGP_CFB,
           Cipher_SafeR_SK128_PGP_CFB,
           Cipher_AES128_PGP_CFB,
           Cipher_AES192_PGP_CFB,
           Cipher_AES256_PGP_CFB,
           Cipher_TWOFISH_PGP_CFB
        );
   for Cipher_Algorithm use
       (  Cipher_Unknown             => 0,
          Cipher_Null                => 1,
          Cipher_ARCFOUR_128         => 2,
          Cipher_3DES_CBC            => 3,
          Cipher_AES_128_CBC         => 4,
          Cipher_AES_256_CBC         => 5,
          Cipher_ARCFOUR_40          => 6,
          Cipher_CAMELLIA_128_CBC    => 7,
          Cipher_CAMELLIA_256_CBC    => 8,
          Cipher_RC2_40_CBC          => 90,
          Cipher_DES_CBC             => 91,
          Cipher_AES_192_CBC         => 92,
          Cipher_AES_128_GCM         => 93,
          Cipher_AES_256_GCM         => 94,
          Cipher_CAMELLIA_192_CBC    => 95,
          Cipher_SALSA20_256         => 96,
          Cipher_ESTREAM_SALSA20_256 => 97,
          Cipher_CAMELLIA_128_GCM    => 98,
          Cipher_CAMELLIA_256_GCM    => 99,
          Cipher_IDEA_PGP_CFB        => 200,
          Cipher_3DES_PGP_CFB        => 201,
          Cipher_CAST5_PGP_CFB       => 202,
          Cipher_BLOWFISH_PGP_CFB    => 203,
          Cipher_SafeR_SK128_PGP_CFB => 204,
          Cipher_AES128_PGP_CFB      => 205,
          Cipher_AES192_PGP_CFB      => 206,
          Cipher_AES256_PGP_CFB      => 207,
          Cipher_TWOFISH_PGP_CFB     => 208
       );
   pragma Convention (C, Cipher_Algorithm);
   type Cipher_Algorithm_Array is
      array (Positive range <>) of aliased Cipher_Algorithm;

   Cipher_RIJNDAEL_128_CBC : constant Cipher_Algorithm :=
                                         Cipher_AES_128_CBC;
   Cipher_RIJNDAEL_256_CBC : constant Cipher_Algorithm :=
                                         Cipher_AES_256_CBC;
   Cipher_RIJNDAEL_CBC     : constant Cipher_Algorithm :=
                                         Cipher_AES_128_CBC;
   Cipher_ARCFOUR          : constant Cipher_Algorithm :=
                                         Cipher_ARCFOUR_128;
--
-- Enumeration of different key exchange algorithms.
--
  type KX_Algorithm is
       (  KX_Unknown,     -- Unknown key-exchange algorithm
          KX_RSA,         -- RSA key-exchange algorithm
          KX_DHE_DSS,     -- DHE-DSS key-exchange algorithm
          KX_DHE_RSA,     -- DHE-RSA key-exchange algorithm
          KX_ANON_DH,     -- Anon-DH key-exchange algorithm
          KX_SRP,         -- SRP key-exchange algorithm
          KX_RSA_Export,  -- RSA-EXPORT key-exchange algorithm (defunc)
          KX_SRP_RSA,     -- SRP-RSA key-exchange algorithm
          KX_SRP_DSS,     -- SRP-DSS key-exchange algorithm
          KX_PSK,         -- PSK key-exchange algorithm
          KX_DHE_PSK,     -- DHE-PSK key-exchange algorithm
          KX_ANON_ECDH,   -- Anon-ECDH key-exchange algorithm
          KX_ECDHE_RSA,   -- ECDHE-RSA key-exchange algorithm
          KX_ECDHE_ECDSA, -- ECDHE-ECDSA key-exchange algorithm
          KX_ECDHE_PSK,   -- ECDHE-PSK key-exchange algorithm
          KX_RSA_PSK      -- RSA-PSK key-exchange algorithm
       );
   pragma Convention (C, KX_Algorithm);
   type KX_Algorithm_Array is
      array (Positive range <>) of aliased KX_Algorithm;
--
-- Enumeration of different TLS session parameter types.
--
   type Params_Type is
        (  Params_RSA_Export, -- RSA-EXPORT parameters (defunc)
           Params_DH,         -- Diffie-Hellman parameters
           Params_ECDH        -- Elliptic-Curve Diffie-Hellman
        );
   for Params_Type use
       (  Params_RSA_Export => 1,
          Params_DH         => 2,
          Params_ECDH       => 3
       );
   pragma Convention (C, Params_Type);
--
-- Enumeration of different credential types
--
   type Credentials_Type is
        (  CRD_Certificate,
           CRD_ANON,
           CRD_SRP,
           CRD_PSK,
           CRD_IA
        );
   for Credentials_Type use
        (  CRD_Certificate => 1,
           CRD_ANON        => 2,
           CRD_SRP         => 3,
           CRD_PSK         => 4,
           CRD_IA          => 5
        );
   pragma Convention (C, Credentials_Type);
--
-- Enumeration of different Message Authentication Code (MAC) algorithms
--
   type MAC_Algorithm is
        (  MAC_Unknown, -- Unknown MAC algorithm
           MAC_NULL,    -- NULL MAC algorithm (empty output)
           MAC_MD5,     -- HMAC-MD5 algorithm
           MAC_SHA1,    -- HMAC-SHA-1 algorithm
           MAC_RMD160,  -- HMAC-RMD160 algorithm
           MAC_MD2,     -- HMAC-MD2 algorithm
           MAC_SHA256,  -- HMAC-SHA-256 algorithm
           MAC_SHA384,  -- HMAC-SHA-384 algorithm
           MAC_SHA512,  -- HMAC-SHA-512 algorithm
           MAC_SHA224,  -- HMAC-SHA-224 algorithm
           MAC_AEAD,    -- MAC implicit through AEAD cipher
           MAC_UMAC_96, -- The UMAC-96 MAC algorithm
           MAC_UMAC_128 -- The UMAC-128 MAC algorithm
        );
   for MAC_Algorithm use
       (  MAC_Unknown  => 0,
          MAC_NULL     => 1,
          MAC_MD5      => 2,
          MAC_SHA1     => 3,
          MAC_RMD160   => 4,
          MAC_MD2      => 5,
          MAC_SHA256   => 6,
          MAC_SHA384   => 7,
          MAC_SHA512   => 8,
          MAC_SHA224   => 9,
          MAC_AEAD     => 200,
          MAC_UMAC_96  => 201,
          MAC_UMAC_128 => 202
       );
   pragma Convention (C, MAC_Algorithm);
   type MAC_Algorithm_Array is
      array (Positive range <>) of aliased MAC_Algorithm;

   MAC_SHA : constant MAC_Algorithm := MAC_SHA1;
--
-- Enumeration of different digest (hash) algorithms.
--
   type Digest_Algorithm is
        (  Dig_Unknown, -- Unknown hash algorithm
           Dig_NULL,    -- NULL hash algorithm (empty output)
           Dig_MD5,     -- MD5 algorithm
           Dig_SHA1,    -- SHA-1 algorithm
           Dig_RMD160,  -- RMD160 algorithm
           Dig_MD2,     -- MD2 algorithm
           Dig_SHA256,  -- SHA-256 algorithm
           Dig_SHA384,  -- SHA-384 algorithm
           Dig_SHA512,  -- SHA-512 algorithm
           Dig_SHA224   -- SHA-224 algorithm
        );
   for Digest_Algorithm use
       (  Dig_Unknown => MAC_Algorithm'Pos (MAC_Unknown),
          Dig_NULL    => MAC_Algorithm'Pos (MAC_NULL),
          Dig_MD5     => MAC_Algorithm'Pos (MAC_MD5),
          Dig_SHA1    => MAC_Algorithm'Pos (MAC_SHA1),
          Dig_RMD160  => MAC_Algorithm'Pos (MAC_RMD160),
          Dig_MD2     => MAC_Algorithm'Pos (MAC_MD2),
          Dig_SHA256  => MAC_Algorithm'Pos (MAC_SHA256),
          Dig_SHA384  => MAC_Algorithm'Pos (MAC_SHA384),
          Dig_SHA512  => MAC_Algorithm'Pos (MAC_SHA512),
          Dig_SHA224  => MAC_Algorithm'Pos (MAC_SHA224)
       );
   pragma Convention (C, Digest_Algorithm);
   type Digest_Algorithm_Array is
      array (Positive range <>) of aliased Digest_Algorithm;

   Dig_SHA : constant Digest_Algorithm := Dig_SHA1;
--
-- Enumeration of different TLS compression methods.
--
   type Compression_Method is
        (  Comp_Unknown, -- Unknown compression method
           Comp_NULL,    -- The NULL compression method (no compression)
           Comp_DEFLATE  -- The DEFLATE compression method from zlib
        );
   pragma Convention (C, Compression_Method);
   type Compression_Method_Array is
      array (Positive range <>) of aliased Compression_Method;

   Comp_ZLIB : constant Compression_Method := Comp_DEFLATE;
--
-- Enumeration of different TLS alert severities.
--
   type Alert_Level is
        (  AL_Warning,
           AL_Fatal
        );
   for Alert_Level use
       (  AL_Warning => 1,
          AL_Fatal   => 2
       );
   pragma Convention (C, Alert_Level);
--
-- Enumeration of different TLS alerts
--
   type Alert_Description is
        (  A_Close_Notify,             -- Close notify
           A_Unexpected_Message,       -- Unexpected message
           A_Bad_Record_MAC,           -- Bad record MAC
           A_Decryption_Failed,        -- Decryption failed
           A_Record_Overflow,          -- Record overflow
           A_Decompression_Failure,    -- Decompression Failed
           A_Handshake_Failure,        -- Handshake Failed
           A_SSL3_No_Certificate,      -- No certificate
           A_Bad_Certificate,          -- Certificate is bad
           A_Uunsupported_Certificate, -- Certificate is not supported
           A_Certificate_Revoked,      -- Certificate was revoked
           A_Certificate_Expired,      -- Certificate is expired
           A_Certificate_Unknown,      -- Unknown certificate
           A_Illegal_Parameter,        -- Illegal parameter
           A_Unknown_CA,               -- CA is Unknown
           A_Access_Denied,            -- Access was denied
           A_Decode_Error,             -- Decode error
           A_Decrypt_Error,            -- Decrypt error
           A_Export_Restriction,       -- Export restriction
           A_Protocol_Version,         -- Error in protocol version
           A_Insufficient_Security,    -- Insufficient security
           A_Internal_Error,           -- Internal error
           A_User_Canceled,            -- User canceled
           A_No_Renegotiation,         -- No renegotiation is allowed
           A_Uunsupported_Extension,   -- An unsupported extension sent
           A_Certificate_Unobtainable, -- Could not retrieve certificate
           A_Unrecognized_Name,        -- The server name notrecognized
           A_Unknown_PSK_Identity,     -- SRP/PSK username is missing
           A_No_Application_Protocol
        );
   for Alert_Description use
       (  A_Close_Notify             => 0,
          A_Unexpected_Message       => 10,
          A_Bad_Record_MAC           => 20,
          A_Decryption_Failed        => 21,
          A_Record_Overflow          => 22,
          A_Decompression_Failure    => 30,
          A_Handshake_Failure        => 40,
          A_SSL3_No_Certificate      => 41,
          A_Bad_Certificate          => 42,
          A_Uunsupported_Certificate => 43,
          A_Certificate_Revoked      => 44,
          A_Certificate_Expired      => 45,
          A_Certificate_Unknown      => 46,
          A_Illegal_Parameter        => 47,
          A_Unknown_CA               => 48,
          A_Access_Denied            => 49,
          A_Decode_Error             => 50,
          A_Decrypt_Error            => 51,
          A_Export_Restriction       => 60,
          A_Protocol_Version         => 70,
          A_Insufficient_Security    => 71,
          A_Internal_Error           => 80,
          A_User_Canceled            => 90,
          A_No_Renegotiation         => 100,
          A_Uunsupported_Extension   => 110,
          A_Certificate_Unobtainable => 111,
          A_Unrecognized_Name        => 112,
          A_Unknown_PSK_Identity     => 115,
          A_No_Application_Protocol  => 120
       );
   pragma Convention (C, Alert_Description);
--
-- Enumeration of different TLS Handshake packets
--
   type Handshake_Description is new unsigned;
   Handshake_Hello_Request        : constant Handshake_Description := 0;
   Handshake_Client_Hello         : constant Handshake_Description := 1;
   Handshake_Server_Hello         : constant Handshake_Description := 2;
   Handshake_Hello_Verify_Request : constant Handshake_Description := 3;
   Handshake_New_Session_Ticket   : constant Handshake_Description := 4;
   Handshake_Certificate_PKT     : constant Handshake_Description := 11;
   Handshake_Server_Key_Exchange : constant Handshake_Description := 12;
   Handshake_Certificate_Request : constant Handshake_Description := 13;
   Handshake_Server_Hello_Done   : constant Handshake_Description := 14;
   Handshake_Certificate_Verify  : constant Handshake_Description := 15;
   Handshake_Client_Key_Exchange : constant Handshake_Description := 16;
   Handshake_Finished            : constant Handshake_Description := 20;
   Handshake_Certificate_Status  : constant Handshake_Description := 22;
   Handshake_Supplemental        : constant Handshake_Description := 23;
   Handshake_Change_Cipher_Spec : constant Handshake_Description := 254;
   Handshake_Client_Hello_V2   : constant Handshake_Description := 1024;
   Handshake_Any               : constant Handshake_Description :=
                                          Handshake_Description'Last;

   type Hook_Type is new int;
   Hook_Post : constant Hook_Type := 1;
   Hook_Pre  : constant Hook_Type := 0;
   Hook_Both : constant Hook_Type := -1;
--
-- Enumeration of certificate status codes.  Note that the status
-- bits may have different meanings in OpenPGP keys and X.509
-- certificate verification.
--
   type Certificate_Status is mod 2**18;
   pragma Convention (C, Certificate_Status);
   Cert_Invalid            : constant Certificate_Status := 2**1;
   Cert_Revoked            : constant Certificate_Status := 2**5;
   Cert_Signer_Not_Found   : constant Certificate_Status := 2**6;
   Cert_Signer_Not_CA      : constant Certificate_Status := 2**7;
   Cert_Insecure_Algorithm : constant Certificate_Status := 2**8;
   Cert_Not_Activated      : constant Certificate_Status := 2**9;
   Cert_Expired            : constant Certificate_Status := 2**10;
   Cert_Signature_Failure  : constant Certificate_Status := 2**11;
   Cert_Revocation_Date_Superseded
                           : constant Certificate_Status := 2**12;
   Cert_Unexpected_Owner   : constant Certificate_Status := 2**14;
   Cert_Revocation_Date_Issued_In_Future
                           : constant Certificate_Status := 2**15;
   Cert_Signer_Constraints_Failure
                           : constant Certificate_Status := 2**16;
   Cert_Mismatch           : constant Certificate_Status := 2**17;
--
-- Enumeration of certificate request types
--
   type Certificate_Request is
        (  Cert_Ignore,
           Cert_Request,
           Cert_Require
        );
   pragma Convention (C, Certificate_Request);
--
-- Enumeration of ways to send OpenPGP certificate
--
   type OpenPGP_Crt_Status is
        (  OpenPGP_Cert,            -- Send entire certificate
           OpenPGP_Cert_Fingerprint -- Send only certificate fingerprint
        );
   pragma Convention (C, OpenPGP_Crt_Status);
--
-- Enumeration of how TLS session should be terminated. See Bye
--
   type Close_Request is
        (  Shut_RDWR,
           Shut_WR
        );
   pragma Convention (C, Close_Request);
--
-- Enumeration of different SSL/TLS protocol Versions.
--
   type Protocol is
        (  No_Protocol,
           SSL3,    -- SSL Version 3.0
           TLS1_0,  -- TLS Version 1.0
           TLS1_1,  -- TLS Version 1.1
           TLS1_2,  -- TLS Version 1.2
           DTLS1_0, -- DTLS Version 1.0
           DTLS0_9, -- DTLS Version 0.9 Cisco AnyConnect/OpenSSL 0.9.8e
           DTLS1_2, -- DTLS Version 1.2
           Version_Unknown -- Unknown SSL/TLS Version
        );
   for Protocol use
       (  No_Protocol => 0,
          SSL3        => 1,
          TLS1_0      => 2,
          TLS1_1      => 3,
          TLS1_2      => 4,
          DTLS0_9     => 6, -- FIXME: at some point change it to 200
          DTLS1_0     => 5,	-- 201
          DTLS1_2     => 202,
          Version_Unknown => 16#FF# -- Change it to 16#FFFF#
       );
   pragma Convention (C, Protocol);
   TLS1             : constant Protocol := TLS1_0;
   DTLS_Version_Min : constant Protocol := DTLS1_0;
   DTLS_Version_Max : constant Protocol := DTLS1_2;
   TLS_Version_Max  : constant Protocol := TLS1_2;
   type Protocol_Array is array (Positive range <>) of aliased Protocol;
--
-- Enumeration of different certificate types
--
   type Certificate_Type is
        (  CRT_Unknown, -- Unknown certificate type
           CRT_X509,    -- X.509 Certificate
           CRT_OpenPGP, -- OpenPGP certificate
           CRT_Raw      -- Raw public key (SubjectPublicKey)
        );
   pragma Convention (C, Certificate_Type);
   type Certificate_Type_Array is
      array (Positive range <>) of aliased Certificate_Type;
--
-- Enumeration of different certificate encoding formats.
--
   type X509_Crt_Fmt is
        (  X509_Fmt_DER, -- X.509 certificate in DER format (binary)
           X509_Fmt_PEM  -- X.509 certificate in PEM format (text)
        );
   pragma Convention (C, X509_Crt_Fmt);
--
-- Enumeration of different certificate printing variants.
--
   type Certificate_Print_Formats is
        (  CRT_Print_Full,          -- Full information
           CRT_Print_OneLine,       -- Information in one line
           CRT_Print_Unsigned_Full, -- All info for an unsigned
                                    -- certificate
           CRT_Print_Compact,       -- Information about certificate
                                    -- name in one line, plus identifi-
                                    -- cation of the public key
           CRT_Print_Full_Numbers   -- Full information and include easy
        );                          -- to parse public key parameters
   pragma Convention (C, Certificate_Print_Formats);
--
-- Enumeration of different public-key algorithms.
--
   type PK_Algorithm is
        (  PK_Unknown, -- Unknown public-key algorithm
           PK_RSA,     -- RSA public-key algorithm
           PK_DSA,     -- DSA public-key algorithm
           PK_DH,      -- Diffie-Hellman algorithm
           PK_EC       -- Elliptic curve algorithm
        );
   pragma Convention (C, PK_Algorithm);
   type PK_Algorithm_Array is
      array (Positive range <>) of aliased PK_Algorithm;
--
-- Enumeration of different digital signature algorithms.
--
   type Sign_Algorithm is
        (  Sign_Unknown,      -- Unknown signature algorithm
           Sign_RSA_SHA1,     -- RSA with SHA-1
           Sign_DSA_SHA1,     -- DSA with SHA-1
           Sign_RSA_MD5,      -- RSA with MD5
           Sign_RSA_MD2,      -- RSA with MD2
           Sign_RSA_RMD160,   -- RSA with RMD-160
           Sign_RSA_SHA256,   -- RSA with SHA-256
           Sign_RSA_SHA384,   -- RSA with SHA-384
           Sign_RSA_SHA512,   -- RSA with SHA-512
           Sign_RSA_SHA224,   -- RSA with SHA-224
           Sign_DSA_SHA224,   -- DSA with SHA-224
           Sign_DSA_SHA256,   -- DSA with SHA-256
           Sign_ECDSA_SHA1,   -- ECDSA with SHA1
           Sign_ECDSA_SHA224, -- ECDSA with SHA-224
           Sign_ECDSA_SHA256, -- ECDSA with SHA-256
           Sign_ECDSA_SHA384, -- ECDSA with SHA-384
           Sign_ECDSA_SHA512  -- ECDSA with SHA-512
        );
   pragma Convention (C, Sign_Algorithm);
   Sign_RSA_SHA : constant Sign_Algorithm := Sign_RSA_SHA1;
   Sign_DSA_SHA : constant Sign_Algorithm := Sign_DSA_SHA1;
   type Sign_Algorithm_Array is
      array (Positive range <>) of aliased Sign_Algorithm;
--
-- Enumeration of ECC curves.
--
   type ECC_Curve is
        (  ECC_Curve_Invalid,   -- Cannot be known
           ECC_Curve_SECP224R1, -- SECP224R1 curve
           ECC_Curve_SECP256R1, -- SECP256R1 curve
           ECC_Curve_SECP384R1, -- SECP384R1 curve
           ECC_Curve_SECP521R1, -- SECP521R1 curve
           ECC_Curve_SECP192R1  -- SECP192R1 curve
        );
   pragma Convention (C, ECC_Curve);
   type ECC_Curve_Array is
      array (Positive range <>) of aliased ECC_Curve;

   function Bits_Are_Curve (Bits : unsigned) return Boolean;
   function Bits_To_Curve (Bits : unsigned) return ECC_Curve;
   function Curve_To_Bits (Curve : ECC_Curve) return unsigned;
--
-- Enumeration of security parameters for passive attacks.
--
   type Sec_Param is new int;
   Sec_Param_Insecure  : constant Sec_Param :=-20; -- Less than 42 bits
   Sec_Param_Export    : constant Sec_Param :=-15; -- 42 bits
   Sec_Param_Very_Weak : constant Sec_Param :=-12; -- 64 bits
   Sec_Param_Weak      : constant Sec_Param :=-10; -- 72 bits
   Sec_Param_Unknown   : constant Sec_Param := 0;  -- Cannot be known
   Sec_Param_Low       : constant Sec_Param := 1;  -- 80 bits
   Sec_Param_Legacy    : constant Sec_Param := 2;  -- 96 bits
   Sec_Param_Normal    : constant Sec_Param := 3;  -- 112 bits
   Sec_Param_High      : constant Sec_Param := 4;  -- 128 bits
   Sec_Param_Ultra     : constant Sec_Param := 5;  -- 192 bits

   type PKCS_Encrypt_Flags is mod 512;
   pragma Convention (C, PKCS_Encrypt_Flags);
   PKCS_Plain              : constant PKCS_Encrypt_Flags := 1;
   PKCS_Use_PKCS12_3DES    : constant PKCS_Encrypt_Flags := 2;
   PKCS_Use_PKCS12_ARCFOUR : constant PKCS_Encrypt_Flags := 4;
   PKCS_Use_PKCS12_RC2_40  : constant PKCS_Encrypt_Flags := 8;
   PKCS_Use_PBES2_3DES     : constant PKCS_Encrypt_Flags := 16;
   PKCS_Use_PBES2_AES_128  : constant PKCS_Encrypt_Flags := 32;
   PKCS_Use_PBES2_AES_192  : constant PKCS_Encrypt_Flags := 64;
   PKCS_Use_PBES2_AES_256  : constant PKCS_Encrypt_Flags := 128;
   PKCS_Null_Password      : constant PKCS_Encrypt_Flags := 256;
--
-- Enumeration of support channel binding types
--
   type Channel_Binding is
        (  CB_TLS_Unique -- "tls-unique" (RFC 5929) channel binding
        );
   pragma Convention (C, Channel_Binding);
--
-- Enumeration of OpenPGP certificate formats
--
   type OpenPGP_Crt_Fmt is
	(  OpenPGP_Fmt_Raw,
           OpenPGP_Fmt_Base64
        );
   pragma Convention (C, OpenPGP_Crt_Fmt);
--
-- Opaque pointers
--
   type X509_Crt        is private;
   type X509_CRL        is private;
   type X509_Privkey    is private;
   type OpenPGP_Crt     is private;
   type OpenPGP_Privkey is private;
   type PKSC11_Privkey  is private;
   type Transport       is private;

   type X509_Crt_Array is array (Positive range <>) of aliased X509_Crt;
   type X509_CRL_Array is array (Positive range <>) of aliased X509_CRL;

   type unsigned_chars_ptr is access unsigned_char;
   type const_unsigned_chars_ptr is access constant unsigned_char;
   pragma Convention (C, unsigned_chars_ptr);
   type Datum is record
      Data : System.Address := System.Null_Address;
      Size : unsigned       := 0;
   end record;
   pragma Convention (C, Datum);
   function New_Datum (Data : Stream_Element_Array) return Datum;
   function To_Datum (Data : Stream_Element_Array) return Datum;
   function To_Datum (Data : char_array) return Datum;
   type Datum_Array is array (Positive range <>) of aliased Datum;
   type Datum_Holder is
      new Ada.Finalization.Limited_Controlled with
   record
      Data : aliased Datum;
   end record;
   procedure Finalize (Data : in out Datum_Holder);

   type Abstract_Params is abstract tagged private;
   type DH_Params       is new Abstract_Params with private;
   type ECDH_Params     is new Abstract_Params with private;
   type RSA_Params      is new Abstract_Params with private;

--     type Params_Data (Kind_Of : Params_Type := Params_DH) is record
--        case Kind_Of is
--  	 when Params_DH =>
--              DH : DH_Params;
--  	 when Params_ECDH =>
--              ECDH : ECDH_Params;
--  	 when Params_RSA_Export =>
--              RSA_Export : RSA_Params;
--        end case;
--     end record;
--     pragma Convention (C, Params_Data);
--     pragma Unchecked_Union (Params_Data);

   type Params_Data is record
      Kind_Of : Params_Type    := Params_DH;
      Data    : System.Address := System.Null_Address;
      Deinit  : int            := 0;
   end record;
   pragma Convention (C, Params_Data);

   type Privkey_Type is
        (  Privkey_X509,
           Privkey_OpenPGP,
           Privkey_PKCS11,
           Privkey_Ext
        );
   pragma Convention (C, Privkey_Type);

   type Retr2_Cert (Kind_Of : Privkey_Type := Privkey_X509) is record
      case Kind_Of is
	 when Privkey_X509 =>
            X509 : X509_Crt;
	 when Privkey_OpenPGP =>
            PGP : OpenPGP_Crt;
	 when Privkey_PKCS11 | Privkey_Ext =>
            null;
      end case;
   end record;
   pragma Convention (C, Retr2_Cert);
   pragma Unchecked_Union (Retr2_Cert);

   type Retr2_Key (Kind_Of : Privkey_Type := Privkey_X509) is record
      case Kind_Of is
	 when Privkey_X509 =>
            X509 : X509_Privkey;
	 when Privkey_OpenPGP =>
            PGP : OpenPGP_Privkey;
	 when Privkey_PKCS11 =>
            PKCS11 : PKSC11_Privkey;
	 when Privkey_Ext =>
            null;
      end case;
   end record;
   pragma Convention (C, Retr2_Key);
   pragma Unchecked_Union (Retr2_Key);

   type Retr2 is record
      Cert_Type  : Certificate_Type;
      Key_Type   : Privkey_Type;
      Cert       : Retr2_Cert;
      NCerts     : unsigned; -- One for PGP keys
      Key        : Retr2_Key;
      Deinit_All : unsigned; -- if non zero all keys will be deinited
   end record;
   pragma Convention (C, Retr2);

   ALPN_MAND : constant := 1;
------------------------------------------------------------------------
-- Flags for Init
--
--    Init_Server: Connection end is a server;
--    Init_Client: Connection end is a client;
--    Init_Datagram: Connection is datagram oriented (DTLS);
--    Nonblock: Connection should not block (DTLS);
--    Init_No_Extensions: Do not enable any TLS extensions by default;
--    Init_No_Replay_Protection: Disable any replay protection in DTLS.
--
   type Init_Flags is mod 2**6;
   pragma Convention (C, Init_Flags);

   Init_Server               : constant Init_Flags := 2**0;
   Init_Client               : constant Init_Flags := 2**1;
   Init_Datagram             : constant Init_Flags := 2**2;
   Init_Nonblock             : constant Init_Flags := 2**3;
   Init_No_Extensions        : constant Init_Flags := 2**4;
   Init_No_Replay_Protection : constant Init_Flags := 2**5;

   type Session_Type (Flags : Init_Flags) is limited private;
   type Session_Type_Ptr is access all Session_Type;

   type Abstract_Credentials is abstract tagged limited private;
--
-- Credentials_Set -- Set credentials
--
--    Session     - The session
--    Credentials - The credentials to set
--
-- Sets  the needed  credentials for  the specified type. E.g. username,
-- password - or public and private keys etc. The cred  parameter  is  a
-- structure  that  depends  on  the  specified  type and on the current
-- session (client or server). In order to minimize  memory  usage,  and
-- share credentials between several threads gnutls keeps a  pointer  to
-- cred, and not the whole cred structure.
--
-- Exceptions :
--
--    Constraint_Error - Invalid credentials
--
   procedure Credentials_Set
             (  Session     : in out Session_Type;
                Credentials : Abstract_Credentials
             )  is abstract;

   type Abstract_Anon_Credentials is abstract
      new Abstract_Credentials with private;

   type Anon_Client_Credentials is
      new Abstract_Anon_Credentials with private;
   type Anon_Server_Credentials is
      new Abstract_Anon_Credentials with private;

   type Certificate_Credentials is
      new Abstract_Credentials with private;

--     type Abstract_SRP_Credentials is abstract
--        new Abstract_Credentials with private;
--     type SRP_Client_Credentials is
--        new Abstract_SRP_Credentials with private;
--     type SRP_Server_Credentials is
--        new Abstract_SRP_Credentials with private;

   type Anon_Client_Credentials_Ref is
      new Anon_Client_Credentials with private;
   type Anon_Server_Credentials_Ref is
      new Anon_Server_Credentials with private;
   type Certificate_Credentials_Ref is
      new Certificate_Credentials with private;
--     type SRP_Client_Credentials_Ref is
--        new SRP_Client_Credentials with private;
--     type SRP_Server_Credentials_Ref is
--        new SRP_Server_Credentials with private;
--  --
--  -- Credentials_Get -- Get previously provided credentials
--  --
--  --    Credentials - A reference to the credentials (initialized)
--  --    Session     - The session
--  --
--  -- Exceptions :
--  --
--  --    Constraint_Error - No credentials set
--  --
--     procedure Credentials_Get
--               (  Credentials : in out Anon_Client_Credentials_Ref;
--                  Session     : Session_Type'Class
--               );
--     procedure Credentials_Get
--               (  Credentials : in out Anon_Server_Credentials_Ref;
--                  Session     : Session_Type'Class
--               );
--     procedure Credentials_Get
--               (  Credentials : in out Certificate_Credentials_Ref;
--                  Session     : Session_Type'Class
--               );
--     procedure Credentials_Get
--               (  Credentials : in out SRP_Client_Credentials_Ref;
--                  Session     : Session_Type'Class
--               );
--     procedure Credentials_Get
--               (  Credentials : in out SRP_Server_Credentials_Ref;
--                  Session     : Session_Type'Class
--               );
   type Priority is limited private;
------------------------------------------------------------------------
--
-- Alert_Get -- Last alert
--
--    Session - A session
--
-- This  function  will  return  the  last  alert  number received. This
-- function  should   be   called   when   E_Warning_Alert_Received   or
-- E_Fatal_Alert_Received errors are returned by a gnutls function.  The
-- peer  may send alerts if he encounters an error. If no alert has been
-- received the returned value is undefined.
--
-- Returns :
--
--    The last alert received
--
   function Alert_Get (Session : Session_Type) return Alert_Description;
--
-- Alert_Get_Name -- Alert description string
--
--    Alert - An alert number
--
-- This function  will return  a string that  describes  the given alert
-- number.
--
-- Returns :
--
--    String corresponding to the value
--
   function Alert_Get_Name (Alert : Alert_Description) return String;
--
-- Alert_Get_StrName -- Alert description string
--
--    Alert - An alert number
--
-- This function will return a string of the name of the alert.
--
-- Returns :
--
--    String corresponding to the value
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function Alert_Get_StrName (Alert : Alert_Description) return String;
--
-- Alert_Send -- Send alert
--
--    Session - The session
--    Level   - The level of the alert
--    Desc    - The alert description
--
-- This function will send an alert to the peer in order to  inform  him
-- of something important (eg. his Certificate could not  be  verified).
-- If  the  alert  level is Fatal then the peer is expected to close the
-- connection, otherwise he may ignore the alert and continue. The error
-- code  of the underlying record send function will be returned, so you
-- may also receive E_Interrupted or E_Again as well.
--
   procedure Alert_Send
             (  Session : in out Session_Type;
                Level   : Alert_Level;
                Desc    : Alert_Description
             );
--
-- Alert_Send_Appropriate -- Send alert
--
--    Session - The session
--    Level   - The level of the alert
--    Desc    - The alert description
--
-- Sends  an alert to the peer depending on the error code returned by a
-- gnutls function. This function will call Error_To_Alert to  determine
-- the appropriate alert to send. This function may also return E_Again,
-- or E_Interrupted. If the return value is E_Invalid_Request, then  no
-- alert has been sent to the peer.
--
   procedure Alert_Send_Appropriate
             (  Session : in out Session_Type;
                Level   : Alert_Level;
                Desc    : Alert_Description
             );
--
-- Anon_Set_Params_Function -- Set callback
--
-- This  generic  package  will  set  a  callback  (the  formal  generic
-- parameter  Handle)  in order for the server to get the Diffie-Hellman
-- or  RSA parameters  for anonymous authentication.  The  callback  may
-- raise a TLS_Error exception to indicate errors.
--
   generic
      with procedure Handle
                     (  Session    : in out Session_Type;
                        Parameters : Abstract_Params'Class
                     );
   package Anon_Params_Function is
      procedure Set
                (  Credentials : in out Anon_Server_Credentials'Class
                );
   end Anon_Params_Function;
--
-- Anon_Set_Server_Params_Function -- Set callback
--
--     Handle - The callback
--
-- This  generic  package  will  set  a  callback  (the  formal  generic
-- parameter  Handle) in order for the server to get the  Diffie-Hellman
-- parameters for anonymous authentication.  The  callback  may  raise a
-- TLS_Error exception to indicate errors.
--
   generic
      with procedure Handle
                     (  Session    : in out Session_Type;
                        Parameters : Abstract_Params'Class
                     );
   package Anon_Server_Params_Function is
      procedure Set
                (  Credentials : in out Anon_Server_Credentials'Class
                );
   end Anon_Server_Params_Function;
--
-- Anon_Set_Server_DH_Params -- Set parameters
--
--    Credentials - The credentials
--    Params      - Holds Diffie-Hellman parameters
--
-- This function will set the Diffie-Hellman parameters for an anonymous
-- server  to  use.  These  parameters  will  be   used   in   anonymous
-- Diffie-Hellman cipher suites.
--
   procedure Anon_Set_Server_DH_Params
             (  Credentials : in out Anon_Server_Credentials;
                Params      : DH_Params'Class
             );
--
-- Alpn_Get_Selected_Protocol -- Get protocol name
--
--    Session  - The session
--
-- Returns:
--
--    Protocol - The protocol name
--
   function Alpn_Get_Selected_Protocol
            (  Session : Session_Type
            )  return Stream_Element_Array;
--
-- Alpn_Set_Selected_Protocol -- Set ALPN protocol
--
--    Session   - The session
--    Protocols - The protocol names to add
--    Flags     - Zero or ALPN_*
--
-- This  function  is to be used by both clients and servers, to declare
-- the  supported ALPN protocols, which are used during negotiation with
-- peer.  If ALPN_MAND is specified the connection will be aborted if no
-- matching ALPN protocol is found.
--
   procedure Alpn_Set_Selected_Protocol
             (  Session   : in out Session_Type;
                Protocols : Datum_Array;
                Flags     : int
             );
--
-- Audit_Log_Function -- Audit log callback
--
--    Session - The session
--    Text    - The text
--
-- The default implementation does nothing. It can be overridden.
--
   procedure Audit_Log_Function
             (  Session : in out Session_Type;
                Text    : String
             );
--
-- Auth_Client_Get_Type -- Credentials type
--
--    Session - The session
--
-- Returns  the  type  of  credentials  that  were   used   for   client
-- authentication. The returned information is to be used to distinguish
-- the function used to access authentication data.
--
-- Returns:
--
--    The type of credentials for the client authentication schema
--
   function Auth_Client_Get_Type
            (  Session : Session_Type
            )  return Credentials_Type;
--
-- Auth_Get_Type -- Credentials type
--
--    Session - The session
--
-- Returns type of credentials for the  current  authentication  schema.
-- The returned information is to be used to  distinguish  the  function
-- used to access authentication data. Eg. for CERTIFICATE  ciphersuites
-- (key exchange algorithms: KX_RSA, KX_DHE_RSA), the same function  are
-- to be used to access the authentication data.
--
-- Returns:
--
--   The type of credentials for the current authentication schema
--
   function Auth_Get_Type
            (  Session : Session_Type
            )  return Credentials_Type;
--
-- Auth_Server_Get_Type -- Credentials type
--
--    Session - The session
--
-- Returns  the  type  of  credentials  that  were   used   for   server
-- authentication. The returned information is to be used to distinguish
-- the function used to access authentication data.
--
-- Returns :
--
--    The type of credentials for the client authentication schema
--
   function Auth_Server_Get_Type
            (  Session : Session_Type
            )  return Credentials_Type;
--
-- Bye -- Credentials type
--
--    Session - The session
--    How     - Close request
--
-- Terminates the current TLS/SSL connection. The connection should have
-- been initiated using Handshake.  How  should  be  one  of  Shut_RDWR,
-- Shut_WR  .  In  case of Shut_RDWR the TLS session gets terminated and
-- further receives and sends will be disallowed. If the return value is
-- zero you may continue using the underlying transport layer. Shut_RDWR
-- sends  an  alert containing a close request and waits for the peer to
-- reply with the same message. In case of Shut_WR the TLS session  gets
-- terminated and further sends will be disallowed. In  order  to  reuse
-- the  connection  you  should  wait  for an EOF from the peer. Shut_WR
-- sends an  alert  containing  a  close  request.  Note  that  not  all
-- implementations  will  properly  terminate  a TLS connection. Some of
-- them, usually  for  performance  reasons,  will  terminate  only  the
-- underlying  transport  layer,  and  thus not distinguishing between a
-- malicious party prematurely terminating  the  connection  and  normal
-- termination.
--
   procedure Bye
             (  Session : in out Session_Type;
                How     : Close_Request
             );
--
-- Certificate_Activation_Time_Peers -- Activation time
--
--    Session - The session
--
-- This is the creation time for OpenPGP keys.
--
-- Returns :
--
--    The time
--
   function Certificate_Activation_Time_Peers
            (  Session : Session_Type
            )  return Time;
--
-- Certificate_Client_Get_Request_Status -- Get request status
--
--    Session - The session
--
-- Get whether client certificate is requested or not.
--
-- Returns :
--
--    False if the peer (server) did not request client authentication
--
   function Certificate_Client_Get_Request_Status
            (  Session : Session_Type
            )  return Boolean;
--
-- Certificate_Expiration_Time_Peers -- Activation time
--
--    Session - The session
--
-- This function will return the peer's certificate expiration time.
--
-- Returns :
--
--    The time
--
   function Certificate_Expiration_Time_Peers
            (  Session : Session_Type
            )  return Time;
--
-- Certificate_Free_CA_Names -- Free CA names
--
--    Credentials - The credentials
--
-- This function will delete all the CA name in the  given  credentials.
-- Clients may call this to save some memory since in client side the CA
-- names are not used. Servers might want to  use  this  function  if  a
-- large list of trusted CAs is present and  sending  the  names  of  it
-- would just consume bandwidth without providing information to client.
-- CA  names  are  used  by servers to advertise the CAs they support to
-- clients.
--
   procedure Certificate_Free_CA_Names
             (  Credentials : in out Certificate_Credentials
             );
--
-- Certificate_Free_CAs -- Free CAs
--
--    Credentials - The credentials
--
-- This  function  will  delete  all  the  CAs associated with the given
-- credentials.  Servers  that  do not use Certificate_Verify_Peers2 may
-- call this to save some memory.
--
   procedure Certificate_Free_CAs
             (  Credentials : in out Certificate_Credentials
             );
--
-- Certificate_Free_CRLs -- Free CRLs
--
--    Credentials - The credentials
--
-- This function  will delete all  the CRLs  associated with  the  given
-- credentials.
--
   procedure Certificate_Free_CRLs
             (  Credentials : in out Certificate_Credentials
             );
--
-- Certificate_Free_Keys -- Free keys
--
--    Credentials - The credentials
--
-- This  function  will  delete  all  the  keys  and  the   certificates
-- associated  with  the  given  credentials.  This function must not be
-- called  when  a  TLS  negotiation  that  uses  the  credentials is in
-- progress.
--
   procedure Certificate_Free_Keys
             (  Credentials : in out Certificate_Credentials
             );
--
-- Certificate_Get_Crt_Raw -- Get certificate
--
--    Credentials - The credentials
--    Idx1        - The index of the certificate if multiple are present
--    Idx2        - The index  in the certificate list.  Zero  gives the
--                  server's certificate
--
-- This  function  will return the DER encoded certificate of the server
-- or any other certificate on its certificate chain  (based  on  Idx2).
-- The  returned  data should be treated as constant and only accessible
-- during the lifetime of Credentials.
--
-- Returns :
--
--    The certificate
--
   function Certificate_Get_Crt_Raw
            (  Credentials : Certificate_Credentials;
               Idx1        : unsigned;
               Idx2        : unsigned
            )  return Stream_Element_Array;
--
-- Certificate_Get_Issuer -- Get certificate
--
--    Credentials - The credentials
--    Certificate - The certificate to find issuer for
--    Flags       - Zero or TL_GET_COPY
--
-- This  function  will return the DER encoded certificate of the server
-- or any other certificate on its certificate chain  (based  on  Idx2).
-- The  returned  data should be treated as constant and only accessible
-- during the lifetime of Credentials.
--
-- Returns :
--
--    The certificate
--
   function Certificate_Get_Issuer
            (  Credentials : Certificate_Credentials;
               Certificate : X509_Crt
            )  return X509_Crt;
--
-- Certificate_Get_Ours -- Get certificate
--
--    Session - The session
--
-- Gets  the  certificate as sent to the peer in the last handshake. The
-- certificate is in raw (DER) format.  No  certificate  list  is  being
-- returned. Only the first certificate.
--
-- Returns :
--
--    The certificate
--
   function Certificate_Get_Ours
            (  Session : Session_Type
            )  return Stream_Element_Array;
--
-- Certificate_Get_Peers -- Get certificates
--
--    Session - The session
--
-- Get the peer's raw certificate (chain) as sent  by  the  peer.  These
-- certificates are in raw format (DER encoded for X.509). In case of  a
-- X.509  then  a certificate list may be present. The first certificate
-- in the  list  is  the  peer's  certificate,  following  the  issuer's
-- certificate,  then the issuer's issuer etc. In case of OpenPGP keys a
-- single key will be returned in raw format.
--
-- Returns :
--
--    The certificates list
--
   function Certificate_Get_Peers
            (  Session : Session_Type
            )  return Datum_Array;
--
-- Certificate_Get_Peers_Subkey_ID -- Get subkey ID
--
--    Session - The session
--
-- Get the peer's subkey ID when  OpenPGP certificates are used
--
-- Returns :
--
--    The subkey ID
--
   function Certificate_Get_Peers_Subkey_ID
            (  Session : Session_Type
            )  return String;
--
-- Certificate_Send_X509_RDN_Sequence -- Send status
--
--    Session - The session
--    Status  - The status (0 or 1)
--
-- If status is non zero, this function will order gnutls  not  to  send
-- the rdnSequence in the  certificate  request  message.  That  is  the
-- server  will  not advertise its trusted CAs to the peer. If status is
-- zero  then  the  default  behaviour  will  take  effect,  which is to
-- advertise the server's trusted CAs. This function has  no  effect  in
-- clients, and in authentication methods other  than  certificate  with
-- X.509 certificates.
--
   procedure Certificate_Send_X509_RDN_Sequence
             (  Session : in out Session_Type;
                Status  : int
             );
--
-- Certificate_Server_Set_DH_Params -- Set parameters
--
--    Credentials - The credentials
--    Parameters  - Holds Diffie-Hellman parameters
--
-- This  function  will  set  the  Diffie-Hellman   parameters   for   a
-- certificate server to use. These parameters will be used in Ephemeral
-- Diffie-Hellman cipher  suites.  Note  that  only  a  pointer  to  the
-- parameters are stored in the certificate  handle,  so  you  must  not
-- deallocate the parameters before the certificate is deallocated.
--
   procedure Certificate_Server_Set_DH_Params
             (  Credentials : in out Certificate_Credentials;
                Parameters  : DH_Params'Class
             );
--
-- Certificate_Set_DH_Params -- Set parameters
--
--    Credentials - The credentials
--    Parameters  - Holds Diffie-Hellman parameters
--
-- This  function  will  set  the  Diffie-Hellman   parameters   for   a
-- certificate server to use. These parameters will be used in Ephemeral
-- Diffie-Hellman cipher  suites.  Note  that  only  a  pointer  to  the
-- parameters are stored in the certificate  handle,  so  you  must  not
-- deallocate the parameters before the certificate is deallocated.
--
   procedure Certificate_Set_DH_Params
             (  Credentials : in out Certificate_Credentials;
                Parameters  : DH_Params'Class
             );
--
-- Certificate_Set_OCSP_Status_Request_File -- Set parameters
--
--    Credentials   - The credentials
--    Response_File - Holds Diffie-Hellman parameters
--
-- This function sets the filename of an OCSP  response,  that  will  be
-- sent to the client if requests an OCSP certificate status. This is  a
-- convenience function which is inefficient on busy servers  since  the
-- file       is       opened       on       every      access.      Use
-- Certificate_Set_OCSP_Status_Request_Function    to   fine-tune   file
-- accesses.
--
   procedure Certificate_Set_OCSP_Status_Request_File
             (  Credentials   : in out Certificate_Credentials;
                Response_File : String;
                Flags         : int := 0
             );
--
-- Certificate_Set_OCSP_Status_Request_Function -- Set status callback
--
--    Credentials - The credentials
--    Request     - The request
--
-- This  function  is  to  be  used  by server to register a callback to
-- handle OCSP status requests from the client. The  Request's  callback
-- Handle  will  be invoked if the client supplied a status-request OCSP
-- extension. The callback may return E_No_Certificate_Status,  if there
-- is  no recent  OCSP response.  The  callback  may  raise a  TLS_Error
-- exception to indicate errors.
--
   type OSCP_Status_Request is abstract
      new Ada.Finalization.Limited_Controlled with null record;
   procedure Handle
             (  Request  : in out OSCP_Status_Request;
                Session  : in out Session_Type;
                Response : in out Datum
             )  is abstract;

   procedure Certificate_Set_OCSP_Status_Request_Function
             (  Credentials : in out Certificate_Credentials;
                Request     : in out OSCP_Status_Request'Class
             );
--
-- Certificate_Server_Set_OpenPGP_Key -- Set OpenPGP key
--
--    Credentials - The credentials
--    Public_Key  - The public key
--    Private_Key - The private key
--
-- This  function  sets  a   certificate/private   key   pair   in   the
-- Certificate_Credentials.  This  function may be called more than once
-- (in  case multiple keys/certificates exist for the server). Note that
-- this function requires that the preferred key ids have been  set  and
-- be used.
--
   procedure Certificate_Server_Set_OpenPGP_Key
             (  Credentials : in out Certificate_Credentials;
                Public_Key  : OpenPGP_Crt;
                Private_Key : OpenPGP_Privkey
             );
--
-- Certificate_Server_Set_OpenPGP_Key_File -- Set OpenPGP key from file
--
--    Credentials  - The credentials
--    Public_Keys  - The file containing the public keys
--    Private_Keys - The file containing the private keys
--  [ Subkey_ID ]  - The subkey ID
--    Format       - The format of the keys
--
-- This funtion is used to load OpenPGP keys into the  credentials.  The
-- file  should  contain  at  least  one valid non-encrypted subkey.
--
   procedure Certificate_Server_Set_OpenPGP_Key_File
             (  Credentials  : in out Certificate_Credentials;
                Public_Keys  : String;
                Private_Keys : String;
                Format       : OpenPGP_Crt_Fmt
             );
   procedure Certificate_Server_Set_OpenPGP_Key_File
             (  Credentials  : in out Certificate_Credentials;
                Public_Keys  : String;
                Private_Keys : String;
                Subkey_ID    : String;
                Format       : OpenPGP_Crt_Fmt
             );
--
-- Certificate_Server_Set_OpenPGP_Mem -- Set OpenPGP key from memory
--
--    Credentials  - The credentials
--    Public_Keys  - The file containing the public keys
--    Private_Keys - The file containing the private keys
--  [ Subkey_ID ]  - The subkey ID
--    Format       - The format of the keys
--
-- This  funtion is used to load OpenPGP keys into the credentials. This
-- funtion is used to load OpenPGP  keys  into  the  GnuTLS  credentials
-- structure. The datum should contain at least one valid  non-encrypted
-- subkey.
--
   procedure Certificate_Server_Set_OpenPGP_Key_Mem
             (  Credentials  : in out Certificate_Credentials;
                Public_Keys  : Stream_Element_Array;
                Private_Keys : Stream_Element_Array;
                Format       : OpenPGP_Crt_Fmt
             );
   procedure Certificate_Server_Set_OpenPGP_Key_Mem
             (  Credentials  : in out Certificate_Credentials;
                Public_Keys  : Stream_Element_Array;
                Private_Keys : Stream_Element_Array;
                Subkey_ID    : String;
                Format       : OpenPGP_Crt_Fmt
             );
--
-- Certificate_Server_Set_OpenPGP_Keyring_File -- Set OpenPGP keys
--
--    Credentials - The credentials
--    File        - The keyring file
--    Format      - The format of the keys
--
-- This function is used to load OpenPGP keys into the credentials.  The
-- file  should  contain  at  least  one valid non-encrypted subkey.
--
   procedure Certificate_Server_Set_OpenPGP_Keyring_File
             (  Credentials : in out Certificate_Credentials;
                File        : String;
                Format      : OpenPGP_Crt_Fmt
             );
--
-- Certificate_Server_Set_OpenPGP_Mem -- Set OpenPGP keys
--
--    Credentials - The credentials
--    Data        - Containing the keys
--    Format      - The format of the keys
--
-- The function is used to set keyrings that will be used internally  by
-- various OpenPGP functions. For example to  find  a  key  when  it  is
-- needed  for  an  operations.  The  keyring  will  also be used at the
-- verification functions.
--
   procedure Certificate_Server_Set_OpenPGP_Mem
             (  Credentials : in out Certificate_Credentials;
                Data        : Stream_Element_Array;
                Format      : OpenPGP_Crt_Fmt
             );
--
-- Certificate_Server_Set_Request -- Set request
--
--    Session - The session
--    Request - Cert_Request or Cert_Require
--
-- This function specifies if we (in case of a server) are going to send
-- a   certificate   request  message  to  the  client.  If  Request  is
-- Cert_Require  then  the  server will return an error if the peer does
-- not provide a certificate. If you do not call this function then  the
-- client will not be asked to send a certificate.
--
   procedure Certificate_Server_Set_Request
             (  Session : in out Session_Type;
                Request : Certificate_Request
             );
--
-- Certificate_Set_Params_Function -- Set callback
--
--     Handle - The callback
--
-- This  generic  package  will  set  a  callback  (the  formal  generic
-- parameter  Handle) in order for the server  to get the Diffie-Hellman
-- or RSA parameters for certificate authentication.  The  callback  may
-- raise a TLS_Error exception to indicate errors.
--
   generic
      with procedure Handle
                     (  Session    : in out Session_Type;
                        Parameters : Abstract_Params'Class
                     );
   package Certificate_Set_Params_Function is
      procedure Set
                (  Credentials : in out Certificate_Credentials'Class
                );
   end Certificate_Set_Params_Function;
--
-- Certificate_Set_PIN_Function -- Set PIN callback
--
--    Credentials - The credentials
--    Request     - The request
--
-- This function will set a callback function to be used  when  required
-- to access a protected  object.  This  function  overrides  any  other
-- global PIN functions. Note that this function must  be  called  right
-- after  initialization  to have  effect.  The  callback  may  raise  a
-- TLS_Error  exception to indicate errors.
--
   type PIN_Request is abstract
      new Ada.Finalization.Limited_Controlled with null record;
   function Handle
            (  Request     : access PIN_Request;
               Attempt     : int;
               Token_URL   : String;
               Token_Label : String;
               Flags       : int
            )  return String is abstract;

   procedure Certificate_Set_PIN_Function
             (  Credentials : in out Certificate_Credentials;
                PIN         : in out PIN_Request'Class
             );
--
-- Certificate_Set_Retrieve_Function -- Set callback
--
--     Handle - The callback
--
-- This  function  sets a callback to be called in order to retrieve the
-- certificate to be used in the handshake. Contains a list with the  CA
-- names that the server considers trusted. Normally we  should  send  a
-- certificate  that  is signed by one of these CAs. These names are DER
-- encoded.   To   get   a   more  meaningful  value  use  the  function
-- X509_RDN_Get. Algorithms contains a  list  with  server's  acceptable
-- signature algorithms. The certificate  returned  should  support  the
-- server's given algorithms. Retrieve should contain  the  certificates
-- and private keys. If the callback function is  provided  then  gnutls
-- will call it, in the handshake, after the certificate request message
-- has been received. In server side Algorithms and Req_CA_RDN are NULL.
-- The  callback function should set the certificate list to be sent. If
-- no certificate was selected then the number of certificates should be
-- set to zero. The TLS_Error with  code  -1  indicates  error  and  the
-- handshake will be terminated.
--
   generic
      with procedure Handle
                     (  Session        : in out Session_Type;
                        Req_CA_RDN     : Stream_Element_Array;
                        Number_Of_Reqs : int;
                        Algorithms     : PK_Algorithm_Array;
                        Retrieve       : in out Retr2
                     );
   package Certificate_Set_Retrieve_Function is
      procedure Set
                (  Credentials : in out Certificate_Credentials'Class
                );
   end Certificate_Set_Retrieve_Function;
--
-- Certificate_Set_Verify_Flags -- Set flags
--
--    Credentials - The credentials
--    Flags       - The flags to set
--
-- This function will set the flags  to  be  used  for  verification  of
-- certificates and override any defaults. The provided flags must be an
-- or of the Certificate_Verify_Flags enumerations.
--
   procedure Certificate_Set_Verify_Flags
             (  Credentials : in out Certificate_Credentials;
                Flags       : unsigned
             );
--
-- Certificate_Set_Verify_Function -- Set callback
--
--     Handle - The callback
--
-- This function sets a callback to be called  when  peer's  certificate
-- has been received in order to verify it on receipt rather than  doing
-- after  the  handshake  is  completed.  TLS_Error  may  be  raised  to
-- terminate the handshake.
--
   generic
      with procedure Handle (Session : in out Session_Type);
   package Certificate_Set_Verify_Function is
      procedure Set
                (  Credentials : in out Certificate_Credentials'Class
                );
   end Certificate_Set_Verify_Function;
--
-- Certificate_Set_Verify_Limits -- Set limits
--
--    Credentials - The credentials
--    Max_Bits    - The number of bits of an acceptable certificate
--    Max_Depth   - The   maximum   depth  of  the   verification  of  a
--                  certificate chain
--
-- This function will set some upper limits for the default verification
-- function.
--
   procedure Certificate_Set_Verify_Limits
             (  Credentials : in out Certificate_Credentials;
                Max_Bits    : unsigned := 8200;
                Max_Depth   : unsigned := 5
             );
--
-- Certificate_Set_X509_CRL -- Set trusted CRLs
--
--    Credentials - The credentials
--    List        - The list of trusted CRLs
--
-- This function  adds the trusted  CRLs in order to  verify  client  or
-- server certificates. In case of a client  this is not required.  This
-- function may be called multiple times.
--
   procedure Certificate_Set_X509_CRL
             (  Credentials : in out Certificate_Credentials;
                List        : X509_CRL_Array
             );
--
-- Certificate_Set_X509_CRL_File -- Set trusted CRLs from a file
--
--    Credentials - The credentials
--    File        - With the list of verified CRLs (DER or PEM list)
--    Format      - PEM or DER
--
-- This function adds the trusted  CRLs in  order to  verify  client  or
-- server  certificates.  In  case of  a client  this is  not  required.
-- This function may be called multiple times.
--
   procedure Certificate_Set_X509_CRL_File
             (  Credentials : in out Certificate_Credentials;
                File        : String;
                Format      : X509_Crt_Fmt
             );
--
-- Certificate_Set_X509_CRL_Mem -- Set trusted CRLs
--
--    Credentials        - The credentials
--    List / Certificate - The list of trusted CRLs
--    Format             - PEM or DER
--
-- This function adds the trusted  CRLs in  order to  verify  client  or
-- server  certificates.  In  case of  a client  this is  not  required.
-- This function may be called multiple times.
--
   procedure Certificate_Set_X509_CRL_Mem
             (  Credentials : in out Certificate_Credentials;
                List        : Datum_Array;
                Format      : X509_Crt_Fmt
             );
   procedure Certificate_Set_X509_CRL_Mem
             (  Credentials : in out Certificate_Credentials;
                Certificate : Stream_Element_Array;
                Format      : X509_Crt_Fmt
             );
--
-- Certificate_Set_X509_Key -- Set trusted CRLs
--
--    Credentials - The credentials
--    List        - A certificate list (path) for the specified key
--    Key         - The key
--
-- This  function  sets  a   certificate/private   key   pair   in   the
-- gnutls_certificate_credentials_t  structure.  This  function  may  be
-- called more than once, in case multiple keys/certificates  exist  for
-- the server. For clients that wants to send more than  their  own  end
-- entity certificate (e.g., also an intermediate CA cert) then put  the
-- certificate chain in cert_list . Note that the certificates and  keys
-- provided,  can be safely deinitialized after this function is called.
-- If  that  function fails to load the res structure is at an undefined
-- state, it must not be reused to load other keys or certificates.
--
   procedure Certificate_Set_X509_Key
             (  Credentials : in out Certificate_Credentials;
                List        : X509_CRL_Array;
                Key         : X509_Privkey
             );
--
-- Certificate_Set_X509_Key_File -- Set certificate/private key pair
--
--    Credentials - The credentials
--    Cert_File   - A file that  containing  the certificate list (path)
--                  for the specified private key, in PKCS7 format, or a
--                  list of certificates
--    Key_File    - A file that contains the private key
--    Format      - PEM or DER
--
-- This  function  sets  a   certificate/private   key   pair   in   the
-- gnutls_certificate_credentials_t  structure.  This  function  may  be
-- called more than once, in case multiple keys/certificates  exist  for
-- the  server.  For  clients  that  need  to send more than its own end
-- entity  certificate,  e.g.,  also  an  intermediate CA cert, then the
-- certfile  must  contain  the ordered certificate chain. Note that the
-- names in the certificate provided will be considered  when  selecting
-- the   appropriate   certificate   to   use   (in   case  of  multiple
-- certificate/key pairs). This function can also accept URLs at keyfile
-- and certfile . In that case  it  will  import  the  private  key  and
-- certificate  indicated  by the URLs. Note that the supported URLs are
-- the ones indicated by  URL_Is_Supported.  In  case  the  certfile  is
-- provided  as  a  PKCS  11  URL, then the certificate, and its present
-- issuers  in  the  token  are  are  imported (i.e., the required trust
-- chain).  If  that  function  fails to load the res structure is at an
-- undefined  state,  it  must  not  be  reused  to  load  other keys or
-- certificates.
--
   procedure Certificate_Set_X509_Key_File
             (  Credentials : in out Certificate_Credentials;
                Cert_File   : String;
                Key_File    : String;
                Format      : X509_Crt_Fmt
             );
--
-- Certificate_Set_X509_Key_File -- Set certificate/private key pair
--
--    Credentials - The credentials
--    Cert_File   - A file that  containing  the certificate list (path)
--                  for the specified private key, in PKCS7 format, or a
--                  list of certificates
--    Key_File    - A file that contains the private key
--    Format      - PEM or DER
--    Password    - The password of the key
--    Flags       - A combination of flags
--
-- This  function  sets  a   certificate/private   key   pair   in   the
-- gnutls_certificate_credentials_t  structure.  This  function  may  be
-- called more than once, in case multiple keys/certificates  exist  for
-- the  server.  For  clients  that  need  to send more than its own end
-- entity  certificate,  e.g.,  also  an  intermediate CA cert, then the
-- certfile  must  contain  the ordered certificate chain. Note that the
-- names in the certificate provided will be considered  when  selecting
-- the   appropriate   certificate   to   use   (in   case  of  multiple
-- certificate/key pairs). This function can also accept URLs at keyfile
-- and certfile . In that case  it  will  import  the  private  key  and
-- certificate  indicated  by the URLs. Note that the supported URLs are
-- the ones indicated by  URL_Is_Supported.  In  case  the  certfile  is
-- provided  as  a  PKCS  11  URL, then the certificate, and its present
-- issuers  in  the  token  are  are  imported (i.e., the required trust
-- chain).  If  that  function  fails to load the res structure is at an
-- undefined  state,  it  must  not  be  reused  to  load  other keys or
-- certificates.
--
   procedure Certificate_Set_X509_Key_File
             (  Credentials : in out Certificate_Credentials;
                Cert_File   : String;
                Key_File    : String;
                Format      : X509_Crt_Fmt;
                Password    : String;
                Flags       : PKCS_Encrypt_Flags
             );
--
-- Certificate_Set_X509_Key_Mem -- Set key
--
--    Credentials - The credentials
--    Cert        - A certificate list (path) for the specified key
--  [ Key ]       - The private key
--    Format      - PEM or DER
--  [ Password    - The password of the key
--    Flags ]     - A combination of flags
--
-- This  function  sets  a   certificate/private   key   pair   in   the
-- gnutls_certificate_credentials_t  structure.  This  function  may  be
-- called more than once, in case multiple keys/certificates  exist  for
-- the  server.  Note  that  the  keyUsage (2.5.29.15) PKIX extension in
-- X.509  certificates  is  supported.  This  means  that   certificates
-- intended for signing cannot be used  for  ciphersuites  that  require
-- encryption. If the certificate and the private key are given  in  PEM
-- encoding  then  the  strings  that  hold  their  values  must be null
-- terminated. The Key may be omitted if you are using a sign  callback,
-- Sign_Callback_Set.
--
   procedure Certificate_Set_X509_Key_Mem
             (  Credentials : in out Certificate_Credentials;
                Cert        : Stream_Element_Array;
                Key         : Stream_Element_Array;
                Format      : X509_Crt_Fmt;
                Password    : String;
                Flags       : PKCS_Encrypt_Flags
             );
   procedure Certificate_Set_X509_Key_Mem
             (  Credentials : in out Certificate_Credentials;
                Cert        : Stream_Element_Array;
                Format      : X509_Crt_Fmt;
                Password    : String;
                Flags       : PKCS_Encrypt_Flags
             );
   procedure Certificate_Set_X509_Key_Mem
             (  Credentials : in out Certificate_Credentials;
                Cert        : Stream_Element_Array;
                Key         : Stream_Element_Array;
                Format      : X509_Crt_Fmt
             );
   procedure Certificate_Set_X509_Key_Mem
             (  Credentials : in out Certificate_Credentials;
                Cert        : Stream_Element_Array;
                Format      : X509_Crt_Fmt
             );
--
-- Certificate_Set_X509_Simple_PKCS12_File -- Set from PKCS12 file
--
--    Credentials - The credentials
--    PKCS12_File - A file containing PKCS12 blob
--    Format      - PEM or DER
--    Password    - The password of the key
--
-- This function sets a certificate/private key pair and/or a CRL in the
-- Certificate_Credentials.  This  function may be called more than once
-- (in  case  multiple  keys/certificates  exist for the server). PKCS12
-- files  with  a  MAC,  encrypted  bags  and  PKCS  8  private keys are
-- supported. However,  only  password  based  security,  and  the  same
-- password for all operations, are supported. PKCS12 file  may  contain
-- many  keys  and/or  certificates,  and  this  function  will  try  to
-- auto-detect based on the key ID the certificate and key pair to  use.
-- If the PKCS12 file contain the issuer of the selected certificate, it
-- will be appended to the certificate to form a chain. If more than one
-- private keys are stored in the PKCS12 file, then only one key will be
-- read (and it is  undefined  which  one).  It  is  believed  that  the
-- limitations  of  this function is acceptable for most usage, and that
-- any  more  flexibility  would introduce complexity that would make it
-- harder to use this functionality at all.
--
   procedure Certificate_Set_X509_Simple_PKCS12_File
             (  Credentials : in out Certificate_Credentials;
                PKCS12_File : String;
                Format      : X509_Crt_Fmt;
                Password    : String
             );
--
-- Certificate_Set_X509_Simple_PKCS12_Mem -- Set from PKCS12 blob
--
--    Credentials - The credentials
--    P12blob     - PKCS12 blob
--    Format      - PEM or DER
--    Password    - The password of the key
--
-- This function sets a certificate/private key pair and/or a CRL in the
-- gnutls_certificate_credentials_t  structure.  This  function  may  be
-- called more than once (in case multiple keys/certificates  exist  for
-- the server).  Encrypted  PKCS12  bags  and  PKCS8  private  keys  are
-- supported. However,  only  password  based  security,  and  the  same
-- password for all operations, are supported. PKCS12 file  may  contain
-- many  keys  and/or  certificates,  and  this  function  will  try  to
-- auto-detect based on the key ID the certificate and key pair to  use.
-- If the PKCS12 file contain the issuer of the selected certificate, it
-- will be appended to the certificate to form a chain. If more than one
-- private keys are stored in the PKCS12 file, then only one key will be
-- read (and it is  undefined  which  one).  It  is  believed  that  the
-- limitations  of  this function is acceptable for most usage, and that
-- any  more  flexibility  would introduce complexity that would make it
-- harder to use this functionality at all.
--
   procedure Certificate_Set_X509_Simple_PKCS12_Mem
             (  Credentials : in out Certificate_Credentials;
                P12blob     : Stream_Element_Array;
                Format      : X509_Crt_Fmt;
                Password    : String
             );
--
-- Certificate_Set_X509_System_Trust -- Set system trusted CAs
--
--    Credentials - The credentials
--
-- This function  adds the  system's  default trusted  CAs  in  order to
-- verify client or server certificates.
--
   procedure Certificate_Set_X509_System_Trust
             (  Credentials : in out Certificate_Credentials
             );
--
-- Certificate_Set_X509_Trust -- Set trusted CAs
--
--    Credentials - The credentials
--    List        - Of trusted CAs
--
-- This function adds the trusted CAs  in  order  to  verify  client  or
-- server certificates. In case of a client this is not  required.  This
-- function may be called multiple times. In case of a  server  the  CAs
-- set here will be sent to the client if a certificate request is sent.
-- This can be disabled using Sertificate_Send_X509_RDN_Sequence.
--
   procedure Certificate_Set_X509_Trust
             (  Credentials : in out Certificate_Credentials;
                List        : X509_Crt_Array
             );
--  --
--  -- Certificate_Set_X509_Trust_Dir -- Set from directory files
--  --
--  --    Credentials - The credentials
--  --    Directory   - Containing the list of trusted CAs (DER or PEM list)
--  --    Format      - PEM or DER
--  --
--  -- This  function adds the trusted CAs present in the directory in order
--  -- to verify client or server certificates. This function  is  identical
--  -- to  Certificate_set_X509_Trust_File  but  loads all certificates in a
--  -- directory.
--  --
--     procedure Certificate_Set_X509_Trust_Dir
--               (  Credentials : in out Certificate_Credentials;
--                  Directory   : String;
--                  Format      : X509_Crt_Fmt
--               );
--
-- Certificate_Set_X509_Trust_File -- Set from file
--
--    Credentials - The credentials
--    File        - Containing the list of trusted CAs (DER or PEM list)
--    Format      - PEM or DER
--
-- This function adds the trusted CAs  in  order  to  verify  client  or
-- server certificates. In case of a client this is not  required.  This
-- function may be called multiple times. In case of a server the  names
-- of the CAs set here will be sent  to  the  client  if  a  certificate
-- request    is    sent.     This     can     be     disabled     using
-- Certificate_Send_X509_RDN_Sequence. This  function  can  also  accept
-- URLs. In that case it will import all certificates that are marked as
-- trusted.  Note  that  the  supported  URLs  are the ones indicated by
-- URL_Is_Supported.
--
   procedure Certificate_Set_X509_Trust_File
             (  Credentials : in out Certificate_Credentials;
                File        : String;
                Format      : X509_Crt_Fmt
             );
--
-- Certificate_Set_X509_Trust_Mem -- Set from memory
--
--    Credentials - The credentials
--    Certificate - A trusted CA
--    Format      - PEM or DER
--
-- This function adds the trusted CA   in  order  to  verify  client  or
-- server certificates. In case of a client this is not  required.  This
-- function may be called multiple times. In case of a  server  the  CAs
-- set here will be sent to the client if a certificate request is sent.
-- This can be disabled using Certificate_Send_X509_RDN_Sequence.
--
   procedure Certificate_Set_X509_Trust_Mem
             (  Credentials : in out Certificate_Credentials;
                Certificate : Stream_Element_Array;
                Format      : X509_Crt_Fmt
             );
--
-- Certificate_Type_Get -- Get certificate type
--
--    Session - The session
--
-- The certificate type is by default X.509,  unless it is negotiated as
-- a TLS extension.
--
-- Returns :
--
--    The currently used certificate type
--
   function Certificate_Type_Get
            (  Session : Session_Type
            )  return Certificate_Type;
--
-- Certificate_Type_Get_ID -- Get certificate type by name
--
--    Name - A certificate type name
--
-- The names are compared in a case insensitive way.
--
-- Returns :
--
--    The currently used certificate type or Crt_Unknown
--
   function Certificate_Type_Get_ID
            (  Name : String
            )  return Certificate_Type;
--
-- Certificate_Type_Get_Name -- Get certificate type name
--
--    Type_Of - A certificate type
--
-- Returns :
--
--    A string that contains the name of the specified certificate type
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function Certificate_Type_Get_Name
            (  Type_Of : Certificate_Type
            )  return String;
--
-- Certificate_Type_List -- Get list of certificate types
--
-- Returns :
--
--    An array of available certificate types
--
   function Certificate_Type_List return Certificate_Type_Array;
--
-- Certificate_Verification_Status_Print -- Pretty print
--
--    Status  - The status flags to be printed
--    Type_Of - The certificate type
--
-- Returns :
--
--    The pretty print output
--
   function Certificate_Verification_Status_Print
            (  Status  : Certificate_Status;
               Type_Of : Certificate_Type
            )  return String;
--
-- Certificate_Verify_Peers -- Verify peer's certificate
--
--    Session     - The session
--  [ Host_Name ] - The expected name of the peer
--
-- This function will verify the peer's certificate and store the status
-- in the status variable as a bitwise or'd  gnutls_certificate_status_t
-- values  or  zero  if  the  certificate is trusted. Note that value in
-- status is set only when the return value of this function is  success
-- (i.e, failure to trust a certificate does not imply a negative return
-- value).  The  default verification flags used by this function can be
-- overridden using  Certificate_Set_Verify_Flags.  This  function  will
-- take into account the OCSP Certificate Status TLS extension, as  well
-- as the following X.509 certificate extensions: Name Constraints,  Key
-- Usage,  and  Basic  Constraints (pathlen). To avoid denial of service
-- attacks  some default upper limits regarding the certificate key size
-- and    chain    size    are    set.    To    override    them     use
-- Certificate_Set_Verify_Limits.  Note  that  you  must  also check the
-- peer's  name in order to check if the verified certificate belongs to
-- the actual peer, see X509_Crt_Check_Hostname.
--
-- Returns :
--
--    Verification output
--
   function Certificate_Verify_Peers
            (  Session   : Session_Type;
               Host_Name : String
            )  return Certificate_Status;
   function Certificate_Verify_Peers
            (  Session : Session_Type
            )  return Certificate_Status;
--
-- Check_Version -- Check version
--
--    Version - Required version
--
-- Check  that the version of the library is at minimum the one given as
-- a string in Version and return  the  actual  version  string  of  the
-- library
--
-- Returns :
--
--    Version
--
-- Exceptions :
--
--    End_Error - Condition is not met
--
   function Check_Version (Version : String) return String;
--
-- Cipher_Get -- Get currently used cipher
--
--    Session - The session
--
-- Returns :
--
--    Currently used cipher
--
   function Cipher_Get (Session : Session_Type) return Cipher_Algorithm;
--
-- Cipher_Get_ID -- Get cipher algorithm by name
--
--    Name - A cipher name
--
-- Returns :
--
--    The currently used certificate type or Cipher_Unknown
--
   function Cipher_Get_ID (Name : String) return Cipher_Algorithm;
--
-- Cipher_Get_Key_Size -- Get cipher algorithm key size
--
--    Algorithm - An encryption algorithm
--
-- Returns :
--
--    Length (in bytes)  of the given  cipher's  key size,  or 0  if the
--    given cipher is invalid
--
   function Cipher_Get_Key_Size
            (  Algorithm : Cipher_Algorithm
            )  return size_t;
--
-- Cipher_Get_Name -- Get cipher algorithm name
--
--    Algorithm - An encryption algorithm
--
-- Returns :
--
--    The name
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function Cipher_Get_Name
            (  Algorithm : Cipher_Algorithm
            )  return String;
--
-- Cipher_List -- Get cipher algorithms
--
-- Returns :
--
--    Supported cipher algorithms
--
   function Cipher_List return Cipher_Algorithm_Array;
--
-- Cipher_Suite_Get_Name -- Get TLS cipher suite name
--
--    KX     - A key exchange algorithm
--    Cipher - A cipher algorithm
--    MAC    - A MAC algorithm
--
-- Note  that the full cipher suite name must be prepended by TLS or SSL
-- depending of the protocol in use.
--
-- Returns :
--
--    The name of a TLS cipher suite, specified by the given algorithms,
--    or an empty string
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function Cipher_Suite_Get_Name
            (  KX     : KX_Algorithm;
               Cipher : Cipher_Algorithm;
               MAC    : MAC_Algorithm
            )  return String;
--
-- Compression_Get -- Get compression algorithm
--
--    Session - The session
--
-- Get currently used compression algorithm.
--
-- Returns :
--
--    The currently used compression method
--
   function Compression_Get
            (  Session : Session_Type
            )  return Compression_Method;
--
-- Compression_Get_ID -- Get compression method by name
--
--    Name - A compression method name
--
-- The names are compared in a case insensitive way.
--
-- Returns :
--
--    The compression method or Comp_Unknown
--
   function Compression_Get_ID
            (  Name : String
            )  return Compression_Method;
--
-- Compression_Get_Name -- Get compression method name
--
--    Algorithm - A compression method algorithm
--
-- Returns :
--
--    The name
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function Compression_Get_Name
            (  Algorithm : Compression_Method
            )  return String;
--
-- Compression_List -- Get compression method algorithms
--
-- Returns :
--
--    Supported compression method algorithms
--
   function Compression_List return Compression_Method_Array;
--
-- Credentials_Clear -- Clears credentials
--
--    Session - The session
--
-- Clears all the credentials previously set in this session
--
   procedure Credentials_Clear (Session : in out Session_Type);
--
-- DB_Check_Entry -- Check if the database entry has expired
--
--    Session       - The session
--    Session_Entry - Session data (not key)
--
-- Returns :
--
--    True if the database entry is valid
--
   function DB_Check_Entry
            (  Session       : Session_Type;
               Session_Entry : Stream_Element_Array
            )  return Boolean;
--
-- DB_Remove_Session -- Remove session
--
--    Session - The session
--
-- This function  will remove the current  session data from the session
-- database.  This will prevent  future handshakes reusing these session
-- data.  This  function should be called  if a session  was  terminated
-- abnormally.
--
   procedure DB_Remove_Session (Session : in out Session_Type);
--
-- DB_Set_Cache_Expiration -- Set timeout
--
--    Session - The session
--    Timeout - The timeout to set
--
-- Set the expiration time for resumed sessions. The default is one hour
-- at the time of this writing.
--
   procedure DB_Set_Cache_Expiration
             (  Session : in out Session_Type;
                Timeout : Duration
             );
--
-- DB_Set_Functions -- Set handlers for resumed sessions database
--
--    Session - The session
--    Request - The object handling operations
--
-- The Request's operations will be used to:
--
--    Remove   - to remove data from the resumed sessions database;
--    Retrieve - to retrieve data from the resumed sessions database;
--    Store    - to store data in the resumed sessions database.
--
   type DB_Request is abstract
      new Ada.Finalization.Limited_Controlled with null record;
   procedure Remove
             (  Request : in out DB_Request;
                Key     : Stream_Element_Array
             )  is abstract;
   function Retrieve
            (  Request : access DB_Request;
               Key     : Stream_Element_Array
            )  return Stream_Element_Array is abstract;
   procedure Store
             (  Request : in out DB_Request;
                Key     : Stream_Element_Array
             )  is abstract;
   procedure DB_Set_Functions
             (  Session : in out Session_Type;
                Request : access DB_Request'Class
             );
--
-- DB_Check_Entry_Time -- Check entry time
--
--    DB_Entry - Entry
--
-- This function returns the time that this entry was active.  It can be
-- used for database entry expiration.
--
-- Returns :
--
--    The entry time
--
-- Exceptions :
--
--    Time_Error - Time error
--
   function DB_Check_Entry_Time
            (  DB_Entry : Stream_Element_Array
            )  return Time;
--
-- DB_Get_Group -- Get group parameters
--
--    Session   - The session
--    Raw_Gen   - Will hold the generator
--    Raw_Prime - Will hold the prime.
--
-- This function  will  return the  group parameters  used  in  the last
-- Diffie-Hellman key exchange  with  the peer.  These are the prime and
-- the generator used.  This function should  be used for both anonymous
-- and ephemeral Diffie-Hellman.
--
   procedure DB_Get_Group
             (  Session   : Session_Type;
                Raw_Gen   : in out Datum_Holder'Class;
                Raw_Prime : in out Datum_Holder'Class
             );
--
-- DB_Get_Default_Cache_Expiration -- Default setting
--
-- Returns :
--
--    The expiration time of stored sessions for resumption
--
   function DB_Get_Default_Cache_Expiration return Duration;
--
-- DB_Get_Peers_Public_Bits -- Get the key bit size used
--
--    Session - The session
--
-- Get  the Diffie-Hellman  public  key bit size.  Can be  used for both
-- anonymous and ephemeral Diffie-Hellman.
--
-- Returns :
--
--    The public key bit size used in the last Diffie-Hellman key
--
   function DB_Get_Peers_Public_Bits
            (  Session : Session_Type
            )  return Natural;
--
-- DB_Get_Prime_Bits -- Get the key bit size used
--
--    Session - The session
--
-- This function  will return the bits  of  the prime  used  in the last
-- Diffie-Hellman key  exchange with the peer.  Should be used  for both
-- anonymous and ephemeral Diffie-Hellman.  Note that some ciphers, like
-- RSA and DSA without DHE,  do not use a Diffie-Hellman  key  exchange,
-- and then this function will return 0.
--
-- Returns :
--
--    The Diffie-Hellman bit strength is returned
--
   function DB_Get_Prime_Bits (Session : Session_Type) return Natural;
--
-- DB_Get_Pubkey -- Get public key
--
--    Session - The session
--    Raw_Key - Will hold the generator
--
-- This function  will return the peer's  public  key  used in  the last
-- Diffie-Hellman key exchange.  This function  should be  used for both
-- anonymous and ephemeral Diffie-Hellman.
--
   procedure DB_Get_Pubkey
             (  Session : Session_Type;
                Raw_Key : in out Datum_Holder'Class
             );
--
-- DB_Get_Secret_Bits -- Get bits used in key exchange
--
--    Session - The session
--
-- This function will return  the  bits  used in the last Diffie-Hellman
-- key exchange with the peer.  Should be used  for  both anonymous  and
-- ephemeral Diffie-Hellman.
--
-- Returns :
--
--    Bits used in key exchange
--
   function DB_Get_Secret_Bits (Session : Session_Type) return Natural;
--
-- DB_Set_Prime_Bits -- Set bits used in key exchange
--
--    Session - The session
--    Bits    - The bits number
--
-- This  function  sets  the number of bits, for use in a Diffie-Hellman
-- key  exchange.  This  is  used  both in DH ephemeral and DH anonymous
-- cipher suites. This will set the minimum size of the prime that  will
-- be  used  for  the  handshake. In the client side it sets the minimum
-- accepted  number  of  bits.  If a server sends a prime with less bits
-- than that E_DH_Prime_Unacceptable will be returned by the  handshake.
-- Note  that  values  lower  than  512 bits may allow decryption of the
-- exchanged  data. The function has no effect in server side. Note that
-- since  3.1.7  this function is deprecated. The minimum number of bits
-- is set by the priority string  level.  Also  this  function  must  be
-- called after Priority_Set_Direct or the set value may  be  overridden
-- by the selected priority options.
--
   procedure DB_Set_Prime_Bits
             (  Session : in out Session_Type;
                Bits    : Positive
             );
--
-- DH_Params_Export2_PKCS3 -- Export parameters
--
--    Params - Holds the DH parameters
--    Format - The format of output params. One of PEM or DER
--    Result - A PKCS3 structure
--
-- This function  will  export  the  given  DH  parameters  to  a  PKCS3
-- DH_Params  structure.  This  is  the  format  generated  by  "openssl
-- dhparam" tool.

   procedure DH_Params_Export2_PKCS3
             (  Params : DH_Params;
                Format : X509_Crt_Fmt;
                Result : in out Datum_Holder'Class
             );
--
-- DH_Params_Export_PKCS3 -- Export parameters
--
--    Params  - Holds the DH parameters
--    Format  - The format of output params. One of PEM or DER
--    Buffer  - The buffer to store parameters
--    Pointer - In the buffer to place parameters
--
-- This function will export the given  dh  parameters  to  a  PKCS3  DH
-- Params structure. This is the format generated by  "openssl  dhparam"
-- tool.  If  the  structure  is  PEM  encoded, it will have a header of
-- "BEGIN DH PARAMETERS".
--
-- Exceptions :
--
--    Layout_Error - Illegal Pointer or else no room for output
--
   procedure DH_Params_Export_PKCS3
             (  Params  : DH_Params;
                Format  : X509_Crt_Fmt;
                Buffer  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- DH_Params_Export_Raw -- Export parameters
--
--    Params    - Holds the DH parameters
--    Prime     - Will hold the new prime
--    Generator - Will hold the new generator
--    Bits      - Will hold the secret key's number of bits
--
-- This function  will export the pair of prime and generator for use in
-- the Diffie-Hellman key exchange.
--
   procedure DH_Params_Export_Raw
             (  Params    : DH_Params;
                Prime     : in out Datum_Holder'Class;
                Generator : in out Datum_Holder'Class;
                Bits      : out Natural
             );
   procedure DH_Params_Export_Raw
             (  Params    : DH_Params;
                Prime     : in out Datum_Holder'Class;
                Generator : in out Datum_Holder'Class
             );
--
-- DH_Params_Generate2 -- Generate prime and generator
--
--    Params - Holds the DH parameters
--    Bits   - The secret key's number of bits
--
-- This function will generate a new pair of prime and generator for use
-- in  the Diffie-Hellman  key exchange. This function is normally slow.
-- Do not set  the number of bits directly,  use Sec_Param_To_PK_Bits to
-- get bits for PK_DSA. Also note that the DH parameters are only useful
-- to servers. Since clients use the parameters sent by the server, it's
-- of no use to call this in client side.
--
   procedure DH_Params_Generate2
             (  Params : in out DH_Params;
                Bits   : Positive
             );
--
-- DH_Params_Import_PKCS3 -- Import parameters
--
--    Params       - Holds the DH parameters
--    PKCS3_Params - PKCS3 DH Params structure PEM or DER encoded
--    Format       - The format of params. PEM or DER.
--
-- This function will extract the DHParams found in  a  PKCS3  formatted
-- structure. This is the format generated by "openssl dhparam" tool. If
-- the structure is PEM encoded, it should have a header  of  "BEGIN  DH
-- PARAMETERS".
--
   procedure DH_Params_Import_PKCS3
             (  Params       : in out DH_Params;
                PKCS3_Params : Stream_Element_Array;
                Format       : X509_Crt_Fmt
             );
--
-- DH_Params_Import_Raw -- Import parameters
--
--    Params    - Holds the DH parameters
--    Prime     - The prime
--    Generator - The generator
--
-- This function will replace the pair of prime and generator for use in
-- the Diffie-Hellman key exchange.
--
   procedure DH_Params_Import_Raw
             (  Params    : in out DH_Params;
                Prime     : Stream_Element_Array;
                Generator : Stream_Element_Array
             );
--
-- Digest_Get_ID -- Get digest algorithm
--
--    Name - A digest algorithm name
--
-- The names are compared in a case insensitive way.
--
-- Returns :
--
--    The specified MAC algorithm, Dig_Unknown on failures
--
   function Digest_Get_ID (Name : String) return Digest_Algorithm;
--
-- Digest_Get_Name -- Get digest algorithm name
--
--    Algorithm - A digest algorithm
--
-- Returns :
--
--    The name
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function Digest_Get_Name
            (  Algorithm : Digest_Algorithm
            )  return String;
--
-- Digest_List -- Get diget algorithms
--
-- Returns :
--
--    Supported diget algorithms
--
   function Digest_List return Digest_Algorithm_Array;
--
-- ECC_Curve_Get -- Get currently used curve
--
--    Session - The session
--
-- Returns  the currently used elliptic curve.  Only valid when using an
-- elliptic curve ciphersuite.
--
-- Returns :
--
--    The currently used curve
--
   function ECC_Curve_Get (Session : Session_Type) return ECC_Curve;
--
-- ECC_Curve_Get_Name -- Get curve name
--
--    Curve - An ECC curve
--
-- Returns :
--
--    The curve name
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function ECC_Curve_Get_Name (Curve : ECC_Curve) return String;
--
-- ECC_Curve_Get_Size -- Get curve size
--
--    Curve - An ECC curve
--
-- Returns :
--
--    Returns the size in bytes of the curve or 0
--
   function ECC_Curve_Get_Size (Curve : ECC_Curve) return Natural;
--
-- ECC_Curve_List -- Get ECC curves
--
-- Returns :
--
--    Supported ECC curves
--
   function ECC_Curve_List return ECC_Curve_Array;
--
-- Error_Is_Fatal -- Check error code
--
--    Error - The error code
--
-- Returns :
--
--    True if the error code indicates a fatal error
--
   function Error_Is_Fatal (Error : int) return Boolean;
--
-- Error_To_Alert -- Error to alert conversion
--
--    Error - Is a negative integer
--    Level - The alert level will be stored there
--
-- Get an alert depending  on  the  error  code  returned  by  a  GNUTLS
-- function. All alerts sent  by  this  function  should  be  considered
-- fatal.  The  only  exception  is  when  err is E_Rehandshake, where a
-- warning  alert  should  be  sent  to  the  peer  indicating  that  no
-- renegotiation  will  be  performed. If there is no mapping to a valid
-- alert the alert to indicate internal error is returned.
--
-- Returns :
--
--    The alert code to use for a particular error code
--
   function Error_To_Alert (Error : int; Level : access int) return int;
--
-- Est_Record_Overhead_Size -- Estimated record overhead
--
--    Version - GNUTLS_protocol
--    Cipher  - Cipher Algorithm
--    MAC     - MAC Algorithm
--    Comp    - Compression method
--
-- This  function  will return the set size in bytes of the overhead due
-- to  TLS  (or  DTLS)  per  record. Note that this function may provide
-- inacurate  values  when  TLS extensions that modify the record format
-- are  negotiated. In these cases a more accurate value can be obtained
-- using Record_Overhead_Size after a completed handshake.
--
-- Returns :
--
--    The number of overhead bytes
--
   function Est_Record_Overhead_Size
            (  Version : Protocol;
               Cipher  : Cipher_Algorithm;
               MAC     : MAC_Algorithm;
               Comp    : Compression_Method;
               Flags   : unsigned := 0
            )  return size_t;
--
-- Fingerprint -- Calculation of
--
--    Algorithm - The digest algorithm
--    Data      - The data
--
-- This function will calculate a fingerprint (actually a hash), of  the
-- given data. The result is not printable data. You should  convert  it
-- to hex, or to something else printable. This  is  the  usual  way  to
-- calculate a fingerprint of an X.509  DER  encoded  certificate.  Note
-- however  that the fingerprint of an OpenPGP certificate is not just a
-- hash and cannot be calculated with this function.
--
-- Returns :
--
--    The digest
--
   function Fingerprint
            (  Algorithm : Digest_Algorithm;
               Data      : Stream_Element_Array
            )  return Stream_Element_Array;
--
-- Get_Error_Code -- Error code from an TLS_Error exception
--
--    Error - The exception occurrence
--
-- Returns :
--
--    The error code or else E_Internal_Error
--
   function Get_Error_Code (Error : Exception_Occurrence) return int;
--
-- Global_Set_Audit_Log_Function -- Enables audit callback
--
-- This function will set Log callbacks with the following profile:
--
--  [ Session ] - The session
--    Text      - The message
--
   generic
      with procedure Log (Text : String);
      with procedure Log (Session : in out Session_Type; Text: String);
   package Global_Set_Audit_Log_Function is
      procedure Set;
   end Global_Set_Audit_Log_Function;
--
-- Global_Set_Log_Function -- Enables logger callback
--
-- This function will set Log callbacks with the following profile:
--
--    Level - Debug level
--    Text  - The message
--
   generic
      with procedure Log (Level : int; Text : String);
   package Global_Set_Log_Function is
      procedure Set (Level : int);
   end Global_Set_Log_Function;
--
-- Handshake -- TLS handshaking
--
--    Session - The session
--
-- This function  does  the  handshake  of  the  TLS/SSL  protocol,  and
-- initializes  the  TLS  connection.  This  function  will  fail if any
-- problem  is  encountered,  and  will return a negative error code. In
-- case  of  a  client, if the client has asked to resume a session, but
-- the server couldn't, then a full handshake will be performed.
--
-- Returns :
--
--    True if handshake is pending
--
   function Handshake (Session : Session_Type) return Boolean;
--
-- Handshake_Description_Get_Name -- Get handshake name
--
--    Description - Handshake description
--
-- Returns :
--
--    The name of
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function Handshake_Description_Get_Name
            (  Description : Handshake_Description
            )  return String;
--
-- Handshake_Get_Last_In -- Get the last handshake message received
--
--    Session - The session
--
-- This function is only  useful  to  check  where  the  last  performed
-- handshake  failed.  If  the  previous  handshake  succeed  or was not
-- performed at all then no meaningful value will be returned.

-- Returns :
--
--    The last handshake message type received
--
   function Handshake_Get_Last_In
            (  Session : Session_Type
            )  return Handshake_Description;
--
-- Handshake_Get_Last_Out -- Get the last handshake message sent
--
--    Session - The session
--
-- This function is only  useful  to  check  where  the  last  performed
-- handshake  failed.  If  the  previous  handshake  succeed  or was not
-- performed at all then no meaningful value will be returned.

-- Returns :
--
--    The last handshake message type sent
--
   function Handshake_Get_Last_Out
            (  Session : Session_Type
            )  return Handshake_Description;
--
-- Handshake_Set_Hook_Function -- Handshake hook
--
--    Session - The session
--    Message - The message to hook at
--    Post    - When the hook function should be called
--    Func    - The callback to call
--
-- This function will set a callback to be called after  or  before  the
-- specified handshake message has been received or generated. This is a
-- generalization  of  Handshake_Set_Post_Client_Hello_Function. To call
-- the hook function prior  to  the  message  being  sent/generated  use
-- Hook_Pre  as  post  parameter, Hook_Post to call after, and Hook_Both
-- for  both  cases. This callback should raise a TLS_Error to terminate
-- the  handshake. Note to hook at all handshake messages use an Message
-- of  Handshake_Any.  Warning:  You  should  not  use  this function to
-- terminate the handshake based on client input unless  you  know  what
-- you are doing. Before the handshake is finished there is  no  way  to
-- know if there is a man-in-the-middle attack being performed.
--
   generic
      with procedure Hook
                     (  Session  : in out Session_Type;
                        Message  : Handshake_Description;
                        Post     : Hook_Type;
                        Incoming : unsigned
                     );
   package Handshake_Set_Hook_Function is
      procedure Set
                (  Session : in out Session_Type;
                   Message : Handshake_Description;
                   Post    : Hook_Type
                );
   end Handshake_Set_Hook_Function;
--
-- Handshake_Set_Max_Packet_Length -- Set the maximum packet length
--
--    Session - The session
--    Max     - The maximum number
--
-- This function will set the maximum size of  all  handshake  messages.
-- Handshakes over this size  are  rejected  with  E_Handshake_Too_Large
-- error code. The default  value  is  48kb  which  is  typically  large
-- enough. Set this to 0 if you do not want to set an upper  limit.  The
-- reason for restricting the  handshake  message  sizes  are  to  limit
-- Denial of Service attacks.
--
   procedure Handshake_Set_Max_Packet_Length
             (  Session : in out Session_Type;
                Max     : size_t
             );
--
-- Handshake_Set_Post_Client_Hello_Function -- Handshake hello hook
--
--    Session - The session
--
-- This function will set a callback to be called after the client hello
-- has  been  received (callback valid in server side only). This allows
-- the server to adjust settings based  on  received  extensions.  Those
-- settings could be ciphersuites, requesting certificate,  or  anything
-- else except for version negotiation (this is done  before  the  hello
-- message is parsed). This callback must raise TLS_Error  to  terminate
-- the  handshake.  Since GnuTLS 3.3.5 the callback is allowed to return
-- True  in  Hold  to  put the handshake on hold. In that case Handshake
-- will  return  E_Interrupted  and can be resumed when needed. Warning:
-- You  should not use this function to terminate the handshake based on
-- client input unless you know what you are doing. Before the handshake
-- is  finished  there is no way to know if there is a man-in-the-middle
-- attack being performed.
--
   generic
      with procedure Hello
                     (  Session : in out Session_Type;
                        Hold    : out Boolean
                     );
   package Handshake_Set_Post_Client_Hello_Function is
      procedure Set (Session : in out Session_Type);
   end Handshake_Set_Post_Client_Hello_Function;
--
-- Handshake_Set_Random -- Set random value
--
--    Session - The session
--    Random  - The value
--
-- This  function  will explicitly set the server or client hello random
-- value in the subsequent TLS handshake. Note that this function should
-- not  normally  be  used  as GNUTLS will select automatically a random
-- value for the handshake.  This  function  should  not  be  used  when
-- resuming a session.
--
   subtype Random_Value is Stream_Element_Array (1..32);
   procedure Handshake_Set_Random
             (  Session : in out Session_Type;
                Random  : Random_Value
             );
--
-- Handshake_Set_Timeout -- Set handshake timeout
--
--    Session - The session
--    Timeout - The timeout
--
-- This  function  sets  the  timeout  for  the handshake process to the
-- provided value. Use 0.0 to disable timeout. Without Timeout specified
-- a default timeout is used.
--
   procedure Handshake_Set_Timeout
             (  Session : Session_Type;
                Timeout : Duration := Duration'First
             );
--
-- Initialize -- Object initialization
--
--    Cache      - The priority cache
--    Parameters - Cache parameters
--
-- Sets  priorities  for the  ciphers,  key  exchange  methods, MACs and
-- compression methods. See gnutls_priority_init for more information.
-- This must be the first operation called.
--
-- Exceptions :
--
--    TLS_Error - The  error  message   contains  "at <string-position>"
--                appended, indicating error location in the string
--
   procedure Initialize
             (  Cache      : in out Priority;
                Parameters : String := "PERFORMANCE:%SERVER_PRECEDENCE"
             );
--
-- Is_Initialized -- Check if Initialize was called
--
--    Cache - The priority cache
--
-- Returns :
--
--    True if cache is already initialized
--
   function Is_Initialized (Cache : Priority) return Boolean;
--
-- KX_Get -- Get currently used key exchange algorithm
--
--    Session - The session
--
-- Returns :
--
--    The key exchange algorithm used in the last handshake
--
   function KX_Get (Session : Session_Type) return KX_Algorithm;
--
-- KX_Get_ID -- Get key exchange algorithm by name
--
--    Name - The key exchange algorithm by name
--
-- Returns :
--
--    The key exchange algorithm
--
   function KX_Get_ID (Name : String) return KX_Algorithm;
--
-- KX_Get_Name -- Get algorithm name
--
--    Algorithm - The algorithm
--
-- Returns :
--
--    The name of
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function KX_Get_Name (Algorithm : KX_Algorithm) return String;
--
-- KX_List -- Get key exchange algorithms
--
-- Returns :
--
--    Supported key exchange algorithms
--
   function KX_List return KX_Algorithm_Array;
--
-- MAC_Get -- Get currently used MAC algorithm
--
--    Session - The session
--
-- Returns :
--
--    The MAC algorithm
--
   function MAC_Get (Session : Session_Type) return MAC_Algorithm;
--
-- MAC_Get_Key_Size -- Get the size of the MAC key used in TLS
--
--    Algorithm - The algorithm
--
-- Returns :
--
--    Returns the size of the MAC key used in TLS
--
   function MAC_Get_Key_Size (Algorithm : KX_Algorithm) return size_t;
--
-- MAC_Get_ID -- Get MAC algorithm by name
--
--    Name - The MAC algorithm by name
--
-- Returns :
--
--    The MAC algorithm
--
   function MAC_Get_ID (Name : String) return MAC_Algorithm;
--
-- MAC_Get_Name -- Get algorithm name
--
--    Algorithm - The algorithm
--
-- Returns :
--
--    The name of
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function MAC_Get_Name (Algorithm : MAC_Algorithm) return String;
--
-- MAC_List -- Get MAC algorithms
--
-- Returns :
--
--    Supported MAC algorithms
--
   function MAC_List return MAC_Algorithm_Array;
--
-- OCSP_Status_Request_Is_Checked -- Check status
--
--    Session - The session
--
-- Check  whether  an OCSP status response was included in the handshake
-- and whether it was checked and valid (not  too  old  or  superseded).
-- This is a helper function when needing to decide whether  to  perform
-- an OCSP validity check on the  peer's  certificate.  Must  be  called
-- after Certificate_Verify_Peers is called.
--
-- Returns :
--
--    False if it wasn't sent, or sent and was invalid.
--
   function OCSP_Status_Request_Is_Checked
            (  Session : Session_Type
            )  return Boolean;
--
-- OpenPGP_Send_Cert -- Send OpenPGP certificate
--
--    Session - The session
--    Status  - The status
--
-- This  function  will order GNUTLS to send the key fingerprint instead
-- of  the  key  in the initial handshake procedure. This should be used
-- with care and only when there is indication  or  knowledge  that  the
-- server can obtain the client's key.
--
   procedure OpenPGP_Send_Cert
             (  Session : Session_Type;
                Status  : OpenPGP_Crt_Status
             );
--
-- PEM_Base64_Decode -- Decode
--
--  [ Header ]  - To search for --BEGIN header
--    Data      - To decode
--  [ Result    - The result buffer
--    Pointer ] - The first element to rewrite, advanced
--
-- This procedure will decode the given  encoded  data.  If  the  header
-- given  is  specified  this procedure will search for "--BEGIN header"
-- and decode only this part. Otherwise it will  decode  the  first  PEM
-- packet found.
--
-- Returns :
--
--  [ The decoded result ]
--
-- Exceptions :
--
--    Layout_Error -- Pointer out of Result or no room
--
   procedure PEM_Base64_Decode
             (  Header  : String;
                Data    : Stream_Element_Array;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure PEM_Base64_Decode
             (  Header  : String;
                Data    : String;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure PEM_Base64_Decode
             (  Data    : Stream_Element_Array;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   procedure PEM_Base64_Decode
             (  Data    : String;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
--
-- PEM_Base64_Encode -- Encode
--
--    Data      - To encode
--  [ Result    - The result buffer
--    Pointer ] - The first element to rewrite, advanced
--
-- This function  will convert  the given data  to printable data, using
-- the base64 encoding. This is the encoding used in PEM messages.
--
-- Returns :
--
--  [ The encoded result ]
--
-- Exceptions :
--
--    Layout_Error -- Pointer out of Result or no room
--
   procedure PEM_Base64_Encode
             (  Data    : Stream_Element_Array;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );
   function PEM_Base64_Encode
            (  Data : Stream_Element_Array
            )  return Stream_Element_Array;
--
-- PK_Algorithm_Get_Name -- Get public key algorithm name
--
--    Algorithm - The public key algorithm
--
-- Returns :
--
--    The name of
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function PK_Algorithm_Get_Name
            (  Algorithm : PK_Algorithm
            )  return String;
--
-- PK_Bits_To_Sec_Param -- Get certificate type by name
--
--    Algorithm - A public key algorithm
--    Bits      - Bits number
--
-- Given  an  algorithm  and  the number of bits,  it  will  return  the
-- security parameter. This is a rough indication.
--
-- Returns :
--
--    Parameter - A security parameter
--
   function PK_Bits_To_Sec_Param
            (  Algorithm : PK_Algorithm;
               Bits      : Positive
            )  return Sec_Param;
--
-- PK_Get_ID -- Get public key algorithm by name
--
--    Name - The public key algorithm by name
--
-- Returns :
--
--    The public key algorithm
--
   function PK_Get_ID (Name : String) return PK_Algorithm;
--
-- PK_Get_Name -- Get public key algorithm name
--
--    Algorithm - The public key algorithm
--
-- Returns :
--
--    The name of
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function PK_Get_Name (Algorithm : PK_Algorithm) return String;
--
-- PK_List -- Get public key algorithms
--
-- Returns :
--
--    Supported public key algorithms
--
   function PK_List return PK_Algorithm_Array;
--
-- PK_To_Sign -- Maps  public  key and  hash algorithms  combinations to
--               signature algorithms
--
--    PK     -  A public key algorithm
--    Digest - A hash algorithm
--
-- Returns :
--
--    The signature algorithm
--
   function PK_To_Sign
            (  PK     : PK_Algorithm;
               Digest : Digest_Algorithm
            )  return Sign_Algorithm;
--
-- Priority_Set -- Set priority cache
--
--    Session - The session
--    Cache   - The priority cache
--
-- Sets the priorities to use on the ciphers, key exchange methods, MACs
-- and compression methods.
--
   procedure Priority_Set
             (  Session : in out Session_Type;
                Cache   : in out Priority
             );
--
-- Priority_Set_Direct -- Set priority cache
--
--    Session    - The session
--    Priorities - Priorities description
--
-- Sets  the priorities  to use on the ciphers,  key  exchange  methods,
-- MACS and compression methods. This function avoids keeping a priority
-- cache and is used to directly set string priorities to a TLS session.
--
   procedure Priority_Set_Direct
             (  Session    : in out Session_Type;
                Priorities : String
             );
--
-- Protocol_Get_ID -- Get protocol by name
--
--    Name - The protocol name
--
-- Returns :
--
--    The protocol
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function Protocol_Get_ID (Name : String) return Protocol;
--
-- Protocol_Get_Name -- Get protocol name
--
--    Protocol - The protocol
--
-- Returns :
--
--    A string that contains the name of the specified TLS version
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function Protocol_Get_Name (Version : Protocol) return String;
--
-- Protocol_Get_Version -- Get protocol version
--
--    Session - The session
--
-- Returns :
--
--    The version of the currently used protocol
--
   function Protocol_Get_Version
            (  Session : Session_Type
            )  return Protocol;
--
-- Protocol_List -- Get supported protocol list
--
-- Get a list of  supported protocols,  e.g. SSL 3.0, TLS 1.0 etc.  This
-- function is not thread safe.
--
-- Returns :
--
--    The available protocols list
--
   function Protocol_List return Protocol_Array;
--
-- Raise_TLS_Error -- Raise exception with code
--
--    Error - The error code E_*
--
-- Raises TLS_Error with the specified code and its message
--
   procedure Raise_TLS_Error (Error : int);
--
-- Record_Check_Corked -- Get corked bytes
--
--    Session - The session
--
-- This function checks if there pending corked data. See Record_Cork.
--
-- Returns :
--
--    The number of corked bytes
--
   function Record_Check_Corked (Session : Session_Type) return size_t;
--
-- Record_Check_Pending -- Get pending bytes
--
--    Session - The session
--
-- This  function checks if there are unread data in the gnutls buffers.
-- If the return value is non-zero  the  next  call  to  Record_Recv  is
-- guaranteed not to block.
--
-- Returns :
--
--    The number of pending bytes
--
   function Record_Check_Pending (Session : Session_Type) return size_t;
--
-- Record_Cork -- Accumulate sent data
--
--    Session - The session
--
-- If called Record_Send will no longer send partial records. All queued
-- records  will  be  sent  when Record_Uncork is called,  or  when  the
-- maximum record size is reached.
--
   procedure Record_Cork (Session : in out Session_Type);
--
-- Record_Disable_Padding -- Disable padding
--
--    Session - The session
--
-- Used to disabled padding in TLS 1.0 and above. Normally  you  do  not
-- need  to use this function, but there are buggy clients that complain
-- if  a  server  pads  the  encrypted data. This of course will disable
-- protection against statistical attacks on the data. This functions is
-- defunt  since  3.1.7.  Random  padding  is disabled by default unless
-- requested using Range_Send_Message.
--
   procedure Record_Disable_Padding (Session : in out Session_Type);
--
-- Record_Get_Direction -- Get pending transport operation
--
--    Session - The session
--
-- Returns :
--
--    The direction
--
   type Direction is (Reading, Writing);
   function Record_Get_Direction (Session : Session_Type)
      return Direction;
--
-- Record_Get_Max_Size -- Get maximum record size
--
--    Session - The session
--
-- Get the record size.  The maximum  record size  is negotiated  by the
-- client after the first handshake message.
--
-- Returns :
--
--    The negotiated record size
--
   function Record_Get_Max_Size (Session : Session_Type) return size_t;
--
-- Record_Overhead_Size -- Get bytes overhead
--
--    Session - The session
--
-- This function  will return  the set size in bytes of the overhead due
-- to TLS (or DTLS) per record.
--
-- Returns :
--
--    The bytes overhead
--
   function Record_Overhead_Size (Session : Session_Type)
      return size_t;
--
-- Record_Recv -- Receive data
--
--    Session - The session
--    Buffer  - Data to receive
--    Last    - The last element received
--
-- This  function  has  the  similar  semantics  with  recv.  The   only
-- difference is that it accepts a GnuTLS session,  and  uses  different
-- error   codes.   In  the  special  case  that  a  server  requests  a
-- renegotiation, the client may receive an error code of E_Rehandshake.
-- This  message  may  be  simply  ignored,  replied   with   an   alert
-- A_No_Renegotiation, or replied with a new handshake, depending on the
-- client's  will.  If  EINTR  is returned by the internal push function
-- (the default  is  recv)  then  E_Interrupted  will  be  returned.  If
-- E_Interrupted or E_Again is returned, you  must  call  this  function
-- again to get the data. See also Record_Get_Direction.  A  server  may
-- also receive E_Rehandshake when a client has initiated  a  handshake.
-- In  that  case  the server can only initiate a handshake or terminate
-- the connection.
--
-- Exceptions :
--
--    End_Error - Connection closed by peer
--    TLS_Error - TLS error
--
   procedure Record_Recv
             (  Session : in out Session_Type;
                Buffer  : out Stream_Element_Array;
                Last    : out Stream_Element_Offset
             );
--
-- Record_Send -- Send data
--
--    Session - The session
--    Buffer  - Data to send
--    Last    - The last element sent
--
-- This  function  has  the  similar  semantics  with  Send.  The   only
-- difference is that it accepts a GnuTLS session,  and  uses  different
-- error codes. Note that if the send buffer is full,  Send  will  block
-- this function. See the Send documentation for more  information.  You
-- can  replace  the  default  push  function  which  is  Send, by using
-- Transport_Set_Push_Function. If the EINTR is returned by the internal
-- push  function  then E_Interrupted will be returned. If E_Interrupted
-- or E_Again is returned, you must call this function again,  with  the
-- exact same parameters; alternatively you could provide a NULL pointer
-- for data, and 0 for size. Note that in DTLS this function will return
-- the  E_Large_Packet  error  code if the send data exceed the data MTU
-- value  -  as  returned by DTLS_Get_Data_MTU. The errno value EMSGSIZE
-- also maps to E_Large_Packet. Note that since 3.2.13 this function can
-- be  called under cork in DTLS mode, and will refuse to send data over
-- the MTU size by returning E_LARGE_PACKET. Last is  set  to  the  last
-- element sent.
--
   procedure Record_Send
             (  Session : in out Session_Type;
                Buffer  : Stream_Element_Array;
                Last    : out Stream_Element_Offset
             );
--
-- Record_Set_Max_Empty_Records -- Set maximum empty records
--
--    Session - The session
--    Count   - Maximum empty records that can be accepted in a row
--
-- Used to set the  maximum  number  of  empty  fragments  that  can  be
-- accepted  in  a  row.  Accepting  many  empty fragments is useful for
-- receiving  length-hidden  content,  where empty fragments filled with
-- pad  are  sent  to  hide  the  real  length  of a message. However, a
-- malicious peer could send empty fragments to mount a DoS  attack,  so
-- as  a safety measure, a maximum number of empty fragments is accepted
-- by default. If you know your application must accept a  given  number
-- of empty fragments in a row, you can use this  function  to  set  the
-- desired value.
--
-- *** Removed ***
--
--     procedure Record_Set_Max_Empty_Records
--               (  Session : in out Session_Type;
--                  Count   : Positive
--               );
--
-- Record_Set_Max_Size -- Set client's record size
--
--    Session - The session
--    Size    - The new size
--
-- This function sets the maximum record packet size in this connection.
-- This  property  can only be set to clients. The server may choose not
-- to  accept  the  requested  size.  Acceptable  values are 512(=2**9),
-- 1024(=2**10),  2048(=2**11)  and  4096(=2**12).  The requested record
-- size does get in effect immediately  only  while  sending  data.  The
-- receive  part  will  take  effect  after a successful handshake. This
-- function uses a TLS extension called max record  size.  Not  all  TLS
-- implementations use or even understand this extension.
--
   procedure Record_Set_Max_Size
             (  Session : in out Session_Type;
                Size    : size_t
             );
--
-- Record_Uncork -- Flush sent data
--
--    Session - The session
--    Wait    - Block
--
-- This resets the effect of Record_Cork, and flushes any pending  data.
-- If the Wait flag is specified then this function will block until the
-- data  is  sent or a fatal error occurs (i.e., the function will retry
-- on  E_Again and E_Interrupted). If the flag Wait is not specified and
-- the  function is interrupted then the E_Again or E_Interrupted errors
-- will  be  pass through and False is returned. To obtain the data left
-- in the corked buffer use Record_Check_Corked.
--
-- Returns :
--
--    True if all data sent
--
   function Record_Uncork
            (  Session : Session_Type;
               Wait    : Boolean
            )  return Boolean;
--
-- Rehandshake -- Renegotiate security parameters
--
--    Session - The session
--
-- This  function  will renegotiate security parameters with the client.
-- This should only be called in case of a server. This message  informs
-- the   peer   that  we  want  to  renegotiate  parameters  (perform  a
-- handshake).  If  this  function succeeds, you must call the Handshake
-- function  in order to negotiate the new parameters. Since TLS is full
-- duplex some application data  might  have  been  sent  during  peer's
-- processing of this message. In that case one should call  Record_Recv
-- until E_Rehandshake is returned to clear any pending data. Care  must
-- be  taken  if  rehandshake  is  mandatory to terminate if it does not
-- start  after  some  threshold.  If  the  client  does  not  wish   to
-- renegotiate  parameters  he  should reply with an alert message, thus
-- the return code will be E_Warning_Alert_Received and the  alert  will
-- be  A_No_Renegotiation.  A  client  may  also  choose  to ignore this
-- message.
--
   procedure Rehandshake (Session : in out Session_Type);
--
-- Safe_Renegotiation_Status -- If safe renegotiation is being used
--
-- Can be used to check whether safe renegotiation is being used in the
-- current session
--
-- Returns :
--
--    True if safe renegotiation is being used
--
   function Safe_Renegotiation_Status
            (  Session : Session_Type
            )  return Boolean;
--
-- Sec_Param_Get_Name -- Get security parameter name
--
--    Param - A security parameter
--
-- Returns :
--
--    The parameter name
--
-- Exceptions :
--
--    End_Error - No parameter matched
--
   function Sec_Param_Get_Name (Param : Sec_Param) return String;
--
-- Sec_Param_To_PK_Bits -- Get certificate type by name
--
--    Algorithm - A public key algorithm
--    Parameter - A security parameter
--
-- When generating private and public key pairs a difficult question  is
-- which size of "bits" the modulus will be in RSA and the group size in
-- DSA. The easy answer is 1024, which is also wrong. This function will
-- convert  a  human understandable security parameter to an appropriate
-- size for the specific algorithm.
--
-- Returns :
--
--    Bits number
--
   function Sec_Param_To_PK_Bits
            (  Algorithm : PK_Algorithm;
               Parameter : Sec_Param
            )  return Natural;
--
-- Session_Channel_Binding -- Extract channel binding data
--
--    Session      - The session
--    Binding_Type - The channel binding type
--    Binding_Data - The data
--
-- Extract given channel binding data of the cbtype (e.g. CB_TLS_UNIQUE)
-- type.
--
   procedure Session_Channel_Binding
             (  Session      : in out Session_Type;
                Binding_Type : Channel_Binding;
                Binding_Data : in out Datum
             );
--
-- Session_Enable_Compatibility_Mode -- Set compatibility mode
--
--    Session - The session
--
   procedure Session_Enable_Compatibility_Mode
             (  Session : in out Session_Type
             );
--
-- Session_Force_Valid -- Force session valid
--
--    Session - The session
--
-- Clears the invalid flag in a session.  That means that  sessions were
-- corrupt or  invalid data were received can be re-used.  Use only when
-- debugging or experimenting with the TLS protocol.  Should not be used
-- in typical applications.
--
   procedure Session_Force_Valid (Session : in out Session_Type);
--
-- Session_Get_Data -- Get current session data
--
--    Session - The session
--    Data    - The current session data
--
   procedure Session_Get_Data
             (  Session : Session_Type;
                Data    : in out Datum_Holder'Class
             );
--
-- Session_Get_Desc -- Get session description
--
--    Session - The session
--
-- Returns :
--
--    A  description  of  the  protocols  and algorithms  in the current
--    session
--
   function Session_Get_Desc (Session : Session_Type) return String;
--
-- Session_Get_ID -- Get current session ID
--
--    Session - The session
--
-- Returns :
--
--    The current session ID
--
   function Session_Get_ID
            (  Session : Session_Type
            )  return Stream_Element_Array;
--
-- Session_Get_Ptr -- Get user pointer
--
--    Session - The session
--
-- Returns :
--
--    The pointer to set using Session_Set_Ptr
--
   function Session_Get_Ptr (Session : Session_Type)
      return System.Address;
--
-- Session_Is_Resumed -- Check if session is resumed
--
--    Session - The session
--
-- Returns :
--
--    The pointer to set using Session_Set_Ptr
--
   function Session_Is_Resumed (Session : Session_Type)
      return Boolean;
--
-- Session_Is_Resumed -- Check if session is resumed
--
--    Session - The session
--
-- Returns :
--
--    The pointer to set using Session_Set_Ptr
--
   function Session_Resumption_Requested (Session : Session_Type)
      return Boolean;
--
-- Session_Set_Data -- Set session data
--
--    Session - The session
--    Data    - The session data
--
-- Sets  all  session  parameters,  in  order  to  resume  a  previously
-- established session. The session data given must be the one  returned
-- by Session_Get_Data. This function should be called before Handshake.
-- Keep in mind that session resuming is advisory. The server may choose
-- not to resume the session, thus a full handshake will be performed.

   procedure Session_Set_Data
             (  Session : in out Session_Type;
                Data    : Stream_Element_Array
             );
--
-- Session_Set_Ptr -- Set user pointer
--
--    Session - The session
--    Pointer - The pointer to set
--
   procedure Session_Set_Ptr
             (  Session : in out Session_Type;
                Pointer : System.Address
             );
--
-- Session_Ticket_Enable_Client -- Enable resumption
--
--    Session - The session
--
-- Reques  that  the client should  attempt  session  resumption  using
-- SessionTicket.

   procedure Session_Ticket_Enable_Client
             (  Session : in out Session_Type
             );
--
-- Session_Ticket_Enable_Server -- Enable resumption
--
--    Session - The session
--
-- Request  that  the  server  should  attempt  session resumption using
-- SessionTicket.     The     key     must     be    initialized    with
-- Session_Ticket_Key_Generate.

   procedure Session_Ticket_Enable_Server
             (  Session : in out Session_Type;
                Key     : Stream_Element_Array
             );
--
-- Session_Ticket_Key_Generate -- Generate key
--
--    Session - The session
--
-- Generate  a  random   key  to   encrypt  security  parameters  within
-- SessionTicket.

   procedure Session_Ticket_Key_Generate
             (  Session : in out Session_Type;
                Key     : in out Datum_Holder'Class
             );
--
-- Set_Default_Priority -- Set default parameters
--
--    Session - The session
--
-- Sets some default priority on the ciphers, key exchange methods, macs
-- and  compression methods. This typically sets a default priority that
-- is  considered  sufficiently  secure to establish encrypted sessions.
-- This function is kept around for backwards compatibility, but because
-- of its wide use it is still fully supported. If  you  wish  to  allow
-- users to provide a string that specify which ciphers to use (which is
-- recommended),  you should   use   Priority_Set_Direct or Priority_Set
-- instead.
--
   procedure Set_Default_Priority (Session : in out Session_Type);
--
-- Set_TLS_Debug -- Enable or disable TLS debug and audit tracing
--
--   Level  - The debugging level
-- [ File ] - The file to write
--
   procedure Set_TLS_Debug (Level : int);
   procedure Set_TLS_Debug (Level : int; File : String);
--
-- Sign_Algorithm_Get -- Get signature algorithm
--
--    Session - The session
--
-- Returns  the  signature  algorithm that  is (or will be) used in this
-- session by the server to sign data.
--
-- Returns :
--
--    The signature algorithm
--
   function Sign_Algorithm_Get (Session : Session_Type)
      return Sign_Algorithm;
--
-- Sign_Algorithm_Get_Client -- Get signature algorithm
--
--    Session - The session
--
-- Returns  the  signature  algorithm that  is (or will be) used in this
-- session by the client to sign data.
--
-- Returns :
--
--    The signature algorithm
--
   function Sign_Algorithm_Get_Client (Session : Session_Type)
      return Sign_Algorithm;
--
-- Sign_Algorithm_Get_Requested -- Get requested signature algorithm
--
--    Session - The session
--    Index   - An index of the signature algorithm to return
--
-- This  function  is useful  in the certificate  callback  functions to
-- assist in selecting the correct certificate.
--
-- Returns :
--
--    The signature algorithm
--
   function Sign_Algorithm_Get_Requested
            (  Session : Session_Type;
               Index   : Positive
            )  return Sign_Algorithm;
--
-- Sign_Get_Hash_Algorithm -- Get digest algorithm
--
--    Sign - The signature algorithm
--
-- This function returns the digest algorithm corresponding to the given
-- signature algorithm.
--
-- Returns :
--
--    The signature algorithm
--
   function Sign_Get_Hash_Algorithm
            (  Sign : Sign_Algorithm
            )  return Digest_Algorithm;
--
-- Sign_Get_ID -- Get signature algorithm by name
--
--    Name - A signature algorithm name
--
-- The names are compared in a case insensitive way.
--
-- Returns :
--
--    The signature algorithm
--
   function Sign_Get_ID (Name : String) return Sign_Algorithm;
--
-- Sign_Get_Name -- Get signature algorithm name
--
--    Algorithm - A signature algorithm
--
-- Returns :
--
--    The signature algorithm name
--
   function Sign_Get_Name (Algorithm : Sign_Algorithm) return String;
--
-- Sign_Get_PK_Algorithm -- Get signature algorithm
--
--    Signature - A signature algorithm
--
-- This function returns the public  key algorithm  corresponding to the
-- given signature algorithms.
--
-- Returns :
--
--    The signature algorithm
--
   function Sign_Get_PK_Algorithm
            (  Signature : Sign_Algorithm
            )  return PK_Algorithm;
--
-- Sign_Is_Secure -- If algorithm is secure
--
--    Algorithm - A signature algorithm
--
-- Returns :
--
--    True if the provided signature algorithm considered to be secure
--
   function Sign_Is_Secure (Algorithm : Sign_Algorithm) return Boolean;
--
-- Sign_List -- List of signature algorithms
--
-- Returns :
--
--    Get a list of supported public key signature algorithms
--
   function Sign_List return Sign_Algorithm_Array;
--
-- StrError -- Text message corresponding to an error code
--
--    Error - The error code
--
-- Returns :
--
--    The corresponding message text
--
-- Exceptions :
--
--    End_Error - Nothing matched
--
   function StrError (Error : int) return String;
--
-- To_Stream_Element_Array -- Conversion to stream element array
--
--    Data - The datum
--
-- Returns :
--
--    The value interpreted as stream element array
--
   function To_Stream_Element_Array (Data : Datum)
      return Stream_Element_Array;
--
-- To_Stream_Element_Array -- Conversion to stream element array
--
--    Data - Text
--
-- Returns :
--
--    The value interpreted as stream element array
--
   function To_Stream_Element_Array (Data : String)
      return Stream_Element_Array;
--
-- To_String -- Conversion to string
--
--    Data - The datum
--
-- Returns :
--
--    The value interpreted as text string
--
   function To_String (Data : Datum) return String;
--
-- Transport_Get_Int -- Get transport integer value
--
--    Session - The session
--
-- Returns :
--
--    The value to set using Transport_Set_Int
--
   function Transport_Get_Int (Session : Session_Type) return int;
--
-- Transport_Get_Int2 -- Get transport user pointers
--
--    Session - The session
--    Receive - Value for receive
--    Send    - Value for send
--
   procedure Transport_Get_Int2
             (  Session : Session_Type;
                Receive : out int;
                Send    : out int
             );
--
-- Transport_Get_Ptr -- Get transport user pointer
--
--    Session - The session
--
-- Returns :
--
--    The pointer to set using Transport_Set_Ptr
--
   function Transport_Get_Ptr (Session : Session_Type)
      return System.Address;
--
-- Transport_Get_Ptr2 -- Get transport user pointers
--
--    Session - The session
--    Receive - Pointer for receive
--    Send    - Pointer for send
--
   procedure Transport_Get_Ptr2
             (  Session : Session_Type;
                Receive : out System.Address;
                Send    : out System.Address
             );
--
-- Transport_Set_Int -- Set transport value
--
--    Session - The session
--    Value   - The value to set
--
   procedure Transport_Set_Int
             (  Session : in out Session_Type;
                Value   : int
             );
--
-- Transport_Set_Int2 -- Set transport values
--
--    Session - The session
--    Receive - Value for receive
--    Send    - Value for send
--
   procedure Transport_Set_Int2
             (  Session : in out Session_Type;
                Receive : int;
                Send    : int
             );
--
-- Transport_Set_Ptr -- Set transport user pointer
--
--    Session - The session
--    Pointer - The pointer to set
--
   procedure Transport_Set_Ptr
             (  Session : in out Session_Type;
                Pointer : System.Address
             );
--
-- Transport_Set_Ptr2 -- Set transport user pointers
--
--    Session - The session
--    Receive - Pointer for receive
--    Send    - Pointer for send
--
   procedure Transport_Set_Ptr2
             (  Session : in out Session_Type;
                Receive : System.Address;
                Send    : System.Address
             );
--
-- Transport_Set_Error_No -- Set error code
--
--    Session - The session
--    Error   - The error code
--
-- Store err in the session-specific errno variable.  Useful values  for
-- err are EINTR, EAGAIN and EMSGSIZE,  other values are treated as real
-- errors in the push/pull function.
--
   procedure Transport_Set_Error_No
             (  Session : in out Session_Type;
                Error   : int
             );
--
-- Transport_Set_Pull_Function -- Enables transport read callback
--
--    Session   - The session
--    Transport - The user-provided object
--
-- This function will set Read callback with the following profile:
--
--    Transport - The user-provided object
--    Data      - The data to read
--    Pointer   - The first element to store read data
--
-- When nothing is read EAGAIN is returned to GNUTLS.
--
-- Exceptions :
--
--    End_Error - Connection closed by peer
--    others    - EIO is returned to GNUTLS
--
   generic
      type Transport_Type (<>) is limited private;
      with function Get_Session
                    (  Client : Transport_Type
                    )  return Session_Type_Ptr;
      with procedure Read
                     (  Transport : in out Transport_Type;
                        Data      : in out Stream_Element_Array;
                        Pointer   : in out Stream_Element_Offset
                     );
   package Transport_Set_Pull_Function is
      procedure Set
                (  Session   : in out Session_Type;
                   Transport : access Transport_Type
                );
   end Transport_Set_Pull_Function;
--
-- Transport_Set_Push_Function -- Enables transport write callback
--
--    Session   - The session
--    Transport - The user-provided object
--
-- This function will set Write callback with the following profile:
--
--    Transport - The user-provided object
--    Data      - The data to write
--    Pointer   - The first buffer element
--
-- Exceptions :
--
--    End_Error - Connection closed by peer
--    others    - EIO is returned to GNUTLS
--
   generic
      type Transport_Type (<>) is limited private;
      with function Get_Session
                    (  Client : Transport_Type
                    )  return Session_Type_Ptr;
      with procedure Write
                     (  Transport : in out Transport_Type;
                        Data      : Stream_Element_Array;
                        Pointer   : in out Stream_Element_Offset
                     );
   package Transport_Set_Push_Function is
      procedure Set
                (  Session   : in out Session_Type;
                   Transport : access Transport_Type
                );
   end Transport_Set_Push_Function;
--
-- URL_Is_Supported -- Set error code
--
--    URL - A PKCS 11 URL
--
-- Check whether url is supported.  Depending  on the  system  libraries
-- GNUTLS may support pkcs11 or tpmkey URLs.
--
-- Returns :
--
--    True if the given URL is supported
--
   function URL_Is_Supported (URL : String) return Boolean;

   E_Success                               : constant :=  0;
   E_Unknown_Compression_Algorithm         : constant := -3;
   E_Unknown_Cipher_Type                   : constant := -6;
   E_Large_Packet                          : constant := -7;
   E_Uunsupported_Version_Packet           : constant := -8;
   E_Unexpected_Packet_LENGTH              : constant := -9;
   E_Invalid_Session                       : constant := -10;
   E_Fatal_Alert_Received                  : constant := -12;
   E_Unexpected_Packet                     : constant := -15;
   E_Warning_Alert_Received                : constant := -16;
   E_Error_In_Finished_Packet              : constant := -18;
   E_Unexpected_Handshake_Packet           : constant := -19;
   E_Unknown_Cipher_Suite                  : constant := -21;
   E_Unwanted_Algorithm                    : constant := -22;
   E_MPI_Scan_Failed                       : constant := -23;
   E_Decryption_Failed                     : constant := -24;
   E_Memory_Error                          : constant := -25;
   E_Decompression_Failed                  : constant := -26;
   E_Compression_Failed                    : constant := -27;
   E_Again                                 : constant := -28;
   E_Expired                               : constant := -29;
   E_DB_Error                              : constant := -30;
   E_SRP_PWD_Error                         : constant := -31;
   E_Insufficient_Credentials              : constant := -32;
   E_Hash_Failed                           : constant := -33;
   E_BASE64_Decoding_Error                 : constant := -34;

   E_MPI_Print_Failed                      : constant := -35;
   E_Rehandshake                           : constant := -37;
   E_Got_Application_Data                  : constant := -38;
   E_Record_Limit_Reached                  : constant := -39;
   E_Encryption_Failed                     : constant := -40;

   E_PK_Encryption_Failed                  : constant := -44;
   E_PK_Decryption_Failed                  : constant := -45;
   E_PK_Sign_Failed                        : constant := -46;
   E_X509_Uunsupported_Critical_Extension  : constant := -47;
   E_Key_Usage_Violation                   : constant := -48;
   E_No_Certificate_Found                  : constant := -49;
   E_Invalid_Request                       : constant := -50;
   E_Short_Memory_Buffer                   : constant := -51;
   E_Interrupted                           : constant := -52;
   E_Push_Error                            : constant := -53;
   E_Pull_Error                            : constant := -54;
   E_Received_Illegal_Parameter            : constant := -55;
   E_Requested_Data_Not_Available          : constant := -56;
   E_PKCS1_Wrong_PAD                       : constant := -57;
   E_Received_Illegal_Extension            : constant := -58;
   E_Internal_Error                        : constant := -59;
   E_DH_Prime_Unacceptable                 : constant := -63;
   E_File_Error                            : constant := -64;
   E_Too_Many_Empty_Packets                : constant := -78;
   E_Unknown_PK_Algorithm                  : constant := -80;
   E_Too_Many_Handshake_Packets            : constant := -81;
--
-- Returned if you need to generate temporary RSA parameters. These  are
-- needed for export cipher suites.
--
   E_No_Temporary_RSA_Params               : constant := -84;

   E_No_Compression_Algorithms             : constant := -86;
   E_No_Cipher_Suites                      : constant := -87;

   E_OpenPGP_Getkey_Failed                 : constant := -88;
   E_PK_Sig_Verify_Failed                  : constant := -89;
   E_Illegal_SRP_Username                  : constant := -90;
   E_SRP_PWD_Parsing_Error                 : constant := -91;
   E_No_Temporary_DH_Params                : constant := -93;
--
-- For certificate and key stuff
--
   E_ASN1_Element_Not_Found                : constant := -67;
   E_ASN1_Identifier_Not_Found             : constant := -68;
   E_ASN1_DER_Error                        : constant := -69;
   E_ASN1_Value_Not_Found                  : constant := -70;
   E_ASN1_Generic_Error                    : constant := -71;
   E_ASN1_Value_Not_Valid                  : constant := -72;
   E_ASN1_Tag_Error                        : constant := -73;
   E_ASN1_Tag_Implicit                     : constant := -74;
   E_ASN1_Type_Any_Error                   : constant := -75;
   E_ASN1_Syntax_Error                     : constant := -76;
   E_ASN1_DER_Overflow                     : constant := -77;
   E_OpenPGP_UID_Revoked                   : constant := -79;
   E_Certificate_Error                     : constant := -43;
   E_Certificate_Key_Mismatch              : constant := -60;
   E_Uunsupported_Certificate_Type         : constant := -61;
   E_X509_Unknown_SAN                      : constant := -62;
   E_OpenPGP_Fingerprint_Uunsupported      : constant := -94;
   E_X509_Uunsupported_Attribute           : constant := -95;
   E_Unknown_Hash_Algorithm                : constant := -96;
   E_Unknown_PKCS_Content_Type             : constant := -97;
   E_Unknown_PKCS_BAG_Type                 : constant := -98;
   E_Invalid_Password                      : constant := -99;
   E_MAC_Verify_Failed                     : constant := -100;
   E_Constraint_Error                      : constant := -101;
   E_Warning_IA_IPHF_Received              : constant := -102;
   E_Warning_IA_FPHF_Received              : constant := -103;
   E_IA_Verify_Failed                      : constant := -104;
   E_Unknown_Algorithm                     : constant := -105;
   E_Uunsupported_Signature_Algorithm      : constant := -106;
   E_Safe_Renegotiation_Failed             : constant := -107;
   E_Unsafe_Renegotiation_Denied           : constant := -108;
   E_Unknown_SRP_Username                  : constant := -109;
   E_Premature_Termination                 : constant := -110;

   E_BASE64_Encoding_Error                 : constant := -201;
   E_Incompatible_GCrypt_Library           : constant := -202;
   E_Incompatible_Crypto_Library           : constant := -202;
   E_Incompatible_LIBTASN1_Library         : constant := -203;
   E_OpenPGP_Keyring_Error                 : constant := -204;
   E_X509_Uunsupported_OID                 : constant := -205;
   E_Random_Failed                         : constant := -206;
   E_BASE64_Unexpected_Header_Error        : constant := -207;
   E_OpenPGP_Subkey_Error                  : constant := -208;
   E_Crypto_Already_Registered             : constant := -209;
   E_Handshake_Too_Large                   : constant := -210;
   E_CryptoDEV_IOCTL_Error                 : constant := -211;
   E_CryptoDEV_Device_Error                : constant := -212;
   E_Channel_Binding_Not_Available         : constant := -213;
   E_Bad_Cookie                            : constant := -214;
   E_OpenPGP_Preferred_Key_Error           : constant := -215;
   E_Incompat_DSA_Key_With_TLS_Protocol    : constant := -216;

   E_Heartbeat_Pong_Received               : constant := -292;
   E_Heartbeat_Ping_Received               : constant := -293;
--
-- PKCS11 related
--
   E_PKCS11_Error                          : constant :=- 300;
   E_PKCS11_Load_Error                     : constant := -301;
   E_Parsing_Error                         : constant := -302;
   E_PKCS11_PIN_Error                      : constant := -303;

   E_PKCS11_Slot_Error                     : constant := -305;
   E_LOCKING_Error                         : constant := -306;
   E_PKCS11_Attribute_Error                : constant := -307;
   E_PKCS11_Device_Error                   : constant := -308;
   E_PKCS11_Data_Error                     : constant := -309;
   E_PKCS11_Uunsupported_Feature_Error     : constant := -310;
   E_PKCS11_Key_Error                      : constant := -311;
   E_PKCS11_PIN_Expired                    : constant := -312;
   E_PKCS11_PIN_Locked                     : constant := -313;
   E_PKCS11_Session_Error                  : constant := -314;
   E_PKCS11_Signature_Error                : constant := -315;
   E_PKCS11_Token_Error                    : constant := -316;
   E_PKCS11_User_Error                     : constant := -317;
   E_Crypto_Init_Failed                    : constant := -318;
   E_Timeout                               : constant := -319;
   E_User_Error                            : constant := -320;
   E_ECC_No_Supported_Curses               : constant := -321;
   E_ECC_Uunsupported_Curve                : constant := -322;
   E_PKCS11_Requested_Object_Not_Available : constant := -323;
   E_Certificate_List_Unsorted             : constant := -324;
   E_Illegal_Parameter                     : constant := -325;
   E_No_Priorities_Were_Set                : constant := -326;
   E_X509_Uunsupported_Extension           : constant := -327;
   E_Session_EOF                           : constant := -328;
   E_TPM_Error                             : constant := -329;
   E_TPM_Key_Password_Error                : constant := -330;
   E_TPM_SRK_Password_Error                : constant := -331;
   E_TPM_Session_Error                     : constant := -332;
   E_TPM_Key_Not_Found                     : constant := -333;
   E_TPM_Uninitialized                     : constant := -334;

   E_No_Certificate_Status                 : constant := -340;
   E_OCSP_Response_Error                   : constant := -341;
   E_Random_Device_Error                   : constant := -342;
   E_Auth_Error                            : constant := -343;
   E_No_Application_Protocol               : constant := -344;

   E_Unimplemented_Feature                 : constant := -1250;

   E_Application_Error_Max                 : constant := -65000;
   E_Application_Error_Min                 : constant := -65500;

   E_X509_Certificate_Error : constant := E_Certificate_Error;

private
   type X509_Crt        is new System.Address;
   type X509_CRL        is new System.Address;
   type X509_Privkey    is new System.Address;
   type OpenPGP_Crt     is new System.Address;
   type OpenPGP_Privkey is new System.Address;
   type PKSC11_Privkey  is new System.Address;
   type Transport       is new System.Address;

   type Session_Holder (Flags : Init_Flags) is
      new Ada.Finalization.Limited_Controlled with
   record
      Handle : aliased System.Address := System.Null_Address;
   end record;
   procedure Finalize   (Session : in out Session_Holder);
   procedure Initialize (Session : in out Session_Holder);

   type Session_Type (Flags : Init_Flags) is limited record
      Holder : Session_Holder (Flags);
   end record;

   type Abstract_Credentials is abstract
      new Ada.Finalization.Limited_Controlled with
   record
      Handle : aliased System.Address := System.Null_Address;
   end record;
   type Abstract_Anon_Credentials is abstract
      new Abstract_Credentials with null record;

   type Anon_Server_Credentials is
      new Abstract_Anon_Credentials with null record;
   procedure Credentials_Set
             (  Session     : in out Session_Type;
                Credentials : Anon_Server_Credentials
             );
   procedure Finalize (Credentials : in out Anon_Server_Credentials);
   procedure Initialize (Credentials : in out Anon_Server_Credentials);

   type Anon_Server_Credentials_Ref is
      new Anon_Server_Credentials with null record;
   procedure Finalize
             (  Credentials : in out Anon_Server_Credentials_Ref
             );
   procedure Initialize
             (  Credentials : in out Anon_Server_Credentials_Ref
             );

   type Anon_Client_Credentials is
      new Abstract_Anon_Credentials with null record;
   procedure Credentials_Set
             (  Session     : in out Session_Type;
                Credentials : Anon_Client_Credentials
             );
   procedure Finalize (Credentials : in out Anon_Client_Credentials);
   procedure Initialize (Credentials : in out Anon_Client_Credentials);

   type Anon_Client_Credentials_Ref is
      new Anon_Client_Credentials with null record;
   procedure Finalize
             (  Credentials : in out Anon_Client_Credentials_Ref
             );
   procedure Initialize
             (  Credentials : in out Anon_Client_Credentials_Ref
             );

   type Certificate_Credentials is
      new Abstract_Credentials with null record;
   procedure Credentials_Set
             (  Session     : in out Session_Type;
                Credentials : Certificate_Credentials
             );
   procedure Finalize (Credentials : in out Certificate_Credentials);
   procedure Initialize (Credentials : in out Certificate_Credentials);

   type Certificate_Credentials_Ref is
      new Certificate_Credentials with null record;
   procedure Finalize
             (  Credentials : in out Certificate_Credentials_Ref
             );
   procedure Initialize
             (  Credentials : in out Certificate_Credentials_Ref
             );

--     type Abstract_SRP_Credentials is abstract
--        new Abstract_Credentials with null record;
--     type SRP_Server_Credentials is
--        new Abstract_SRP_Credentials with null record;
--     procedure Finalize (Credentials : in out SRP_Server_Credentials);
--     procedure Initialize (Credentials : in out SRP_Server_Credentials);
--     procedure Credentials_Set
--               (  Session     : in out Session_Type;
--                  Credentials : SRP_Server_Credentials
--               );
--
--     type SRP_Server_Credentials_Ref is
--        new SRP_Server_Credentials with null record;
--     procedure Finalize
--               (  Credentials : in out SRP_Server_Credentials_Ref
--               );
--     procedure Initialize
--               (  Credentials : in out SRP_Server_Credentials_Ref
--               );
--
--     type SRP_Client_Credentials is
--        new Abstract_SRP_Credentials with null record;
--     procedure Finalize (Credentials : in out SRP_Client_Credentials);
--     procedure Initialize (Credentials : in out SRP_Client_Credentials);
--     procedure Credentials_Set
--               (  Session     : in out Session_Type;
--                  Credentials : SRP_Client_Credentials
--               );
--
--     type SRP_Client_Credentials_Ref is
--        new SRP_Client_Credentials with null record;
--     procedure Finalize
--               (  Credentials : in out SRP_Client_Credentials_Ref
--               );
--     procedure Initialize
--               (  Credentials : in out SRP_Client_Credentials_Ref
--               );

   type Abstract_Params is abstract new Ada.Finalization.Controlled with
   record
      Handle : aliased System.Address;
   end record;
   function Create
            (  Type_Of    : Params_Type;
               Parameters : Params_Data
            )  return Abstract_Params'Class;

   type DH_Params is new Abstract_Params with null record;
   procedure Adjust (Data : in out DH_Params);
   procedure Initialize (Data : in out DH_Params);
   procedure Finalize (Data : in out DH_Params);

   type DH_Params_Ref is new DH_Params with null record;
   procedure Adjust (Data : in out DH_Params_Ref);
   procedure Initialize (Data : in out DH_Params_Ref);
   procedure Finalize (Data : in out DH_Params_Ref);

   type ECDH_Params is new Abstract_Params with null record;
   procedure Initialize (Data : in out ECDH_Params);
   procedure Finalize (Data : in out ECDH_Params);

   type RSA_Params is new Abstract_Params with null record;
   procedure Initialize (Data : in out RSA_Params);
   procedure Finalize (Data : in out RSA_Params);

   type Priority is new Ada.Finalization.Limited_Controlled with record
      Handle : aliased System.Address := System.Null_Address;
   end record;
   procedure Finalize (Cache : in out Priority);
--
-- Check -- Error code and raise exception if necessary
--
--    Error - The error code
--
-- Exceptions :
--
--    TLS_Error - if Error is negative
--
   procedure Check (Error : int);

   type time_t is new long;

   function To_Time (Stamp : time_t) return Time;

   pragma Assert (Stream_Element'Size = 8);

   pragma Import (C, Cipher_Get_Key_Size, "gnutls_cipher_get_key_size");
   pragma Import (C, Error_To_Alert,      "gnutls_error_to_alert");
   pragma Import (C, Sign_Get_Hash_Algorithm,
                                      "gnutls_sign_get_hash_algorithm");
   pragma Import (C, Sign_Get_PK_Algorithm,
                                        "gnutls_sign_get_pk_algorithm");
   pragma Import (C, Est_Record_Overhead_Size,
                                     "gnutls_est_record_overhead_size");
   pragma Import (C, MAC_Get_Key_Size, "gnutls_mac_get_key_size");
   pragma Import (C, PK_To_Sign, "gnutls_pk_to_sign");

end GNUTLS;
