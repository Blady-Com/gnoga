--                                                                    --
--  package OpenSSL                 Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2019       --
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
--
--  Thin bindings to OpenSSL
--
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Streams;           use Ada.Streams;
with Interfaces;            use Interfaces;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

with Interfaces.C.Pointers;
with System;

package OpenSSL is
--
-- These are the classes of BIOs
--
   type BIO_TYPE is new int;
   BIO_TYPE_DESCRIPTOR  : constant BIO_TYPE := 16#0100#; -- socket, fd..
   BIO_TYPE_FILTER      : constant BIO_TYPE := 16#0200#;
   BIO_TYPE_SOURCE_SINK : constant BIO_TYPE := 16#0400#;
--
-- These are the 'types' of BIOs
--
   BIO_TYPE_NONE       : constant BIO_TYPE :=  0;
   BIO_TYPE_MEM        : constant BIO_TYPE :=  1 + BIO_TYPE_SOURCE_SINK;
   BIO_TYPE_FILE       : constant BIO_TYPE :=  2 + BIO_TYPE_SOURCE_SINK;
   BIO_TYPE_FD         : constant BIO_TYPE :=  4 + BIO_TYPE_SOURCE_SINK
                                                 + BIO_TYPE_DESCRIPTOR;
   BIO_TYPE_SOCKET     : constant BIO_TYPE :=  5 + BIO_TYPE_SOURCE_SINK
                                                 + BIO_TYPE_DESCRIPTOR;
   BIO_TYPE_NULL       : constant BIO_TYPE :=  6 + BIO_TYPE_SOURCE_SINK;
   BIO_TYPE_SSL        : constant BIO_TYPE :=  7 + BIO_TYPE_FILTER;
   BIO_TYPE_MD         : constant BIO_TYPE :=  8 + BIO_TYPE_FILTER;
   BIO_TYPE_BUFFER     : constant BIO_TYPE :=  9 + BIO_TYPE_FILTER;
   BIO_TYPE_CIPHER     : constant BIO_TYPE := 10 + BIO_TYPE_FILTER;
   BIO_TYPE_BASE64     : constant BIO_TYPE := 11 + BIO_TYPE_FILTER;
   BIO_TYPE_CONNECT    : constant BIO_TYPE := 12 + BIO_TYPE_SOURCE_SINK
                                                  + BIO_TYPE_DESCRIPTOR;
   BIO_TYPE_ACCEPT     : constant BIO_TYPE := 13 + BIO_TYPE_SOURCE_SINK
                                                 + BIO_TYPE_DESCRIPTOR;
   BIO_TYPE_NBIO_TEST  : constant BIO_TYPE := 16 + BIO_TYPE_FILTER;
   BIO_TYPE_NULL_FILTER :
                         constant BIO_TYPE := 17 + BIO_TYPE_FILTER;
   BIO_TYPE_BIO        : constant BIO_TYPE := 19 + BIO_TYPE_SOURCE_SINK;
   BIO_TYPE_LINEBUFFER : constant BIO_TYPE := 20 + BIO_TYPE_FILTER;
   BIO_TYPE_DGRAM      : constant BIO_TYPE := 21 + BIO_TYPE_SOURCE_SINK
                                                 + BIO_TYPE_DESCRIPTOR;
   BIO_TYPE_ASN1       : constant BIO_TYPE := 22 + BIO_TYPE_FILTER;
   BIO_TYPE_COMP       : constant BIO_TYPE := 23 + BIO_TYPE_FILTER;
--
-- These are used in the following macros and are passed to BIO_ctrl
--
   type BIO_CTRL is new int;                        -- Optional
   BIO_CTRL_RESET        : constant BIO_CTRL :=  1; -- Rewind/zero etc
   BIO_CTRL_EOF          : constant BIO_CTRL :=  2; -- Is at the eof
   BIO_CTRL_INFO         : constant BIO_CTRL :=  3; -- Extra tit-bits
                                                    -- Mandatory
   BIO_CTRL_SET          : constant BIO_CTRL :=  4; -- Set the 'IO' type
   BIO_CTRL_GET          : constant BIO_CTRL :=  5; -- Set the 'IO' type
                                                    -- Optional
   BIO_CTRL_PUSH         : constant BIO_CTRL :=  6; -- Signify change
   BIO_CTRL_POP          : constant BIO_CTRL :=  7; -- Signify change
                                                    -- Mandatory
   BIO_CTRL_GET_CLOSE    : constant BIO_CTRL :=  8; -- Close on free
   BIO_CTRL_SET_CLOSE    : constant BIO_CTRL :=  9; -- Set the Close
                                                    -- Optional
   BIO_CTRL_PENDING      : constant BIO_CTRL := 10; -- More data
   BIO_CTRL_FLUSH        : constant BIO_CTRL := 11; -- Flush buffered
                                                    -- Mandatory
   BIO_CTRL_DUP          : constant BIO_CTRL := 12; -- 'duped' BIO
                                                    -- Optional
   BIO_CTRL_WPENDING     : constant BIO_CTRL := 13; -- Bytes to write
   BIO_CTRL_SET_CALLBACK : constant BIO_CTRL := 14;
   BIO_CTRL_GET_CALLBACK : constant BIO_CTRL := 15;
                                                    -- Special
   BIO_CTRL_PEEK         : constant BIO_CTRL := 29;
   BIO_CTRL_SET_FILENAME : constant BIO_CTRL := 30;

   type BIO        is private;
   type BIO_METHOD is private;
   type EVP_MD     is private;
   type SSL        is private;
   type SSL_CTX    is private;
   type SSL_METHOD is private;

   No_BIO     : constant BIO;
   No_METHOD  : constant BIO_METHOD;
   No_SSL     : constant SSL;
   No_SSL_CTX : constant SSL_CTX;

   package Stream_Element_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Stream_Element_Offset,
             Element            => Stream_Element,
             Element_Array      => Stream_Element_Array,
             Default_Terminator => 0
          );

   type BIO_Info_CB_Ptr is access function
        (  b   : BIO;
           cmd : int;
           arg : int
        )  return int;
   pragma Convention (C, BIO_Info_CB_Ptr);
   type Write_Ptr is access function
        (  b       : BIO;
           data    : System.Address;
           dlen    : size_t;
           written : access size_t
        )  return int;
   pragma Convention (C, Write_Ptr);
   type Write_Old_Ptr is access function
        (  b    : BIO;
           data : System.Address;
           dlen : int
        )  return int;
   pragma Convention (C, Write_Old_Ptr);
   type Read_Ptr is access function
        (  b         : BIO;
           data      : System.Address;
           dlen      : size_t;
           readbytes : access size_t
        )  return int;
   pragma Convention (C, Read_Ptr);
   type Read_Old_Ptr is access function
        (  b    : BIO;
           data : System.Address;
           dlen : int
        )  return int;
   pragma Convention (C, Read_Old_Ptr);
   type Puts_Ptr is access function
        (  b    : BIO;
           data : Stream_Element_Pointers.Pointer
        )  return int;
   pragma Convention (C, Puts_Ptr);
   type Gets_Ptr is access function
        (  b    : BIO;
           data : Stream_Element_Pointers.Pointer;
           dlen : int
        )  return int;
   pragma Convention (C, Gets_Ptr);
   type Ctrl_Ptr is access function
        (  b   : BIO;
           cmd : BIO_CTRL;
           num : long;
           ptr : System.Address
        )  return long;
   pragma Convention (C, Ctrl_Ptr);
   type Create_Ptr is access function
        (  b : BIO
        )  return int;
   pragma Convention (C, Create_Ptr);
   type Destroy_Ptr is access function
        (  b : BIO
        )  return int;
   pragma Convention (C, Destroy_Ptr);
   type Callback_Ctlr_Ptr is access function
        (  b   : BIO;
           cmd : BIO_CTRL;
           num : long;
           ptr : System.Address
        )  return long;
   pragma Convention (C, Callback_Ctlr_Ptr);

   type BIO_Flags is new int;
   BIO_FLAGS_READ         : constant BIO_Flags := 16#001#;
   BIO_FLAGS_WRITE        : constant BIO_Flags := 16#002#;
   BIO_FLAGS_IO_SPECIAL   : constant BIO_Flags := 16#004#;
   BIO_FLAGS_SHOULD_RETRY : constant BIO_Flags := 16#008#;
   BIO_FLAGS_BASE64_NO_NL : constant BIO_Flags := 16#100#;
   BIO_FLAGS_MEM_RDONLY   : constant BIO_Flags := 16#200#;
   BIO_FLAGS_NONCLEAR_RST : constant BIO_Flags := 16#400#;
   BIO_FLAGS_RWS          : constant BIO_Flags := BIO_FLAGS_READ
                                                + BIO_FLAGS_WRITE
                                                + BIO_FLAGS_IO_SPECIAL;

   procedure BIO_clear_flags (b : BIO; flags : BIO_Flags);
   function BIO_free (b : BIO) return int;
   function BIO_get_data (b : BIO) return System.Address;
   function BIO_get_new_index return int;
   procedure BIO_meth_free (biom : BIO_METHOD);
   function BIO_meth_get_create (biom : BIO_METHOD) return Create_Ptr;
   function BIO_meth_get_ctrl
            (  biom : BIO_METHOD
            )  return Callback_Ctlr_Ptr;
   function BIO_meth_get_destroy (biom : BIO_METHOD) return Destroy_Ptr;
   function BIO_meth_get_gets (biom : BIO_METHOD) return Gets_Ptr;
   function BIO_meth_get_puts (biom : BIO_METHOD) return Puts_Ptr;
   function BIO_meth_get_read (biom : BIO_METHOD) return Read_Old_Ptr;
   function BIO_meth_get_read_ex (biom : BIO_METHOD) return Read_Ptr;
   function BIO_meth_get_write (biom : BIO_METHOD) return Write_Old_Ptr;
   function BIO_meth_get_write_ex (biom : BIO_METHOD) return Write_Ptr;
   function BIO_meth_new
            (  type_of : int;
               name    : char_array
            )  return BIO_METHOD;
   function BIO_meth_set_create
            (  biom : BIO_METHOD;
               cb   : Create_Ptr
            )  return int;
   function BIO_meth_set_ctrl
            (  biom : BIO_METHOD;
               cb   : Callback_Ctlr_Ptr
            )  return int;
   function BIO_meth_set_destroy
            (  biom : BIO_METHOD;
               cb   : Destroy_Ptr
            )  return int;
   function BIO_meth_set_gets
            (  biom : BIO_METHOD;
               cb   : Gets_Ptr
            )  return int;
   function BIO_meth_set_puts
            (  biom : BIO_METHOD;
               cb   : Puts_Ptr
            )  return int;
   function BIO_meth_set_read
            (  biom : BIO_METHOD;
               cb   : Read_Old_Ptr
            )  return int;
   function BIO_meth_set_read_ex
            (  biom : BIO_METHOD;
               cb   : Read_Ptr
            )  return int;
   function BIO_meth_set_write
            (  biom : BIO_METHOD;
               cb   : Write_Old_Ptr
            )  return int;
   function BIO_meth_set_write_ex
            (  biom : BIO_METHOD;
               cb   : Write_Ptr
            )  return int;
   function BIO_new (type_of : BIO_METHOD) return BIO;
   function BIO_pop (b : BIO) return BIO;
   function BIO_read
            (  b    : BIO;
               data : access Stream_Element;
               dlen : int
            )  return int;
   function BIO_read_ex
            (  b         : BIO;
               data      : System.Address;
               dlen      : int;
               readbytes : access size_t
            )  return int;
-- function BIO_reset(b : BIO) return int;
   function BIO_s_null return BIO_METHOD;
   procedure BIO_set_data
             (  a   : BIO;
                ptr : System.Address
             );
   procedure BIO_set_flags (b : BIO; flags : BIO_Flags);
   function BIO_s_mem return BIO_METHOD;
   function BIO_test_flags
            (  b     : BIO;
               flags : BIO_Flags
            )  return BIO_flags;
   function BIO_up_ref (b : BIO) return int;
   function BIO_write
            (  b    : BIO;
               data : access Stream_Element;
               dlen : int
            )  return int;
   function BIO_write_ex
            (  b       : BIO;
               data    : access Stream_Element;
               dlen    : int;
               written : access size_t
            )  return int;
--
-- Library
--
   type Err_Lib is new int;
   ERR_LIB_NONE            : constant Err_Lib := 1;
   ERR_LIB_SYS             : constant Err_Lib := 2;
   ERR_LIB_BN              : constant Err_Lib := 3;
   ERR_LIB_RSA             : constant Err_Lib := 4;
   ERR_LIB_DH              : constant Err_Lib := 5;
   ERR_LIB_EVP             : constant Err_Lib := 6;
   ERR_LIB_BUF             : constant Err_Lib := 7;
   ERR_LIB_OBJ             : constant Err_Lib := 8;
   ERR_LIB_PEM             : constant Err_Lib := 9;
   ERR_LIB_DSA             : constant Err_Lib := 10;
   ERR_LIB_X509            : constant Err_Lib := 11;
-- ERR_LIB_METH            : constant Err_Lib := 12;
   ERR_LIB_ASN1            : constant Err_Lib := 13;
   ERR_LIB_CONF            : constant Err_Lib := 14;
   ERR_LIB_CRYPTO          : constant Err_Lib := 15;
   ERR_LIB_EC              : constant Err_Lib := 16;
   ERR_LIB_SSL             : constant Err_Lib := 20;
-- ERR_LIB_SSL23           : constant Err_Lib := 21;
-- ERR_LIB_SSL2            : constant Err_Lib := 22;
-- ERR_LIB_SSL3            : constant Err_Lib := 23;
-- ERR_LIB_RSAREF          : constant Err_Lib := 30;
-- ERR_LIB_PROXY           : constant Err_Lib := 31;
   ERR_LIB_BIO             : constant Err_Lib := 32;
   ERR_LIB_PKCS7           : constant Err_Lib := 33;
   ERR_LIB_X509V3          : constant Err_Lib := 34;
   ERR_LIB_PKCS12          : constant Err_Lib := 35;
   ERR_LIB_RAND            : constant Err_Lib := 36;
   ERR_LIB_DSO             : constant Err_Lib := 37;
   ERR_LIB_ENGINE          : constant Err_Lib := 38;
   ERR_LIB_OCSP            : constant Err_Lib := 39;
   ERR_LIB_UI              : constant Err_Lib := 40;
   ERR_LIB_COMP            : constant Err_Lib := 41;
   ERR_LIB_ECDSA           : constant Err_Lib := 42;
   ERR_LIB_ECDH            : constant Err_Lib := 43;
   ERR_LIB_OSSL_STORE      : constant Err_Lib := 44;
   ERR_LIB_FIPS            : constant Err_Lib := 45;
   ERR_LIB_CMS             : constant Err_Lib := 46;
   ERR_LIB_TS              : constant Err_Lib := 47;
   ERR_LIB_HMAC            : constant Err_Lib := 48;
-- ERR_LIB_JPAKE           : constant Err_Lib := 49;
   ERR_LIB_CT              : constant Err_Lib := 50;
   ERR_LIB_ASYNC           : constant Err_Lib := 51;
   ERR_LIB_KDF             : constant Err_Lib := 52;
   ERR_LIB_SM2             : constant Err_Lib := 53;
   ERR_LIB_ESS             : constant Err_Lib := 54;

   ERR_LIB_USER            : constant Err_Lib := 128;

   procedure ERR_clear_error;
   procedure ERR_error_string_n
             (  e   : unsigned_long;
                buf : access char;
                len  : size_t
             );
   function ERR_get_error return unsigned_long;
   function ERR_peek_error return unsigned_long;
   procedure ERR_put_error
             (  lib    : Err_Lib   := ERR_LIB_USER;
                func   : int;
                reason : int;
                file   : chars_ptr := null_ptr;
                line   : int       := 0
             );
--
-- SSL_CIPHER
--
   type SSL_CIPHER is private;
   No_CIPHER : constant SSL_CIPHER;
   function SSL_CIPHER_get_version (c : SSL_CIPHER) return chars_ptr;
   function SSL_CIPHER_get_name (c : SSL_CIPHER) return chars_ptr;
   function SSL_CIPHER_standard_name (c : SSL_CIPHER) return chars_ptr;
--
-- OPENSSL_STACK
--
   type OPENSSL_STACK is private;
   No_STACK : constant OPENSSL_STACK;
   function OPENSSL_sk_num (st : OPENSSL_STACK) return int;
   function OPENSSL_sk_value
            (  st       : OPENSSL_STACK;
               Position : int
            )  return System.Address;
   function OPENSSL_sk_value
            (  st       : OPENSSL_STACK;
               Position : int
            )  return SSL_CIPHER;
--
-- Standard initialisation options
--
   OPENSSL_INIT_NO_LOAD_CRYPTO_STRINGS : constant := 16#00000001#;
   OPENSSL_INIT_LOAD_CRYPTO_STRINGS    : constant := 16#00000002#;
   OPENSSL_INIT_ADD_ALL_CIPHERS        : constant := 16#00000004#;
   OPENSSL_INIT_ADD_ALL_DIGESTS        : constant := 16#00000008#;
   OPENSSL_INIT_NO_ADD_ALL_CIPHERS     : constant := 16#00000010#;
   OPENSSL_INIT_NO_ADD_ALL_DIGESTS     : constant := 16#00000020#;
   OPENSSL_INIT_LOAD_CONFIG            : constant := 16#00000040#;
   OPENSSL_INIT_NO_LOAD_CONFIG         : constant := 16#00000080#;
   OPENSSL_INIT_ASYNC                  : constant := 16#00000100#;
   OPENSSL_INIT_ENGINE_RDRAND          : constant := 16#00000200#;
   OPENSSL_INIT_ENGINE_DYNAMIC         : constant := 16#00000400#;
   OPENSSL_INIT_ENGINE_OPENSSL         : constant := 16#00000800#;
   OPENSSL_INIT_ENGINE_CRYPTODEV       : constant := 16#00001000#;
   OPENSSL_INIT_ENGINE_CAPI            : constant := 16#00002000#;
   OPENSSL_INIT_ENGINE_PADLOCK         : constant := 16#00004000#;
   OPENSSL_INIT_ENGINE_AFALG           : constant := 16#00008000#;
-- OPENSSL_INIT_ZLIB                   : constant := 16#00010000#;
   OPENSSL_INIT_ATFORK                 : constant := 16#00020000#;
-- OPENSSL_INIT_BASE_ONLY              : constant := 16#00040000#;
   OPENSSL_INIT_NO_ATEXIT              : constant := 16#00080000#;
   OPENSSL_INIT_NO_ADD_ALL_MACS        : constant := 16#04000000#;
   OPENSSL_INIT_ADD_ALL_MACS           : constant := 16#08000000#;

   OPENSSL_INIT_NO_LOAD_SSL_STRINGS    : constant := 16#00100000#;
   OPENSSL_INIT_LOAD_SSL_STRINGS       : constant := 16#00200000#;
   OPENSSL_INIT_SSL_DEFAULT            : constant :=
      OPENSSL_INIT_LOAD_SSL_STRINGS + OPENSSL_INIT_LOAD_CRYPTO_STRINGS;

   SSL_ERROR_NONE                 : constant := 0;
   SSL_ERROR_SSL                  : constant := 1;
   SSL_ERROR_WANT_READ            : constant := 2;
   SSL_ERROR_WANT_WRITE           : constant := 3;
   SSL_ERROR_WANT_X509_LOOKUP     : constant := 4;
   SSL_ERROR_SYSCALL              : constant := 5;
                               -- look at error stack/return value/errno
   SSL_ERROR_ZERO_RETURN          : constant := 6;
   SSL_ERROR_WANT_CONNECT         : constant := 7;
   SSL_ERROR_WANT_ACCEPT          : constant := 8;
   SSL_ERROR_WANT_ASYNC           : constant := 9;
   SSL_ERROR_WANT_ASYNC_JOB       : constant := 10;
   SSL_ERROR_WANT_CLIENT_HELLO_CB : constant := 11;

   type OSSL_HANDSHAKE_STATE is
        (  TLS_ST_BEFORE,
           TLS_ST_OK,
           DTLS_ST_CR_HELLO_VERIFY_REQUEST,
           TLS_ST_CR_SRVR_HELLO,
           TLS_ST_CR_CERT,
           TLS_ST_CR_CERT_STATUS,
           TLS_ST_CR_KEY_EXCH,
           TLS_ST_CR_CERT_REQ,
           TLS_ST_CR_SRVR_DONE,
           TLS_ST_CR_SESSION_TICKET,
           TLS_ST_CR_CHANGE,
           TLS_ST_CR_FINISHED,
           TLS_ST_CW_CLNT_HELLO,
           TLS_ST_CW_CERT,
           TLS_ST_CW_KEY_EXCH,
           TLS_ST_CW_CERT_VRFY,
           TLS_ST_CW_CHANGE,
           TLS_ST_CW_NEXT_PROTO,
           TLS_ST_CW_FINISHED,
           TLS_ST_SW_HELLO_REQ,
           TLS_ST_SR_CLNT_HELLO,
           DTLS_ST_SW_HELLO_VERIFY_REQUEST,
           TLS_ST_SW_SRVR_HELLO,
           TLS_ST_SW_CERT,
           TLS_ST_SW_KEY_EXCH,
           TLS_ST_SW_CERT_REQ,
           TLS_ST_SW_SRVR_DONE,
           TLS_ST_SR_CERT,
           TLS_ST_SR_KEY_EXCH,
           TLS_ST_SR_CERT_VRFY,
           TLS_ST_SR_NEXT_PROTO,
           TLS_ST_SR_CHANGE,
           TLS_ST_SR_FINISHED,
           TLS_ST_SW_SESSION_TICKET,
           TLS_ST_SW_CERT_STATUS,
           TLS_ST_SW_CHANGE,
           TLS_ST_SW_FINISHED,
           TLS_ST_SW_ENCRYPTED_EXTENSIONS,
           TLS_ST_CR_ENCRYPTED_EXTENSIONS,
           TLS_ST_CR_CERT_VRFY,
           TLS_ST_SW_CERT_VRFY,
           TLS_ST_CR_HELLO_REQ,
           TLS_ST_SW_KEY_UPDATE,
           TLS_ST_CW_KEY_UPDATE,
           TLS_ST_SR_KEY_UPDATE,
           TLS_ST_CR_KEY_UPDATE,
           TLS_ST_EARLY_DATA,
           TLS_ST_PENDING_EARLY_DATA_END,
           TLS_ST_CW_END_OF_EARLY_DATA,
           TLS_ST_SR_END_OF_EARLY_DATA
        );
   pragma Convention (C, OSSL_HANDSHAKE_STATE);

   type ossl_init_settings_st is record
      filename : chars_ptr;
      appname  : chars_ptr;
      flags    : unsigned_long;
   end record;
   pragma Convention (C, ossl_init_settings_st);
   type ossl_init_settings_st_Ptr is access all ossl_init_settings_st;
   pragma Convention (C, ossl_init_settings_st_Ptr);

   function OPENSSL_init_ssl
            (  opts     : Unsigned_64 := 0;
               settings : ossl_init_settings_st_Ptr := null
            )  return int;

   type SSL_MODE_TYPE is new long;
   SSL_MODE_ENABLE_PARTIAL_WRITE : constant SSL_MODE_TYPE := 16#000001#;
--
-- Make  it  possible  to retry SSL_write() with changed buffer location
-- (buffer contents must stay the same!); this is  not  the  default  to
-- avoid  the  misconception  that non-blocking SSL_write() behaves like
-- non-blocking write():
--
   SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER : constant SSL_MODE_TYPE :=
                                                             16#000002#;
--
-- Never bother the application with retries if the transport is blocking:
--
   SSL_MODE_AUTO_RETRY           : constant SSL_MODE_TYPE := 16#000004#;
--
-- Don't attempt to automatically build certificate chain
--
   SSL_MODE_NO_AUTO_CHAIN        : constant SSL_MODE_TYPE := 16#000008#;
--
-- Save RAM  by releasing read  and  write  buffers when  they're empty.
-- (SSL3 and TLS only.) Released buffers are freed.
--
   SSL_MODE_RELEASE_BUFFERS      : constant SSL_MODE_TYPE := 16#000010#;
--
-- Send the current time in the Random fields  of  the  ClientHello  and
-- ServerHello   records    for    compatibility    with    hypothetical
-- implementations that require it.
--
   SSL_MODE_SEND_CLIENTHELLO_TIME : constant SSL_MODE_TYPE := 16#00020#;
   SSL_MODE_SEND_SERVERHELLO_TIME : constant SSL_MODE_TYPE := 16#00040#;
--
-- Send  TLS_FALLBACK_SCSV  in  the  ClientHello.  To  be  set  only  by
-- applications  that  reconnect with a downgraded protocol version; see
-- draft-ietf-tls-downgrade-scsv-00 for details. DO NOT ENABLE  THIS  if
-- your  application  attempts  a  normal  handshake.  Only  use this in
-- explicit   fallback    retries,    following    the    guidance    in
-- draft-ietf-tls-downgrade-scsv-00.
--
   SSL_MODE_SEND_FALLBACK_SCSV   : constant SSL_MODE_TYPE := 16#000080#;
--
-- Support Asynchronous operation
--
   SSL_MODE_ASYNC                : constant SSL_MODE_TYPE := 16#000100#;
--
-- When using DTLS/SCTP, include the terminating zero in the label  used
-- for  computing  the  endpoint-pair  shared   secret.   Required   for
-- interoperability  with  implementations  having  this  bug like these
-- older version of OpenSSL:
--
-- - OpenSSL 1.0.0 series
-- - OpenSSL 1.0.1 series
-- - OpenSSL 1.0.2 series
-- - OpenSSL 1.1.0 series
-- - OpenSSL 1.1.1 and 1.1.1a
--
   SSL_MODE_DTLS_SCTP_LABEL_LENGTH_BUG : constant SSL_MODE_TYPE :=
                                                              16#00400#;
--
-- Options
--
   type SSL_OP is new unsigned_long;
--
-- Allow initial connection to servers that don't support RI
--
   SSL_OP_LEGACY_SERVER_CONNECT  : constant SSL_OP := 16#00000004#;
   SSL_OP_TLSEXT_PADDING         : constant SSL_OP := 16#00000010#;
   SSL_OP_SAFARI_ECDHE_ECDSA_BUG : constant SSL_OP := 16#00000040#;
--
-- In TLSv1.3 allow a non-(ec)dhe based kex_mode
--
   SSL_OP_ALLOW_NO_DHE_KEX       : constant SSL_OP := 16#00000400#;
--
-- Disable SSL 3.0/TLS 1.0 CBC  vulnerability  workaround that was added
-- in OpenSSL 0.9.6d.  Usually  (depending  on the application protocol)
-- the  workaround  is not needed.  Unfortunately  some  broken  SSL/TLS
-- implementations cannot handle it at all,  which is why  we include it
-- in SSL_OP_ALL. Added in 0.9.6e
--
   SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS :
                                   constant SSL_OP := 16#00000800#;
--
-- DTLS options
--
   SSL_OP_NO_QUERY_MTU           : constant SSL_OP := 16#00001000#;
--
-- Turn on Cookie Exchange (on relevant for servers)
--
   SSL_OP_COOKIE_EXCHANGE        : constant SSL_OP := 16#00002000#;
--
-- Don't use RFC4507 ticket extension
--
   SSL_OP_NO_TICKET              : constant SSL_OP := 16#00004000#;
--
-- Use Cisco's "speshul" version of DTLS_BAD_VER (only with deprecated
-- DTLSv1_client_method())
--
   SSL_OP_CISCO_ANYCONNECT       : constant SSL_OP := 16#00008000#;
--
-- As server, disallow session resumption on renegotiation
--
   SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION :
                                   constant SSL_OP := 16#00010000#;
-- Don't use compression even if supported
--
   SSL_OP_NO_COMPRESSION         : constant SSL_OP := 16#00020000#;
--
-- Permit unsafe legacy renegotiation
--
   SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION :
                                   constant SSL_OP := 16#00040000#;
-- Disable encrypt-then-mac
--
   SSL_OP_NO_ENCRYPT_THEN_MAC    : constant SSL_OP := 16#00080000#;
--
-- Enable TLSv1.3  Compatibility mode.  This is on by default.  A future
-- version of OpenSSL may have this disabled by default.
--
   SSL_OP_ENABLE_MIDDLEBOX_COMPAT :
                                   constant SSL_OP := 16#00100000#;
--
-- Prioritize Chacha20Poly1305 when client does. Modifies
-- SSL_OP_CIPHER_SERVER_PREFERENCE
--
   SSL_OP_PRIORITIZE_CHACHA      : constant SSL_OP := 16#00200000#;
--
-- Set on  servers  to choose  the  cipher  according  to  the  server's
-- preferences
--
   SSL_OP_CIPHER_SERVER_PREFERENCE :
                                   constant SSL_OP := 16#00400000#;
--
-- If set,  a server  will allow  a client  to issue  a SSLv3.0  version
-- number as latest version supported in the premaster secret, even when
-- TLSv1.0 (version 3.1)  was announced  in  the client hello.  Normally
-- this is forbidden to prevent version rollback attacks.
--
   SSL_OP_TLS_ROLLBACK_BUG       : constant SSL_OP := 16#00800000#;
--
-- Switches off automatic TLSv1.3 anti-replay protection for early data.
-- This is a server-side option only (no effect on the client).
--
   SSL_OP_NO_ANTI_REPLAY         : constant SSL_OP := 16#01000000#;

   SSL_OP_NO_SSLv3               : constant SSL_OP := 16#02000000#;
   SSL_OP_NO_TLSv1               : constant SSL_OP := 16#04000000#;
   SSL_OP_NO_TLSv1_2             : constant SSL_OP := 16#08000000#;
   SSL_OP_NO_TLSv1_1             : constant SSL_OP := 16#10000000#;
   SSL_OP_NO_TLSv1_3             : constant SSL_OP := 16#20000000#;

   SSL_OP_NO_DTLSv1              : constant SSL_OP := 16#04000000#;
   SSL_OP_NO_DTLSv1_2            : constant SSL_OP := 16#08000000#;
--
-- Disallow all renegotiation
--
   SSL_OP_NO_RENEGOTIATION       : constant SSL_OP := 16#40000000#;
--
-- Make  server  add  server-hello   extension  from  early  version  of
-- cryptopro draft,  when GOST  ciphersuite is negotiated.  Required for
-- interoperability with CryptoPro CSP 3.x
--
   SSL_OP_CRYPTOPRO_TLSEXT_BUG   : constant SSL_OP := 16#80000000#;
--
-- SSL_OP_ALL: various bug workarounds that should be rather harmless
--
   SSL_OP_ALL           : constant SSL_OP :=
                                   SSL_OP_CRYPTOPRO_TLSEXT_BUG
                                or SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS
                                or SSL_OP_LEGACY_SERVER_CONNECT
                                or SSL_OP_TLSEXT_PADDING
                                or SSL_OP_SAFARI_ECDHE_ECDSA_BUG;
   SSL_OP_NO_SSL_MASK  : constant SSL_OP :=
                                   SSL_OP_NO_SSLv3
                                or SSL_OP_NO_TLSv1
                                or SSL_OP_NO_TLSv1_1
                                or SSL_OP_NO_TLSv1_2
                                or SSL_OP_NO_TLSv1_3;
   SSL_OP_NO_DTLS_MASK : constant SSL_OP :=
                                   SSL_OP_NO_DTLSv1
                                or SSL_OP_NO_DTLSv1_2;

   type SSL_WANT_TYPE is new int;
   SSL_NOTHING         : constant SSL_WANT_TYPE := 1;
   SSL_WRITING         : constant SSL_WANT_TYPE := 2;
   SSL_READING         : constant SSL_WANT_TYPE := 3;
   SSL_X509_LOOKUP     : constant SSL_WANT_TYPE := 4;
   SSL_ASYNC_PAUSED    : constant SSL_WANT_TYPE := 5;
   SSL_ASYNC_NO_JOBS   : constant SSL_WANT_TYPE := 6;
   SSL_CLIENT_HELLO_CB : constant SSL_WANT_TYPE := 7;
--
-- SSL_CTRL_TYPE -- Control commands
--
   type SSL_CTRL_TYPE is new int;
   SSL_CTRL_SET_TMP_DH                  : constant SSL_CTRL_TYPE := 3;
   SSL_CTRL_SET_TMP_ECDH                : constant SSL_CTRL_TYPE := 4;
   SSL_CTRL_SET_TMP_DH_CB               : constant SSL_CTRL_TYPE := 6;
   SSL_CTRL_GET_CLIENT_CERT_REQUEST     : constant SSL_CTRL_TYPE := 9;
   SSL_CTRL_GET_NUM_RENEGOTIATIONS      : constant SSL_CTRL_TYPE := 10;
   SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS    : constant SSL_CTRL_TYPE := 11;
   SSL_CTRL_GET_TOTAL_RENEGOTIATIONS    : constant SSL_CTRL_TYPE := 12;
   SSL_CTRL_GET_FLAGS                   : constant SSL_CTRL_TYPE := 13;
   SSL_CTRL_EXTRA_CHAIN_CERT            : constant SSL_CTRL_TYPE := 14;
   SSL_CTRL_SET_MSG_CALLBACK            : constant SSL_CTRL_TYPE := 15;
   SSL_CTRL_SET_MSG_CALLBACK_ARG        : constant SSL_CTRL_TYPE := 16;
       -- Only applies to datagram connections
   SSL_CTRL_SET_MTU                     : constant SSL_CTRL_TYPE := 17;
       -- Stats
   SSL_CTRL_SESS_NUMBER                 : constant SSL_CTRL_TYPE := 20;
   SSL_CTRL_SESS_CONNECT                : constant SSL_CTRL_TYPE := 21;
   SSL_CTRL_SESS_CONNECT_GOOD           : constant SSL_CTRL_TYPE := 22;
   SSL_CTRL_SESS_CONNECT_RENEGOTIATE    : constant SSL_CTRL_TYPE := 23;
   SSL_CTRL_SESS_ACCEPT                 : constant SSL_CTRL_TYPE := 24;
   SSL_CTRL_SESS_ACCEPT_GOOD            : constant SSL_CTRL_TYPE := 25;
   SSL_CTRL_SESS_ACCEPT_RENEGOTIATE     : constant SSL_CTRL_TYPE := 26;
   SSL_CTRL_SESS_HIT                    : constant SSL_CTRL_TYPE := 27;
   SSL_CTRL_SESS_CB_HIT                 : constant SSL_CTRL_TYPE := 28;
   SSL_CTRL_SESS_MISSES                 : constant SSL_CTRL_TYPE := 29;
   SSL_CTRL_SESS_TIMEOUTS               : constant SSL_CTRL_TYPE := 30;
   SSL_CTRL_SESS_CACHE_FULL             : constant SSL_CTRL_TYPE := 31;
   SSL_CTRL_MODE                        : constant SSL_CTRL_TYPE := 33;
   SSL_CTRL_GET_READ_AHEAD              : constant SSL_CTRL_TYPE := 40;
   SSL_CTRL_SET_READ_AHEAD              : constant SSL_CTRL_TYPE := 41;
   SSL_CTRL_SET_SESS_CACHE_SIZE         : constant SSL_CTRL_TYPE := 42;
   SSL_CTRL_GET_SESS_CACHE_SIZE         : constant SSL_CTRL_TYPE := 43;
   SSL_CTRL_SET_SESS_CACHE_MODE         : constant SSL_CTRL_TYPE := 44;
   SSL_CTRL_GET_SESS_CACHE_MODE         : constant SSL_CTRL_TYPE := 45;
   SSL_CTRL_GET_MAX_CERT_LIST           : constant SSL_CTRL_TYPE := 50;
   SSL_CTRL_SET_MAX_CERT_LIST           : constant SSL_CTRL_TYPE := 51;
   SSL_CTRL_SET_MAX_SEND_FRAGMENT       : constant SSL_CTRL_TYPE := 52;
       -- see tls1.h for macros based on these
   SSL_CTRL_SET_TLSEXT_SERVERNAME_CB    : constant SSL_CTRL_TYPE := 53;
   SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG   : constant SSL_CTRL_TYPE := 54;
   SSL_CTRL_SET_TLSEXT_HOSTNAME         : constant SSL_CTRL_TYPE := 55;
   SSL_CTRL_SET_TLSEXT_DEBUG_CB         : constant SSL_CTRL_TYPE := 56;
   SSL_CTRL_SET_TLSEXT_DEBUG_ARG        : constant SSL_CTRL_TYPE := 57;
   SSL_CTRL_GET_TLSEXT_TICKET_KEYS      : constant SSL_CTRL_TYPE := 58;
   SSL_CTRL_SET_TLSEXT_TICKET_KEYS      : constant SSL_CTRL_TYPE := 59;
-- SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT    :
--                                        constant SSL_CTRL_TYPE := 60;
-- SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB :
--                                        constant SSL_CTRL_TYPE := 61;
-- SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB_ARG :
--                                        constant SSL_CTRL_TYPE := 62;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB    : constant SSL_CTRL_TYPE := 63;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG :
                                          constant SSL_CTRL_TYPE := 64;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE  : constant SSL_CTRL_TYPE := 65;
   SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS  : constant SSL_CTRL_TYPE := 66;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS  : constant SSL_CTRL_TYPE := 67;
   SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS   : constant SSL_CTRL_TYPE := 68;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS   : constant SSL_CTRL_TYPE := 69;
   SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP :
                                          constant SSL_CTRL_TYPE := 70;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP :
                                          constant SSL_CTRL_TYPE := 71;
   SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB    : constant SSL_CTRL_TYPE := 72;
   SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB : constant SSL_CTRL_TYPE := 75;
   SSL_CTRL_SET_SRP_VERIFY_PARAM_CB     : constant SSL_CTRL_TYPE := 76;
   SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB  : constant SSL_CTRL_TYPE := 77;
   SSL_CTRL_SET_SRP_ARG                 : constant SSL_CTRL_TYPE := 78;
   SSL_CTRL_SET_TLS_EXT_SRP_USERNAME    : constant SSL_CTRL_TYPE := 79;
   SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH    : constant SSL_CTRL_TYPE := 80;
   SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD    : constant SSL_CTRL_TYPE := 81;
   SSL_CTRL_DTLS_EXT_SEND_HEARTBEAT     : constant SSL_CTRL_TYPE := 85;
   SSL_CTRL_GET_DTLS_EXT_HEARTBEAT_PENDING :
                                          constant SSL_CTRL_TYPE := 86;
   SSL_CTRL_SET_DTLS_EXT_HEARTBEAT_NO_REQUESTS :
                                          constant SSL_CTRL_TYPE := 87;
   DTLS_CTRL_GET_TIMEOUT                : constant SSL_CTRL_TYPE := 73;
   DTLS_CTRL_HANDLE_TIMEOUT             : constant SSL_CTRL_TYPE := 74;
   SSL_CTRL_GET_RI_SUPPORT              : constant SSL_CTRL_TYPE := 76;
   SSL_CTRL_CLEAR_MODE                  : constant SSL_CTRL_TYPE := 78;
   SSL_CTRL_SET_NOT_RESUMABLE_SESS_CB   : constant SSL_CTRL_TYPE := 79;
   SSL_CTRL_GET_EXTRA_CHAIN_CERTS       : constant SSL_CTRL_TYPE := 82;
   SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS     : constant SSL_CTRL_TYPE := 83;
   SSL_CTRL_CHAIN                       : constant SSL_CTRL_TYPE := 88;
   SSL_CTRL_CHAIN_CERT                  : constant SSL_CTRL_TYPE := 89;
   SSL_CTRL_GET_GROUPS                  : constant SSL_CTRL_TYPE := 90;
   SSL_CTRL_SET_GROUPS                  : constant SSL_CTRL_TYPE := 91;
   SSL_CTRL_SET_GROUPS_LIST             : constant SSL_CTRL_TYPE := 92;
   SSL_CTRL_GET_SHARED_GROUP            : constant SSL_CTRL_TYPE := 93;
   SSL_CTRL_SET_SIGALGS                 : constant SSL_CTRL_TYPE := 97;
   SSL_CTRL_SET_SIGALGS_LIST            : constant SSL_CTRL_TYPE := 98;
   SSL_CTRL_CERT_FLAGS                  : constant SSL_CTRL_TYPE := 99;
   SSL_CTRL_CLEAR_CERT_FLAGS            : constant SSL_CTRL_TYPE := 100;
   SSL_CTRL_SET_CLIENT_SIGALGS          : constant SSL_CTRL_TYPE := 101;
   SSL_CTRL_SET_CLIENT_SIGALGS_LIST     : constant SSL_CTRL_TYPE := 102;
   SSL_CTRL_GET_CLIENT_CERT_TYPES       : constant SSL_CTRL_TYPE := 103;
   SSL_CTRL_SET_CLIENT_CERT_TYPES       : constant SSL_CTRL_TYPE := 104;
   SSL_CTRL_BUILD_CERT_CHAIN            : constant SSL_CTRL_TYPE := 105;
   SSL_CTRL_SET_VERIFY_CERT_STORE       : constant SSL_CTRL_TYPE := 106;
   SSL_CTRL_SET_CHAIN_CERT_STORE        : constant SSL_CTRL_TYPE := 107;
   SSL_CTRL_GET_PEER_SIGNATURE_NID      : constant SSL_CTRL_TYPE := 108;
   SSL_CTRL_GET_PEER_TMP_KEY            : constant SSL_CTRL_TYPE := 109;
   SSL_CTRL_GET_RAW_CIPHERLIST          : constant SSL_CTRL_TYPE := 110;
   SSL_CTRL_GET_EC_POINT_FORMATS        : constant SSL_CTRL_TYPE := 111;
   SSL_CTRL_GET_CHAIN_CERTS             : constant SSL_CTRL_TYPE := 115;
   SSL_CTRL_SELECT_CURRENT_CERT         : constant SSL_CTRL_TYPE := 116;
   SSL_CTRL_SET_CURRENT_CERT            : constant SSL_CTRL_TYPE := 117;
   SSL_CTRL_SET_DH_AUTO                 : constant SSL_CTRL_TYPE := 118;
   DTLS_CTRL_SET_LINK_MTU               : constant SSL_CTRL_TYPE := 120;
   DTLS_CTRL_GET_LINK_MIN_MTU           : constant SSL_CTRL_TYPE := 121;
   SSL_CTRL_GET_EXTMS_SUPPORT           : constant SSL_CTRL_TYPE := 122;
   SSL_CTRL_SET_MIN_PROTO_VERSION       : constant SSL_CTRL_TYPE := 123;
   SSL_CTRL_SET_MAX_PROTO_VERSION       : constant SSL_CTRL_TYPE := 124;
   SSL_CTRL_SET_SPLIT_SEND_FRAGMENT     : constant SSL_CTRL_TYPE := 125;
   SSL_CTRL_SET_MAX_PIPELINES           : constant SSL_CTRL_TYPE := 126;
   SSL_CTRL_GET_TLSEXT_STATUS_REQ_TYPE  : constant SSL_CTRL_TYPE := 127;
   SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB    : constant SSL_CTRL_TYPE := 128;
   SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB_ARG
                                        : constant SSL_CTRL_TYPE := 129;
   SSL_CTRL_GET_MIN_PROTO_VERSION       : constant SSL_CTRL_TYPE := 130;
   SSL_CTRL_GET_MAX_PROTO_VERSION       : constant SSL_CTRL_TYPE := 131;
   SSL_CTRL_GET_SIGNATURE_NID           : constant SSL_CTRL_TYPE := 132;
   SSL_CTRL_GET_TMP_KEY                 : constant SSL_CTRL_TYPE := 133;

   type SSL_FILETYPE is new int;
   SSL_FILETYPE_ASN1 : constant SSL_FILETYPE := 2;
   SSL_FILETYPE_PEM  : constant SSL_FILETYPE := 1;

   type SSL_Version_No is new int;
   SSL3_VERSION    : constant SSL_Version_No := 16#00300#;
   SSL2_VERSION    : constant SSL_Version_No := 16#00002#;
   TLS1_VERSION    : constant SSL_Version_No := 16#00301#;
   TLS1_1_VERSION  : constant SSL_Version_No := 16#00302#;
   TLS1_2_VERSION  : constant SSL_Version_No := 16#00303#;
   TLS1_3_VERSION  : constant SSL_Version_No := 16#00304#;
   DTLS1_VERSION   : constant SSL_Version_No := 16#0FEFF#;
   DTLS1_2_VERSION : constant SSL_Version_No := 16#0FEFD#;
   TLS_ANY_VERSION : constant SSL_Version_No := 16#10000#;

   SSL_SENT_SHUTDOWN     : constant := 1;
   SSL_RECEIVED_SHUTDOWN : constant := 2;

   function SSL_CTX_check_private_key (ctx : SSL_CTX) return int;
   function SSL_CTX_clear_mode
            (  ctx  : SSL_CTX;
               mode : SSL_MODE_TYPE
            )  return SSL_MODE_TYPE;
   function SSL_CTX_clear_options
            (  ctx : SSL_CTX;
               op  : SSL_OP
            )  return SSL_OP;
   function SSL_CTX_ctrl
            (  ctx  : SSL_CTX;
               cmd  : SSL_CTRL_TYPE;
               larg : long;
               parg : System.Address := System.Null_Address
            )  return long;
   procedure SSL_CTX_free (ctx : SSL_CTX);
   function SSL_CTX_get_ciphers (ctx : SSL_CTX) return OPENSSL_STACK;
   function SSL_CTX_get_mode (ctx : SSL_CTX) return SSL_MODE_TYPE;
   function SSL_CTX_get_options (ctx : SSL_CTX) return SSL_OP;
   function SSL_CTX_get_timeout (ctx : SSL_CTX) return long;
   function SSL_CTX_load_verify_locations
            (  ctx    : SSL_CTX;
               CAfile : chars_ptr;
               CApath : chars_ptr
            )  return int;
   function SSL_CTX_new (meth : SSL_METHOD) return SSL_CTX;
   function SSL_CTX_set_cipher_list
            (  ctx : SSL_CTX;
               str : char_array
            )  return int;
   function SSL_CTX_set_ciphersuites
            (  ctx : SSL_CTX;
               str : char_array
            )  return int;
   function SSL_CTX_set_default_verify_dir (ctx : SSL_CTX) return int;
   function SSL_CTX_set_default_verify_file (ctx : SSL_CTX) return int;
   function SSL_CTX_set_default_verify_paths (ctx : SSL_CTX) return int;
   function SSL_CTX_set_mode
            (  ctx  : SSL_CTX;
               mode : SSL_MODE_TYPE
            )  return SSL_MODE_TYPE;
   function SSL_CTX_set_options
            (  ctx : SSL_CTX;
               op  : SSL_OP
            )  return SSL_OP;
   function SSL_CTX_set_timeout (ctx : SSL_CTX; t : long) return long;
   function SSL_CTX_up_ref (ctx : SSL_CTX) return int;
   function SSL_CTX_use_certificate_ASN1
            (  ctx : SSL_CTX;
               len : int;
               d   : System.Address
            )  return int;
   function SSL_CTX_use_certificate_chain_file
            (  ctx  : SSL_CTX;
               file : char_array
            )  return int;
   function SSL_CTX_use_certificate_file
            (  ctx    : SSL_CTX;
               file   : char_array;
               format : SSL_FILETYPE
            )  return int;
   function SSL_CTX_use_PrivateKey_ASN1
            (  ctx : SSL_CTX;
               len : int;
               d   : System.Address
            )  return int;
   function SSL_CTX_use_PrivateKey_file
            (  ctx    : SSL_CTX;
               file   : char_array;
               format : SSL_FILETYPE
            )  return int;
   function SSL_CTX_use_RSAPrivateKey_ASN1
            (  ctx : SSL_CTX;
               len : int;
               d   : System.Address
            )  return int;
   function SSL_CTX_use_RSAPrivateKey_file
            (  ctx    : SSL_CTX;
               file   : char_array;
               format : SSL_FILETYPE
            )  return int;

   function SSL_accept (s : SSL) return int;
   function SSL_check_private_key (s : SSL) return int;
   procedure SSL_certs_clear (s : SSL);
   function SSL_clear (s : SSL) return int;
   function SSL_clear_mode
            (  s    : SSL;
               mode : SSL_MODE_TYPE
            )  return SSL_MODE_TYPE;
   function SSL_clear_options (s : SSL; op : SSL_OP) return SSL_OP;
   function SSL_connect (s : SSL) return int;
   function SSL_ctrl
            (  s    : SSL;
               cmd  : SSL_CTRL_TYPE;
               larg : long;
               parg : System.Address := System.Null_Address
            )  return long;
   procedure SSL_free (s : SSL);
   function SSL_get_ciphers (s : SSL) return OPENSSL_STACK;
   function SSL_get_client_ciphers (s : SSL)  return OPENSSL_STACK;
   function SSL_get1_supported_ciphers (s : SSL) return OPENSSL_STACK;
   function SSL_get_cipher_list (s : SSL; priority : int)
      return chars_ptr;
   function SSL_get_error (s : SSL; ret_code : int) return int;
   function SSL_get_mode (s : SSL) return SSL_MODE_TYPE;
   function SSL_get_options (s : SSL) return SSL_OP;
   function SSL_get_rbio (s : SSL) return BIO;
   function SSL_get_shared_ciphers
            (  s    : SSL;
               buf  : char_array;
               size : int
            )  return chars_ptr;
   function SSL_get_state (s : SSL) return OSSL_HANDSHAKE_STATE;
   function SSL_get_version (s : SSL_CTX) return chars_ptr;
   function SSL_get_wbio (s : SSL) return BIO;
   function SSL_get_SSL_CTX (s : SSL) return SSL_CTX;
   function SSL_in_before (s : SSL) return int;
   function SSL_in_init (s : SSL) return int;
   function SSL_is_init_finished (s : SSL) return int;
   function SSL_is_server (s : SSL) return int;
   function SSL_new (ctx : SSL_CTX) return SSL;
   function SSL_read
            (  s      : SSL;
               buf    : access Stream_Element;
               num    : int
            )  return int;
   function SSL_read_ex
            (  s         : SSL;
               buf       : access Stream_Element;
               num       : int;
               readbytes : access size_t
            )  return int;
   function SSL_session_reused (s : SSL) return int;
   procedure SSL_set_accept_state (s : SSL);
   procedure SSL_set_bio (s : SSL; rbio : BIO; wbio : BIO);
   function SSL_set_cipher_list
            (  s   : SSL;
               str : char_array
            )  return int;
   function SSL_set_ciphersuites
            (  s   : SSL;
               str : char_array
            )  return int;
   procedure SSL_set_connect_state (s : SSL);
   function SSL_set_mode
            (  s    : SSL;
               mode : SSL_MODE_TYPE
            )  return SSL_MODE_TYPE;
   function SSL_set_options (s : SSL; op : SSL_OP) return SSL_OP;
   procedure SSL_set_shutdown (s : SSL; mode : int);
   procedure SSL_set0_rbio (s : SSL; rbio : BIO);
   procedure SSL_set0_wbio (s : SSL; wbio : BIO);
   function SSL_shutdown (s : SSL) return int;
   function SSL_use_certificate_ASN1
            (  s   : SSL;
               len : int;
               d   : System.Address
            )  return int;
   function SSL_use_certificate_chain_file
            (  s    : SSL;
               file : char_array
            )  return int;
   function SSL_use_certificate_file
            (  s      : SSL;
               file   : char_array;
               format : SSL_FILETYPE
            )  return int;
   function SSL_use_PrivateKey_ASN1
            (  s   : SSL;
               len : int;
               d   : System.Address
            )  return int;
   function SSL_use_PrivateKey_file
            (  s      : SSL;
               file   : char_array;
               format : SSL_FILETYPE
            )  return int;
   function SSL_use_RSAPrivateKey_ASN1
            (  s   : SSL;
               len : int;
               d   : System.Address
            )  return int;
   function SSL_use_RSAPrivateKey_file
            (  s      : SSL;
               file   : char_array;
               format : SSL_FILETYPE
            )  return int;
   function SSL_version (s : SSL) return int;
   function SSL_want (s : SSL) return SSL_WANT_TYPE;
   function SSL_write
            (  s      : SSL;
               buf    : System.Address;
               num    : int
            )  return int;
   function SSL_write_ex
            (  s       : SSL;
               buf     : System.Address;
               num     : int;
               written : access size_t
            )  return int;

   function TLS_client_method return SSL_METHOD;
   function TLS_method        return SSL_METHOD;
   function TLS_server_method return SSL_METHOD;

   function MD_Null    return EVP_MD;
   function MD2        return EVP_MD;
   function MD5        return EVP_MD;
   function SHA1       return EVP_MD;
   function MDC2       return EVP_MD;
   function RIPEMD160  return EVP_MD;
   function Blake2b512 return EVP_MD;
   function Blake2s256 return EVP_MD;
   function SHA224     return EVP_MD;
   function SHA256     return EVP_MD;
   function SHA384     return EVP_MD;
   function SHA512     return EVP_MD;

   function Get_Digest_By_Name (Name : String) return EVP_MD;
   function Get_Digest_By_NID (ID : int) return EVP_MD;
--
-- PKCS5_PBKDF2_HMAC -- Key derivation
--
--    Password   - The password
--  [ Salt ]     - The salt (e.g. 4 octets)
--    Iterations - At least 1_000
--    Digest     - The digest method
--    Output     - The result (e.g. 20 octets)
--
-- Exceptions :
--
--    Constraint_Error - Error
--
   procedure PKCS5_PBKDF2_HMAC
             (  Password   : String;
                Salt       : Stream_Element_Array;
                Iterations : Positive;
                Digest     : EVP_MD;
                Output     : out Stream_Element_Array
             );
   procedure PKCS5_PBKDF2_HMAC
             (  Password   : String;
                Iterations : Positive;
                Digest     : EVP_MD;
                Output     : out Stream_Element_Array
             );
--
-- PKCS5_PBKDF2_HMAC_SHA1 -- Key derivation
--
--    Password   - The password
--  [ Salt ]     - The salt (e.g. 4 octets)
--    Iterations - At least 1_000
--    Output     - The result (e.g. 20 octets)
--
-- Exceptions :
--
--    Constraint_Error - Error
--
   procedure PKCS5_PBKDF2_HMAC_SHA1
             (  Password   : String;
                Salt       : Stream_Element_Array;
                Iterations : Positive;
                Output     : out Stream_Element_Array
             );
   procedure PKCS5_PBKDF2_HMAC_SHA1
             (  Password   : String;
                Iterations : Positive;
                Output     : out Stream_Element_Array
             );
--
-- RAND_Bytes -- Cryptographically strong pseudo-random bytes
--
--    Buffer - To be filled in
--
-- Exceptions :
--
--    Constraint_Error - Error
--
   procedure RAND_Bytes (Buffer : out Stream_Element_Array);
--
-- RAND_Priv_Bytes -- Pseudo-random private bytes
--
--    Buffer - To be filled in
--
-- Exceptions :
--
--    Constraint_Error - Error
--
   procedure RAND_Priv_Bytes (Buffer : out Stream_Element_Array);
--
-- Check_Error - Raise exception on error
--
--    ID - Of the exception to propagate on an error
--
-- Exceptions :
--
--    ID - If any SSL error is pending
--
   procedure Check_Error (ID : Exception_ID);

   function Address_Of (Object : BIO) return System.Address;
   function Address_Of (Object : BIO_METHOD) return System.Address;
   function Address_Of (Object : EVP_MD) return System.Address;
   function Address_Of (Object : SSL) return System.Address;
   function Address_Of (Object : SSL_CTX) return System.Address;
   function Address_Of (Object : SSL_METHOD) return System.Address;
private
   type BIO           is new System.Address;
   type BIO_METHOD    is new System.Address;
   type EVP_MD        is new System.Address;
   type OPENSSL_STACK is new System.Address;
   type SSL           is new System.Address;
   type SSL_CIPHER    is new System.Address;
   type SSL_CTX       is new System.Address;
   type SSL_METHOD    is new System.Address;

   No_CIPHER  : constant SSL_CIPHER := SSL_CIPHER (System.Null_Address);
   No_BIO     : constant BIO        := BIO (System.Null_Address);
   No_METHOD  : constant BIO_METHOD := BIO_METHOD (System.Null_Address);
   No_SSL     : constant SSL        := SSL (System.Null_Address);
   No_SSL_CTX : constant SSL_CTX    := SSL_CTX (System.Null_Address);
   No_STACK   : constant OPENSSL_STACK :=
                                    OPENSSL_STACK (System.Null_Address);

   pragma Import (C, BIO_clear_flags,        "BIO_clear_flags");
   pragma Import (C, BIO_free,               "BIO_free");
   pragma Import (C, BIO_get_data,           "BIO_get_data");
   pragma Import (C, BIO_get_new_index,      "BIO_get_new_index");
   pragma Import (C, BIO_meth_free,          "BIO_meth_free");
   pragma Import (C, BIO_meth_get_destroy,   "BIO_meth_get_destroy");
   pragma Import (C, BIO_meth_get_create,    "BIO_meth_get_create");
   pragma Import (C, BIO_meth_get_ctrl,      "BIO_meth_get_ctrl");
   pragma Import (C, BIO_meth_get_gets,      "BIO_meth_get_gets");
   pragma Import (C, BIO_meth_get_puts,      "BIO_meth_get_puts");
   pragma Import (C, BIO_meth_get_read,      "BIO_meth_get_read");
   pragma Import (C, BIO_meth_get_read_ex,   "BIO_meth_get_read_ex");
   pragma Import (C, BIO_meth_get_write,     "BIO_meth_get_write");
   pragma Import (C, BIO_meth_get_write_ex,  "BIO_meth_get_write_ex");
   pragma Import (C, BIO_meth_new,           "BIO_meth_new");
   pragma Import (C, BIO_meth_set_destroy,   "BIO_meth_set_destroy");
   pragma Import (C, BIO_meth_set_create,    "BIO_meth_set_create");
   pragma Import (C, BIO_meth_set_ctrl,      "BIO_meth_set_ctrl");
   pragma Import (C, BIO_meth_set_gets,      "BIO_meth_set_gets");
   pragma Import (C, BIO_meth_set_puts,      "BIO_meth_set_puts");
   pragma Import (C, BIO_meth_set_read,      "BIO_meth_set_read");
   pragma Import (C, BIO_meth_set_read_ex,   "BIO_meth_set_read_ex");
   pragma Import (C, BIO_meth_set_write,     "BIO_meth_set_write");
   pragma Import (C, BIO_meth_set_write_ex,  "BIO_meth_set_write_ex");
   pragma Import (C, BIO_new,                "BIO_new");
   pragma Import (C, BIO_pop,                "BIO_pop");
   pragma Import (C, BIO_read,               "BIO_read");
   pragma Import (C, BIO_read_ex,            "BIO_read_ex");
-- pragma Import (C, BIO_reset,              "BIO_reset");
   pragma Import (C, BIO_s_null,             "BIO_s_null");
   pragma Import (C, BIO_set_data,           "BIO_set_data");
   pragma Import (C, BIO_set_flags,          "BIO_set_flags");
   pragma Import (C, BIO_s_mem,              "BIO_s_mem");
   pragma Import (C, BIO_test_flags,         "BIO_test_flags");
   pragma Import (C, BIO_up_ref,             "BIO_up_ref");
   pragma Import (C, BIO_write,              "BIO_write");
   pragma Import (C, BIO_write_ex,           "BIO_write_ex");

   pragma Import (C, MD_Null,           "EVP_md_null");
   pragma Import (C, MD2,               "EVP_md2");
   pragma Import (C, MD5,               "EVP_md5");
   pragma Import (C, SHA1,              "EVP_sha1");
   pragma Import (C, MDC2,              "EVP_mdc2");
   pragma Import (C, RIPEMD160,         "EVP_ripemd160");
   pragma Import (C, Blake2b512,        "EVP_blake2b512");
   pragma Import (C, Blake2s256,        "EVP_blake2s256");
   pragma Import (C, SHA224,            "EVP_sha224");
   pragma Import (C, SHA256,            "EVP_sha256");
   pragma Import (C, SHA384,            "EVP_sha384");
   pragma Import (C, SHA512,            "EVP_sha512");
   pragma Import (C, Get_Digest_By_NID, "EVP_get_digestbynid");

   pragma Import (C, ERR_clear_error,        "ERR_clear_error");
   pragma Import (C, ERR_error_string_n,     "ERR_error_string_n");
   pragma Import (C, ERR_get_error,          "ERR_get_error");
   pragma Import (C, ERR_peek_error,         "ERR_peek_error");
   pragma Import (C, ERR_put_error,          "ERR_put_error");

   pragma Import (C, OPENSSL_init_ssl,       "OPENSSL_init_ssl");
   pragma Import (C, OPENSSL_sk_num,         "OPENSSL_sk_num");
   pragma Import (C, OPENSSL_sk_value,       "OPENSSL_sk_value");

   pragma Import (C, SSL_CIPHER_get_version, "SSL_CIPHER_get_version");
   pragma Import (C, SSL_CIPHER_get_name,    "SSL_CIPHER_get_name");
   pragma Import (C, SSL_CIPHER_standard_name,
                                            "SSL_CIPHER_standard_name");
   pragma Import (C, SSL_CTX_check_private_key,
                                           "SSL_CTX_check_private_key");
   pragma Import (C, SSL_CTX_clear_options,  "SSL_CTX_clear_options");
   pragma Import (C, SSL_CTX_ctrl,           "SSL_CTX_ctrl");
   pragma Import (C, SSL_CTX_free,           "SSL_CTX_free");
   pragma Import (C, SSL_CTX_get_ciphers,    "SSL_CTX_get_ciphers");
   pragma Import (C, SSL_CTX_get_options,    "SSL_CTX_get_options");
   pragma Import (C, SSL_CTX_get_timeout,    "SSL_CTX_get_timeout");
   pragma Import (C, SSL_CTX_load_verify_locations,
                                       "SSL_CTX_load_verify_locations");
   pragma Import (C, SSL_CTX_new,           "SSL_CTX_new");
   pragma Import (C, SSL_CTX_up_ref,        "SSL_CTX_up_ref");
   pragma Import (C, SSL_CTX_set_cipher_list,
                                            "SSL_CTX_set_cipher_list");
   pragma Import (C, SSL_CTX_set_ciphersuites,
                                            "SSL_CTX_set_ciphersuites");
   pragma Import (C, SSL_CTX_set_default_verify_paths,
                                    "SSL_CTX_set_default_verify_paths");
   pragma Import (C, SSL_CTX_set_default_verify_dir,
                                      "SSL_CTX_set_default_verify_dir");
   pragma Import (C, SSL_CTX_set_default_verify_file,
                                     "SSL_CTX_set_default_verify_file");
   pragma Import (C, SSL_CTX_set_options,   "SSL_CTX_set_options");
   pragma Import (C, SSL_CTX_set_timeout,   "SSL_CTX_set_timeout");
   pragma Import (C, SSL_CTX_use_certificate_ASN1,
                                        "SSL_CTX_use_certificate_ASN1");
   pragma Import (C, SSL_CTX_use_certificate_chain_file,
                                  "SSL_CTX_use_certificate_chain_file");
   pragma Import (C, SSL_CTX_use_certificate_file,
                                        "SSL_CTX_use_certificate_file");
   pragma Import (C, SSL_CTX_use_PrivateKey_ASN1,
                                         "SSL_CTX_use_PrivateKey_ASN1");
   pragma Import (C, SSL_CTX_use_PrivateKey_file,
                                         "SSL_CTX_use_PrivateKey_file");
   pragma Import (C, SSL_CTX_use_RSAPrivateKey_ASN1,
                                      "SSL_CTX_use_RSAPrivateKey_ASN1");
   pragma Import (C, SSL_CTX_use_RSAPrivateKey_file,
                                      "SSL_CTX_use_RSAPrivateKey_file");


   pragma Import (C, SSL_accept,             "SSL_accept");
   pragma Import (C, SSL_certs_clear,        "SSL_certs_clear");
   pragma Import (C, SSL_check_private_key,  "SSL_check_private_key");
   pragma Import (C, SSL_connect,            "SSL_connect");
   pragma Import (C, SSL_clear,              "SSL_clear");
   pragma Import (C, SSL_clear_options,      "SSL_clear_options");
   pragma Import (C, SSL_ctrl,               "SSL_ctrl");
   pragma Import (C, SSL_free,               "SSL_free");
   pragma Import (C, SSL_get_cipher_list,    "SSL_get_cipher_list");
   pragma Import (C, SSL_get_ciphers,        "SSL_get_ciphers");
   pragma Import (C, SSL_get_client_ciphers, "SSL_get_client_ciphers");
   pragma Import (C, SSL_get1_supported_ciphers,
                                          "SSL_get1_supported_ciphers");
   pragma Import (C, SSL_get_error,          "SSL_get_error");
   pragma Import (C, SSL_get_options,        "SSL_get_options");
   pragma Import (C, SSL_get_rbio,           "SSL_get_rbio");
   pragma Import (C, SSL_get_shared_ciphers, "SSL_get_shared_ciphers");
   pragma Import (C, SSL_get_SSL_CTX,        "SSL_get_SSL_CTX");
   pragma Import (C, SSL_get_state,          "SSL_get_state");
   pragma Import (C, SSL_get_version,        "SSL_get_version");
   pragma Import (C, SSL_get_wbio,           "SSL_get_wbio");
   pragma Import (C, SSL_in_before,          "SSL_in_before");
   pragma Import (C, SSL_in_init,            "SSL_in_init");
   pragma Import (C, SSL_is_init_finished,   "SSL_is_init_finished");
   pragma Import (C, SSL_is_server,          "SSL_is_server");
   pragma Import (C, SSL_new,                "SSL_new");
   pragma Import (C, SSL_read,               "SSL_read");
   pragma Import (C, SSL_read_ex,            "SSL_read_ex");
   pragma Import (C, SSL_session_reused,     "SSL_session_reused");
   pragma Import (C, SSL_set_accept_state,   "SSL_set_accept_state");
   pragma Import (C, SSL_set_bio,            "SSL_set_bio");
   pragma Import (C, SSL_set_cipher_list,    "SSL_set_cipher_list");
   pragma Import (C, SSL_set_ciphersuites,   "SSL_set_ciphersuites");
   pragma Import (C, SSL_set_connect_state,  "SSL_set_connect_state");
   pragma Import (C, SSL_set_options,        "SSL_set_options");
   pragma Import (C, SSL_set_shutdown,       "SSL_set_shutdown");
   pragma Import (C, SSL_set0_rbio,          "SSL_set0_rbio");
   pragma Import (C, SSL_set0_wbio,          "SSL_set0_wbio");
   pragma Import (C, SSL_shutdown,           "SSL_shutdown");
   pragma Import (C, SSL_want,               "SSL_want");
   pragma Import (C, SSL_use_certificate_ASN1,
                                            "SSL_use_certificate_ASN1");
   pragma Import (C, SSL_use_certificate_chain_file,
                                      "SSL_use_certificate_chain_file");
   pragma Import (C, SSL_use_certificate_file,
                                            "SSL_use_certificate_file");
   pragma Import (C, SSL_use_PrivateKey_ASN1,
                                             "SSL_use_PrivateKey_ASN1");
   pragma Import (C, SSL_use_PrivateKey_file,
                                             "SSL_use_PrivateKey_file");
   pragma Import (C, SSL_use_RSAPrivateKey_ASN1,
                                          "SSL_use_RSAPrivateKey_ASN1");
   pragma Import (C, SSL_use_RSAPrivateKey_file,
                                          "SSL_use_RSAPrivateKey_file");
   pragma Import (C, SSL_version,         "SSL_version");
   pragma Import (C, SSL_write,           "SSL_write");
   pragma Import (C, SSL_write_ex,        "SSL_write_ex");

   pragma Import (C, TLS_client_method,   "TLS_client_method");
   pragma Import (C, TLS_method,          "TLS_method");
   pragma Import (C, TLS_server_method,   "TLS_server_method");

end OpenSSL;
