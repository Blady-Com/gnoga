--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Secure.X509             Luebeck            --
-- Interface                                       Winter, 2015       --
--                                                                    --
--                                Last revision :  12:47 19 Jun 2016  --
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

package GNAT.Sockets.Server.Secure.X509 is
--
-- The priorities applied if not set explicitly
--
   Default_Priorities : constant String := "NORMAL";
--
-- X509_Authentication_Factory -- Abstract  factory  for  X.509  authen-
--                                tication.
--
   type X509_Authentication_Factory is
      new Abstract_GNUTLS_Factory with private;
--
-- Add_CRL -- Add trusted CRLs
--
--    Factory - The credentials connection factory
--    List    - The array of trusted CRLs
--
-- This procedure adds  the  trusted  CRLs  in  order  to verify  client
-- certificates
--
   procedure Add_CRL
             (  Factory : in out X509_Authentication_Factory;
                List    : X509_CRL_Array
             );
--
-- Add_CRL_{DER|PEM} -- Add trusted CRL
--
--    Factory - The credentials connection factory
--    CRL     - Trusted CRL
--
-- This procedure adds  the  trusted  CRL   in  order  to verify  client
-- certificates
--
   procedure Add_CRL_DER
             (  Factory : in out X509_Authentication_Factory;
                CRL     : Stream_Element_Array
             );
   procedure Add_CRL_PEM
             (  Factory : in out X509_Authentication_Factory;
                CRL     : String
             );
--
-- Add_CRL_From_{DER|PEM}_File -- Add CRLs from file PEM or DER format
--
--    Factory    - The credentials connection factory
--    File       - The name of
--
-- These procedures add the  trusted  CRLs  in  order  to verify  client
-- certificates
--
   procedure Add_CRL_From_DER_File
             (  Factory    : in out X509_Authentication_Factory;
                File       : String
             );
   procedure Add_CRL_From_PEM_File
             (  Factory    : in out X509_Authentication_Factory;
                File       : String
             );
--
-- Add_Key -- Add OpenPGP public/private keys pair
--
--    Factory     - The credentials connection factory
--    Public_Key  - The public key
--    Private_Key - The private key
--
-- This procedure sets a certificate/private key pair in  Factory.  This
-- procedure   may   be   called   more  than  once,  in  case  multiple
-- keys/certificates exist for the server.
--
   procedure Add_Key
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : OpenPGP_Crt;
                Private_Key : OpenPGP_Privkey
             );
--
-- Add_Key_{DER|PEM} -- Add certificate and key from memory
--
--    Factory     - The credentials connection factory
--    Certificate - The certificate
--  [ Key      ]  - The key
--  [ Encryption  - The method of encryption
--    Password ]  - The password to decrypt the key
--
-- These procedures set a certificate/private key pair in factory. These
-- procedures  may  be  called  more  than  once,   in   case   multiple
-- keys/certificates  exist  for  the  server. These procedures can also
-- accept URLs at Key_File and Certificate_File. In that  case  it  will
-- import the private key and certificate indicated by  the  URLs.  Note
-- that the supported URLs are the ones indicated  by  URL_Is_Supported.
-- In  case  the Certificate_File is provided as a PKCS 11 URL, then the
-- certificate, and its present issuers in the token  are  are  imported
-- (i.e.,  the required trust chain). If the procedure fails to load the
-- res structure is at an undefined state, it must not be reused to load
-- other keys or certificates.
--
   procedure Add_Key_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array;
                Key         : Stream_Element_Array;
                Encryption  : PKCS_Encrypt_Flags;
                Password    : String
             );
   procedure Add_Key_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array;
                Encryption  : PKCS_Encrypt_Flags;
                Password    : String
             );
   procedure Add_Key_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array;
                Key         : Stream_Element_Array
             );
   procedure Add_Key_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array
             );
   procedure Add_Key_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String;
                Key         : String;
                Encryption  : PKCS_Encrypt_Flags;
                Password    : String
             );
   procedure Add_Key_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String;
                Encryption  : PKCS_Encrypt_Flags;
                Password    : String
             );
   procedure Add_Key_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String;
                Key         : String
             );
   procedure Add_Key_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String
             );
--
-- Add_Key_From_{DER|PEM}_File -- Add trusted CAs
--
--    Factory          - The credentials connection factory
--    Certificate_File - The name of
--    Key_File         - The name of
--  [ Encryption       - The method of encryption
--    Password ]       - The password to decrypt the key
--
-- These procedures set a certificate/private key pair in factory. These
-- procedures  may  be  called  more  than  once,   in   case   multiple
-- keys/certificates  exist  for  the  server. These procedures can also
-- accept URLs at Key_File and Certificate_File. In that  case  it  will
-- import the private key and certificate indicated by  the  URLs.  Note
-- that the supported URLs are the ones indicated  by  URL_Is_Supported.
-- In  case  the Certificate_File is provided as a PKCS 11 URL, then the
-- certificate, and its present issuers in the token  are  are  imported
-- (i.e.,  the required trust chain). If the procedure fails to load the
-- res structure is at an undefined state, it must not be reused to load
-- other keys or certificates.
--
   procedure Add_Key_From_DER_File
             (  Factory          : in out X509_Authentication_Factory;
                Certificate_File : String;
                Key_File         : String;
                Encryption       : PKCS_Encrypt_Flags;
                Password         : String
             );
   procedure Add_Key_From_DER_File
             (  Factory          : in out X509_Authentication_Factory;
                Certificate_File : String;
                Key_File         : String
             );
   procedure Add_Key_From_PEM_File
             (  Factory          : in out X509_Authentication_Factory;
                Certificate_File : String;
                Key_File         : String;
                Encryption       : PKCS_Encrypt_Flags;
                Password         : String
             );
   procedure Add_Key_From_PEM_File
             (  Factory          : in out X509_Authentication_Factory;
                Certificate_File : String;
                Key_File         : String
             );
--
-- Add_Key_From_{Raw|Base64}_File -- Add OpenPGP public/private keys
--
--    Factory      - The credentials connection factory
--    Public_File  - The public key file
--    Private_File - The private key file
--  [ Subkey_ID ]  - The subkey ID
--
-- These procedures add OpenPGP keys into the factory. The files  should
-- contain at least one valid non-encrypted subkey.
--
   procedure Add_Key_From_Base64_File
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : String;
                Private_Key : String;
                Subkey_ID   : String
             );
   procedure Add_Key_From_Base64_File
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : String;
                Private_Key : String
             );
   procedure Add_Key_From_Raw_File
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : String;
                Private_Key : String;
                Subkey_ID   : String
             );
   procedure Add_Key_From_Raw_File
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : String;
                Private_Key : String
             );
--
-- Add_Key_From_{Raw|Base64}_Ring -- Add OpenPGP keys from a keyring
--
--    Factory - The credentials connection factory
--    Keyring - The file
--
-- This funtion is used to load OpenPGP keys into the  credentials.  The
-- file  should  contain  at  least  one valid non-encrypted subkey.
--
   procedure Add_Key_From_Base64_Ring
             (  Factory : in out X509_Authentication_Factory;
                File    : String
             );
   procedure Add_Key_From_Raw_Ring
             (  Factory : in out X509_Authentication_Factory;
                File    : String
             );
--
-- Add_Trust -- Add trusted CAs
--
--    Factory - The credentials connection factory
--    List    - An array of trusted CAs
--
-- This  procedure  adds  the  trusted  CAs  in  order  to verify client
-- certificates.
--
   procedure Add_Trust
             (  Factory : in out X509_Authentication_Factory;
                List    : X509_Crt_Array
             );
--
-- Add_Trust_{DER|PEM} -- Add trusted CA
--
--    Factory     - The credentials connection factory
--    Certificate - A trusted certificate to add
--
-- This  procedure  adds  the  trusted  CA   in  order  to verify client
-- certificates.
--
   procedure Add_Trust_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array
             );
   procedure Add_Trust_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String
             );
--
-- Add_Trust_From_{DER|PEM}_File -- Add from file PEM or DER format
--
--    Factory    - The credentials connection factory
--    File       - The name of
--
-- These procedures add the  trusted  CAs  in  order  to  verify  client
-- certificates. They may be called multiple times. They can also accept
-- URLs. In that case it will import all certificates that are marked as
-- trusted.  Note  that  the  supported  URLs  are the ones indicated by
-- URL_Is_Supported.
--
   procedure Add_Trust_From_DER_File
             (  Factory    : in out X509_Authentication_Factory;
                File       : String
             );
   procedure Add_Trust_From_PEM_File
             (  Factory    : in out X509_Authentication_Factory;
                File       : String
             );
--
-- Add_System_Trust -- Add system default CAs
--
--    Factory - The credentials connection factory
--
-- This procedure adds the system's default  trusted  CAs  in  order  to
-- verify client certificates
--
   procedure Add_System_Trust
             (  Factory : in out X509_Authentication_Factory
             );
--
-- Generate_Diffie_Hellman_Parameters -- Generate parameters
--
--    Factory - The credentials connection factory
--
-- This procedure generates Diffie-Hellman parameters
--
   procedure Generate_Diffie_Hellman_Parameters
             (  Factory : in out X509_Authentication_Factory
             );
--
-- Set_OCSP_Response_File -- Set OCSP response file
--
--    Factory - The credentials connection factory
--    File    - A filename of the OCSP response
--
-- This procedure sets the filename of an OCSP  response,  that will  be
-- sent to the client if requests an OCSP certificate status. This is  a
-- convenience procedure which is inefficient on busy servers  since the
-- file is opened on every access
--
   procedure Set_OCSP_Response_File
             (  Factory : in out X509_Authentication_Factory;
                File    : String
             );
--
-- Set_Priorities -- Set priorities for the authentication
--
--    Factory    - The credentials connection factory
--    Priorities - The priorities to set
--
   procedure Set_Priorities
             (  Factory    : in out X509_Authentication_Factory;
                Priorities : String
             );
--
-- Overriddes ...HTTP_Server.Secure...
--
   procedure Prepare
             (  Factory : in out X509_Authentication_Factory;
                Client  : in out Connection'Class;
                Session : in out Session_Type
             );
--
-- Overriddes ...HTTP_Server.Secure...
--
   procedure Initialize (Factory : in out X509_Authentication_Factory);

private
   type X509_Authentication_Factory is
      new Abstract_GNUTLS_Factory with
   record
      Credentials  : Certificate_Credentials;
      Parameters   : DH_Params;
      Cache        : Priority;
      DH_Generated : Boolean := False;
   end record;

end GNAT.Sockets.Server.Secure.X509;
