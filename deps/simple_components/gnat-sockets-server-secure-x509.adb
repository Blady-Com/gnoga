--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Server.Secure.X509             Luebeck            --
--  Implementation                                 Winter, 2015       --
--                                                                    --
--                                Last revision :  18:53 15 Jan 2015  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body GNAT.Sockets.Server.Secure.X509 is

   procedure Add_CRL
             (  Factory : in out X509_Authentication_Factory;
                List    : X509_CRL_Array
             )  is
   begin
      Certificate_Set_X509_CRL (Factory.Credentials, List);
   end Add_CRL;

   procedure Add_CRL_DER
             (  Factory : in out X509_Authentication_Factory;
                CRL     : Stream_Element_Array
             )  is
   begin
      Certificate_Set_X509_CRL_Mem
      (  Factory.Credentials,
         CRL,
         X509_Fmt_DER
      );
   end Add_CRL_DER;

   procedure Add_CRL_From_DER_File
             (  Factory    : in out X509_Authentication_Factory;
                File       : String
             )  is
   begin
      Certificate_Set_X509_CRL_File
      (  Factory.Credentials,
         File,
         X509_Fmt_DER
      );
   end Add_CRL_From_DER_File;

   procedure Add_CRL_From_PEM_File
             (  Factory    : in out X509_Authentication_Factory;
                File       : String
             )  is
   begin
      Certificate_Set_X509_CRL_File
      (  Factory.Credentials,
         File,
         X509_Fmt_PEM
      );
   end Add_CRL_From_PEM_File;

   procedure Add_CRL_PEM
             (  Factory : in out X509_Authentication_Factory;
                CRL     : String
             )  is
   begin
      Certificate_Set_X509_CRL_Mem
      (  Factory.Credentials,
         To_Stream_Element_Array (CRL),
         X509_Fmt_PEM
      );
   end Add_CRL_PEM;

   procedure Add_Key
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : OpenPGP_Crt;
                Private_Key : OpenPGP_Privkey
             )  is
   begin
      Certificate_Server_Set_OpenPGP_Key
      (  Factory.Credentials,
         Public_Key,
         Private_Key
      );
   end Add_Key;

   procedure Add_Key_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array;
                Key         : Stream_Element_Array;
                Encryption  : PKCS_Encrypt_Flags;
                Password    : String
             )  is
   begin
      Certificate_Set_X509_Key_Mem
      (  Factory.Credentials,
         Certificate,
         Key,
         X509_Fmt_DER,
         Password,
         Encryption
      );
   end Add_Key_DER;

   procedure Add_Key_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array;
                Encryption  : PKCS_Encrypt_Flags;
                Password    : String
             )  is
   begin
      Certificate_Set_X509_Key_Mem
      (  Factory.Credentials,
         Certificate,
         X509_Fmt_DER,
         Password,
         Encryption
      );
   end Add_Key_DER;

   procedure Add_Key_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array;
                Key         : Stream_Element_Array
             )  is
   begin
      Certificate_Set_X509_Key_Mem
      (  Factory.Credentials,
         Certificate,
         Key,
         X509_Fmt_DER
      );
   end Add_Key_DER;

   procedure Add_Key_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array
             )  is
   begin
      Certificate_Set_X509_Key_Mem
      (  Factory.Credentials,
         Certificate,
         X509_Fmt_DER
      );
   end Add_Key_DER;

   procedure Add_Key_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String;
                Key         : String;
                Encryption  : PKCS_Encrypt_Flags;
                Password    : String
             )  is
   begin
      Certificate_Set_X509_Key_Mem
      (  Factory.Credentials,
         To_Stream_Element_Array (Certificate),
         To_Stream_Element_Array (Key),
         X509_Fmt_PEM,
         Password,
         Encryption
      );
   end Add_Key_PEM;

   procedure Add_Key_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String;
                Encryption  : PKCS_Encrypt_Flags;
                Password    : String
             )  is
   begin
      Certificate_Set_X509_Key_Mem
      (  Factory.Credentials,
         To_Stream_Element_Array (Certificate),
         X509_Fmt_PEM,
         Password,
         Encryption
      );
   end Add_Key_PEM;

   procedure Add_Key_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String;
                Key         : String
             )  is
   begin
      Certificate_Set_X509_Key_Mem
      (  Factory.Credentials,
         To_Stream_Element_Array (Certificate),
         To_Stream_Element_Array (Key),
         X509_Fmt_PEM
      );
   end Add_Key_PEM;

   procedure Add_Key_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String
             )  is
   begin
      Certificate_Set_X509_Key_Mem
      (  Factory.Credentials,
         To_Stream_Element_Array (Certificate),
         X509_Fmt_PEM
      );
   end Add_Key_PEM;

   procedure Add_Key_From_Base64_File
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : String;
                Private_Key : String;
                Subkey_ID   : String
             )  is
   begin
      Certificate_Server_Set_OpenPGP_Key_File
      (  Factory.Credentials,
         Public_Key,
         Private_Key,
         Subkey_ID,
         OpenPGP_Fmt_Base64
      );
   end Add_Key_From_Base64_File;

   procedure Add_Key_From_Base64_File
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : String;
                Private_Key : String
             )  is
   begin
      Certificate_Server_Set_OpenPGP_Key_File
      (  Factory.Credentials,
         Public_Key,
         Private_Key,
         OpenPGP_Fmt_Base64
      );
   end Add_Key_From_Base64_File;

   procedure Add_Key_From_DER_File
             (  Factory          : in out X509_Authentication_Factory;
                Certificate_File : String;
                Key_File         : String;
                Encryption       : PKCS_Encrypt_Flags;
                Password         : String
             )  is
   begin
      Certificate_Set_X509_Key_File
      (  Factory.Credentials,
         Certificate_File,
         Key_File,
         X509_Fmt_DER,
         Password,
         Encryption
      );
   end Add_Key_From_DER_File;

   procedure Add_Key_From_DER_File
             (  Factory          : in out X509_Authentication_Factory;
                Certificate_File : String;
                Key_File         : String
             )  is
   begin
      Certificate_Set_X509_Key_File
      (  Factory.Credentials,
         Certificate_File,
         Key_File,
         X509_Fmt_DER
      );
   end Add_Key_From_DER_File;

   procedure Add_Key_From_PEM_File
             (  Factory          : in out X509_Authentication_Factory;
                Certificate_File : String;
                Key_File         : String;
                Encryption       : PKCS_Encrypt_Flags;
                Password         : String
             )  is
   begin
      Certificate_Set_X509_Key_File
      (  Factory.Credentials,
         Certificate_File,
         Key_File,
         X509_Fmt_PEM,
         Password,
         Encryption
      );
   end Add_Key_From_PEM_File;

   procedure Add_Key_From_PEM_File
             (  Factory          : in out X509_Authentication_Factory;
                Certificate_File : String;
                Key_File         : String
             )  is
   begin
      Certificate_Set_X509_Key_File
      (  Factory.Credentials,
         Certificate_File,
         Key_File,
         X509_Fmt_PEM
      );
   end Add_Key_From_PEM_File;

   procedure Add_Key_From_Raw_File
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : String;
                Private_Key : String;
                Subkey_ID   : String
             )  is
   begin
      Certificate_Server_Set_OpenPGP_Key_File
      (  Factory.Credentials,
         Public_Key,
         Private_Key,
         Subkey_ID,
         OpenPGP_Fmt_Raw
      );
   end Add_Key_From_Raw_File;

   procedure Add_Key_From_Raw_File
             (  Factory     : in out X509_Authentication_Factory;
                Public_Key  : String;
                Private_Key : String
             )  is
   begin
      Certificate_Server_Set_OpenPGP_Key_File
      (  Factory.Credentials,
         Public_Key,
         Private_Key,
         OpenPGP_Fmt_Raw
      );
   end Add_Key_From_Raw_File;

   procedure Add_Key_From_Base64_Ring
             (  Factory : in out X509_Authentication_Factory;
                File    : String
             )  is
   begin
      Certificate_Server_Set_OpenPGP_Keyring_File
      (  Factory.Credentials,
         File,
         OpenPGP_Fmt_Base64
      );
   end Add_Key_From_Base64_Ring;

   procedure Add_Key_From_Raw_Ring
             (  Factory : in out X509_Authentication_Factory;
                File    : String
             )  is
   begin
      Certificate_Server_Set_OpenPGP_Keyring_File
      (  Factory.Credentials,
         File,
         OpenPGP_Fmt_Raw
      );
   end Add_Key_From_Raw_Ring;

   procedure Add_Trust_DER
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : Stream_Element_Array
             )  is
   begin
      Certificate_Set_X509_Trust_Mem
      (  Factory.Credentials,
         Certificate,
         X509_Fmt_DER
      );
   end Add_Trust_DER;

   procedure Add_Trust_PEM
             (  Factory     : in out X509_Authentication_Factory;
                Certificate : String
             )  is
   begin
      Certificate_Set_X509_Trust_Mem
      (  Factory.Credentials,
         To_Stream_Element_Array (Certificate),
         X509_Fmt_PEM
      );
   end Add_Trust_PEM;

   procedure Add_Trust
             (  Factory : in out X509_Authentication_Factory;
                List    : X509_Crt_Array
             )  is
   begin
      Certificate_Set_X509_Trust (Factory.Credentials, List);
   end Add_Trust;

   procedure Add_Trust_From_DER_File
             (  Factory : in out X509_Authentication_Factory;
                File    : String
             )  is
   begin
      Certificate_Set_X509_Trust_File
      (  Factory.Credentials,
         File,
         X509_Fmt_DER
      );
   end Add_Trust_From_DER_File;

   procedure Add_Trust_From_PEM_File
             (  Factory : in out X509_Authentication_Factory;
                File    : String
             )  is
   begin
      Certificate_Set_X509_Trust_File
      (  Factory.Credentials,
         File,
         X509_Fmt_PEM
      );
   end Add_Trust_From_PEM_File;

   procedure Add_System_Trust
             (  Factory : in out X509_Authentication_Factory
             )  is
   begin
      Certificate_Set_X509_System_Trust (Factory.Credentials);
   end Add_System_Trust;

   procedure Generate_Diffie_Hellman_Parameters
             (  Factory : in out X509_Authentication_Factory
             )  is
   begin
      DH_Params_Generate2
      (  Factory.Parameters,
         Sec_Param_To_PK_Bits (PK_DH, SEC_PARAM_LEGACY)
      );
      Certificate_Set_DH_Params
      (  Factory.Credentials,
         Factory.Parameters
      );
      Factory.DH_Generated := True;
   end Generate_Diffie_Hellman_Parameters;

   procedure Initialize
             (  Factory : in out X509_Authentication_Factory
             )  is
   begin
      Initialize (Abstract_GNUTLS_Factory (Factory));
   end Initialize;

   procedure Prepare
             (  Factory : in out X509_Authentication_Factory;
                Client  : in out Connection'Class;
                Session : in out Session_Type
             )  is
   begin
      if not Is_Initialized (Factory.Cache) then
         Initialize (Factory.Cache, Default_Priorities);
      end if;
      Priority_Set (Session, Factory.Cache);
      if not Factory.DH_Generated then
         DH_Params_Generate2
         (  Factory.Parameters,
            Sec_Param_To_PK_Bits (PK_DH, SEC_PARAM_LEGACY)
         );
         Certificate_Set_DH_Params
         (  Factory.Credentials,
            Factory.Parameters
         );
         Factory.DH_Generated := True;
      end if;
      Credentials_Set (Session, Factory.Credentials);
      Certificate_Server_Set_Request (Session, Cert_Ignore);
   end Prepare;

   procedure Set_OCSP_Response_File
             (  Factory : in out X509_Authentication_Factory;
                File    : String
             )  is
   begin
      Certificate_Set_OCSP_Status_Request_File
      (  Factory.Credentials,
         File
      );
   end Set_OCSP_Response_File;

   procedure Set_Priorities
             (  Factory    : in out X509_Authentication_Factory;
                Priorities : String
             )  is
   begin
      if Is_Initialized (Factory.Cache) then
         Raise_Exception
         (  Status_Error'Identity,
            "Priorities are already set"
         );
      else
         Initialize (Factory.Cache, Priorities);
      end if;
   end Set_Priorities;

end GNAT.Sockets.Server.Secure.X509;
