--                                                                    --
--  package GNUTLS                  Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2015       --
--                                                                    --
--                                Last revision :  08:30 04 Aug 2022  --
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

with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System_Errno;          use System_Errno;

with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C.Pointers;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body GNUTLS is
   use System;

   type ssize_t is new ptrdiff_t;
   function To_Unsigned is new Ada.Unchecked_Conversion (int, unsigned);
   function To_Address is
      new Ada.Unchecked_Conversion (chars_ptr, Address);

   package Certificate_Type_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => Certificate_Type,
             Element_Array      => Certificate_Type_Array,
             Default_Terminator => CRT_Unknown
          );
   package Cipher_Algorithm_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => Cipher_Algorithm,
             Element_Array      => Cipher_Algorithm_Array,
             Default_Terminator => Cipher_Unknown
          );
   package Compression_Method_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => Compression_Method,
             Element_Array      => Compression_Method_Array,
             Default_Terminator => Comp_Unknown
          );
   package Datum_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => Datum,
             Element_Array      => Datum_Array,
             Default_Terminator => (Null_Address, 0)
          );
   package Digest_Algorithm_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => Digest_Algorithm,
             Element_Array      => Digest_Algorithm_Array,
             Default_Terminator => Dig_Unknown
          );
   package ECC_Curve_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => ECC_Curve,
             Element_Array      => ECC_Curve_Array,
             Default_Terminator => ECC_Curve_Invalid
          );
   package KX_Algorithm_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => KX_Algorithm,
             Element_Array      => KX_Algorithm_Array,
             Default_Terminator => KX_Unknown
          );
   package MAC_Algorithm_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => MAC_Algorithm,
             Element_Array      => MAC_Algorithm_Array,
             Default_Terminator => MAC_Unknown
          );
   package PK_Algorithm_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => PK_Algorithm,
             Element_Array      => PK_Algorithm_Array,
             Default_Terminator => PK_Unknown
          );
   package Sign_Algorithm_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => Sign_Algorithm,
             Element_Array      => Sign_Algorithm_Array,
             Default_Terminator => Sign_Unknown
          );
   package Protocol_Pointers is
      new Interfaces.C.Pointers
          (  Index              => Positive,
             Element            => Protocol,
             Element_Array      => Protocol_Array,
             Default_Terminator => No_Protocol
          );

   procedure Adjust (Data : in out DH_Params) is
      function Internal (Destination, Source : Address) return int;
      pragma Import (C, Internal, "gnutls_dh_params_cpy");
      Source : Address := Data.Handle;
   begin
      Initialize (Data);
      Check (Internal (Data.Handle, Source));
   end Adjust;

   procedure Adjust (Data : in out DH_Params_Ref) is
   begin
      null;
   end Adjust;

   function Alert_Get (Session : Session_Type)
      return Alert_Description is
      function Internal (Session : Address) return Alert_Description;
      pragma Import (C, Internal, "gnutls_alert_get");
   begin
      return Internal (Session.Holder.Handle);
   end Alert_Get;

   function Alert_Get_Name (Alert : Alert_Description) return String is
      function Internal (Alert : Alert_Description) return chars_ptr;
      pragma Import
             (  C,
                Internal,
                "gnutls_alert_get_name"
             );
      Result : chars_ptr := Internal (Alert);
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No alert matched");
      else
         return Value (Result);
      end if;
   end Alert_Get_Name;

   function Alert_Get_StrName
            (  Alert : Alert_Description
            )  return String is
      function Internal (Alert : Alert_Description) return chars_ptr;
      pragma Import (C, Internal, "gnutls_alert_get_strname");
   begin
      return Value (Internal (Alert));
   end Alert_Get_StrName;

   procedure Alert_Send
             (  Session : in out Session_Type;
                Level   : Alert_level;
                Desc    : Alert_Description
             )  is
      function Internal
               (  Session : Address;
                  Level   : Alert_level;
                  Desc    : Alert_Description
               )  return int;
      pragma Import (C, Internal, "gnutls_alert_send");
   begin
      Check (Internal (Session.Holder.Handle, Level, Desc));
   end Alert_Send;

   procedure Alert_Send_Appropriate
             (  Session : in out Session_Type;
                Level   : Alert_level;
                Desc    : Alert_Description
             )  is
      function Internal
               (  Session : Address;
                  Level   : Alert_level;
                  Desc    : Alert_Description
               )  return int;
      pragma Import (C, Internal, "gnutls_alert_send_appropriate");
   begin
      Check (Internal (Session.Holder.Handle, Level, Desc));
   end Alert_Send_Appropriate;

   function Alpn_Get_Selected_Protocol
            (  Session : Session_Type
            )  return Stream_Element_Array is
      function Internal
               (  Session  : Address;
                  Protocol : access Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_alpn_get_selected_protocol");
      Protocol : aliased Datum;
   begin
      Check (Internal (Session.Holder.Handle, Protocol'Access));
      return To_Stream_Element_Array (Protocol);
   end Alpn_Get_Selected_Protocol;

   procedure Alpn_Set_Selected_Protocol
             (  Session   : in out Session_Type;
                Protocols : Datum_Array;
                Flags     : int
             )  is
      function Internal
               (  Session   : Address;
                  Protocols : access constant Datum;
                  Size      : unsigned;
                  Flags     : int
               )  return int;
      pragma Import (C, Internal, "gnutls_alpn_set_protocols");
   begin
      Check
      (  Internal
         (  Session.Holder.Handle,
            Protocols (Protocols'First)'Access,
            Protocols'Length,
            Flags
      )  );
   end Alpn_Set_Selected_Protocol;

   package body Anon_Params_Function is
      type Params_Function is access function
           (  Session    : Address;
              Type_Of    : Params_Type;
              Parameters : Params_Data
           )  return int;
      pragma Convention (C, Params_Function);
      procedure Internal
                (  Credentials : Address;
                   Func        : Params_Function
                );
      pragma Import
             (  C,
                Internal,
                "gnutls_anon_set_params_function"
             );
      function Func
               (  Session    : Address;
                  Type_Of    : Params_Type;
                  Parameters : Params_Data
               )  return int;
      pragma Convention (C, Func);

      function Func
               (  Session    : Address;
                  Type_Of    : Params_Type;
                  Parameters : Params_Data
               )  return int is
         Parent : Session_Type (0);
      begin
         Parent.Holder.Handle := Session;
         Handle (Parent, Create (Type_Of, Parameters));
         return E_Success;
      exception
         when Error : others =>
            return Get_Error_Code (Error);
      end Func;

      procedure Set
                (  Credentials : in out Anon_Server_Credentials'Class
                )  is
      begin
         Internal (Credentials.Handle, Func'Access);
      end Set;
   end Anon_Params_Function;

   package body Anon_Server_Params_Function is
      type Params_Function is access function
           (  Session    : Address;
              Type_Of    : Params_Type;
              Parameters : Params_Data
           )  return int;
      pragma Convention (C, Params_Function);
      procedure Internal
                (  Credentials : Address;
                   Func        : Params_Function
                );
      pragma Import
             (  C,
                Internal,
                "gnutls_anon_set_server_params_function"
             );
      function Func
               (  Session    : Address;
                  Type_Of    : Params_Type;
                  Parameters : Params_Data
               )  return int;
      pragma Convention (C, Func);

      function Func
               (  Session    : Address;
                  Type_Of    : Params_Type;
                  Parameters : Params_Data
               )  return int is
         Parent : Session_Type (0);
      begin
         Parent.Holder.Handle := Session;
         Handle (Parent, Create (Type_Of, Parameters));
         return E_Success;
      exception
         when Error : others =>
            return Get_Error_Code (Error);
      end Func;

      procedure Set
                (  Credentials : in out Anon_Server_Credentials'Class
                )  is
      begin
         Internal (Credentials.Handle, Func'Access);
      end Set;
   end Anon_Server_Params_Function;

   procedure Anon_Set_Server_DH_Params
             (  Credentials : in out Anon_Server_Credentials;
                Params      : DH_Params'Class
             )  is
      procedure Internal
                (  Credentials : Address;
                   Params      : Address
                );
      pragma Import (C, Internal, "gnutls_anon_set_server_dh_params");
   begin
      Internal (Credentials.Handle, Params.Handle);
   end Anon_Set_Server_DH_Params;

   function Auth_Client_Get_Type
            (  Session : Session_Type
            )  return Credentials_Type is
      function Internal (Session : Address) return Credentials_Type;
      pragma Import (C, Internal, "gnutls_auth_client_get_type");
   begin
      return Internal (Session.Holder.Handle);
   end Auth_Client_Get_Type;

   function Auth_Get_Type
            (  Session : Session_Type
            )  return Credentials_Type is
      function Internal (Session : Address) return Credentials_Type;
      pragma Import (C, Internal, "gnutls_auth_get_type");
   begin
      return Internal (Session.Holder.Handle);
   end Auth_Get_Type;

   procedure Audit_Log_Function
             (  Session : in out Session_Type;
                Text    : String
             )  is
   begin
      null;
   end Audit_Log_Function;

   function Auth_Server_Get_Type
            (  Session : Session_Type
            )  return Credentials_Type is
      function Internal (Session : Address) return Credentials_Type;
      pragma Import (C, Internal, "gnutls_auth_server_get_type");
   begin
      return Internal (Session.Holder.Handle);
   end Auth_Server_Get_Type;

   function Bits_Are_Curve (Bits : unsigned) return Boolean is
   begin
      return 0 /= (16#80000000# and Bits);
   end Bits_Are_Curve;

   function Bits_To_Curve (Bits : unsigned) return ECC_Curve is
   begin
      return ECC_Curve'Val (16#7FFFFFFF# and Bits);
   end Bits_To_Curve;

   procedure Bye
             (  Session : in out Session_Type;
                How     : Close_Request
             )  is
      function Internal
               (  Session : Address;
                  How     : Close_Request
               )  return int;
      pragma Import (C, Internal, "gnutls_bye");
   begin
      Check (Internal (Session.Holder.Handle, How));
   end Bye;

   function Certificate_Activation_Time_Peers
            (  Session : Session_Type
            )  return Time is
      function Internal (Session : Address) return time_t;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_activation_time_peers"
             );
   begin
      return To_Time (Internal (Session.Holder.Handle));
   end Certificate_Activation_Time_Peers;

   function Certificate_Client_Get_Request_Status
            (  Session : Session_Type
            )  return Boolean is
      function Internal (Session : Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_client_get_request_status"
             );
   begin
      return Internal (Session.Holder.Handle) /= 0;
   end Certificate_Client_Get_Request_Status;

   function Certificate_Expiration_Time_Peers
            (  Session : Session_Type
            )  return Time is
      function Internal (Session : Address) return time_t;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_expiration_time_peers"
             );
   begin
      return To_Time (Internal (Session.Holder.Handle));
   end Certificate_Expiration_Time_Peers;

   procedure Certificate_Free_CA_Names
             (  Credentials : in out Certificate_Credentials
             )  is
      procedure Internal (Credentials : Address);
      pragma Import (C, Internal, "gnutls_certificate_free_ca_names");
   begin
      Internal (Credentials.Handle);
   end Certificate_Free_CA_Names;

   procedure Certificate_Free_CAs
             (  Credentials : in out Certificate_Credentials
             )  is
      procedure Internal (Credentials : Address);
      pragma Import (C, Internal, "gnutls_certificate_free_cas");
   begin
      Internal (Credentials.Handle);
   end Certificate_Free_CAs;

   procedure Certificate_Free_CRLs
             (  Credentials : in out Certificate_Credentials
             )  is
      procedure Internal (Credentials : Address);
      pragma Import (C, Internal, "gnutls_certificate_free_crls");
   begin
      Internal (Credentials.Handle);
   end Certificate_Free_CRLs;

   procedure Certificate_Free_Keys
             (  Credentials : in out Certificate_Credentials
             )  is
      procedure Internal (Credentials : Address);
      pragma Import (C, Internal, "gnutls_certificate_free_keys");
   begin
      Internal (Credentials.Handle);
   end Certificate_Free_Keys;

   function Certificate_Get_Crt_Raw
            (  Credentials : Certificate_Credentials;
               Idx1        : unsigned;
               Idx2        : unsigned
            )  return Stream_Element_Array is
      function Internal
               (  Credentials : Address;
                  Idx1        : unsigned;
                  Idx2        : unsigned;
                  Result      : access Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_certificate_get_crt_raw");
      Result : aliased Datum;
   begin
      Check (Internal (Credentials.Handle, Idx1, Idx2, Result'Access));
      return To_Stream_Element_Array (Result);
   end Certificate_Get_Crt_Raw;

   function Certificate_Get_Issuer
            (  Credentials : Certificate_Credentials;
               Certificate : X509_Crt
            )  return X509_Crt is
      function Internal
               (  Credentials : Address;
                  Certificate : X509_Crt;
                  Result      : access X509_Crt;
                  Flags       : unsigned
               )  return int;
      pragma Import (C, Internal, "gnutls_certificate_get_issuer");
      Result : aliased X509_Crt;
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            Certificate,
            Result'Access,
            0
      )  );
      return Result;
   end Certificate_Get_Issuer;

   function Certificate_Get_Ours
            (  Session : Session_Type
            )  return Stream_Element_Array is
      function Internal (Credentials : Address) return access Datum;
      pragma Import (C, Internal, "gnutls_certificate_get_ours");
   begin
      return To_Stream_Element_Array
             (  Internal (Session.Holder.Handle).all
             );
   end Certificate_Get_Ours;

   function Certificate_Get_Peers
            (  Session : Session_Type
            )  return Datum_Array is
      use Datum_Pointers;
      function Internal
               (  Session : Address;
                  Size    : access unsigned
               )  return Pointer;
      pragma Import (C, Internal, "gnutls_certificate_get_peers");
      Size   : aliased unsigned;
      List   : Pointer := Internal (Session.Holder.Handle, Size'Access);
      Result : Datum_Array (1..Positive (Size));
   begin
      for Index in Result'Range loop
         Result (Index) := List.all;
         List := List + 1;
      end loop;
      return Result;
   end Certificate_Get_Peers;

   function Certificate_Get_Peers_Subkey_ID
            (  Session : Session_Type
            )  return String is
      function Internal
               (  Credentials : Address;
                  ID          : access Datum
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_get_peers_subkey_id"
             );
      Result : aliased Datum;
   begin
      Check (Internal (Session.Holder.Handle, Result'Access));
      return To_String (Result);
   end Certificate_Get_Peers_Subkey_ID;

   procedure Certificate_Send_X509_RDN_Sequence
             (  Session : in out Session_Type;
                Status  : int
             )  is
      procedure Internal (Session : Address; Status : int);
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_send_x509_rdn_sequence"
             );
   begin
      Internal (Session.Holder.Handle, Status);
   end Certificate_Send_X509_RDN_Sequence;

   procedure Certificate_Server_Set_Request
             (  Session : in out Session_Type;
                Request : Certificate_Request
             )  is
      procedure Internal
                (  Session : Address;
                   Request : Certificate_Request
                );
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_server_set_request"
             );
   begin
      Internal (Session.Holder.Handle, Request);
   end Certificate_Server_Set_Request;

   procedure Certificate_Server_Set_DH_Params
             (  Credentials : in out Certificate_Credentials;
                Parameters  : DH_Params'Class
             )  is
      procedure Internal (Credentials : Address; Parameters : Address);
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_dh_params"
             );
   begin
      Internal (Credentials.Handle, Parameters.Handle);
   end Certificate_Server_Set_DH_Params;

   procedure Certificate_Set_DH_Params
             (  Credentials : in out Certificate_Credentials;
                Parameters  : DH_Params'Class
             )  is
      procedure Internal (Credentials : Address; Parameters : Address);
      pragma Import (C, Internal, "gnutls_certificate_set_dh_params");
   begin
      Internal (Credentials.Handle, Parameters.Handle);
   end Certificate_Set_DH_Params;

   procedure Certificate_Set_OCSP_Status_Request_File
             (  Credentials   : in out Certificate_Credentials;
                Response_File : String;
                Flags         : int := 0
             )  is
      function Internal
               (  Credentials   : Address;
                  Response_File : char_array;
                  Flags         : int
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_ocsp_status_request_file"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_C (Response_File),
            Flags
      )  );
   end Certificate_Set_OCSP_Status_Request_File;

   procedure Certificate_Server_Set_OpenPGP_Key
             (  Credentials : in out Certificate_Credentials;
                Public_Key  : OpenPGP_Crt;
                Private_Key : OpenPGP_Privkey
             )  is
      function Internal
               (  Credentials : Address;
                  Crt         : OpenPGP_Crt;
                  Key         : OpenPGP_Privkey
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_openpgp_key"
             );
   begin
      Check (Internal (Credentials.Handle, Public_Key, Private_Key));
   end Certificate_Server_Set_OpenPGP_Key;

   procedure Certificate_Server_Set_OpenPGP_Key_File
             (  Credentials  : in out Certificate_Credentials;
                Public_Keys  : String;
                Private_Keys : String;
                Format       : OpenPGP_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Crt         : char_array;
                  Key         : char_array;
                  Format      : OpenPGP_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_openpgp_key_file"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_C (Public_Keys),
            To_C (Private_Keys),
            Format
      )  );
   end Certificate_Server_Set_OpenPGP_Key_File;

   procedure Certificate_Server_Set_OpenPGP_Key_File
             (  Credentials  : in out Certificate_Credentials;
                Public_Keys  : String;
                Private_Keys : String;
                Subkey_ID    : String;
                Format       : OpenPGP_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Crt         : char_array;
                  Key         : char_array;
                  Subkey_ID   : char_array;
                  Format      : OpenPGP_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_openpgp_key_file2"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_C (Public_Keys),
            To_C (Private_Keys),
            To_C (Subkey_ID),
            Format
      )  );
   end Certificate_Server_Set_OpenPGP_Key_File;

   procedure Certificate_Server_Set_OpenPGP_Key_Mem
             (  Credentials  : in out Certificate_Credentials;
                Public_Keys  : Stream_Element_Array;
                Private_Keys : Stream_Element_Array;
                Format       : OpenPGP_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Crt         : Datum;
                  Key         : Datum;
                  Format      : OpenPGP_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_openpgp_key_mem"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_Datum (Public_Keys),
            To_Datum (Private_Keys),
            Format
      )  );
   end Certificate_Server_Set_OpenPGP_Key_Mem;

   procedure Certificate_Server_Set_OpenPGP_Key_Mem
             (  Credentials  : in out Certificate_Credentials;
                Public_Keys  : Stream_Element_Array;
                Private_Keys : Stream_Element_Array;
                Subkey_ID    : String;
                Format       : OpenPGP_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Crt         : Datum;
                  Key         : Datum;
                  Subkey_ID   : char_array;
                  Format      : OpenPGP_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_openpgp_key_mem2"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_Datum (Public_Keys),
            To_Datum (Private_Keys),
            To_C (Subkey_ID),
            Format
      )  );
   end Certificate_Server_Set_OpenPGP_Key_Mem;

   procedure Certificate_Server_Set_OpenPGP_Keyring_File
             (  Credentials : in out Certificate_Credentials;
                File        : String;
                Format      : OpenPGP_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  File        : char_array;
                  Format      : OpenPGP_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_openpgp_keyring_file"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_C (File),
            Format
      )  );
   end Certificate_Server_Set_OpenPGP_Keyring_File;

   procedure Certificate_Server_Set_OpenPGP_Mem
             (  Credentials : in out Certificate_Credentials;
                Data        : Stream_Element_Array;
                Format      : OpenPGP_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Data        : Address;
                  Length      : size_t;
                  Format      : OpenPGP_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_openpgp_keyring_mem"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            Data'Address,
            Data'Length,
            Format
      )  );
   end Certificate_Server_Set_OpenPGP_Mem;

   package body Certificate_Set_Params_Function is
      type Params_Function is access function
           (  Session    : Address;
              Type_Of    : Params_Type;
              Parameters : Params_Data
           )  return int;
      pragma Convention (C, Params_Function);
      procedure Internal
                (  Credentials : Address;
                   Func        : Params_Function
                );
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_params_function"
             );
      function Func
               (  Session    : Address;
                  Type_Of    : Params_Type;
                  Parameters : Params_Data
               )  return int;
      pragma Convention (C, Func);

      function Func
               (  Session    : Address;
                  Type_Of    : Params_Type;
                  Parameters : Params_Data
               )  return int is
         Parent : Session_Type (0);
      begin
         Parent.Holder.Handle := Session;
         Handle (Parent, Create (Type_Of, Parameters));
         return E_Success;
      exception
         when Error : others =>
            return Get_Error_Code (Error);
      end Func;

      procedure Set
                (  Credentials : in out Certificate_Credentials'Class
                )  is
      begin
         Internal (Credentials.Handle, Func'Access);
      end Set;
   end Certificate_Set_Params_Function;

   type Status_Request_OSCP_Func_Ptr is access function
        (  Session  : Address;
           Ptr      : Address;
           Response : access Datum
        )  return int;
   pragma Convention (C, Status_Request_OSCP_Func_Ptr);
   function Status_Request_OSCP_Func
            (  Session  : Address;
               Ptr      : Address;
               Response : access Datum
            )  return int;
   pragma Convention (C, Status_Request_OSCP_Func);

   function Status_Request_OSCP_Func
            (  Session  : Address;
               Ptr      : Address;
               Response : access Datum
            )  return int is
      package Conversions is
         new Address_To_Access_Conversions (OSCP_Status_Request'Class);
      use Conversions;
      Data : Object_Pointer := To_Pointer (Ptr);
   begin
      if Data = null then
         return E_Received_Illegal_Parameter;
      else
         declare
            Parent  : Session_Type (0);
            Request : OSCP_Status_Request'Class renames Data.all;
         begin
            Parent.Holder.Handle := Session;
            Handle (Request, Parent, Response.all);
            return E_Success;
         exception
            when Error : others =>
               return Get_Error_Code (Error);
         end;
      end if;
   end Status_Request_OSCP_Func;

   procedure Certificate_Set_OCSP_Status_Request_Function
             (  Credentials : in out Certificate_Credentials;
                Request     : in out OSCP_Status_Request'Class
             )  is
      procedure Internal
                (  Credentials : Address;
                   Func        : Status_Request_OSCP_Func_Ptr;
                   Data        : Address
                );
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_ocsp_status_request_function"
             );
   begin
      Internal
      (  Credentials.Handle,
         Status_Request_OSCP_Func'Access,
         Request'Address
      );
   end Certificate_Set_OCSP_Status_Request_Function;

   type PIN_Request_Func_Ptr is access function
        (  User_Data   : Address;
           Attempt     : int;
           Token_URL   : chars_ptr;
           Token_Label : chars_ptr;
           Flags       : int;
           PIN         : chars_ptr;
           PIN_Max     : size_t
        )  return int;
   pragma Convention (C, PIN_Request_Func_Ptr);
   function PIN_Request_Func
            (  User_Data   : Address;
               Attempt     : int;
               Token_URL   : chars_ptr;
               Token_Label : chars_ptr;
               Flags       : int;
               PIN         : chars_ptr;
               PIN_Max     : size_t
            )  return int;
   pragma Convention (C, PIN_Request_Func);

   function PIN_Request_Func
            (  User_Data   : Address;
               Attempt     : int;
               Token_URL   : chars_ptr;
               Token_Label : chars_ptr;
               Flags       : int;
               PIN         : chars_ptr;
               PIN_Max     : size_t
            )  return int is
      package Conversions is
         new Address_To_Access_Conversions (PIN_Request'Class);
      use Conversions;
      Data : Object_Pointer := To_Pointer (User_Data);
   begin
      if Data = null then
         return E_Received_Illegal_Parameter;
      else
         declare
            Result : String := Handle
                               (  Data.all'Access,
                                  Attempt,
                                  Value (Token_URL),
                                  Value (Token_Label),
                                  Flags
                               );
         begin
            if Result'Length >= PIN_Max then
               return E_Short_Memory_Buffer;
            else
               Update (PIN, 0, Result & Character'Val (0), False);
               return E_Success;
            end if;
         exception
            when Error : TLS_Error =>
               return Get_Error_Code (Error);
            when others =>
               return E_Internal_Error;
         end;
      end if;
   end PIN_Request_Func;

   procedure Certificate_Set_PIN_Function
             (  Credentials : in out Certificate_Credentials;
                PIN         : in out PIN_Request'Class
             )  is
      procedure Internal
                (  Credentials : Address;
                   Func        : PIN_Request_Func_Ptr;
                   Data        : Address
                );
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_pin_function"
             );
   begin
      Internal
      (  Credentials.Handle,
         PIN_Request_Func'Access,
         PIN'Address
      );
   end Certificate_Set_PIN_Function;

   package body Certificate_Set_Retrieve_Function is
      package PK_Algorithm_Pointers is
         new Interfaces.C.Pointers
             (  Positive,
                PK_Algorithm,
                PK_Algorithm_Array,
                PK_Unknown
             );
      use PK_Algorithm_Pointers;
      type Retrieve_Function is access function
           (  Session           : Address;
              Req_CA_RDN        : Datum;
              NReqs             : int;
              Algorithms        : Pointer;
              Algorithms_Length : int;
              Retrieve          : access Retr2
           )  return int;
      pragma Convention (C, Retrieve_Function);
      procedure Internal
                (  Credentials : Address;
                   Func        : Retrieve_Function
                );
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_retrieve_function"
             );
      function Func
               (  Session           : Address;
                  Req_CA_RDN        : Datum;
                  NReqs             : int;
                  Algorithms        : Pointer;
                  Algorithms_Length : int;
                  Retrieve          : access Retr2
               )  return int;
      pragma Convention (C, Func);

      function Func
               (  Session           : Address;
                  Req_CA_RDN        : Datum;
                  NReqs             : int;
                  Algorithms        : Pointer;
                  Algorithms_Length : int;
                  Retrieve          : access Retr2
               )  return int is
         Parent : Session_Type (0);
      begin
         Parent.Holder.Handle := Session;
         if Algorithms_Length  = 0 then
            Handle
            (  Parent,
               To_Stream_Element_Array (Req_CA_RDN),
               NReqs,
               (1..0 => PK_Unknown),
               Retrieve.all
            );
         else
            Handle
            (  Parent,
               To_Stream_Element_Array (Req_CA_RDN),
               NReqs,
               Value (Algorithms, ptrdiff_t (Algorithms_Length)),
               Retrieve.all
            );
         end if;
         return E_Success;
      exception
         when Error : others =>
            return Get_Error_Code (Error);
      end Func;

      procedure Set
                (  Credentials : in out Certificate_Credentials'Class
                )  is
      begin
         Internal (Credentials.Handle, Func'Access);
      end Set;
   end Certificate_Set_Retrieve_Function;

   procedure Certificate_Set_Verify_Flags
             (  Credentials : in out Certificate_Credentials;
                Flags       : unsigned
             )  is
      procedure Internal (Credentials : Address; Flags : unsigned);
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_verify_flags"
             );
   begin
      Internal (Credentials.Handle, Flags);
   end Certificate_Set_Verify_Flags;

   package body Certificate_Set_Verify_Function is
      type Verify_Function is access function
           (  Session : Address
           )  return int;
      pragma Convention (C, Verify_Function);
      procedure Internal
                (  Credentials : Address;
                   Func        : Verify_Function
                );
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_verify_function"
             );
      function Func (Session : Address) return int;
      pragma Convention (C, Func);

      function Func (Session : Address) return int is
         Parent : Session_Type (0);
      begin
         Parent.Holder.Handle := Session;
         Handle (Parent);
         return E_Success;
      exception
         when Error : others =>
            return Get_Error_Code (Error);
      end Func;

      procedure Set
                (  Credentials : in out Certificate_Credentials'Class
                )  is
      begin
         Internal (Credentials.Handle, Func'Access);
      end Set;
   end Certificate_Set_Verify_Function;

   procedure Certificate_Set_Verify_Limits
             (  Credentials : in out Certificate_Credentials;
                Max_Bits    : unsigned := 8200;
                Max_Depth   : unsigned := 5
             )  is
      procedure Internal
                (  Credentials : Address;
                   Max_Bits    : unsigned;
                   Max_Depth   : unsigned
                );
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_verify_limits"
             );
   begin
      Internal (Credentials.Handle, Max_Bits, Max_Depth);
   end Certificate_Set_Verify_Limits;

   procedure Certificate_Set_X509_CRL
             (  Credentials : in out Certificate_Credentials;
                List        : X509_Crl_Array
             )  is
      function Internal
               (  Credentials : Address;
                  List        : Address;
                  Size        : int
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_crl"
             );
      Result : int;
   begin
      Result :=
         Internal
         (  Credentials.Handle,
            List'Address,
            List'Length
         );
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
   end Certificate_Set_X509_CRL;

   procedure Certificate_Set_X509_CRL_File
             (  Credentials : in out Certificate_Credentials;
                File        : String;
                Format      : X509_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  File        : char_array;
                  Format      : X509_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_crl_file"
             );
      Result : int;
   begin
      Result := Internal (Credentials.Handle, To_C (File), Format);
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
   end Certificate_Set_X509_CRL_File;

   procedure Certificate_Set_X509_CRL_Mem
             (  Credentials : in out Certificate_Credentials;
                List        : Datum_Array;
                Format      : X509_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  List        : Address;
                  Size        : int;
                  Format      : X509_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_crl_mem"
             );
      Result : int;
   begin
      Result :=
         Internal
         (  Credentials.Handle,
            List'Address,
            List'Length,
            Format
         );
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
   end Certificate_Set_X509_CRL_Mem;

   procedure Certificate_Set_X509_CRL_Mem
             (  Credentials : in out Certificate_Credentials;
                Certificate : Stream_Element_Array;
                Format      : X509_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  List        : Address;
                  Size        : int;
                  Format      : X509_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_crl_mem"
             );
      List   : Datum_Array (1..1) := (1 => To_Datum (Certificate));
      Result : int;
   begin
      Result :=
         Internal
         (  Credentials.Handle,
            List'Address,
            List'Length,
            Format
         );
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
   end Certificate_Set_X509_CRL_Mem;

   procedure Certificate_Set_X509_Key
             (  Credentials : in out Certificate_Credentials;
                List        : X509_CRL_Array;
                Key         : X509_Privkey
             )  is
      function Internal
               (  Credentials : Address;
                  List        : Address;
                  Size        : int;
                  Key         : X509_Privkey
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_key"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            List'Address,
            List'Length,
            Key
      )  );
   end Certificate_Set_X509_Key;

   procedure Certificate_Set_X509_Key_File
             (  Credentials : in out Certificate_Credentials;
                Cert_File   : String;
                Key_File    : String;
                Format      : X509_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Cert_File   : char_array;
                  Key_File    : char_array;
                  Format      : X509_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_key_file"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_C (Cert_File),
            To_C (Key_File),
            Format
      )  );
   end Certificate_Set_X509_Key_File;

   procedure Certificate_Set_X509_Key_File
             (  Credentials : in out Certificate_Credentials;
                Cert_File   : String;
                Key_File    : String;
                Format      : X509_Crt_Fmt;
                Password    : String;
                Flags       : PKCS_Encrypt_Flags
             )  is
      function Internal
               (  Credentials : Address;
                  Cert_File   : char_array;
                  Key_File    : char_array;
                  Format      : X509_Crt_Fmt;
                  Password    : char_array;
                  Flags       : PKCS_Encrypt_Flags
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_key_file2"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_C (Cert_File),
            To_C (Key_File),
            Format,
            To_C (Password),
            Flags
      )  );
   end Certificate_Set_X509_Key_File;

   procedure Certificate_Set_X509_Key_Mem
             (  Credentials : in out Certificate_Credentials;
                Cert        : Stream_Element_Array;
                Key         : Stream_Element_Array;
                Format      : X509_Crt_Fmt;
                Password    : String;
                Flags       : PKCS_Encrypt_Flags
             )  is
      function Internal
               (  Credentials : Address;
                  Cert        : Datum;
                  Key         : Datum;
                  Format      : X509_Crt_Fmt;
                  Password    : char_array;
                  Flags       : PKCS_Encrypt_Flags
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_key_mem2"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_Datum (Cert),
            To_Datum (Key),
            Format,
            To_C (Password),
            Flags
      )  );
   end Certificate_Set_X509_Key_Mem;

   procedure Certificate_Set_X509_Key_Mem
             (  Credentials : in out Certificate_Credentials;
                Cert        : Stream_Element_Array;
                Format      : X509_Crt_Fmt;
                Password    : String;
                Flags       : PKCS_Encrypt_Flags
             )  is
      function Internal
               (  Credentials : Address;
                  Cert        : Datum;
                  Key         : Address;
                  Format      : X509_Crt_Fmt;
                  Password    : char_array;
                  Flags       : PKCS_Encrypt_Flags
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_key_mem2"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_Datum (Cert),
            Null_Address,
            Format,
            To_C (Password),
            Flags
      )  );
   end Certificate_Set_X509_Key_Mem;

   procedure Certificate_Set_X509_Key_Mem
             (  Credentials : in out Certificate_Credentials;
                Cert        : Stream_Element_Array;
                Key         : Stream_Element_Array;
                Format      : X509_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Cert        : Datum;
                  Key         : Datum;
                  Format      : X509_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_key_mem"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_Datum (Cert),
            To_Datum (Key),
            Format
      )  );
   end Certificate_Set_X509_Key_Mem;

   procedure Certificate_Set_X509_Key_Mem
             (  Credentials : in out Certificate_Credentials;
                Cert        : Stream_Element_Array;
                Format      : X509_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Cert        : Datum;
                  Key         : Address;
                  Format      : X509_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_key_mem"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_Datum (Cert),
            Null_Address,
            Format
      )  );
   end Certificate_Set_X509_Key_Mem;

   procedure Certificate_Set_X509_Simple_PKCS12_File
             (  Credentials : in out Certificate_Credentials;
                PKCS12_File : String;
                Format      : X509_Crt_Fmt;
                Password    : String
             )  is
      function Internal
               (  Credentials : Address;
                  PKCS12_File : char_array;
                  Format      : X509_Crt_Fmt;
                  Password    : char_array
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_simple_pkcs12_file"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_C (PKCS12_File),
            Format,
            To_C (Password)
      )  );
   end Certificate_Set_X509_Simple_PKCS12_File;

   procedure Certificate_Set_X509_Simple_PKCS12_Mem
             (  Credentials : in out Certificate_Credentials;
                P12blob     : Stream_Element_Array;
                Format      : X509_Crt_Fmt;
                Password    : String
             )  is
      function Internal
               (  Credentials : Address;
                  P12blob     : Datum;
                  Format      : X509_Crt_Fmt;
                  Password    : char_array
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_simple_pkcs12_mem"
             );
   begin
      Check
      (  Internal
         (  Credentials.Handle,
            To_Datum (P12blob),
            Format,
            To_C (Password)
      )  );
   end Certificate_Set_X509_Simple_PKCS12_Mem;

   procedure Certificate_Set_X509_System_Trust
             (  Credentials : in out Certificate_Credentials
             )  is
      function Internal (Credentials : Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_system_trust"
             );
   begin
      Check (Internal (Credentials.Handle));
   end Certificate_Set_X509_System_Trust;

   procedure Certificate_Set_X509_Trust
             (  Credentials : in out Certificate_Credentials;
                List        : X509_Crt_Array
             )  is
      function Internal
               (  Credentials : Address;
                  List        : Address;
                  Size        : int
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_trust"
             );
      Result : int;
   begin
      Result :=
         Internal
         (  Credentials.Handle,
            List'Address,
            List'Length
         );
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
   end Certificate_Set_X509_Trust;

--     procedure Certificate_Set_X509_Trust_Dir
--               (  Credentials : in out Certificate_Credentials;
--                  Directory   : String;
--                  Format      : X509_Crt_Fmt
--               )  is
--        function Internal
--                 (  Credentials : Address;
--                    Directory   : char_array;
--                    Format      : X509_Crt_Fmt
--                 )  return int;
--        pragma Import
--               (  C,
--                  Internal,
--                  "gnutls_certificate_set_x509_trust_dir"
--               );
--        Result : int;
--     begin
--        Result :=
--           Internal
--           (  Credentials.Handle,
--              To_C (Directory),
--              Format
--           );
--        if Result < 0 then
--           Raise_TLS_Error (Result);
--        end if;
--     end Certificate_Set_X509_Trust_Dir;

   procedure Certificate_Set_X509_Trust_File
             (  Credentials : in out Certificate_Credentials;
                File        : String;
                Format      : X509_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Files       : char_array;
                  Format      : X509_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_trust_file"
             );
      Result : int;
   begin
      Result :=
         Internal
         (  Credentials.Handle,
            To_C (File),
            Format
         );
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
   end Certificate_Set_X509_Trust_File;

   procedure Certificate_Set_X509_Trust_Mem
             (  Credentials : in out Certificate_Credentials;
                Certificate : Stream_Element_Array;
                Format      : X509_Crt_Fmt
             )  is
      function Internal
               (  Credentials : Address;
                  Certificate : Datum;
                  Format      : X509_Crt_Fmt
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_set_x509_trust_mem"
             );
      Result : int;
   begin
      Result :=
         Internal
         (  Credentials.Handle,
            To_Datum (Certificate),
            Format
         );
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
   end Certificate_Set_X509_Trust_Mem;

   function Certificate_Type_Get
            (  Session : Session_Type
            )  return Certificate_Type is
      function Internal (Session : Address) return Certificate_Type;
      pragma Import (C, Internal, "gnutls_certificate_type_get");
   begin
      return Internal (Session.Holder.Handle);
   end Certificate_Type_Get;

   function Certificate_Type_Get_ID
            (  Name : String
            )  return Certificate_Type is
      function Internal
               (  Name : char_array
               )  return Certificate_Type;
      pragma Import (C, Internal, "gnutls_certificate_type_get_id");
   begin
      return Internal (To_C (Name));
   end Certificate_Type_Get_ID;

   function Certificate_Type_Get_Name
            (  Type_Of : Certificate_Type
            )  return String is
      function Internal
               (  Type_Of : Certificate_Type
               )  return chars_ptr;
      pragma Import (C, Internal, "gnutls_certificate_type_get_name");
      Result : chars_ptr := Internal (Type_Of);
   begin
      if Result = Null_Ptr then
         Raise_Exception
         (  End_Error'Identity,
            "No certificate type matched"
         );
      else
         return Value (Result);
      end if;
   end Certificate_Type_Get_Name;

   function Certificate_Type_List return Certificate_Type_Array is
      use Certificate_Type_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_certificate_type_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => CRT_Unknown);
      else
         return Value (Result);
      end if;
   end Certificate_Type_List;

   function Certificate_Verification_Status_Print
            (  Status  : Certificate_Status;
               Type_Of : Certificate_Type
            )  return String is
      function Internal
               (  Status  : Certificate_Status;
                  Type_Of : Certificate_Type;
                  Output  : Datum;
                  Flags   : unsigned := 0
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_verification_status_print"
             );
      Output : Datum_Holder;
   begin
      Check (Internal (Status, Type_Of, Output.Data));
      return To_String (Output.Data);
   end Certificate_Verification_Status_Print;

   function Certificate_Verify_Peers
            (  Session   : Session_Type;
               Host_Name : String
            )  return Certificate_Status is
      function Internal
               (  Session   : Address;
                  Host_Name : char_array;
                  Status    : access unsigned
               )  return int;
      pragma Import (C, Internal, "gnutls_certificate_verify_peers3");
      Status : aliased unsigned;
   begin
      Check
      (  Internal
         (  Session.Holder.Handle,
            To_C (Host_Name),
            Status'Access
      )  );
      return Certificate_Status (Status);
   end Certificate_Verify_Peers;

   function Certificate_Verify_Peers (Session : Session_Type)
      return Certificate_Status is
      function Internal
               (  Session : Address;
                  Status  : access unsigned
               )  return int;
      pragma Import (C, Internal, "gnutls_certificate_verify_peers2");
      Status : aliased unsigned;
   begin
      Check (Internal (Session.Holder.Handle, Status'Access));
      return Certificate_Status (Status);
   end Certificate_Verify_Peers;

   function Check_Version (Version : String) return String is
      function Internal (Version : char_array) return chars_ptr;
      pragma Import (C, Internal, "gnutls_check_version");
      Result : chars_ptr := Internal (To_C (Version));
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No version matched");
      else
         return Value (Result);
      end if;
   end Check_Version;

   procedure Check (Error : int) is
   begin
      if Error < 0 then
         Raise_Exception
         (  TLS_Error'Identity,
            StrError (Error) & '[' & int'Image (Error) & ']'
         );
      end if;
   end Check;

   function Cipher_Get (Session : Session_Type)
      return Cipher_Algorithm is
      function Internal
               (  Session : Address
               )  return Cipher_Algorithm;
      pragma Import (C, Internal, "gnutls_cipher_get");
   begin
      return Internal (Session.Holder.Handle);
   end Cipher_Get;

   function Cipher_Get_ID (Name : String) return Cipher_Algorithm is
      function Internal
               (  Name : char_array
               )  return Cipher_Algorithm;
      pragma Import (C, Internal, "gnutls_cipher_get_id");
   begin
      return Internal (To_C (Name));
   end Cipher_Get_ID;

   function Cipher_Get_Name
            (  Algorithm : Cipher_Algorithm
            )  return String is
      function Internal
               (  Algorithm : Cipher_Algorithm
               )  return chars_ptr;
      pragma Import (C, Internal, "gnutls_cipher_get_name");
      Result : chars_ptr := Internal (Algorithm);
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No cipher matched");
      else
         return Value (Result);
      end if;
   end Cipher_Get_Name;

   function Cipher_List return Cipher_Algorithm_Array is
      use Cipher_Algorithm_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_cipher_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => Cipher_Unknown);
      else
         return Value (Result);
      end if;
   end Cipher_List;

   function Cipher_Suite_Get_Name
            (  KX     : KX_Algorithm;
               Cipher : Cipher_Algorithm;
               MAC    : MAC_Algorithm
            )  return String is
      function Internal
               (  KX     : KX_Algorithm;
                  Cipher : Cipher_Algorithm;
                  MAC    : MAC_Algorithm
               )  return chars_ptr;
      pragma Import (C, Internal, "gnutls_cipher_suite_get_name");
      Result : chars_ptr := Internal (KX, Cipher, MAC);
   begin
      if Result = Null_Ptr then
         Raise_Exception
         (  End_Error'Identity,
            "No cipher suite matched"
         );
      else
         return Value (Result);
      end if;
   end Cipher_Suite_Get_Name;

   function Compression_Get
            (  Session : Session_Type
            )  return Compression_Method is
      function Internal (Session : Address) return Compression_Method;
      pragma Import (C, Internal, "gnutls_compression_get");
   begin
      return Internal (Session.Holder.Handle);
   end Compression_Get;

   function Compression_Get_ID
            (  Name : String
            )  return Compression_Method is
      function Internal (Name : char_array) return Compression_Method;
      pragma Import (C, Internal, "gnutls_compression_get_id");
   begin
      return Internal (To_C (Name));
   end Compression_Get_ID;

   function Compression_Get_Name
            (  Algorithm : Compression_Method
            )  return String is
      function Internal
               (  Algorithm : Compression_Method
               )  return chars_ptr;
      pragma Import (C, Internal, "gnutls_compression_get_name");
      Result : chars_ptr := Internal (Algorithm);
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No cipher matched");
      else
         return Value (Result);
      end if;
   end Compression_Get_Name;

   function Compression_List return Compression_Method_Array is
      use Compression_Method_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_compression_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => Comp_Unknown);
      else
         return Value (Result);
      end if;
   end Compression_List;

   function Create
            (  Type_Of    : Params_Type;
               Parameters : Params_Data
            )  return Abstract_Params'Class is
   begin
      case Type_Of is
         when Params_RSA_Export =>
            return RSA_Params'
                   (  Ada.Finalization.Controlled with Parameters.Data
                   );
         when Params_DH =>
            if Parameters.Deinit = 0 then
               return DH_Params_Ref'
                      (  Ada.Finalization.Controlled
                      with
                         Parameters.Data
                      );
            else
               return DH_Params'
                      (  Ada.Finalization.Controlled
                      with
                         Parameters.Data
                      );
            end if;
         when Params_ECDH =>
            return ECDH_Params'
                   (  Ada.Finalization.Controlled with Parameters.Data
                   );
      end case;
   end Create;

   procedure Credentials_Clear (Session : in out Session_Type) is
      procedure Internal (Session : Address);
      pragma Import (C, Internal, "gnutls_credentials_clear");
   begin
      Internal (Session.Holder.Handle);
   end Credentials_Clear;

--     procedure Credentials_Get
--               (  Credentials : in out Anon_Client_Credentials_Ref;
--                  Session     : Session_Type
--               )  is
--        function Internal
--                 (  Session : Address;
--                    Type_Of : Credentials_Type;
--                    Cred    : access Address
--                 )  return int;
--        pragma Import (C, Internal, "gnutls_credentials_get");
--     begin
--        if 0 /= (Session.Flags and Init_Client) then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "Session is not a client"
--           );
--        end if;
--        Check
--        (  Internal
--           (  Session.Holder.Handle,
--              Crd_Anon,
--              Credentials.Handle'Access
--        )  );
--        if Credentials.Handle /= Null_Address then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "No anon credentials set"
--           );
--        end if;
--     end Credentials_Get;
--
--     procedure Credentials_Get
--               (  Credentials : in out Anon_Server_Credentials_Ref;
--                  Session     : Session_Type
--               )  is
--        function Internal
--                 (  Session : Address;
--                    Type_Of : Credentials_Type;
--                    Cred    : access Address
--                 )  return int;
--        pragma Import (C, Internal, "gnutls_credentials_get");
--     begin
--        if 0 /= (Session.Flags and Init_Server) then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "Session is not a server"
--           );
--        end if;
--        Check
--        (  Internal
--           (  Session.Holder.Handle,
--              Crd_Anon,
--              Credentials.Handle'Access
--        )  );
--        if Credentials.Handle /= Null_Address then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "No anon credentials set"
--           );
--        end if;
--     end Credentials_Get;
--
--     procedure Credentials_Get
--               (  Credentials : in out Certificate_Credentials_Ref;
--                  Session     : Session_Type
--               )  is
--        function Internal
--                 (  Session : Address;
--                    Type_Of : Credentials_Type;
--                    Cred    : access Address
--                 )  return int;
--        pragma Import (C, Internal, "gnutls_credentials_get");
--     begin
--        Check
--        (  Internal
--           (  Session.Holder.Handle,
--              Crd_Certificate,
--              Credentials.Handle'Access
--        )  );
--        if Credentials.Handle /= Null_Address then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "No certificate credentials set"
--           );
--        end if;
--     end Credentials_Get;
--
--     procedure Credentials_Get
--               (  Credentials : in out SRP_Client_Credentials_Ref;
--                  Session     : Session_Type
--               )  is
--        function Internal
--                 (  Session : Address;
--                    Type_Of : Credentials_Type;
--                    Cred    : access Address
--                 )  return int;
--        pragma Import (C, Internal, "gnutls_credentials_get");
--     begin
--        if 0 /= (Session.Flags and Init_Client) then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "Session is not a client"
--           );
--        end if;
--        Check
--        (  Internal
--           (  Session.Holder.Handle,
--              Crd_SRP,
--              Credentials.Handle'Access
--        )  );
--        if Credentials.Handle /= Null_Address then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "No SRP credentials set"
--           );
--        end if;
--     end Credentials_Get;
--
--     procedure Credentials_Get
--               (  Credentials : in out SRP_Server_Credentials_Ref;
--                  Session     : Session_Type
--               )  is
--        function Internal
--                 (  Session : Address;
--                    Type_Of : Credentials_Type;
--                    Cred    : access Address
--                 )  return int;
--        pragma Import (C, Internal, "gnutls_credentials_get");
--     begin
--        if 0 /= (Session.Flags and Init_Server) then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "Session SRP is not a server"
--           );
--        end if;
--        Check
--        (  Internal
--           (  Session.Holder.Handle,
--              Crd_SRP,
--              Credentials.Handle'Access
--        )  );
--        if Credentials.Handle /= Null_Address then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "No credentials set"
--           );
--        end if;
--     end Credentials_Get;

   procedure Credentials_Set
             (  Session     : in out Session_Type;
                Credentials : Anon_Client_Credentials
             )  is
      function Internal
               (  Session : Address;
                  Type_Of : Credentials_Type;
                  Cred    : Address
               )  return int;
      pragma Import (C, Internal, "gnutls_credentials_set");
   begin
      if Credentials.Handle = Null_Address then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid credentials"
         );
      end if;
      Check (Internal (Session.Holder.Handle, Crd_Anon, Credentials.Handle));
   end Credentials_Set;

   procedure Credentials_Set
             (  Session     : in out Session_Type;
                Credentials : Anon_Server_Credentials
             )  is
      function Internal
               (  Session : Address;
                  Type_Of : Credentials_Type;
                  Cred    : Address
               )  return int;
      pragma Import (C, Internal, "gnutls_credentials_set");
   begin
      if Credentials.Handle = Null_Address then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid credentials"
         );
      end if;
      Check
      (  Internal
         (  Session.Holder.Handle,
            Crd_Anon,
            Credentials.Handle
      )  );
   end Credentials_Set;

   procedure Credentials_Set
             (  Session     : in out Session_Type;
                Credentials : Certificate_Credentials
             )  is
      function Internal
               (  Session : Address;
                  Type_Of : Credentials_Type;
                  Cred    : Address
               )  return int;
      pragma Import (C, Internal, "gnutls_credentials_set");
   begin
      if Credentials.Handle = Null_Address then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid credentials"
         );
      end if;
      Check
      (  Internal
         (  Session.Holder.Handle,
            Crd_Certificate,
            Credentials.Handle
      )  );
   end Credentials_Set;

--     procedure Credentials_Set
--               (  Session     : in out Session_Type;
--                  Credentials : SRP_Client_Credentials
--               )  is
--        function Internal
--                 (  Session : Address;
--                    Type_Of : Credentials_Type;
--                    Cred    : Address
--                 )  return int;
--        pragma Import (C, Internal, "gnutls_credentials_set");
--     begin
--        if Credentials.Handle = Null_Address then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "Invalid credentials"
--           );
--        end if;
--        Check (Internal (Session.Holder.Handle, Crd_SRP, Credentials.Handle));
--     end Credentials_Set;
--
--     procedure Credentials_Set
--               (  Session     : in out Session_Type;
--                  Credentials : SRP_Server_Credentials
--               )  is
--        function Internal
--                 (  Session : Address;
--                    Type_Of : Credentials_Type;
--                    Cred    : Address
--                 )  return int;
--        pragma Import (C, Internal, "gnutls_credentials_set");
--     begin
--        if Credentials.Handle = Null_Address then
--           Raise_Exception
--           (  Constraint_Error'Identity,
--              "Invalid credentials"
--           );
--        end if;
--        Check (Internal (Session.Holder.Handle, Crd_SRP, Credentials.Handle));
--     end Credentials_Set;

   function Curve_To_Bits (Curve : ECC_Curve) return unsigned is
   begin
      return 2**31 or ECC_Curve'Pos (Curve);
   end Curve_To_Bits;

   function DB_Check_Entry
            (  Session       : Session_Type;
               Session_Entry : Stream_Element_Array
            )  return Boolean is
      function Internal
               (  Session       : Address;
                  Session_Entry : Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_db_check_entry");
      Result : int := Internal
                      (  Session.Holder.Handle,
                         To_Datum (Session_Entry)
                      );
   begin
      case Result is
         when E_Success =>
            return True;
         when E_Expired =>
            return False;
         when others =>
            Raise_TLS_Error (Result);
            return False;
      end case;
   end DB_Check_Entry;

   function DB_Check_Entry_Time
            (  DB_Entry : Stream_Element_Array
            )  return Time is
      function Internal (DB_Entry : Datum) return time_t;
      pragma Import (C, Internal, "gnutls_db_check_entry_time");
      Result : time_t := Internal (To_Datum (DB_Entry));
   begin
      if Result = 0 then
         Raise_Exception
         (  Time_Error'Identity,
            "Error database entry time"
         );
      end if;
      return To_Time (Result);
   end DB_Check_Entry_Time;

   function DB_Get_Default_Cache_Expiration return Duration is
      function Internal return unsigned;
      pragma Import
             (  C,
                Internal,
                "gnutls_db_get_default_cache_expiration"
             );
   begin
      return Duration (Internal);
   end DB_Get_Default_Cache_Expiration;

   procedure DB_Remove_Session (Session : in out Session_Type) is
      procedure Internal (Session : Address);
      pragma Import (C, Internal, "gnutls_db_remove_session");
   begin
      Internal (Session.Holder.Handle);
   end DB_Remove_Session;

   procedure DB_Set_Cache_Expiration
             (  Session : in out Session_Type;
                Timeout : Duration
             )  is
      procedure Internal (Session : Address; Seconds : unsigned);
      pragma Import (C, Internal, "gnutls_db_set_cache_expiration");
   begin
      if Timeout < 1.0 then
         Raise_Exception
         (  Time_Error'Identity,
            "Timeout is too small"
         );
      end if;
      Internal (Session.Holder.Handle, unsigned (Timeout));
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Time_Error'Identity,
            "Timeout is too large"
         );
   end DB_Set_Cache_Expiration;

   package DB_Conversions is
      new Address_To_Access_Conversions (DB_Request'Class);

   type DB_Remove_Func_Ptr is access function
        (  Request : Address;
           Key     : Datum
        )  return int;
   pragma Convention (C, DB_Remove_Func_Ptr);

   function DB_Remove_Func
            (  Request : Address;
               Key     : Datum
            )  return int;
   pragma Convention (C, DB_Remove_Func);

   function DB_Remove_Func
            (  Request : Address;
               Key     : Datum
            )  return int is
      use DB_Conversions;
      Data : Object_Pointer := To_Pointer (Request);
   begin
      if Data = null then
         return E_Internal_Error;
      else
         Remove (Data.all, To_Stream_Element_Array (Key));
         return E_Success;
      end if;
   exception
      when Error : others =>
         return Get_Error_Code (Error);
   end DB_Remove_Func;

   type DB_Retrieve_Func_Ptr is access function
        (  Request : Address;
           Key     : Datum
        )  return Datum;
   pragma Convention (C, DB_Retrieve_Func_Ptr);

   function DB_Retrieve_Func
            (  Request : Address;
               Key     : Datum
            )  return Datum;
   pragma Convention (C, DB_Retrieve_Func);

   function DB_Retrieve_Func
            (  Request : Address;
               Key     : Datum
            )  return Datum is
      use DB_Conversions;
      Data : Object_Pointer := To_Pointer (Request);
   begin
      if Data = null then
         return (Null_Address, 0);
      else
         return New_Datum
                (  Retrieve (Data, To_Stream_Element_Array (Key))
                );
      end if;
   exception
      when Error : others =>
         return (Null_Address, 0);
   end DB_Retrieve_Func;

   type DB_Store_Func_Ptr is access function
        (  Request : Address;
           Key     : Datum
        )  return int;
   pragma Convention (C, DB_Store_Func_Ptr);

   function DB_Store_Func
            (  Request : Address;
               Key     : Datum
            )  return int;
   pragma Convention (C, DB_Store_Func);

   function DB_Store_Func
            (  Request : Address;
               Key     : Datum
            )  return int is
      use DB_Conversions;
      Data : Object_Pointer := To_Pointer (Request);
   begin
      if Data = null then
         return E_Internal_Error;
      else
         Store (Data.all, To_Stream_Element_Array (Key));
         return E_Success;
      end if;
   exception
      when Error : others =>
         return Get_Error_Code (Error);
   end DB_Store_Func;

   procedure DB_Set_Functions
             (  Session : in out Session_Type;
                Request : access DB_Request'Class
             )  is
      procedure Set_Remove
                (  Session : Address;
                   Ptr     : DB_Remove_Func_Ptr
                );
      pragma Import (C, Set_Remove, "gnutls_db_set_remove_function");

      procedure Set_Retrieve
                (  Session : Address;
                   Ptr     : DB_Retrieve_Func_Ptr
                );
      pragma Import
             (  C,
                Set_Retrieve,
                "gnutls_db_set_retrieve_function"
             );

      procedure Set_Ptr (Session : Address; Ptr : Address);
      pragma Import (C, Set_Ptr, "gnutls_db_set_ptr");

      procedure Set_Store (Session : Address; Ptr : DB_Store_Func_Ptr);
      pragma Import (C, Set_Store, "gnutls_db_set_store_function");
   begin
      Set_Ptr      (Session.Holder.Handle, Request.all'Address);
      Set_Remove   (Session.Holder.Handle, DB_Remove_Func'Access);
      Set_Retrieve (Session.Holder.Handle, DB_Retrieve_Func'Access);
      Set_Store    (Session.Holder.Handle, DB_Store_Func'Access);
   end DB_Set_Functions;

   procedure DB_Get_Group
             (  Session   : Session_Type;
                Raw_Gen   : in out Datum_Holder'Class;
                Raw_Prime : in out Datum_Holder'Class
             )  is
      function Internal
               (  Session   : Address;
                  Raw_Gen   : access Datum;
                  Raw_Prime : access Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_dh_get_group");
   begin
      Finalize (Raw_Gen);
      Finalize (Raw_Prime);
      Check
      (  Internal
         (  Session.Holder.Handle,
            Raw_Gen.Data'Access,
            Raw_Prime.Data'Access
      )  );
   end DB_Get_Group;

   function DB_Get_Peers_Public_Bits
            (  Session : Session_Type
            )  return Natural is
      function Internal (Session : Address) return int;
      pragma Import (C, Internal, "gnutls_dh_get_peers_public_bits");
      Result : int := Internal (Session.Holder.Handle);
   begin
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
      return Natural (Result);
   end DB_Get_Peers_Public_Bits;

   function DB_Get_Prime_Bits (Session : Session_Type) return Natural is
      function Internal (Session : Address) return int;
      pragma Import (C, Internal, "gnutls_dh_get_prime_bits");
      Result : int := Internal (Session.Holder.Handle);
   begin
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
      return Natural (Result);
   end DB_Get_Prime_Bits;

   procedure DB_Get_Pubkey
             (  Session : Session_Type;
                Raw_Key : in out Datum_Holder'Class
             )  is
      function Internal
               (  Session : Address;
                  Raw_Key : access Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_dh_get_pubkey");
   begin
      Finalize (Raw_Key);
      Check (Internal (Session.Holder.Handle, Raw_Key.Data'Access));
   end DB_Get_Pubkey;

   function DB_Get_Secret_Bits (Session : Session_Type)
      return Natural is
      function Internal (Session : Address) return int;
      pragma Import (C, Internal, "gnutls_dh_get_secret_bits");
      Result : int := Internal (Session.Holder.Handle);
   begin
      if Result < 0 then
         Raise_TLS_Error (Result);
      end if;
      return Natural (Result);
   end DB_Get_Secret_Bits;

   procedure DH_Params_Export2_PKCS3
             (  Params : DH_Params;
                Format : X509_Crt_Fmt;
                Result : in out Datum_Holder'Class
             )  is
      function Internal
               (  Params : Address;
                  Format : X509_Crt_Fmt;
                  Result : access Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_dh_params_export2_pkcs3");
   begin
      Finalize (Result);
      Check (Internal (Params.Handle, Format, Result.Data'Access));
   end DH_Params_Export2_PKCS3;

   procedure DH_Params_Export_PKCS3
             (  Params  : DH_Params;
                Format  : X509_Crt_Fmt;
                Buffer  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      function Internal
               (  Params : Address;
                  Format : X509_Crt_Fmt;
                  Buffer : Address;
                  Size   : access size_t
               )  return int;
      pragma Import (C, Internal, "gnutls_dh_params_export_pkcs3");
      Size   : aliased size_t;
      Result : int;
   begin
      if Pointer not in Buffer'First..Buffer'Last then
         Raise_Exception
         (  Layout_Error'Identity,
            "Illegal pointer, out of the buffer"
         );
      end if;
      Size := size_t (Buffer'Last - Pointer + 1);
      Result :=
         Internal
         (  Params.Handle,
            Format,
            Buffer (Pointer)'Address,
            Size'Access
         );
      if Result >= 0 then
         Pointer := Pointer + Stream_Element_Count (Size - 1);
      elsif Result = E_Short_Memory_Buffer then
         Raise_Exception
         (  Layout_Error'Identity,
            "No room for output"
         );
      else
         Raise_TLS_Error (Result);
      end if;
   end DH_Params_Export_PKCS3;

   procedure DH_Params_Export_Raw
             (  Params    : DH_Params;
                Prime     : in out Datum_Holder'Class;
                Generator : in out Datum_Holder'Class
             )  is
      Bits : Natural;
   begin
      DH_Params_Export_Raw (Params, Prime, Generator, Bits);
   end DH_Params_Export_Raw;

   procedure DH_Params_Export_Raw
             (  Params    : DH_Params;
                Prime     : in out Datum_Holder'Class;
                Generator : in out Datum_Holder'Class;
                Bits      : out Natural
             )  is
      function Internal
               (  Params    : Address;
                  Prime     : Datum;
                  Generator : Datum;
                  Bits      : access unsigned
               )  return int;
      pragma Import (C, Internal, "gnutls_dh_params_export_raw");
      Size : aliased unsigned;
   begin
      Check
      (  Internal
         (  Params.Handle,
            Prime.Data,
            Generator.Data,
            Size'Access
      )  );
      Bits := Natural (Size);
   end DH_Params_Export_Raw;

   procedure DH_Params_Generate2
             (  Params : in out DH_Params;
                Bits   : Positive
             )  is
      function Internal (Params : Address; Bits : unsigned) return int;
      pragma Import (C, Internal, "gnutls_dh_params_generate2");
   begin
      Check (Internal (Params.Handle,  unsigned (Bits)));
   end DH_Params_Generate2;

   procedure DH_Params_Import_PKCS3
             (  Params       : in out DH_Params;
                PKCS3_Params : Stream_Element_Array;
                Format       : X509_Crt_Fmt
             )  is
      function Internal
               (  Params       : Address;
                  PKCS3_Params : Datum;
                  Format       : X509_Crt_Fmt
               )  return int;
      pragma Import (C, Internal, "gnutls_dh_params_import_pkcs3");
   begin
      Check (Internal (Params.Handle, To_Datum (PKCS3_Params), Format));
   end DH_Params_Import_PKCS3;

   procedure DH_Params_Import_Raw
             (  Params    : in out DH_Params;
                Prime     : Stream_Element_Array;
                Generator : Stream_Element_Array
             )  is
      function Internal
               (  Params    : Address;
                  Prime     : Datum;
                  Generator : Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_dh_params_import_raw");
   begin
      Check
      (  Internal
         (  Params.Handle,
            To_Datum (Prime),
            To_Datum (Generator)
      )  );
   end DH_Params_Import_Raw;

   procedure DB_Set_Prime_Bits
             (  Session : in out Session_Type;
                Bits    : Positive
             )  is
      procedure Internal (Session : Address; Bits : unsigned);
      pragma Import (C, Internal, "gnutls_dh_set_prime_bits");
   begin
      Internal (Session.Holder.Handle, unsigned (Bits));
   end DB_Set_Prime_Bits;

   function Digest_Get_ID (Name : String) return Digest_Algorithm is
      function Internal (Name : char_array) return Digest_Algorithm;
      pragma Import (C, Internal, "gnutls_digest_get_id");
   begin
      return Internal (To_C (Name));
   end Digest_Get_ID;

   function Digest_Get_Name
            (  Algorithm : Digest_Algorithm
            )  return String is
      function Internal (Algorithm : Digest_Algorithm) return chars_ptr;
      pragma Import (C, Internal, "gnutls_digest_get_name");
      Result : chars_ptr := Internal (Algorithm);
   begin
      if Result = Null_Ptr then
         Raise_Exception
         (  End_Error'Identity,
            "No digest algorithm matched"
         );
      end if;
      return Value (Result);
   end Digest_Get_Name;

   function Digest_List return Digest_Algorithm_Array is
      use Digest_Algorithm_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_digest_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => Dig_Unknown);
      else
         return Value (Result);
      end if;
   end Digest_List;

   function ECC_Curve_Get (Session : Session_Type) return ECC_Curve is
      function Internal (Session : Address) return ECC_Curve;
      pragma Import (C, Internal, "gnutls_ecc_curve_get");
   begin
      return Internal (Session.Holder.Handle);
   end ECC_Curve_Get;

   function ECC_Curve_Get_Name (Curve : ECC_Curve) return String is
      function Internal (Curve : ECC_Curve) return chars_ptr;
      pragma Import (C, Internal, "gnutls_ecc_curve_get_name");
      Result : chars_ptr := Internal (Curve);
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No ECC curve matched");
      end if;
      return Value (Result);
   end ECC_Curve_Get_Name;

   function ECC_Curve_Get_Size (Curve : ECC_Curve) return Natural is
      function Internal (Curve : ECC_Curve) return int;
      pragma Import (C, Internal, "gnutls_ecc_curve_get_size");
   begin
      return Natural (Internal (Curve));
   end ECC_Curve_Get_Size;

   function ECC_Curve_List return ECC_Curve_Array is
      use ECC_Curve_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_ecc_curve_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => ECC_Curve_Invalid);
      else
         return Value (Result);
      end if;
   end ECC_Curve_List;

   function Error_Is_Fatal (Error : int) return Boolean is
      function Internal (Error : int) return int;
      pragma Import (C, Internal, "gnutls_error_is_fatal");
   begin
      return Internal (Error) /= 0;
   end Error_Is_Fatal;

   procedure Finalize (Data : in out Datum_Holder) is
      procedure Internal (Ptr : Address);
      pragma Import (C, Internal, "gnutls_free");
   begin
      if Data.Data.Data /= Null_Address then
         Internal (Data.Data.Data);
         Data.Data.Data := Null_Address;
      end if;
      Data.Data.Size := 0;
   end Finalize;

   procedure Finalize (Credentials : in out Anon_Client_Credentials) is
      function Internal (Ptr : Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_anon_free_client_credentials"
             );
   begin
      if Credentials.Handle /= Null_Address then
         Check (Internal (Credentials.Handle));
         Credentials.Handle := Null_Address;
      end if;
   end Finalize;

   procedure Finalize
             (  Credentials : in out Anon_Client_Credentials_Ref
             )  is
   begin
      null;
   end Finalize;

   procedure Finalize (Credentials : in out Anon_Server_Credentials) is
      function Internal (Ptr : Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_anon_free_server_credentials"
             );
   begin
      if Credentials.Handle /= Null_Address then
         Check (Internal (Credentials.Handle));
         Credentials.Handle := Null_Address;
      end if;
   end Finalize;

   procedure Finalize
             (  Credentials : in out Anon_Server_Credentials_Ref
             )  is
   begin
      null;
   end Finalize;

   procedure Finalize (Session : in out Session_Holder) is
      procedure Internal (Ptr : Address);
      pragma Import (C, Internal, "gnutls_deinit");
   begin
      if Session.Handle /= Null_Address and then Session.Flags /= 0 then
         Internal (Session.Handle);
         Session.Handle := Null_Address;
      end if;
   end Finalize;

   procedure Finalize (Credentials : in out Certificate_Credentials) is
      function Internal (Ptr : Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_free_credentials"
             );
   begin
     if Credentials.Handle /= Null_Address then
        Check (Internal (Credentials.Handle));
        Credentials.Handle := Null_Address;
     end if;
   end Finalize;

   procedure Finalize
             (  Credentials : in out Certificate_Credentials_Ref
             )  is
   begin
      null;
   end Finalize;

   procedure Finalize (Data : in out DH_Params) is
      procedure Internal (Ptr : Address);
      pragma Import (C, Internal, "gnutls_dh_params_deinit");
   begin
      if Data.Handle /= Null_Address then
         Internal (Data.Handle);
         Data.Handle := Null_Address;
      end if;
   end Finalize;

   procedure Finalize (Data : in out DH_Params_Ref) is
   begin
      null;
   end Finalize;

   procedure Finalize (Data : in out ECDH_Params) is
   begin
      null;
   end Finalize;

   procedure Finalize (Data : in out RSA_Params) is
   begin
      null;
   end Finalize;

--     procedure Finalize (Credentials : in out SRP_Client_Credentials) is
--        function Internal (Ptr : Address) return int;
--        pragma Import
--               (  C,
--                  Internal,
--                  "gnutls_srp_free_client_credentials"
--               );
--     begin
--        if Credentials.Handle /= Null_Address then
--           Check (Internal (Credentials.Handle));
--           Credentials.Handle := Null_Address;
--        end if;
--     end Finalize;
--
--     procedure Finalize
--               (  Credentials : in out SRP_Client_Credentials_Ref
--               )  is
--     begin
--        null;
--     end Finalize;
--
--     procedure Finalize (Credentials : in out SRP_Server_Credentials) is
--        function Internal (Ptr : Address) return int;
--        pragma Import
--               (  C,
--                  Internal,
--                  "gnutls_srp_free_server_credentials"
--               );
--     begin
--        if Credentials.Handle /= Null_Address then
--           Check (Internal (Credentials.Handle));
--           Credentials.Handle := Null_Address;
--        end if;
--     end Finalize;
--
--     procedure Finalize
--               (  Credentials : in out SRP_Server_Credentials_Ref
--               )  is
--     begin
--        null;
--     end Finalize;

   procedure Finalize (Cache : in out Priority) is
      procedure Internal (Cache : Address);
      pragma Import (C, Internal, "gnutls_priority_deinit");
   begin
      if Cache.Handle /= Null_Address then
         Internal (Cache.Handle);
         Cache.Handle := Null_Address;
      end if;
   end Finalize;

   function Fingerprint
            (  Algorithm : Digest_Algorithm;
               Data      : Stream_Element_Array
            )  return Stream_Element_Array is
      function Internal
               (  Algorithm : Digest_Algorithm;
                  Data      : Datum;
                  Result    : access Datum;
                  Size      : access size_t
               )  return int;
      pragma Import (C, Internal, "gnutls_fingerprint");
      Result   : aliased Datum_Holder;
      Size     : aliased size_t;
   begin
      Check
      (  Internal
         (  Algorithm,
            To_Datum (Data),
            Result.Data'Access,
            Size'Access
      )  );
      return To_Stream_Element_Array (Result.Data);
   end Fingerprint;

   function Get_Error_Code (Error : Exception_Occurrence) return int is
   begin
      if Exception_Identity (Error) = TLS_Error'Identity then
         declare
            Text : String := Exception_Message (Error);
         begin
            for Index in reverse Text'Range loop
               if Text (Index) = '[' then
                  return int'Value (Text (Index + 1..Text'Last));
               end if;
            end loop;
         exception
            when others =>
               null;
         end;
      end if;
      return E_Internal_Error;
   end Get_Error_Code;

   package body Global_Set_Audit_Log_Function is
      type Audit_Log_Func_Ptr is access procedure
           (  Session : Address;
              Text    : chars_ptr
           );
      pragma Convention (C, Audit_Log_Func_Ptr);

      procedure Audit_Log_Func (Session : Address; Text : chars_ptr);
      pragma Convention (C, Audit_Log_Func);

      procedure Audit_Log_Func (Session : Address; Text : chars_ptr) is
      begin
         if Session = Null_Address then
            Log (Value (Text));
         else
            declare
               Current : Session_Type (0);
            begin
               Current.Holder.Handle := Session;
               Log (Current, Value (Text));
            end;
         end if;
      end Audit_Log_Func;

      procedure Set is
         procedure Internal (Func : Audit_Log_Func_Ptr);
         pragma Import
                (  C,
                   Internal,
                   "gnutls_global_set_audit_log_function"
                );
      begin
         Internal (Audit_Log_Func'Access);
      end Set;
   end Global_Set_Audit_Log_Function;

   package body Global_Set_Log_Function is
      type Log_Func_Ptr is access procedure
           (  Level : int;
              Text  : chars_ptr
           );
      pragma Convention (C, Log_Func_Ptr);

      procedure Log_Func (Level : int; Text : chars_ptr);
      pragma Convention (C, Log_Func);

      procedure Log_Func (Level : int; Text : chars_ptr) is
      begin
         Log (Level, Value (Text));
      end Log_Func;

      procedure Set (Level : int) is
         procedure Set_Func (Func : Log_Func_Ptr);
         pragma Import
                (  C,
                   Set_Func,
                   "gnutls_global_set_log_function"
                );
         procedure Set_Level (Level : int);
         pragma Import
                (  C,
                   Set_Level,
                   "gnutls_global_set_log_level"
                );
      begin
         Set_Level (Level);
         Set_Func  (Log_Func'Access);
      end Set;
   end Global_Set_Log_Function;

   function Handshake (Session : Session_Type) return Boolean is
      function Internal (Session : Address) return int;
      pragma Import (C, Internal, "gnutls_handshake");
      Code : int := Internal (Session.Holder.Handle);
   begin
      case Code is
         when 0..int'Last =>
            return False;
         when E_Again |
              E_Got_Application_Data |
              E_Interrupted |
              E_Warning_Alert_Received =>
            return True;
         when others =>
            Raise_TLS_Error (Code);
            return True;
      end case;
   end Handshake;

   function Handshake_Description_Get_Name
            (  Description : Handshake_Description
            )  return String is
      function Internal
               (  Description : Handshake_Description
               )  return chars_ptr;
      pragma Import
             (  C,
                Internal,
                "gnutls_handshake_description_get_name"
             );
      Result : chars_ptr := Internal (Description);
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No description matched");
      else
         return Value (Result);
      end if;
   end Handshake_Description_Get_Name;

   function Handshake_Get_Last_In
            (  Session : Session_Type
            )  return Handshake_Description is
      function Internal (Session : Address)
         return Handshake_Description;
      pragma Import (C, Internal, "gnutls_handshake_get_last_in");
   begin
      return Internal (Session.Holder.Handle);
   end Handshake_Get_Last_In;

   function Handshake_Get_Last_Out
            (  Session : Session_Type
            )  return Handshake_Description is
      function Internal (Session : Address)
         return Handshake_Description;
      pragma Import (C, Internal, "gnutls_handshake_get_last_out");
   begin
      return Internal (Session.Holder.Handle);
   end Handshake_Get_Last_Out;

   package body Handshake_Set_Hook_Function is
      type Handshake_Hook_Ptr is access function
           (  Session  : Address;
              Message  : Handshake_Description;
              Post     : Hook_Type;
              Incoming : unsigned
           )  return int;
      pragma Convention (C, Handshake_Hook_Ptr);

      function Handshake_Hook
               (  Session  : Address;
                  Message  : Handshake_Description;
                  Post     : Hook_Type;
                  Incoming : unsigned
               )  return int;
      pragma Convention (C, Handshake_Hook);

      function Handshake_Hook
               (  Session  : Address;
                  Message  : Handshake_Description;
                  Post     : Hook_Type;
                  Incoming : unsigned
               )  return int is
         Current : Session_Type (0);
      begin
         Current.Holder.Handle := Session;
         Hook (Current, Message, Post, Incoming);
         return E_Success;
      exception
         when Error : others =>
            return Get_Error_Code (Error);
      end Handshake_Hook;

      procedure Set
                (  Session : in out Session_Type;
                   Message : Handshake_Description;
                   Post    : Hook_Type
                )  is
         procedure Internal
                   (  Session : Address;
                      Message : Handshake_Description;
                      Post    : Hook_Type;
                      Hook    : Handshake_Hook_Ptr
                   );
         pragma Import
                (  C,
                   Internal,
                   "gnutls_handshake_set_hook_function"
                );
      begin
         Internal
         (  Session.Holder.Handle,
            Message,
            Post,
            Handshake_Hook'Access
         );
      end Set;
   end Handshake_Set_Hook_Function;

   procedure Handshake_Set_Max_Packet_Length
             (  Session : in out Session_Type;
                Max     : size_t
             )  is
      procedure Internal (Session : Address; Max : size_t);
      pragma Import
             (  C,
                Internal,
                "gnutls_handshake_set_max_packet_length"
             );
   begin
      Internal (Session.Holder.Handle, Max);
   end Handshake_Set_Max_Packet_Length;

   package body Handshake_Set_Post_Client_Hello_Function is
      type Handshake_Hook_Ptr is access function
           (  Session : Address
           )  return int;
      pragma Convention (C, Handshake_Hook_Ptr);

      function Handshake_Hook (Session : Address) return int;
      pragma Convention (C, Handshake_Hook);

      function Handshake_Hook (Session : Address) return int is
         Current : Session_Type (0);
         Hold    : Boolean;
      begin
         Current.Holder.Handle := Session;
         Hello (Current, Hold);
         if Hold then
            return E_Interrupted;
         else
            return E_Success;
         end if;
      exception
         when Error : others =>
            return Get_Error_Code (Error);
      end Handshake_Hook;

      procedure Set (Session : in out Session_Type) is
         procedure Internal
                   (  Session : Address;
                      Hook    : Handshake_Hook_Ptr
                   );
         pragma Import
                (  C,
                   Internal,
                   "gnutls_handshake_set_post_client_hello_function"
                );
      begin
         Internal (Session.Holder.Handle, Handshake_Hook'Access);
      end Set;
   end Handshake_Set_Post_Client_Hello_Function;

   procedure Handshake_Set_Random
             (  Session : in out Session_Type;
                Random  : Random_Value
             )  is
      function Internal (Session : Address; Random : Datum) return int;
      pragma Import (C, Internal, "gnutls_handshake_set_random");
   begin
      Check (Internal (Session.Holder.Handle, To_Datum (Random)));
   end Handshake_Set_Random;

   procedure Handshake_Set_Timeout
             (  Session : Session_Type;
                Timeout : Duration := Duration'First
             )  is
      procedure Internal (Session : Address; Timeout : unsigned);
      pragma Import (C, Internal, "gnutls_handshake_set_timeout");
   begin
      if Timeout < 0.0 then
         Internal (Session.Holder.Handle, To_unsigned (-1));
      else
         Internal (Session.Holder.Handle, unsigned (Timeout * 1_000.0));
      end if;
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Time_Error'Identity,
            "Illegal timeout value"
         );
   end Handshake_Set_Timeout;

   procedure Initialize
             (  Credentials : in out Anon_Client_Credentials
             )  is
      function Internal (Credentials : access Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_anon_allocate_client_credentials"
             );
   begin
      Check (Internal (Credentials.Handle'Access));
   end Initialize;

   procedure Initialize
             (  Credentials : in out Anon_Client_Credentials_Ref
             )  is
   begin
      null;
   end Initialize;

   procedure Initialize
             (  Credentials : in out Anon_Server_Credentials
             )  is
      function Internal (Credentials : access Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_anon_allocate_server_credentials"
             );
   begin
      Check (Internal (Credentials.Handle'Access));
   end Initialize;

   procedure Initialize
             (  Credentials : in out Anon_Server_Credentials_Ref
             )  is
   begin
      null;
   end Initialize;

   procedure Initialize (Data : in out DH_Params) is
      function Internal (Data : access Address) return int;
      pragma Import (C, Internal, "gnutls_dh_params_init");
   begin
      Check (Internal (Data.Handle'Access));
   end Initialize;

   procedure Initialize (Data : in out DH_Params_Ref) is
   begin
      null;
   end Initialize;

   procedure Initialize (Data : in out ECDH_Params) is
   begin
      null;
   end Initialize;

   procedure Initialize (Data : in out RSA_Params) is
   begin
      null;
   end Initialize;

   procedure Initialize (Session : in out Session_Holder) is
      function Internal
               (  Session : access Address;
                  Flags   : unsigned
               )  return int;
      pragma Import (C, Internal, "gnutls_init");
   begin
      if Session.Handle = Null_Address and then Session.Flags /= 0 then
         Check
         (  Internal
            (  Session.Handle'Access,
               Init_Flags'Pos (Session.Flags)
         )  );
      end if;
   end Initialize;

   procedure Initialize
             (  Credentials : in out Certificate_Credentials
             )  is
      function Internal (Credentials : access Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_certificate_allocate_credentials"
             );
   begin
      Check (Internal (Credentials.Handle'Access));
   end Initialize;

   procedure Initialize
             (  Credentials : in out Certificate_Credentials_Ref
             )  is
   begin
      null;
   end Initialize;

--     procedure Initialize
--               (  Credentials : in out SRP_Client_Credentials
--               )  is
--        function Internal (Credentials : access Address) return int;
--        pragma Import
--               (  C,
--                  Internal,
--                  "gnutls_srp_allocate_client_credentials"
--               );
--     begin
--        Check (Internal (Credentials.Handle'Access));
--     end Initialize;
--
--     procedure Initialize
--               (  Credentials : in out SRP_Client_Credentials_Ref
--               )  is
--     begin
--        null;
--     end Initialize;
--
--     procedure Initialize
--               (  Credentials : in out SRP_Server_Credentials
--               )  is
--        function Internal (Credentials : access Address) return int;
--        pragma Import
--               (  C,
--                  Internal,
--                  "gnutls_srp_allocate_server_credentials"
--               );
--     begin
--        Check (Internal (Credentials.Handle'Access));
--     end Initialize;
--
--     procedure Initialize
--               (  Credentials : in out SRP_Server_Credentials_Ref
--               )  is
--     begin
--        null;
--     end Initialize;

   procedure Initialize
             (  Cache      : in out Priority;
                Parameters : String := "PERFORMANCE:%SERVER_PRECEDENCE"
             )  is
      use System.Storage_Elements;
      function Internal
               (  Cache      : access Address;
                  Priorities : char_array;
                  Err_Pos    : access Address
               )  return int;
      pragma Import (C, Internal, "gnutls_priority_init");
      Value  : char_array := To_C (Parameters);
      Error  : aliased Address;
      Result : int;
   begin
      if Cache.Handle /= Null_Address then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid priority cache (alread initialized)"
         );
      end if;
      Result := Internal (Cache.Handle'Access, Value, Error'Access);
      if Result < 0 then
         Raise_Exception
         (  TLS_Error'Identity,
            (  StrError (Result)
            &  '['
            &  int'Image (Result)
            &  "] at"
            &  Integer'Image
               (  Parameters'First
               +  Integer (Error - Value'Address)
         )  )  );
      end if;
   end Initialize;

   function Is_Initialized (Cache : Priority) return Boolean is
   begin
      return Cache.Handle /= Null_Address;
   end Is_Initialized;

   function KX_Get (Session : Session_Type) return KX_Algorithm is
      function Internal (Session : Address) return KX_Algorithm;
      pragma Import (C, Internal, "gnutls_kx_get");
   begin
      return Internal (Session.Holder.Handle);
   end KX_Get;

   function KX_Get_ID (Name : String) return KX_Algorithm is
      function Internal (Name : char_array) return KX_Algorithm;
      pragma Import (C, Internal, "gnutls_kx_get_id");
   begin
      return Internal (To_C (Name));
   end KX_Get_ID;

   function KX_Get_Name (Algorithm : KX_Algorithm) return String is
      function Internal (Algorithm : KX_Algorithm) return chars_ptr;
      pragma Import (C, Internal, "gnutls_kx_get_name");
      Result : chars_ptr := Internal (Algorithm);
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No algorithm matched");
      else
         return Value (Result);
      end if;
   end KX_Get_Name;

   function KX_List return KX_Algorithm_Array is
      use KX_Algorithm_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_kx_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => KX_Unknown);
      else
         return Value (Result);
      end if;
   end KX_List;

   function MAC_Get (Session : Session_Type) return MAC_Algorithm is
      function Internal (Session : Address) return MAC_Algorithm;
      pragma Import (C, Internal, "gnutls_mac_get");
   begin
      return Internal (Session.Holder.Handle);
   end MAC_Get;

   function MAC_Get_ID (Name : String) return MAC_Algorithm is
      function Internal (Name : char_array) return MAC_Algorithm;
      pragma Import (C, Internal, "gnutls_mac_get_id");
   begin
      return Internal (To_C (Name));
   end MAC_Get_ID;

   function MAC_Get_Name (Algorithm : MAC_Algorithm) return String is
      function Internal (Algorithm : MAC_Algorithm) return chars_ptr;
      pragma Import (C, Internal, "gnutls_mac_get_name");
      Result : chars_ptr := Internal (Algorithm);
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No algorithm matched");
      else
         return Value (Result);
      end if;
   end MAC_Get_Name;

   function MAC_List return MAC_Algorithm_Array is
      use MAC_Algorithm_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_mac_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => MAC_Unknown);
      else
         return Value (Result);
      end if;
   end MAC_List;

   function New_Datum (Data : Stream_Element_Array) return Datum is
      Result : Datum;
   begin
      if Data'Length > 0 then
         declare
            function Internal (Size : size_t) return Address;
            pragma Import (C, Internal, "gnutls_malloc");
            subtype Copy_Array is Stream_Element_Array (Data'Range);
            Copy : Copy_Array;
            for Copy'Address use Internal (Data'Length);
            pragma Import (Ada, Copy);
         begin
            Result.Size := Data'Length;
            Result.Data := Copy'Address;
            Copy := Data;
         end;
      end if;
      return Result;
   end New_Datum;

   function OCSP_Status_Request_Is_Checked
            (  Session : Session_Type
            )  return Boolean is
      function Internal (Session : Address; Flags : unsigned)
         return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_ocsp_status_request_is_checked"
             );
   begin
      return Internal (Session.Holder.Handle, 0) /= 0;
   end OCSP_Status_Request_Is_Checked;

   procedure OpenPGP_Send_Cert
             (  Session : Session_Type;
                Status  : OpenPGP_Crt_Status
             )  is
      procedure Internal
                (  Session : Address;
                   Status  : OpenPGP_Crt_Status
                );
      pragma Import (C, Internal, "gnutls_openpgp_send_cert");
   begin
      Internal (Session.Holder.Handle, Status);
   end OpenPGP_Send_Cert;

   procedure PEM_Base64_Decode
             (  Header  : String;
                Data    : Stream_Element_Array;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      function Internal
               (  Header : char_array;
                  Data   : Datum;
                  Result : Address;
                  Size   : access size_t
               )  return int;
      pragma Import (C, Internal, "gnutls_pem_base64_decode");
      Size : aliased size_t := Result'Length;
   begin
      if Pointer < Result'First or else Pointer > Result'Last then
         Raise_Exception
         (  Layout_Error'Identity,
            "Invalid pointer is out of buffer range"
         );
      end if;
      Check
      (  Internal
         (  To_C (Header),
            To_Datum (Data),
            Result (Pointer)'Address,
            Size'Access
      )  );
      Pointer := Pointer + (Stream_Element_Offset (Size) - 1);
   exception
      when Error : TLS_Error =>
         if Get_Error_Code (Error) = E_Short_Memory_Buffer then
            Raise_Exception
            (  Layout_Error'Identity,
               "No room in the buffer"
            );
         else
            raise;
         end if;
   end PEM_Base64_Decode;

   procedure PEM_Base64_Decode
             (  Header  : String;
                Data    : String;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      function Internal
               (  Header : char_array;
                  Data   : Datum;
                  Result : Address;
                  Size   : access size_t
               )  return int;
      pragma Import (C, Internal, "gnutls_pem_base64_decode");
      Size : aliased size_t := Result'Length;
      Copy : char_array := To_C (Data);
   begin
      if Pointer < Result'First or else Pointer > Result'Last then
         Raise_Exception
         (  Layout_Error'Identity,
            "Invalid pointer is out of buffer range"
         );
      end if;
      Check
      (  Internal
         (  To_C (Header),
            To_Datum (Copy),
            Result (Pointer)'Address,
            Size'Access
      )  );
      Pointer := Pointer + (Stream_Element_Offset (Size) - 1);
   exception
      when Error : TLS_Error =>
         if Get_Error_Code (Error) = E_Short_Memory_Buffer then
            Raise_Exception
            (  Layout_Error'Identity,
               "No room in the buffer"
            );
         else
            raise;
         end if;
   end PEM_Base64_Decode;

   procedure PEM_Base64_Decode
             (  Data    : Stream_Element_Array;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      function Internal
               (  Header : chars_ptr;
                  Data   : Datum;
                  Result : Address;
                  Size   : access size_t
               )  return int;
      pragma Import (C, Internal, "gnutls_pem_base64_decode");
      Size : aliased size_t := Result'Length;
   begin
      if Pointer < Result'First or else Pointer > Result'Last then
         Raise_Exception
         (  Layout_Error'Identity,
            "Invalid pointer is out of buffer range"
         );
      end if;
      Check
      (  Internal
         (  Null_Ptr,
            To_Datum (Data),
            Result (Pointer)'Address,
            Size'Access
      )  );
      Pointer := Pointer + (Stream_Element_Offset (Size) - 1);
   exception
      when Error : TLS_Error =>
         if Get_Error_Code (Error) = E_Short_Memory_Buffer then
            Raise_Exception
            (  Layout_Error'Identity,
               "No room in the buffer"
            );
         else
            raise;
         end if;
   end PEM_Base64_Decode;

   procedure PEM_Base64_Decode
             (  Data    : String;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      function Internal
               (  Header : chars_ptr;
                  Data   : Datum;
                  Result : Address;
                  Size   : access size_t
               )  return int;
      pragma Import (C, Internal, "gnutls_pem_base64_decode");
      Size : aliased size_t := Result'Length;
      Copy : char_array := To_C (Data);
   begin
      if Pointer < Result'First or else Pointer > Result'Last then
         Raise_Exception
         (  Layout_Error'Identity,
            "Invalid pointer is out of buffer range"
         );
      end if;
      Check
      (  Internal
         (  Null_Ptr,
            To_Datum (Copy),
            Result (Pointer)'Address,
            Size'Access
      )  );
      Pointer := Pointer + (Stream_Element_Offset (Size) - 1);
   exception
      when Error : TLS_Error =>
         if Get_Error_Code (Error) = E_Short_Memory_Buffer then
            Raise_Exception
            (  Layout_Error'Identity,
               "No room in the buffer"
            );
         else
            raise;
         end if;
   end PEM_Base64_Decode;

   procedure PEM_Base64_Encode
             (  Data    : Stream_Element_Array;
                Result  : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
      function Internal
               (  Header : chars_ptr;
                  Data   : Datum;
                  Result : Address;
                  Size   : access size_t
               )  return int;
      pragma Import (C, Internal, "gnutls_pem_base64_encode");
      Size : aliased size_t := Result'Length;
   begin
      if Pointer < Result'First or else Pointer > Result'Last then
         Raise_Exception
         (  Layout_Error'Identity,
            "Invalid pointer is out of buffer range"
         );
      end if;
      Check
      (  Internal
         (  Null_Ptr,
            To_Datum (Data),
            Result (Pointer)'Address,
            Size'Access
      )  );
      Pointer := Pointer + (Stream_Element_Offset (Size) - 1);
   exception
      when Error : TLS_Error =>
         if Get_Error_Code (Error) = E_Short_Memory_Buffer then
            Raise_Exception
            (  Layout_Error'Identity,
               "No room in the buffer"
            );
         else
            raise;
         end if;
   end PEM_Base64_Encode;

   function PEM_Base64_Encode
            (  Data : Stream_Element_Array
            )  return Stream_Element_Array is
      Result  : Stream_Element_Array (1..Data'Length * 4 / 3 + 4);
      Pointer : Stream_Element_Offset := Result'First;
   begin
      PEM_Base64_Encode (Data, Result, Pointer);
      return Result (Result'First..Pointer - 1);
   end PEM_Base64_Encode;

   function PK_Algorithm_Get_Name
            (  Algorithm : PK_Algorithm
            )  return String is
      function Internal (Algorithm : PK_Algorithm) return chars_ptr;
      pragma Import (C, Internal, "gnutls_pk_algorithm_get_name");
      Result : chars_ptr := Internal (Algorithm);
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No algorithm matched");
      else
         return Value (Result);
      end if;
   end PK_Algorithm_Get_Name;

   function PK_Bits_To_Sec_Param
            (  Algorithm : PK_Algorithm;
               Bits      : Positive
            )  return Sec_Param is
      function Internal
               (  Algorithm : PK_Algorithm;
                  Bits      : unsigned
               )  return Sec_Param;
      pragma Import
             (  C,
                Internal,
                "gnutls_pk_bits_to_sec_param"
             );
   begin
      return Internal (Algorithm, unsigned (Bits));
   end PK_Bits_To_Sec_Param;

   function PK_Get_ID (Name : String) return PK_Algorithm is
      function Internal (Name : char_array) return PK_Algorithm;
      pragma Import (C, Internal, "gnutls_pk_get_id");
   begin
      return Internal (To_C (Name));
   end PK_Get_ID;

   function PK_Get_Name (Algorithm : PK_Algorithm) return String is
      function Internal (Algorithm : PK_Algorithm) return chars_ptr;
      pragma Import (C, Internal, "gnutls_pk_get_name");
      Result : chars_ptr := Internal (Algorithm);
   begin
      if Result = Null_Ptr then
         Raise_Exception (End_Error'Identity, "No algorithm matched");
      else
         return Value (Result);
      end if;
   end PK_Get_Name;

   function PK_List return PK_Algorithm_Array is
      use PK_Algorithm_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_pk_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => PK_Unknown);
      else
         return Value (Result);
      end if;
   end PK_List;

   procedure Priority_Set
             (  Session : in out Session_Type;
                Cache   : in out Priority
             )  is
      function Internal
               (  Session : Address;
                  Cache   : Address
               )  return int;
      pragma Import (C, Internal, "gnutls_priority_set");
   begin
      if Cache.Handle = Null_Address then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Invalid priority cache (not yet initialized)"
         );
      end if;
      Check (Internal (Session.Holder.Handle, Cache.Handle));
   end Priority_Set;

   procedure Priority_Set_Direct
             (  Session    : in out Session_Type;
                Priorities : String
             )  is
      use System.Storage_Elements;
      function Internal
               (  Session    : Address;
                  Priorities : char_array;
                  Err_Pos    : access Address
               )  return int;
      pragma Import (C, Internal, "gnutls_priority_set_direct");
      Value  : char_array := To_C (Priorities);
      Error  : aliased Address;
      Result : int;
   begin
      Result := Internal (Session.Holder.Handle, Value, Error'Access);
      if Result < 0 then
         Raise_Exception
         (  TLS_Error'Identity,
            (  StrError (Result)
            &  '['
            &  int'Image (Result)
            &  "] at"
            &  Integer'Image
               (  Priorities'First
               +  Integer (Error - Value'Address)
         )  )  );
      end if;
   end Priority_Set_Direct;

   procedure Raise_TLS_Error (Error : int) is
   begin
      Raise_Exception
      (  TLS_Error'Identity,
         StrError (Error) & '[' & int'Image (Error) & ']'
      );
   end Raise_TLS_Error;

   procedure Read
             (  Session : in out Session_Type;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is
   begin
      Pointer := Data'First - 1;
   end Read;

   function Protocol_Get_ID (Name : String) return Protocol is
      function Internal (Name : char_array) return Protocol;
      pragma Import (C, Internal, "gnutls_protocol_get_id");
      Result : Protocol := Internal (To_C (Name));
   begin
      if Result = Version_Unknown then
         Raise_Exception
         (  End_Error'Identity,
            "No protocol matched"
         );
      end if;
      return Result;
   end Protocol_Get_ID;

   function Protocol_Get_Name (Version : Protocol) return String is
      function Internal (Version : Protocol) return chars_ptr;
      pragma Import (C, Internal, "gnutls_protocol_get_name");
      Result : chars_ptr := Internal (Version);
   begin
      if Result = Null_Ptr then
         Raise_Exception
         (  End_Error'Identity,
            "No protocol version matched"
         );
      end if;
      return Value (Result);
   end Protocol_Get_Name;

   function Protocol_Get_Version
            (  Session : Session_Type
            )  return Protocol is
      function Internal (Session : Address) return Protocol;
      pragma Import (C, Internal, "gnutls_protocol_get_version");
   begin
      return Internal (Session.Holder.Handle);
   end Protocol_Get_Version;

   function Protocol_List return Protocol_Array is
      use Protocol_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_protocol_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => No_Protocol);
      else
         return Value (Result);
      end if;
   end Protocol_List;

   function Record_Check_Corked (Session : Session_Type)
      return size_t is
      function Internal (Session : Address) return size_t;
      pragma Import (C, Internal, "gnutls_record_check_corked");
   begin
      return Internal (Session.Holder.Handle);
   end Record_Check_Corked;

   function Record_Check_Pending (Session : Session_Type)
      return size_t is
      function Internal (Session : Address) return size_t;
      pragma Import (C, Internal, "gnutls_record_check_pending");
   begin
      return Internal (Session.Holder.Handle);
   end Record_Check_Pending;

   procedure Record_Cork (Session : in out Session_Type) is
      procedure Internal (Session : Address);
      pragma Import (C, Internal, "gnutls_record_cork");
   begin
      Internal (Session.Holder.Handle);
   end Record_Cork;

   procedure Record_Disable_Padding (Session : in out Session_Type) is
      procedure Internal (Session : Address);
      pragma Import (C, Internal, "gnutls_record_disable_padding");
   begin
      Internal (Session.Holder.Handle);
   end Record_Disable_Padding;

   function Record_Get_Direction (Session : Session_Type)
      return Direction is
      function Internal (Session : Address) return int;
      pragma Import (C, Internal, "gnutls_record_get_direction");
   begin
      if Internal (Session.Holder.Handle) = 0 then
         return Reading;
      else
         return Writing;
      end if;
   end Record_Get_Direction;

   function Record_Get_Max_Size (Session : Session_Type)
      return size_t is
      function Internal (Session : Address) return size_t;
      pragma Import (C, Internal, "gnutls_record_get_max_size");
   begin
      return Internal (Session.Holder.Handle);
   end Record_Get_Max_Size;

   function Record_Overhead_Size (Session : Session_Type)
      return size_t is
      function Internal (Session : Address) return size_t;
      pragma Import (C, Internal, "gnutls_record_overhead_size");
   begin
      return Internal (Session.Holder.Handle);
   end Record_Overhead_Size;

   procedure Record_Recv
             (  Session : in out Session_Type;
                Buffer  : out Stream_Element_Array;
                Last    : out Stream_Element_Offset
             )  is
      function Internal
               (  Session : Address;
                  Buffer  : Address;
                  Size    : size_t
               )  return ptrdiff_t;
      pragma Import (C, Internal, "gnutls_record_recv");
      Result : ptrdiff_t;
   begin
      if Buffer'Length > 0 then
         Result :=
            Internal
            (  Session.Holder.Handle,
               Buffer (Buffer'First)'Address,
               Buffer'Length
            );
         if Result > 0 then
            Last := Buffer'First + Stream_Element_Offset (Result) - 1;
         elsif Result = 0 then
            Raise_Exception
            (  End_Error'Identity,
               "Connection closed by peer"
            );
         elsif Result = E_Again or else Result = E_Interrupted then
            Last := Buffer'First - 1;
         else
            Raise_TLS_Error (int (Result));
         end if;
      else
         Last := Buffer'First - 1;
      end if;
   end Record_Recv;

   procedure Record_Send
             (  Session : in out Session_Type;
                Buffer  : Stream_Element_Array;
                Last    : out Stream_Element_Offset
             )  is
      function Internal
               (  Session : Address;
                  Buffer  : Address;
                  Size    : size_t
               )  return ptrdiff_t;
      pragma Import (C, Internal, "gnutls_record_send");
      Result : ptrdiff_t;
   begin
      if Buffer'Length > 0 then
         Result :=
            Internal
            (  Session.Holder.Handle,
               Buffer (Buffer'First)'Address,
               Buffer'Length
            );
         if Result >= 0 then
            Last := Buffer'First + Stream_Element_Offset (Result) - 1;
         elsif Result = E_Again or else Result = E_Interrupted then
            Last := Buffer'First - 1;
         else
            Raise_TLS_Error (int (Result));
         end if;
      else
         Last := Buffer'First - 1;
      end if;
   end Record_Send;
--
-- *** Removed ***
--
--     procedure Record_Set_Max_Empty_Records
--               (  Session : in out Session_Type;
--                  Count   : Positive
--               )  is
--        procedure Internal (Session : Address; Count : int);
--        pragma Import
--               (  C,
--                  Internal,
--                  "gnutls_record_set_max_empty_records"
--               );
--     begin
--        Internal (Session.Holder.Handle, int (Count));
--     end Record_Set_Max_Empty_Records;

   procedure Record_Set_Max_Size
             (  Session : in out Session_Type;
                Size    : size_t
             )  is
      function Internal (Session : Address; Size : size_t)
         return ssize_t;
      pragma Import (C, Internal, "gnutls_record_set_max_size");
      Result : ssize_t := Internal (Session.Holder.Handle, Size);
   begin
      if Result < 0 then
         Raise_TLS_Error (int (Result));
      end if;
   end Record_Set_Max_Size;

   function Record_Uncork
            (  Session : Session_Type;
               Wait    : Boolean
            )  return Boolean is
      function Internal (Session : Address; Wait : int) return int;
      pragma Import (C, Internal, "gnutls_record_uncork");
      Result : int;
   begin
      if Wait then
         Result := Internal (Session.Holder.Handle, 1);
      else
         Result := Internal (Session.Holder.Handle, 0);
      end if;
      case Result is
         when 0..int'Last =>
            return True;
         when E_Again | E_Interrupted =>
            return False;
         when others =>
            Raise_TLS_Error (Result);
            return False;
      end case;
   end Record_Uncork;

   procedure Rehandshake (Session : in out Session_Type) is
      function Internal (Session : Address) return int;
      pragma Import (C, Internal, "gnutls_rehandshake");
   begin
      Check (Internal (Session.Holder.Handle));
   end Rehandshake;

   function Safe_Renegotiation_Status
            (  Session : Session_Type
            )  return Boolean is
      function Internal (Session : Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_safe_renegotiation_status"
             );
   begin
      return Internal (Session.Holder.Handle) /= 0;
   end Safe_Renegotiation_Status;

   function Sec_Param_Get_Name (Param : Sec_Param) return String is
      function Internal (Param : Sec_Param) return chars_ptr;
      pragma Import (C, Internal, "gnutls_sec_param_get_name");
      Result : chars_ptr := Internal (Param);
   begin
      if Result = Null_Ptr then
         Raise_Exception
         (  End_Error'Identity,
            "No security parameter matched"
         );
      else
         return Value (Result);
      end if;
   end Sec_Param_Get_Name;

   function Sec_Param_To_PK_Bits
            (  Algorithm : PK_Algorithm;
               Parameter : Sec_Param
            )  return Natural is
      function Internal
               (  Algorithm : PK_Algorithm;
                  Parameter : Sec_Param
               )  return unsigned;
      pragma Import
             (  C,
                Internal,
                "gnutls_sec_param_to_pk_bits"
             );
   begin
      return Natural (Internal (Algorithm, Parameter));
   end Sec_Param_To_PK_Bits;

   procedure Session_Channel_Binding
             (  Session      : in out Session_Type;
                Binding_Type : Channel_Binding;
                Binding_Data : in out Datum
             )  is
      function Internal
               (  Session      : Address;
                  Binding_Type : Channel_Binding;
                  Binding_Data : access Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_session_channel_binding");
      Result : aliased Datum := Binding_Data;
   begin
      Check
      (  Internal
         (  Session.Holder.Handle,
            Binding_Type,
            Result'Access
      )  );
      Binding_Data := Result;
   end Session_Channel_Binding;

   procedure Session_Enable_Compatibility_Mode
             (  Session : in out Session_Type
             )  is
      procedure Internal (Session : Address);
      pragma Import
             (  C,
                Internal,
                "gnutls_session_enable_compatibility_mode"
             );
   begin
      Internal (Session.Holder.Handle);
   end Session_Enable_Compatibility_Mode;

   procedure Session_Force_Valid (Session : in out Session_Type) is
      procedure Internal (Session : Address);
      pragma Import (C, Internal, "gnutls_session_force_valid");
   begin
      Internal (Session.Holder.Handle);
   end Session_Force_Valid;

   procedure Session_Get_Data
             (  Session : Session_Type;
                Data    : in out Datum_Holder'Class
             )  is
      function Internal (Session : Address; Data : access Datum)
         return int;
      pragma Import (C, Internal, "gnutls_session_get_data2");
   begin
      Finalize (Data);
      Check (Internal (Session.Holder.Handle, Data.Data'Access));
   end Session_Get_Data;

   function Session_Get_Desc (Session : Session_Type) return String is
      function Internal (Session : Address) return chars_ptr;
      pragma Import (C, Internal, "gnutls_session_get_desc");
      Ptr : chars_ptr := Internal (Session.Holder.Handle);
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         declare
            Result : String := Value (Ptr);
         begin
            Free (Ptr);
            return Result;
         end;
      end if;
   end Session_Get_Desc;

   function Session_Get_ID
            (  Session : Session_Type
            )  return Stream_Element_Array is
      function Internal
               (  Session : Address;
                  Data    : access Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_session_get_id2");
      Data : aliased Datum;
   begin
      Check (Internal (Session.Holder.Handle, Data'Access));
      return To_Stream_Element_Array (Data);
   end Session_Get_ID;

   function Session_Get_Ptr (Session : Session_Type) return Address is
      function Internal (Session : Address) return Address;
      pragma Import (C, Internal, "gnutls_session_get_ptr");
   begin
      return Internal (Session.Holder.Handle);
   end Session_Get_Ptr;

   function Session_Is_Resumed (Session : Session_Type)
      return Boolean is
      function Internal (Session : Address) return int;
      pragma Import (C, Internal, "gnutls_session_is_resumed");
   begin
      return Internal (Session.Holder.Handle) /= 0;
   end Session_Is_Resumed;

   function Session_Resumption_Requested (Session : Session_Type)
      return Boolean is
      function Internal (Session : Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_session_resumption_requested"
             );
   begin
      return Internal (Session.Holder.Handle) /= 0;
   end Session_Resumption_Requested;

   procedure Session_Set_Data
             (  Session : in out Session_Type;
                Data    : Stream_Element_Array
             )  is
      function Internal (Session : Address; Data : Datum) return int;
      pragma Import (C, Internal, "gnutls_session_set_data");
   begin
      Check (Internal (Session.Holder.Handle, To_Datum (Data)));
   end Session_Set_Data;

   procedure Session_Set_Ptr
             (  Session : in out Session_Type;
                Pointer : Address
             )  is
      procedure Internal (Session : Address; Pointer : Address);
      pragma Import (C, Internal, "gnutls_session_set_ptr");
   begin
      Internal (Session.Holder.Handle, Pointer);
   end Session_Set_Ptr;

   procedure Session_Ticket_Enable_Client
             (  Session : in out Session_Type
             )  is
      function Internal (Session : Address) return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_session_ticket_enable_client"
             );
   begin
      Check (Internal (Session.Holder.Handle));
   end Session_Ticket_Enable_Client;

   procedure Session_Ticket_Enable_Server
             (  Session : in out Session_Type;
                Key     : Stream_Element_Array
             )  is
      function Internal (Session : Address; Key : Datum) return int;
      pragma Import (C, Internal, "gnutls_session_ticket_enable_server");
   begin
      Check (Internal (Session.Holder.Handle, To_Datum (Key)));
   end Session_Ticket_Enable_Server;

   procedure Session_Ticket_Key_Generate
             (  Session : in out Session_Type;
                Key     : in out Datum_Holder'Class
             )  is
      function Internal
               (  Session : Address;
                  Key     : access Datum
               )  return int;
      pragma Import (C, Internal, "gnutls_session_ticket_key_generate");
   begin
      Finalize (Key);
      Check (Internal (Session.Holder.Handle, Key.Data'Access));
   end Session_Ticket_Key_Generate;

   procedure Set_Default_Priority (Session : in out Session_Type) is
      function Internal (Session : Address) return int;
      pragma Import (C, Internal, "gnutls_set_default_priority");
   begin
      Check (Internal (Session.Holder.Handle));
   end Set_Default_Priority;

   function Sign_Algorithm_Get
            (  Session : Session_Type
            )  return Sign_Algorithm is
      function Internal
               (  Session : Address
               )  return int;
      pragma Import (C, Internal, "gnutls_sign_algorithm_get");
   begin
      return Sign_Algorithm'Val (Internal (Session.Holder.Handle));
   end Sign_Algorithm_Get;

   function Sign_Algorithm_Get_Client
            (  Session : Session_Type
            )  return Sign_Algorithm is
      function Internal
               (  Session : Address
               )  return int;
      pragma Import (C, Internal, "gnutls_sign_algorithm_get_client");
   begin
      return Sign_Algorithm'Val (Internal (Session.Holder.Handle));
   end Sign_Algorithm_Get_Client;

   function Sign_Algorithm_Get_Requested
            (  Session : Session_Type;
               Index   : Positive
            )  return Sign_Algorithm is
      function Internal
               (  Session   : Address;
                  Index     : size_t;
                  Algorithm : access Sign_Algorithm
               )  return int;
      pragma Import
             (  C,
                Internal,
                "gnutls_sign_algorithm_get_requested"
             );
      Result : aliased Sign_Algorithm;
   begin
      Check
      (  Internal
         (  Session.Holder.Handle,
            size_t (Index) - 1,
            Result'Access
      )  );
      return Result;
   end Sign_Algorithm_Get_Requested;

   function Sign_Get_ID (Name : String) return Sign_Algorithm is
      function Internal (Name : char_array) return Sign_Algorithm;
      pragma Import (C, Internal, "gnutls_sign_get_id");
   begin
      return Internal (To_C (Name));
   end Sign_Get_ID;

   function Sign_Get_Name (Algorithm : Sign_Algorithm) return String is
      function Internal (Algorithm : Sign_Algorithm) return chars_ptr;
      pragma Import (C, Internal, "gnutls_sign_get_name");
      Result : chars_ptr := Internal (Algorithm);
   begin
      if Result = Null_Ptr then
         Raise_Exception
         (  End_Error'Identity,
            "No signature algorithm matched"
         );
      else
         return Value (Result);
      end if;
   end Sign_Get_Name;

   function Sign_Is_Secure (Algorithm : Sign_Algorithm)
      return Boolean is
      function Internal (Algorithm : Sign_Algorithm) return int;
      pragma Import (C, Internal, "gnutls_sign_is_secure");
   begin
      return Internal (Algorithm) /= 0;
   end Sign_Is_Secure;

   function Sign_List return Sign_Algorithm_Array is
      use Sign_Algorithm_Pointers;
      function Internal return Pointer;
      pragma Import (C, Internal, "gnutls_sign_list");
      Result : Pointer := Internal;
   begin
      if Result = null then
         return (1..0 => Sign_Unknown);
      else
         return Value (Result);
      end if;
   end Sign_List;

   function StrError (Error : int) return String is
      function Internal (Error : int) return chars_ptr;
      pragma Import (C, Internal, "gnutls_strerror");
   begin
      return Value (Internal (Error));
   end StrError;

   function To_Stream_Element_Array (Data : Datum)
      return Stream_Element_Array is
   begin
      if Data.Size = 0 or else Data.Data = Null_Address then
         return (1..0 => 0);
      else
         declare
            subtype Buffer_Type is Stream_Element_Array
                                   (  1
                                   .. Stream_Element_Offset (Data.Size)
                                   );
            Buffer : Buffer_Type;
            for Buffer'Address use Data.Data;
            pragma Import (Ada, Buffer);
         begin
            return Buffer;
         end;
      end if;
   end To_Stream_Element_Array;

   function To_Stream_Element_Array (Data : String)
      return Stream_Element_Array is
   begin
      if Data'Length = 0 then
         return (1..0 => 0);
      else
         declare
            Result  : Stream_Element_Array (1..Data'Length);
            Pointer : Stream_Element_Offset := Result'First;
         begin
            for Index in Data'Range loop
               Result (Pointer) := Character'Pos (Data (Index));
               Pointer := Pointer + 1;
            end loop;
            return Result;
         end;
      end if;
   end To_Stream_Element_Array;

   function To_String (Data : Datum) return String is
   begin
      if Data.Size = 0 or else Data.Data = Null_Address then
         return "";
      else
         declare
            subtype Buffer_Type is char_array (1..size_t (Data.Size));
            Buffer : Buffer_Type;
            for Buffer'Address use Data.Data;
            pragma Import (Ada, Buffer);
         begin
            return To_Ada (Buffer, False);
         end;
      end if;
   end To_String;

   procedure Transport_Set_Error_No
             (  Session : in out Session_Type;
                Error   : int
             )  is
      procedure Internal (Session : Address; Error : int);
      pragma Import (C, Internal, "gnutls_transport_set_errno");
   begin
      Internal (Session.Holder.Handle, Error);
   end Transport_Set_Error_No;

   package body Transport_Set_Pull_Function is
      package Conversions is
         new Address_To_Access_Conversions (Transport_Type);
      use Conversions;

      type Pull_Func_Ptr is access function
           (  Transport : Address;
              Data      : Address;
              Size      : size_t
           )  return ssize_t;
      pragma Convention (C, Pull_Func_Ptr);
      function Pull_Func
               (  Transport : Address;
                  Data      : Address;
                  Size      : size_t
               )  return ssize_t;
      pragma Convention (C, Pull_Func);
      function Pull_Func
               (  Transport : Address;
                  Data      : Address;
                  Size      : size_t
               )  return ssize_t is
         Object : Object_Pointer;
      begin
         if Transport = Null_Address then
            return -1;
         end if;
         Object := To_Pointer (Transport);
         if Object = null then
            return -1;
         end if;
         declare
            subtype Data_Buffer is Stream_Element_Array
                                   (  1
                                   .. Stream_Element_Offset (Size)
                                   );
            Buffer : Data_Buffer;
            for Buffer'Address use Data;
            Pointer : Stream_Element_Offset := Buffer'First;
         begin
            Read (Object.all, Buffer, Pointer);
            if Pointer <= Buffer'First then
               Transport_Set_Error_No
               (  Get_Session (Object.all).all,
                  EAGAIN
               );
               return -1;
            elsif Pointer > Buffer'Last then
               return Buffer'Length;
            else
               return ssize_t (Pointer - Buffer'First);
            end if;
         exception
            when End_Error =>
               return 0;
            when Error : others =>
               Transport_Set_Error_No
               (  Get_Session (Object.all).all,
                  EIO
               );
               return -1;
         end;
      exception
         when others =>
            return -1;
      end Pull_Func;

      procedure Set
                (  Session   : in out Session_Type;
                   Transport : access Transport_Type
                )  is
         procedure Internal (Session : Address; Func : Pull_Func_Ptr);
         pragma Import
                (  C,
                   Internal,
                   "gnutls_transport_set_pull_function"
                );
         Receive : Address;
         Send    : Address;
      begin
         Transport_Get_Ptr2 (Session, Receive, Send);
         Transport_Set_Ptr2 (Session, Transport.all'Address, Send);
         Internal (Session.Holder.Handle, Pull_Func'Access);
      end Set;
   end Transport_Set_Pull_Function;

   function Transport_Get_Int (Session : Session_Type) return int is
      function Internal (Session : Address) return  int;
      pragma Import (C, Internal, "gnutls_transport_get_int");
   begin
      return Internal (Session.Holder.Handle);
   end Transport_Get_Int;

   procedure Transport_Get_Int2
             (  Session : Session_Type;
                Receive : out int;
                Send    : out int
             )  is
      procedure Internal
                (  Session : Address;
                   Receive : access int;
                   Send    : access int
                );
      pragma Import (C, Internal, "gnutls_transport_get_int2");
      Receive_Int : aliased int;
      Send_Int    : aliased int;
   begin
      Internal
      (  Session.Holder.Handle,
         Receive_Int'Access,
         Send_Int'Access
      );
      Receive := Receive_Int;
      Send    := Send_Int;
   end Transport_Get_Int2;

   function Transport_Get_Ptr (Session : Session_Type) return Address is
      function Internal (Session : Address) return  Address;
      pragma Import (C, Internal, "gnutls_transport_get_ptr");
   begin
      return Internal (Session.Holder.Handle);
   end Transport_Get_Ptr;

   procedure Transport_Get_Ptr2
             (  Session : Session_Type;
                Receive : out Address;
                Send    : out Address
             )  is
      procedure Internal
                (  Session : Address;
                   Receive : access Address;
                   Send    : access Address
                );
      pragma Import (C, Internal, "gnutls_transport_get_ptr2");
      Receive_Ptr : aliased Address;
      Send_Ptr    : aliased Address;
   begin
      Internal
      (  Session.Holder.Handle,
         Receive_Ptr'Access,
         Send_Ptr'Access
      );
      Receive := Receive_Ptr;
      Send    := Send_Ptr;
   end Transport_Get_Ptr2;

   procedure Transport_Set_Int
             (  Session : in out Session_Type;
                Value   : int
             )  is
      procedure Internal (Session : Address; Receive, Send : int);
      pragma Import (C, Internal, "gnutls_transport_set_int2");
   begin
      Internal (Session.Holder.Handle, Value, Value);
   end Transport_Set_Int;

   procedure Transport_Set_Int2
             (  Session : in out Session_Type;
                Receive : int;
                Send    : int
             )  is
      procedure Internal
                (  Session : Address;
                   Receive : int;
                   Send    : int
                );
      pragma Import (C, Internal, "gnutls_transport_set_int2");
   begin
      Internal (Session.Holder.Handle, Receive, Send);
   end Transport_Set_Int2;

   procedure Transport_Set_Ptr
             (  Session : in out Session_Type;
                Pointer : Address
             )  is
      procedure Internal (Session : Address; Pointer : Address);
      pragma Import (C, Internal, "gnutls_transport_set_ptr");
   begin
      Internal (Session.Holder.Handle, Pointer);
   end Transport_Set_Ptr;

   procedure Transport_Set_Ptr2
             (  Session : in out Session_Type;
                Receive : Address;
                Send    : Address
             )  is
      procedure Internal
                (  Session : Address;
                   Receive : Address;
                   Send    : Address
                );
      pragma Import (C, Internal, "gnutls_transport_set_ptr2");
   begin
      Internal (Session.Holder.Handle, Receive, Send);
   end Transport_Set_Ptr2;

   package body Transport_Set_Push_Function is
      package Conversions is
         new Address_To_Access_Conversions (Transport_Type);
      use Conversions;

      type Push_Func_Ptr is access function
           (  Transport : Address;
              Data      : Address;
              Size      : size_t
           )  return ssize_t;
      pragma Convention (C, Push_Func_Ptr);
      function Push_Func
               (  Transport : Address;
                  Data      : Address;
                  Size      : size_t
               )  return ssize_t;
      pragma Convention (C, Push_Func);
      function Push_Func
               (  Transport : Address;
                  Data      : Address;
                  Size      : size_t
               )  return ssize_t is
         Object : Object_Pointer;
      begin
         if Transport = Null_Address then
            return -1;
         end if;
         Object := To_Pointer (Transport);
         if Object = null then
            return -1;
         end if;
         declare
            subtype Data_Buffer is Stream_Element_Array
                                   (  1
                                   .. Stream_Element_Offset (Size)
                                   );
            Buffer : Data_Buffer;
            for Buffer'Address use Data;
            pragma Import (Ada, Buffer);
            Pointer : Stream_Element_Offset := Buffer'First;
         begin
            Write (Object.all, Buffer, Pointer);
            if Pointer <= Buffer'First then
               Transport_Set_Error_No
               (  Get_Session (Object.all).all,
                  EAGAIN
               );
               return -1;
            elsif Pointer > Buffer'Last then
               return Buffer'Length;
            else
               return ssize_t (Buffer'Last - Pointer + 1);
            end if;
         exception
            when End_Error =>
               return 0;
            when Error : others =>
               Transport_Set_Error_No
               (  Get_Session (Object.all).all,
                  EIO
               );
               return -1;
         end;
      exception
         when others =>
            return -1;
      end Push_Func;

      procedure Set
                (  Session   : in out Session_Type;
                   Transport : access Transport_Type
                )  is
         procedure Internal (Session : Address; Func : Push_Func_Ptr);
         pragma Import
                (  C,
                   Internal,
                   "gnutls_transport_set_push_function"
                );
         Receive : Address;
         Send    : Address;
      begin
         Transport_Get_Ptr2 (Session, Receive, Send);
         Transport_Set_Ptr2 (Session, Receive, Transport.all'Address);
         Internal (Session.Holder.Handle, Push_Func'Access);
      end Set;
   end Transport_Set_Push_Function;

   Epoch : constant Time := Time_Of (1970, 1, 1);

   function To_Time (Stamp : time_t) return Time is
   begin
      if Stamp = -1 then
         Raise_Exception (Time_Error'Identity, "Wrong POSIX time");
      end if;
      return Epoch + Duration (Stamp);
   end To_Time;

   Debug_To_File : Boolean := False;
   Debug_File    : Ada.Text_IO.File_Type;

   procedure Debug_Trace (Level : int; Text : String) is
      use Ada.Text_IO;
   begin
      if Debug_To_File then
         if Level in 0..99 then
            Put (Debug_File, "GNUTLS" & int'Image (Level) & ": ");
         else
            Put (Debug_File, "GNUTLS audit: ");
         end if;
         Put (Debug_File, Text);
      else
         if Level in 0..99 then
            Put ("GNUTLS" & int'Image (Level) & ": ");
         else
            Put ("GNUTLS audit: ");
         end if;
         Put (Text);
      end if;
   end Debug_Trace;

   procedure Log_Trace (Text : String) is
   begin
      Debug_Trace (-1, Text);
   end Log_Trace;

   procedure Log_Trace (Session : in out Session_Type; Text : String) is
   begin
      Debug_Trace (-1, Text);
   end Log_Trace;

   package Debug_Log is
      new Global_Set_Log_Function (Debug_Trace);

   package Audit_Log is
      new Global_Set_Audit_Log_Function (Log_Trace, Log_Trace);

   procedure Set_TLS_Debug (Level : int) is
      use Ada.Text_IO;
   begin
      if Debug_To_File then
         Close (Debug_File);
         Debug_To_File := False;
      end if;
      Audit_Log.Set;
      Debug_Log.Set (Level);
   end Set_TLS_Debug;

   procedure Set_TLS_Debug (Level : int; File : String) is
      use Ada.Text_IO;
   begin
      if Debug_To_File then
         Close (Debug_File);
         Debug_To_File := False;
      end if;
      Create (Debug_File, Out_File, File);
      Debug_To_File := True;
      Audit_Log.Set;
      Debug_Log.Set (Level);
   end Set_TLS_Debug;

   function To_Datum (Data : Stream_Element_Array) return Datum is
      Result : Datum;
   begin
      if Data'Length > 0 then
         Result.Size := Data'Length;
         Result.Data := Data (Data'First)'Address;
      end if;
      return Result;
   end To_Datum;

   function To_Datum (Data : char_array) return Datum is
      Result : Datum;
   begin
      if Data'Length > 0 then
         Result.Size := Data'Length;
         Result.Data := Data (Data'First)'Address;
      end if;
      return Result;
   end To_Datum;

   function URL_Is_Supported (URL : String) return Boolean is
      function Internal (URL : char_array) return int;
      pragma Import (C, Internal, "gnutls_url_is_supported");
   begin
      return Internal (To_C (URL)) /= 0;
   end URL_Is_Supported;

begin
   declare
      procedure Internal;
      pragma Import (C, Internal, "gnutls_global_init");
   begin
      Internal;
   end;
end GNUTLS;
