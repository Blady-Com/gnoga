--                                                                    --
--  package OpenSSL                 Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  08:31 04 Aug 2022  --
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

with System;

package body OpenSSL is

   function Address_Of (Object : BIO) return System.Address is
   begin
      return System.Address (Object);
   end Address_Of;

   function Address_Of (Object : BIO_METHOD) return System.Address is
   begin
      return System.Address (Object);
   end Address_Of;

   function Address_Of (Object : EVP_MD) return System.Address is
   begin
      return System.Address (Object);
   end Address_Of;

   function Address_Of (Object : SSL) return System.Address is
   begin
      return System.Address (Object);
   end Address_Of;

   function Address_Of (Object : SSL_CTX) return System.Address is
   begin
      return System.Address (Object);
   end Address_Of;

   function Address_Of (Object : SSL_METHOD) return System.Address is
   begin
      return System.Address (Object);
   end Address_Of;

   procedure Check_Error (ID : Exception_ID) is
      Error : constant unsigned_long := ERR_get_error;
   begin
      if Error /= 0 then
         declare
            Buffer : char_array (1..200);
         begin
            ERR_error_string_n
            (  Error,
               Buffer (1)'Access,
               Buffer'Length
            );
            ERR_clear_error;
            Raise_Exception (ID, To_Ada (Buffer));
         end;
      end if;
   end Check_Error;

   function Get_Digest_By_Name (Name : String) return EVP_MD is
      function Internal (Name : char_array) return EVP_MD;
      pragma Import (C, Internal, "EVP_get_digestbyname");
   begin
      return Internal (To_C (Name));
   end Get_Digest_By_Name;

   procedure PKCS5_PBKDF2_HMAC
             (  Password   : String;
                Salt       : Stream_Element_Array;
                Iterations : Positive;
                Digest     : EVP_MD;
                Output     : out Stream_Element_Array
             )  is
      function Internal
               (  Pass    : System.Address;
                  Passlen : int;
                  Salt    : System.Address;
                  Saltlen : int;
                  Iter    : int;
                  Digest  : EVP_MD;
                  Keylen  : int;
                  Output  : System.Address
               )  return int;
      pragma Import (C, Internal, "PKCS5_PBKDF2_HMAC");
   begin
      if 1 /= Internal
              (  Pass    => Password'Address,
                 Passlen => Password'Length,
                 Salt    => Salt'Address,
                 Saltlen => Salt'Length,
                 Iter    => int (Iterations),
                 Digest  => Digest,
                 Keylen  => Output'Length,
                 Output  => Output'Address
              )  then
         Raise_Exception
         (  Constraint_Error'Identity,
            "PKCS5_PBKDF2_HMAC fault"
         );
      end if;
   end PKCS5_PBKDF2_HMAC;

   procedure PKCS5_PBKDF2_HMAC
             (  Password   : String;
                Iterations : Positive;
                Digest     : EVP_MD;
                Output     : out Stream_Element_Array
             )  is
      function Internal
               (  Pass    : System.Address;
                  Passlen : int;
                  Salt    : System.Address := System.Null_Address;
                  Saltlen : int            := 0;
                  Iter    : int;
                  Digest  : EVP_MD;
                  Keylen  : int;
                  Output  : System.Address
               )  return int;
      pragma Import (C, Internal, "PKCS5_PBKDF2_HMAC");
   begin
      if 1 /= Internal
              (  Pass    => Password'Address,
                 Passlen => Password'Length,
                 Iter    => int (Iterations),
                 Digest  => Digest,
                 Keylen  => Output'Length,
                 Output  => Output'Address
              )  then
         Raise_Exception
         (  Constraint_Error'Identity,
            "PKCS5_PBKDF2_HMAC fault"
         );
      end if;
   end PKCS5_PBKDF2_HMAC;

   procedure PKCS5_PBKDF2_HMAC_SHA1
             (  Password   : String;
                Salt       : Stream_Element_Array;
                Iterations : Positive;
                Output     : out Stream_Element_Array
             )  is
      function Internal
               (  Pass    : System.Address;
                  Passlen : int;
                  Salt    : System.Address;
                  Saltlen : int;
                  Iter    : int;
                  Keylen  : int;
                  Output  : System.Address
               )  return int;
      pragma Import (C, Internal, "PKCS5_PBKDF2_HMAC_SHA1");
   begin
      if 1 /= Internal
              (  Pass    => Password'Address,
                 Passlen => Password'Length,
                 Salt    => Salt'Address,
                 Saltlen => Salt'Length,
                 Iter    => int (Iterations),
                 Keylen  => Output'Length,
                 Output  => Output'Address
              )  then
         Raise_Exception
         (  Constraint_Error'Identity,
            "PKCS5_PBKDF2_HMAC_SHA1 fault"
         );
      end if;
   end PKCS5_PBKDF2_HMAC_SHA1;

   procedure PKCS5_PBKDF2_HMAC_SHA1
             (  Password   : String;
                Iterations : Positive;
                Output     : out Stream_Element_Array
             )  is
      function Internal
               (  Pass    : System.Address;
                  Passlen : int;
                  Salt    : System.Address := System.Null_Address;
                  Saltlen : int            := 0;
                  Iter    : int;
                  Keylen  : int;
                  Output  : System.Address
               )  return int;
      pragma Import (C, Internal, "PKCS5_PBKDF2_HMAC_SHA1");
   begin
      if 1 /= Internal
              (  Pass    => Password'Address,
                 Passlen => Password'Length,
                 Iter    => int (Iterations),
                 Keylen  => Output'Length,
                 Output  => Output'Address
              )  then
         Raise_Exception
         (  Constraint_Error'Identity,
            "PKCS5_PBKDF2_HMAC_SHA1 fault"
         );
      end if;
   end PKCS5_PBKDF2_HMAC_SHA1;

   procedure RAND_Bytes (Buffer : out Stream_Element_Array) is
      function Internal (Buf : System.Address; Num : int) return int;
      pragma Import (C, Internal, "RAND_bytes");
   begin
      case Internal (Buffer'Address, Buffer'Length) is
         when 1 =>
            null;
         when -1 =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "Not supported RAND_Bytes method"
            );
         when others =>
            Check_Error (Constraint_Error'Identity);
      end case;
   end RAND_Bytes;

   procedure RAND_Priv_Bytes (Buffer : out Stream_Element_Array) is
      function Internal (Buf : System.Address; Num : int) return int;
      pragma Import (C, Internal, "RAND_priv_bytes");
   begin
      case Internal (Buffer'Address, Buffer'Length) is
         when 1 =>
            null;
         when -1 =>
            Raise_Exception
            (  Constraint_Error'Identity,
               "Not supported RAND_Priv_Bytes method"
            );
         when others =>
            Check_Error (Constraint_Error'Identity);
      end case;
   end RAND_Priv_Bytes;

   function SSL_clear_mode
            (  s    : SSL;
               mode : SSL_MODE_TYPE
            )  return SSL_MODE_TYPE is
   begin
      return SSL_MODE_TYPE
             (  SSL_ctrl
                (  s,
                   SSL_CTRL_CLEAR_MODE,
                   long (mode),
                   System.Null_Address
             )  );
   end SSL_clear_mode;

   function SSL_get_mode (s : SSL) return SSL_MODE_TYPE is
   begin
      return SSL_MODE_TYPE
             (  SSL_ctrl
                (  s,
                   SSL_CTRL_MODE,
                   0,
                   System.Null_Address
             )  );
   end SSL_get_mode;

   function SSL_set_mode
            (  s    : SSL;
               mode : SSL_MODE_TYPE
            )  return SSL_MODE_TYPE is
   begin
      return SSL_MODE_TYPE
             (  SSL_ctrl
                (  s,
                   SSL_CTRL_MODE,
                   long (mode),
                   System.Null_Address
             )  );
   end SSL_set_mode;

   function SSL_CTX_clear_mode
            (  ctx  : SSL_CTX;
               mode : SSL_MODE_TYPE
            )  return SSL_MODE_TYPE is
   begin
      return SSL_MODE_TYPE
             (  SSL_CTX_ctrl
                (  ctx,
                   SSL_CTRL_CLEAR_MODE,
                   long (mode),
                   System.Null_Address
             )  );
   end SSL_CTX_clear_mode;

   function SSL_CTX_get_mode (ctx : SSL_CTX) return SSL_MODE_TYPE is
   begin
      return SSL_MODE_TYPE
             (  SSL_CTX_ctrl
                (  ctx,
                   SSL_CTRL_MODE,
                   0,
                   System.Null_Address
             )  );
   end SSL_CTX_get_mode;

   function SSL_CTX_set_mode
            (  ctx  : SSL_CTX;
               mode : SSL_MODE_TYPE
            )  return SSL_MODE_TYPE is
   begin
      return SSL_MODE_TYPE
             (  SSL_CTX_ctrl
                (  ctx,
                   SSL_CTRL_MODE,
                   long (mode),
                   System.Null_Address
             )  );
   end SSL_CTX_set_mode;

end OpenSSL;
