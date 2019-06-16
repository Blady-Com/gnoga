--                                                                    --
--  package OpenSSL                 Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2019       --
--                                                                    --
--                                Last revision :  10:33 11 May 2019  --
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
              (  Pass    => Password (Password'First)'Address,
                 Passlen => Password'Length,
                 Salt    => Salt (Salt'First)'Address,
                 Saltlen => Salt'Length,
                 Iter    => int (Iterations),
                 Keylen  => Output'Length,
                 Output  => Output (Output'First)'Address
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
              (  Pass    => Password (Password'First)'Address,
                 Passlen => Password'Length,
                 Iter    => int (Iterations),
                 Keylen  => Output'Length,
                 Output  => Output (Output'First)'Address
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
      case Internal (Buffer (Buffer'First)'Address, Buffer'Length) is
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
      case Internal (Buffer (Buffer'First)'Address, Buffer'Length) is
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

end OpenSSL;
