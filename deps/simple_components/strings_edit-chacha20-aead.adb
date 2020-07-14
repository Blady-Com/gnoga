--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.ChaCha20.AEAD                  Luebeck            --
--  Implementation                                 Summer, 2018       --
--                                                                    --
--                                Last revision :  11:26 29 May 2020  --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Strings_Edit.ChaCha20.Poly1305; use Strings_Edit.ChaCha20.Poly1305;


package body Strings_Edit.ChaCha20.AEAD is

--     function Image (Data : Stream_Element_Array) return String is
--        use Strings_Edit.Integers;
--        Result  : String (1..Data'Length * 2);
--        Pointer : Integer := Result'First;
--     begin
--        for Index in Data'Range loop
--           Put
--           (  Destination => Result,
--              Pointer     => Pointer,
--              Value       => Integer (Data (Index)),
--              Base        => 16,
--              Fill        => '0',
--              Field       => 2,
--              Justify     => Strings_Edit.Right
--           );
--        end loop;
--        return Result;
--     end Image;

   function Little_Endian
            (  Value : Unsigned_64
            )  return Stream_Element_Array is
   begin
      return
      (  1 => Stream_Element ( Value          mod 2**8),
         2 => Stream_Element ((Value / 2** 8) mod 2**8),
         3 => Stream_Element ((Value / 2**16) mod 2**8),
         4 => Stream_Element ((Value / 2**24) mod 2**8),
         5 => Stream_Element ((Value / 2**32) mod 2**8),
         6 => Stream_Element ((Value / 2**40) mod 2**8),
         7 => Stream_Element ((Value / 2**48) mod 2**8),
         8 => Stream_Element ((Value / 2**56) mod 2**8)
      );
   end Little_Endian;

   procedure Encrypt
             (  Cipher  : in out ChaCha20_Cipher;
                Text    : Stream_Element_Array;
                Data    : Stream_Element_Array;
                Message : out Stream_Element_Array
             )  is
      Digest : Poly1305_Stream;
   begin
      if Message'Length /= Text'Length + 16 then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Message length must be"
            &  Stream_Element_Offset'Image (Text'Length + 16)
         )  );
      end if;
      declare
         Key : constant Stream_Element_Array :=
                        Get_Key_Stream (Cipher'Access, True);
      begin
         Start (Digest, Key (Key'First..Key'First + 31));
      end;
      Write (Digest, Data);
      Write (Digest, (1..(-Data'Length) mod 16 => 0));
      Encrypt
      (  Cipher,
         Text,
         Message (Message'First..Message'First + Text'Length - 1)
      );
      Write
      (  Digest,
         Message (Message'First..Message'First + Text'Length - 1)
      );
      Write (Digest, (1..(-Text'Length) mod 16 => 0));
      Write (Digest, Little_Endian (Data'Length));
      Write (Digest, Little_Endian (Text'Length));
      Stop
      (  Digest,
         Message (Message'First + Text'Length..Message'Last)
      );
   end Encrypt;

   procedure Encrypt
             (  Cipher  : in out ChaCha20_Cipher;
                Text    : String;
                Data    : String;
                Message : out Stream_Element_Array
             )  is
      Digest : aliased Poly1305_Stream;
   begin
      if Message'Length /= Text'Length + 16 then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Message length must be"
            &  Stream_Element_Offset'Image (Text'Length + 16)
         )  );
      end if;
      declare
         Key : constant Stream_Element_Array :=
                        Get_Key_Stream (Cipher'Access, True);
      begin
         Start (Digest, Key (Key'First..Key'First + 31));
      end;
      String'Write (Digest'Access, Data);
      Write (Digest, (1..(-Data'Length) mod 16 => 0));
      Encrypt
      (  Cipher,
         Text,
         Message (Message'First..Message'First + Text'Length - 1)
      );
      Write
      (  Digest,
         Message (Message'First..Message'First + Text'Length - 1)
      );
      Write (Digest, (1..(-Text'Length) mod 16 => 0));
      Write (Digest, Little_Endian (Data'Length));
      Write (Digest, Little_Endian (Text'Length));
      Stop
      (  Digest,
         Message (Message'Last - 15..Message'Last)
      );
   end Encrypt;

   procedure Decrypt
             (  Cipher  : in out ChaCha20_Cipher;
                Message : Stream_Element_Array;
                Data    : Stream_Element_Array;
                Text    : out Stream_Element_Array
             )  is
      Digest : Poly1305_Stream;
      Tag    : ChaCha20_Tag;
   begin
      if Message'Length /= Text'Length + 16 then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Text length must be"
            &  Stream_Element_Offset'Image (Message'Length - 16)
         )  );
      end if;
      declare
         Key : constant Stream_Element_Array :=
                        Get_Key_Stream (Cipher'Access, True);
      begin
         Start (Digest, Key (Key'First..Key'First + 31));
      end;
      Write (Digest, Data);
      Write (Digest, (1..(-Data'Length) mod 16 => 0));
      Write
      (  Digest,
         Message (Message'First..Message'Last - 16)
      );
      Write (Digest, (1..(-Text'Length) mod 16 => 0));
      Write (Digest, Little_Endian (Data'Length));
      Write (Digest, Little_Endian (Text'Length));
      Stop (Digest, Tag);
      if Tag /= Message (Message'Last - 15..Message'Last) then
         Raise_Exception (Data_Error'Identity, "Signature error");
      end if;
      Decrypt
      (  Cipher,
         Message (Message'First..Message'Last - 16),
         Text
      );
   end Decrypt;

   procedure Decrypt
             (  Cipher  : in out ChaCha20_Cipher;
                Message : Stream_Element_Array;
                Data    : String;
                Text    : out String
             )  is
      Digest : aliased Poly1305_Stream;
      Tag    : ChaCha20_Tag;
   begin
      if Message'Length /= Text'Length + 16 then
         Raise_Exception
         (  Use_Error'Identity,
            (  "Text length must be"
            &  Stream_Element_Offset'Image (Message'Length - 16)
         )  );
      end if;
      declare
         Key : constant Stream_Element_Array :=
                        Get_Key_Stream (Cipher'Access, True);
      begin
         Start (Digest, Key (Key'First..Key'First + 31));
      end;
      String'Write (Digest'Access, Data);
      Write (Digest, (1..(-Data'Length) mod 16 => 0));
      Write
      (  Digest,
         Message (Message'First..Message'Last - 16)
      );
      Write (Digest, (1..(-Text'Length) mod 16 => 0));
      Write (Digest, Little_Endian (Data'Length));
      Write (Digest, Little_Endian (Text'Length));
      Stop (Digest, Tag);
      if Tag /= Message (Message'Last - 15..Message'Last) then
         Raise_Exception (Data_Error'Identity, "Signature error");
      end if;
      Decrypt
      (  Cipher,
         Message (Message'First..Message'Last - 16),
         Text
      );
   end Decrypt;

end Strings_Edit.ChaCha20.AEAD;
