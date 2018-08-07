--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.ChaCha20.Poly1305              Luebeck            --
--  Interface                                      Summer, 2018       --
--                                                                    --
--                                Last revision :  10:21 03 Aug 2018  --
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

package Strings_Edit.ChaCha20.Poly1305 is

   subtype ChaCha20_Tag is Stream_Element_Array (1..16);
--
-- Digest -- Calculate Poly1305 digest
--
--    Message - The message to calculate digest of
--    Key     - The key
--  [ Nonce ] - One-time key generation
--
-- When both Key and  Nonce are specified a one-time key is generated as
-- recommended in the section 2.6 of RFC 8439
--
-- Returns :
--
--    Poly1305 digest of Message
--
   function Digest
            (  Message : Stream_Element_Array;
               Key     : ChaCha20_Key
            )  return ChaCha20_Tag;
   function Digest
            (  Message : String;
               Key     : ChaCha20_Key
            )  return ChaCha20_Tag;
   function Digest
            (  Message : Stream_Element_Array;
               Key     : ChaCha20_Key;
               Nonce   : ChaCha20_Nonce
            )  return ChaCha20_Tag;
   function Digest
            (  Message : String;
               Key     : ChaCha20_Key;
               Nonce   : ChaCha20_Nonce
            )  return ChaCha20_Tag;
--
-- Poly1305_Stream -- Stream  to evaluate  Poly1305.  Writing  into  the
--                    stream calculates a portion of the digest over the
--                    data written
--
   type Poly1305_Stream is new Root_Stream_Type with private;
--
-- Start -- Initiate Poly1305 digest computation
--
--    Stream  - The stream object
--    Key     - The 256-bit key
--  [ Nonce ] - One-time key generation
--
-- When both Key and  Nonce are specified a one-time key is generated as
-- recommended in the section 2.6 of RFC 8439.
--
   procedure Start
             (  Stream : in out Poly1305_Stream;
                Key    : ChaCha20_Key
             );
   procedure Start
             (  Stream : in out Poly1305_Stream;
                Key    : ChaCha20_Key;
                Nonce  : ChaCha20_Nonce
             );
--
-- Stop -- End Poly1305 digest computation
--
--    Stream - The stream object
--    Digest - The result
--
   procedure Stop
             (  Stream : in out Poly1305_Stream;
                Digest : out ChaCha20_Tag
             );
--
-- Read -- Overrides Ada.Streams...
--
-- Exceptions :
--
--    End_Error - The stream cannot be read
--
   procedure Read
             (  Stream : in out Poly1305_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
--
-- Write -- Overrides Ada.Streams...
--
   procedure Write
             (  Stream : in out Poly1305_Stream;
                Item   : Stream_Element_Array
             );
private
   subtype Bit_Offset is Integer range 0..9 * 32 - 1;
   type Unsigned is array (1..9) of Unsigned_32;

   type Poly1305_Stream is new Root_Stream_Type with record
      Accum : Unsigned := (others => 0);
      R     : Unsigned := (others => 0);
      S     : Unsigned := (others => 0);
      Count : Stream_Element_Count := 0;
      Data  : Stream_Element_Array (1..16) := (others => 0);
   end record;

end Strings_Edit.ChaCha20.Poly1305;
