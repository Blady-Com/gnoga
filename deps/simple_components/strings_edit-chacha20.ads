--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.ChaCha20                       Luebeck            --
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
--
--  The package provides an implementation of the ChaCha20 stream cipher
--  as described in the RFC 8439
--
--     https://tools.ietf.org/html/rfc8439
--
--  The cipher was originally developed by D. J. Bernstein.
--
with Ada.Streams;  use Ada.Streams;
with Interfaces;   use Interfaces;

with Ada.Finalization;

package Strings_Edit.ChaCha20 is

   ChaCha20_Block_Size : constant := 16 * 4;
   subtype ChaCha20_Key   is Stream_Element_Array (1..32);
   subtype ChaCha20_Nonce is Stream_Element_Array (1..12);
--
-- ChaCha20_Cipher -- Cipher object
--
   type ChaCha20_Cipher is new Ada.Finalization.Controlled with private;
--
-- Decrypt|Encrypt -- An array
--
--    Cipher - The cipher object to use
--    Input  - The array to decrypt (stream element array or string)
--
-- Since this  is a stream cipher  Decrypt  and  Encrypt  are same.  The
-- object maintains its state it  is safe to call these  functions  with
-- input arrays of any size consequently.
--
-- Returns :
--
--    The result
--
   function Decrypt
            (  Cipher : access ChaCha20_Cipher;
               Input  : Stream_Element_Array
            )  return Stream_Element_Array;
   function Decrypt
            (  Cipher : access ChaCha20_Cipher;
               Input  : String
            )  return String;
   function Encrypt
            (  Cipher : access ChaCha20_Cipher;
               Input  : Stream_Element_Array
            )  return Stream_Element_Array;
   function Encrypt
            (  Cipher : access ChaCha20_Cipher;
               Input  : String
            )  return String;
--
-- Decrypt|Encrypt -- An array
--
--    Cipher - The cipher object to use
--    Input  - The array to decrypt (stream element array or string)
--    Output - The result
--
-- Since this is a stream cipher Decrypt and Encrypt are same.
--
-- Exceptions :
--
--    Constraint_Error - Input and Output have different sizes
--
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : Stream_Element_Array;
                Output : out Stream_Element_Array
             );
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : Stream_Element_Array;
                Output : out String
             );
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : String;
                Output : out String
             );
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : String;
                Output : out Stream_Element_Array
             );
   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : Stream_Element_Array;
                Output : out Stream_Element_Array
             );
   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : Stream_Element_Array;
                Output : out String
             );
   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : String;
                Output : out String
             );
   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Input  : String;
                Output : out Stream_Element_Array
             );
--
-- Decrypt|Encrypt -- An array
--
--    Cipher - The cipher object to use
--    Data   - The array to decrypt (stream element array or string)
--
-- Since this is a stream cipher Decrypt and Encrypt are same.
--
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Data   : in out Stream_Element_Array
             );
   procedure Decrypt
             (  Cipher : in out ChaCha20_Cipher;
                Data   : in out String
             );
   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Data   : in out Stream_Element_Array
             );
   procedure Encrypt
             (  Cipher : in out ChaCha20_Cipher;
                Data   : in out String
             );
--
-- Get_Count -- Get block count
--
--    Cipher - The cipher object
--
-- The current cipher block count.  When the count nears  2**32  the key
-- and nonce should be changed.
--
-- Returns :
--
--    Block count
--
   function Get_Count (Cipher : ChaCha20_Cipher) return Unsigned_32;
--
-- Get_Key_Stream -- Get key stream
--
--    Cipher - The cipher object
--    Full   - Generate a new block, if true
--
-- This function returns  a portion of the key stream.  The elements  of
-- the  stream  key are xor-ed  with  the input  in order to  encrypt or
-- decrypt it.  The result length is  1..64 elements  depending  on  the
-- cipher state. When Full is true, the result is always 64 elements.
--
-- Returns :
--
--    Key stream
--
   function Get_Key_Stream
            (  Cipher : access ChaCha20_Cipher;
               Full   : Boolean := False
            )  return Stream_Element_Array;
--
-- Set_Key -- Set cipher key and nonce
--
--    Cipher - The cipher object
--    Key    - The 256-bit key
--    Nonce  - The 64-bit nonce
--    Count  - The block count
--
   procedure Set_Key
             (  Cipher : in out ChaCha20_Cipher;
                Key    : ChaCha20_Key;
                Nonce  : ChaCha20_Nonce := (others => 0);
                Count  : Unsigned_32    := 0
             );
--
-- ChaCha20_Stream -- Ciphering stream
--
--   Transport - The carrier stream
--   Size      - The output buffer
--
-- The stream object when read takes input from the transport stream and
-- deciphers it.  The result is the data read.  Writing into the  stream
-- ciphers the input and then writes the result into the transport.
--
   type ChaCha20_Stream
        (  Transport : access Root_Stream_Type'Class;
           Size      : Stream_Element_Count
        )  is new Root_Stream_Type with private;
--
-- Get_Count -- Get block count
--
--    Cipher - The cipher object
--
-- The current cipher block count.  When the count nears  2**32  the key
-- and nonce should be changed.
--
-- Returns :
--
--    Block count
--
   function Get_Count (Stream : ChaCha20_Stream) return Unsigned_32;
--
-- Set_Key -- Set cipher key and nonce
--
--    Stream - The stream object
--    Key    - The 256-bit key
--    Nonce  - The 64-bit nonce
--    Count  - The block count
--
   procedure Set_Key
             (  Stream : in out ChaCha20_Stream;
                Key    : ChaCha20_Key;
                Nonce  : ChaCha20_Nonce := (others => 0);
                Count  : Unsigned_32    := 0
             );
--
-- Get_Key_Stream -- Get key stream
--
--    Stream - The stream object
--    Full   - Generate a new block, if true
--
-- This function returns  a portion of the key stream.  The elements  of
-- the  stream  key are xor-ed  with  the input  in order to  encrypt or
-- decrypt it.  The result length is  1..64 elements  depending  on  the
-- cipher state. When Full is true, the result is always 64 elements.
--
-- Returns :
--
--    Key stream
--
   function Get_Key_Stream
            (  Stream : access ChaCha20_Stream;
               Full   : Boolean := False
            )  return Stream_Element_Array;
--
-- Read -- Overrides Ada.Streams...
--
   procedure Read
             (  Stream : in out ChaCha20_Stream;
                Item   : out Stream_Element_Array;
                Last   : out Stream_Element_Offset
             );
--
-- Write -- Overrides Ada.Streams...
--
   procedure Write
             (  Stream : in out ChaCha20_Stream;
                Item   : Stream_Element_Array
             );
private
   pragma Assert (Stream_Element'Size = 8);
   pragma Assert (Character'Size = 8);
   type Block_Type is array (0..15) of Unsigned_32;
   subtype ChaCha20_Count is
      Stream_Element_Count range 0..ChaCha20_Block_Size - 1;
   type ChaCha20_Cipher is new Ada.Finalization.Controlled with record
      State  : Block_Type :=
               (  16#61707865#,
                  16#3320646e#,
                  16#79622d32#,
                  16#6b206574#,
                  0, 0, 0, 0,
                  0, 0, 0, 0,
                  0, 0, 0, 0
               );
      Stream : Stream_Element_Array (ChaCha20_Count);
      Count  : ChaCha20_Count := ChaCha20_Count'Last;
   end record;

   type ChaCha20_Stream
        (  Transport : access Root_Stream_Type'Class;
           Size      : Stream_Element_Count
        )  is new Root_Stream_Type with
   record
      Cipher : aliased ChaCha20_Cipher;
      Buffer : Stream_Element_Array (1..Size);
   end record;

   function "+" (Data : Stream_Element_Array) return Unsigned_32;

end Strings_Edit.ChaCha20;
