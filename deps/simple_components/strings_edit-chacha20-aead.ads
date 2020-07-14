--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.ChaCha20.AEAD                  Luebeck            --
--  Interface                                      Summer, 2018       --
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

package Strings_Edit.ChaCha20.AEAD is
--
-- Encrypt -- Authenticated encryption with additional data
--
--    Cipher  - The cipher
--    Text    - The text to encrypt
--    Data    - The additional data
--    Message - The result
--
-- These procedures  encrypt  and sign Text using Cipher.  Data  is  the
-- additional data. The length of Message is the length of Text + 16.
--
-- Exceptions :
--
--    Use_Error - Invalid length of Message
--
   procedure Encrypt
             (  Cipher  : in out ChaCha20_Cipher;
                Text    : Stream_Element_Array;
                Data    : Stream_Element_Array;
                Message : out Stream_Element_Array
             );
   procedure Encrypt
             (  Cipher  : in out ChaCha20_Cipher;
                Text    : String;
                Data    : String;
                Message : out Stream_Element_Array
             );
--
-- Decrypt -- Authenticated encryption with additional data
--
--    Cipher  - The cipher
--    Text    - The text to encrypt
--    Data    - The additional data
--    Message - The result
--
-- These procedures  decrypt  and sign Text using Cipher.  Data  is  the
-- additional data. The length of Message is the length of Text + 16.
--
-- Exceptions :
--
--    Data_Error - Invalid signature
--    Use_Error  - Invalid length of Message
--
   procedure Decrypt
             (  Cipher  : in out ChaCha20_Cipher;
                Message : Stream_Element_Array;
                Data    : Stream_Element_Array;
                Text    : out Stream_Element_Array
             );
   procedure Decrypt
             (  Cipher  : in out ChaCha20_Cipher;
                Message : Stream_Element_Array;
                Data    : String;
                Text    : out String
             );
end Strings_Edit.ChaCha20.AEAD;
