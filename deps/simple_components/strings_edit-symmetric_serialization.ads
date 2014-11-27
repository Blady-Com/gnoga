--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Symmetric_Serialization        Luebeck            --
--  Interface                                      Winter, 2008       --
--                                                                    --
--                                Last revision :  18:59 10 Feb 2008  --
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
--  The package provides a  simple  symmetric  encryption  and  encoding
--  tools  for  serializing  plain strings. The main objective is saving
--  and transporting user credentials in a  mangled  format.  The  cases
--  where   this   should  be  applied  are  only  ones  where  stronger
--  asymmetric   methods  were  impossible.  For  example  when  it   is
--  necessary  to  store  user  password  as a string with an ability to
--  restore  it  back.  The  encoding  uses  64 bits alphabet of digits,
--  letters  and characters '_' and '~'. The key used for encryption can
--  be derived from the application name, registered user name etc.
--
package Strings_Edit.Symmetric_Serialization is
   pragma Elaborate_Body (Strings_Edit.Symmetric_Serialization);

   subtype Encoded_String is String (1..(256 * 8 + 5) / 6);
--
-- Decode -- An encoded string
--
--    Data - To decode
--    Key  - The key
--
-- This function decodes Data using Key.
--
-- Returns :
--
--    Dencrypted and decoded Data
--
-- Exceptions :
--
--    Data_Error - Illegal content of Data
--
   function Decode (Data : Encoded_String; Key : String) return String;
--
-- Encode -- A plain string
--
--    Text - To encode, up to 256 characters long
--    Key  - The key
--
-- This  function encodes Text using Key. The key will be later required
-- in  order  to  decode the text. Text is padded with random characters
-- before encryption to improve cryptographic strength. Thus the content
-- of Text should carry its length.
--
-- Returns :
--
--    Encrypted and encoded Text
--
-- Exceptions :
--
--    Constraint_Error - Text is too long
--
   function Encode (Text : String; Key : String) return Encoded_String;

end Strings_Edit.Symmetric_Serialization;
