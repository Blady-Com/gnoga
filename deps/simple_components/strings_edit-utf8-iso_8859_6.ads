--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.ISO_8859_6                Luebeck            --
--  Interface                                      Autumn, 2018       --
--                                                                    --
--                                Last revision :  12:27 04 Nov 2018  --
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
--  This package provides conversions between UTF-8 strings and  ISO/IEC
--  8859-6 encoded strings. 
--
package Strings_Edit.UTF8.ISO_8859_6 is
--
-- From_ISO_8859_6 -- Conversion from ISO/IEC 8859-6
--
--    Value        - Character or a ISO/IEC 8859-6 string
--  [ Substitute ] - For unused codes
--
-- These functions convert the parameter Value from ISO 8859-6.
--
-- Returns :
--
--    Value encoded in UTF-8 or Wide_String (UCS-2)
--
-- Exceptions :
--
--    Constraint_Error - Unused ISO/IEC 8859-6 code
--
   function From_ISO_8859_6 (Value : Character) return Code_Point;
   function From_ISO_8859_6
            (  Value      : Character;
               Substitute : Code_Point
            )  return Code_Point;
   function From_ISO_8859_6 (Value : String) return String;
   function From_ISO_8859_6
            (  Value      : String;
               Substitute : Code_Point
            )  return String;
   function From_ISO_8859_6 (Value : String) return Wide_String;
   function From_ISO_8859_6
            (  Value      : String;
               Substitute : Wide_Character
            )  return Wide_String;
--
-- To_ISO_8859_6 -- Conversion to ISO 8859-6
--
--    Value        - The UTF-8 string to convert
--  [ Substitute ] - For unsupported code points
--
-- These   functions  convert  to  ISO/IEC 8859-6 character string.  The
-- parameter Substitute specifies the character that substitutes invalid
-- code  points in Value. If omitted Constraint_Error is propagated when
-- invalid code points appear in Value. 
--
-- Returns :
--
--    ISO 8859-6 equivalent
--
-- Exceptions :
--
--    Constraint_Error - Non-ISO/IEC code points in Value
--    Data_Error       - Illegal UTF-8 string
--
   function To_ISO_8859_6 (Value : Code_Point) return Character;
   function To_ISO_8859_6
            (  Value      : Code_Point;
               Substitute : Character
            )  return Character;
   function To_ISO_8859_6 (Value : String) return String;
   function To_ISO_8859_6
            (  Value      : String;
               Substitute : Character
            )  return String;
   function To_ISO_8859_6 (Value : Wide_String) return String;
   function To_ISO_8859_6
            (  Value      : Wide_String;
               Substitute : Character
            )  return String;

end Strings_Edit.UTF8.ISO_8859_6;
