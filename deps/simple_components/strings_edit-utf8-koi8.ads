--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.KOI8                      Luebeck            --
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
--  This package provides  conversions  between UTF-8 strings  and  KOI8
--  encoded strings. 
--
package Strings_Edit.UTF8.KOI8 is
--
-- From_KOI8 -- Conversion from KOI8
--
--    Value - Character or a KOI8 string
--
-- These functions convert the parameter Value from KOI8.
--
-- Returns :
--
--    Value encoded in UTF-8 or Wide_String (UCS-2)
--
   function From_KOI8 (Value : Character) return Code_Point;
   function From_KOI8 (Value : String   ) return String;
   function From_KOI8 (Value : String   ) return Wide_String;
--
-- To_KOI8 -- Conversion to KOI8
--
--    Value        - The UTF-8 string to convert
--  [ Substitute ] - For KOI8 code points
--
-- These   functions  convert  to  KOI8 character string.  The parameter
-- Substitute  specifies  the character  that  substitutes invalid  code
-- points in Value.  If omitted  Constraint_Error  is propagated when an
-- invalid code points appear in Value. 
--
-- Returns :
--
--    KOI8 equivalent
--
-- Exceptions :
--
--    Constraint_Error - Non-KOI8 code points in Value
--    Data_Error       - Illegal UTF-8 string
--
   function To_KOI8 (Value : Code_Point) return Character;
   function To_KOI8
            (  Value      : Code_Point;
               Substitute : Character
            )  return Character;
   function To_KOI8 (Value : String) return String;
   function To_KOI8
            (  Value      : String;
               Substitute : Character
            )  return String;
   function To_KOI8 (Value : Wide_String) return String;
   function To_KOI8
            (  Value      : Wide_String;
               Substitute : Character
            )  return String;

end Strings_Edit.UTF8.KOI8;
