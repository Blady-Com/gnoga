--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.RADIX50                   Luebeck            --
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
--  This package  provides conversions  between UTF-8  strings and DEC's
--  RADIX-50 encoded strings. 
--
package Strings_Edit.UTF8.RADIX50 is
--
-- From_RADIX50 -- Conversion from RADIX-150
--
--    Value - Wide_Character string encoded in RADIX-50
--
-- This function converts the parameter Value from RADIX-50.  The source
-- is 16-bit elements each containing 3 characters.
--
-- Returns :
--
--    Value encoded in UTF-8 (ASCII)
--
-- Exceptions :
--
--    Data_Error - Invalid RADIX-50 code
--
   function From_RADIX50 (Value : Wide_String) return String;
--
-- To_RADIX50 -- Conversion to RADIX-50
--
--    Value        - The UTF-8 string to convert
--  [ Substitute ] - For non-RADIX-50 code points
--
-- These   functions  convert  to RADIX-50  words string.  The parameter
-- Substitute  specifies  the character  that  substitutes  invalid code
-- points  in Value.  If omitted  Constraint_Error  is  propagated  when
-- invalid code points appear in Value. 
--
-- Returns :
--
--    RADIX-50 equivalent
--
-- Exceptions :
--
--    Constraint_Error - Non-RADIX-50 code points in Value
--    Data_Error       - Illegal UTF-8 string
--    Use_Error        - Substitute is Non-RADIX-50
--
   function To_RADIX50 (Value : String) return Wide_String;
   function To_RADIX50
            (  Value      : String;
               Substitute : Character
            )  return Wide_String;

end Strings_Edit.UTF8.RADIX50;
