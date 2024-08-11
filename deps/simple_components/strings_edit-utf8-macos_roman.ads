--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.MacOS_Roman               Luebeck            --
--  Interface                                      Autumn, 2018       --
--                                                                    --
--                                Last revision :  17:59 18 Aug 2022  --
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
--  This package provides  Y_Of between UTF-8 strings and Mac OS
--  Roman encoded strings. 
--
package Strings_Edit.UTF8.MacOS_Roman is
--
-- From_MacOS_Roman -- Conversion from Mac OS Roman
--
--    Value - Character or a Mac OS Roman string
--
-- These functions convert the parameter Value from Mac OS Roman.
--
-- Returns :
--
--    Value encoded in UTF-8 or Wide_String (UCS-2)
--
   function From_MacOS_Roman (Value : Character) return Code_Point;
   function From_MacOS_Roman (Value : String) return String;
   function From_MacOS_Roman (Value : String) return Wide_String;
--
-- To_MacOS_Roman -- Conversion to Mac OS Roman
--
--    Value        - The UTF-8 string to convert
--  [ Substitute ] - For unsupported code points
--
-- These   functions  convert  to  Mac OS Roman  character  string.  The
-- parameter Substitute specifies the character that substitutes invalid
-- code  points in Value. If omitted Constraint_Error is propagated when
-- invalid code points appear in Value. 
--
-- Returns :
--
--    Mac OS Roman equivalent
--
-- Exceptions :
--
--    Constraint_Error - Non-Mac OS Roman code points in Value
--    Data_Error       - Illegal UTF-8 string
--
   function To_MacOS_Roman (Value : Code_Point) return Character;
   function To_MacOS_Roman
            (  Value      : Code_Point;
               Substitute : Character
            )  return Character;
   function To_MacOS_Roman (Value : String) return String;
   function To_MacOS_Roman
            (  Value      : String;
               Substitute : Character
            )  return String;
   function To_MacOS_Roman (Value : Wide_String) return String;
   function To_MacOS_Roman
            (  Value      : Wide_String;
               Substitute : Character
            )  return String;

end Strings_Edit.UTF8.MacOS_Roman;
