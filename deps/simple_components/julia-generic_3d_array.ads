--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Julia.Generic_3D_Array                      Luebeck            --
--  Interface                                      Winter, 2019       --
--                                                                    --
--                                Last revision :  11:37 20 Jan 2019  --
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

generic
   type X_Index_Type is range <>;
   type Y_Index_Type is range <>;
   type Z_Index_Type is range <>;
   type Element_Type is private;
   type Element_Array_Type is
      array (  X_Index_Type range <>,
               Y_Index_Type range <>,
               Z_Index_Type range <>
            )  of Element_Type;
   with function Julia_Type return datatype_t;
package Julia.Generic_3D_Array is
--
-- Get -- Julia array element
--
--    Object  - The Julia array
--    X, Y, Z - The element co-ordinates
--
-- Returns :
--
--    The element value
--
-- Exceptions :
--
--    Constraint_Error - The index is out of range
--
   function Get
            (  Object  : value_t;
               X, Y, Z : Positive
            )  return Element_Type;
--
-- Set -- Julia array element
--
--    Object  - The Julia array
--    X, Y, Z - The element co-ordinates
--    Value   - The element value to set
--
-- Exceptions :
--
--    Constraint_Error - The index is out of range
--
   procedure Set
             (  Object  : value_t;
                X, Y, Z : Positive;
                Element : Element_Type
             );
--
-- To_Julia -- Convert Ada array to Julia object
--
--    Value - The value to convert
--
-- Returns :
--
--    Julia array (copy)
--
   function To_Julia (Value : Element_Array_Type) return value_t;
--
-- Value -- Convert Julia object to Ada array
--
--    Object - The value to convert
--
-- Returns :
--
--    Ada array (copy)
--
   function Value (Object : value_t) return Element_Array_Type;
--
-- {X|Y|Z}_Length -- The array's length
--
--    Object - A Julia array object
--
-- Returns :
--
--    The number of columns
--
   function X_Length (Object : value_t) return Natural;
   function Y_Length (Object : value_t) return Natural;
   function Z_Length (Object : value_t) return Natural;

end Julia.Generic_3D_Array;
