--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Julia.Generic_1D_Array                      Luebeck            --
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
   type Index_Type is range <>;
   type Element_Type is private;
   type Element_Array_Type is
      array (Index_Type range <>) of Element_Type;
   with function Julia_Type return datatype_t;
package Julia.Generic_1D_Array is
--
-- Get -- Julia array element
--
--    Object   - The Julia array
--    Position - The element position 1..Length
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
            (  Object   : value_t;
               Position : Positive
            )  return Element_Type;
--
-- Length -- The array length
--
--    Object - A Julia array object
--
-- Returns :
--
--    The length of
--
   function Length (Object : value_t) return Natural;
--
-- Set -- Julia array element
--
--    Object   - The Julia array
--    Position - The element position 1..Length
--    Value    - The element value to set
--
-- Exceptions :
--
--    Constraint_Error - The index is out of range
--
   procedure Set
             (  Object   : value_t;
                Position : Positive;
                Element  : Element_Type
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

end Julia.Generic_1D_Array;
