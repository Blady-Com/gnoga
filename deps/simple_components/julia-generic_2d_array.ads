--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Julia.Generic_2D_Array                      Luebeck            --
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
   type Row_Index_Type is range <>;
   type Column_Index_Type is range <>;
   type Element_Type is private;
   type Element_Array_Type is
      array (Row_Index_Type range <>, Column_Index_Type range <>)
         of Element_Type;
   with function Julia_Type return datatype_t;
package Julia.Generic_2D_Array is
--
-- Columns -- The array's length in the second dimension
--
--    Object - A Julia array object
--
-- Returns :
--
--    The number of columns
--
   function Columns (Object : value_t) return Natural;
--
-- Get -- Julia array element
--
--    Object - The Julia array
--    Row    - The element row 1..Rows
--    Column - The element column 1..Columns
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
            (  Object : value_t;
               Row    : Positive;
               Column : Positive
            )  return Element_Type;
--
-- Rows -- The array's length in the first dimension
--
--    Object - A Julia array object
--
-- Returns :
--
--    The number of rows
--
   function Rows (Object : value_t) return Natural;
--
-- Set -- Julia array element
--
--    Object - The Julia array
--    Row    - The element row 1..Rows
--    Column - The element column 1..Columns
--    Value  - The element value to set
--
-- Exceptions :
--
--    Constraint_Error - The index is out of range
--
   procedure Set
             (  Object  : value_t;
                Row     : Positive;
                Column  : Positive;
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

end Julia.Generic_2D_Array;
