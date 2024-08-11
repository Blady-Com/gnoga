--                                                                    --
--  package Py.Class                Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2022       --
--                                                                    --
--                                Last revision :  20:47 23 Jun 2022  --
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
-- Python class example.  This package provides a class point wrapped in
-- the module point. So the type is referenced as point.point.  The type
-- instances contain Ada record type Point.  The type has two attributes
-- x and y.  And method length() that calculates the vector length.  The
-- object constructor takes two keyed parameters x and y.
--
package Py.Class is
--
-- Create_Point_Type -- Create the type and the module
--
-- Returns :
--
--    The type object
--
   function Create_Point_Type return Handle;
--
-- Point -- Contained Ada type
--
   type Point is record
      X : Float;
      Y : Float;
   end record;
--
-- Get -- Ada value of the object
--
--    Object - The Python object
--
-- Returns :
--
--    The object's contents
--
   function Get (Object : Handle) return Point;
--
-- Set -- Ada value into the object
--
--    Object - The Python object
--    Value  - The value to set into
--
   procedure Set (Object : Handle; Value : Point);

end Py.Class;
