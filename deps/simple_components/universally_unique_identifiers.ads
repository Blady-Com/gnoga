--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Universally_Unique_Identifiers              Luebeck            --
--  Interface                                      Winter, 2021       --
--                                                                    --
--                                Last revision :  13:12 05 Jan 2021  --
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

with Ada.Calendar;  use Ada.Calendar;
with IEEE_754;      use IEEE_754;

package Universally_Unique_Identifiers is
--
-- UUID_Value -- RFC-4122, section 4.1.2
--
   type UUID_Value is array (1..16) of Byte;
--
-- UUID_String -- The textual represenation of UUID
--
   subtype UUID_String is String (1..36);
--
-- Node_ID -- The node ID used in UUID generation
--
   subtype Node_ID is String (1..6);
--
-- Create -- An UUID using pseudo-random numbers
--
-- Returns :
--
--    UUID
--
   function Create return UUID_Value;
--
-- Create -- An UUID using the time stamp and node ID
--
--    ID    - The node identification, 6 characters
--    Stamp - The time stamp
--
-- Returns :
--
--    UUID
--
   function Create
            (  ID    : Node_ID;
               Stamp : Time := Clock
            )  return UUID_Value;
--
-- <, <=, >=, > -- Comparisons
--
--    Left  - First argument to compare
--    Right - Second argument to compare
--
-- Returns :
--
--    Comparison result
--
   function "<"  (Left, Right : UUID_Value) return Boolean;
   function "<=" (Left, Right : UUID_Value) return Boolean;
   function ">=" (Left, Right : UUID_Value) return Boolean;
   function ">"  (Left, Right : UUID_Value) return Boolean;

end Universally_Unique_Identifiers;
