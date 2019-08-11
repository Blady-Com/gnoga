--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Object_Identifiers             Luebeck            --
--                                                 Summer, 2019       --
--  Interface                                                         --
--                                Last revision :  18:40 01 Aug 2019  --
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
-- The package provides text  edit for object identifiers (OID)  defined
-- by  the  International  Telecommunications  Union  (ITU).  OID  is  a
-- sequence of decimal natural integers separated by dots.
--
with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

package Strings_Edit.Object_Identifiers is
--
-- Object Identifiers (OID)
--
   type Subindentifier_Type is new Natural;
   type Object_Identifier is
      array (Positive range <>) of Subindentifier_Type;
--
-- < -- Ordering.  OID are ordered by numbers of the components. I.e.
--      1.231.4 precedes 2.1 because 1 precedes 2
--
   function "<"  (Left, Right : Object_Identifier) return Boolean;
   function "<=" (Left, Right : Object_Identifier) return Boolean;
   function ">"  (Left, Right : Object_Identifier) return Boolean;
   function ">=" (Left, Right : Object_Identifier) return Boolean;
--
-- Compare -- Compare identifiers
--
--    Left, Right - Object identifiers
--
-- Returns :
--
--    Comparison result
--
   function Compare (Left, Right : Object_Identifier) return Precedence;
--
-- Get -- Object identifier in dotted notation for string
--
--    Source  - The source string
--    Pointer - The location in the string to start with, advanced
--    Value   - The object identifier
--    Last    - The last subidentifier stored
--
-- Exceptions :
--
--    Constraint_Error - The object is too large
--    End_Error        - No object identifier
--    Data_Error       - Syntax error
--    Layout_Error     - Pointer is outside Source'First..Source'Last +1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : in out Object_Identifier;
                Last    : out Integer
             );
--
-- Get -- Object identifier in dotted notation for string
--
--    Source  - The source string
--    Pointer - The location in the string to start with, advanced
--
-- Returns :
--
--    The object identifier
--
-- Exceptions :
--
--    Constraint_Error - The object is too large
--    End_Error        - No object identifier
--    Data_Error       - Syntax error
--    Layout_Error     - Pointer is outside Source'First..Source'Last +1
--
   function Get
            (  Source  : String;
               Pointer : access Integer
            )  return Object_Identifier;
--
-- Image -- A dotted representation of identifier
--
--    Value - The identifier
--
-- Returns :
--
--    The result string, e.g. 1.23.10
--
   function Image (Value : Object_Identifier) return String;
--
-- Put -- Object identifier into string
--
--    Destination - The string to put object identifier into
--    Pointer     - The first element to write
--    Value       - The value
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- The parameter Pointer is advanced beyond the value output. The object
-- is put in dotted notation
--
-- Exceptions :
--
--    Layout_Error - Pointer is outside bounds or no room for output
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Object_Identifier;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Value -- Object identifier in dotted notation from string
--
--    Source - The source string
--
-- Exceptions :
--
--    End_Error  - No object identifier
--    Data_Error - Syntax error or not all string parsed
--
   function Value (Source : String) return Object_Identifier;

end Strings_Edit.Object_Identifiers;
