--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Universally_Unique_Identifiers.Edit         Luebeck            --
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

with Strings_Edit;  use Strings_Edit;

package Universally_Unique_Identifiers.Edit is
--
-- Get -- Skip blank characters
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The UUID
--
-- This procedure gets the UUID starting from  Source (Pointer). Pointer
-- is advanced after the UUID.
--
-- Exceptions :
--
--    Data_Error   - Invalid UUID format
--    End_Error    - No UUID found
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out UUID_Value
             );
--
-- Image -- Textual representation of an UUID
--
--    Value - The value
--
-- Returns :
--
--    The value in the format xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx
--
   function Image (Value : UUID_Value) return UUID_String;
--
-- Put -- Put an UUID into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The character to be put
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places  the specified character (Value parameter) into
-- the output string  Destination.  The string is  written starting from
-- the Destination (Pointer).
--
-- Exceptions:
--
--    Layout_Error - Pointer  is not in Destination'Range or there is no
--                   room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : UUID_Value;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Value -- Conversion from textual representation to an UUID
--
--    Source - In the format xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx
--
-- Returns :
--
--    The UUID
--
-- Exceptions :
--
--    Data_Error - Invalid format
--
   function Value (Source : String) return UUID_Value;

end Universally_Unique_Identifiers.Edit;
