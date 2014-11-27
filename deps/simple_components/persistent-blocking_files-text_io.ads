--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Blocking_Files.                  Luebeck            --
--        Text_IO                                  Spring, 2014       --
--  Interface                                                         --
--                                Last revision :  10:05 22 Nov 2014  --
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

package Persistent.Blocking_Files.Text_IO is
--
-- Get -- A memory index from string
--
--    Source  - The source string
--    Pointer - To start at, advanced to the following position
--    Value   - The result
--
-- Exceptions :
--
--    Constraint_Error - Data field is out of range
--    Data_Error       - Syntax error
--    End_Error        - No data
--    Layout_Error     - Pointer is not in range
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Byte_Index
             );
--
-- Image -- Textual representation of a file block number
--
--    Value - The block number
--
-- Returns :
--
--    The representation of
--
   function Image (Value : Block_Count) return String;
--
-- Image -- Textual representation of memory index
--
--    Value      - The memory byte index
--    Put_Offset - Don't output offset if false
--
-- Returns :
--
--    The offset's representation of
--
   function Image
            (  Value      : Byte_Index;
               Put_Offset : Boolean := True
            )  return String;
--
-- Image -- Textual representation of block offset
--
--    Value - The offset
--
-- Returns :
--
--    The offset's representation of
--
   function Image (Value : Block_Offset) return String;
--
-- Put -- Put block number into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The block number to be put
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places the number specified  by  the  parameter  Value
-- into  the  output  string Destination. The string is written starting
-- from Destination (Pointer).
--
-- Exceptions:
--
--    Layout_Error - Pointer is not in Destination'Range or  there is no
--                   room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Block_Count;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Put -- Put memory index into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The value to be put
--    Put_Offset - Don't output offset if false
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places the number specified  by  the  parameter  Value
-- into  the  output  string Destination. The string is written starting
-- from Destination (Pointer).
--
-- Exceptions:
--
--    Layout_Error - Pointer is not in Destination'Range or  there is no
--                   room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Byte_Index;
                Put_Offset  : Boolean   := True;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Put -- Put block offset into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The value to be put
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This procedure places the number specified  by  the  parameter  Value
-- into  the  output  string Destination. The string is written starting
-- from Destination (Pointer).
--
-- Exceptions:
--
--    Layout_Error - Pointer is not in Destination'Range or  there is no
--                   room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Block_Offset;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Value -- A memory index from string
--
--    Source - The source string
--
-- Returns :
--
--    Memory index
--
-- Exceptions :
--
--    Constraint_Error - Data field is out of range
--    Data_Error       - Syntax error
--    End_Error        - No data
--
   function Value (Source : String) return Byte_Index;

end Persistent.Blocking_Files.Text_IO;
