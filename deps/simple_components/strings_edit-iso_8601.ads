--                                                                    --
--  package Strings_Edit.ISO_8601   Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2019       --
--                                                                    --
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

with Ada.Calendar;  use Ada.Calendar;

package Strings_Edit.ISO_8601 is
   subtype Second_Fraction is Integer range 0..6;
--
-- Get -- ISO 8601 time or duration
--
--    Source  - The destination string
--    Pointer - The string pointer
--    Value   - The result
--
-- This procedure gets time in ISO 8601 format
--
-- Exceptions :
--
--    Constraint_Error   - Too large values
--    Data_Error         - Syntax error in time format
--    End_Error          - No time found
--    Layout_Error       - Illegal Pointer or no space available
--    Time_Error         - Time errors
--    Unknown_Zone_Error - Time zone error
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Time
             );
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Duration
             );
--
-- Image -- Text representation of time
--
--    Value    - The time
--    Fraction - Of the second output
--  [ Local ]  - Use local time if True
--
-- Returns :
--
--    The ISO 8601 result
--
-- Exceptions :
--
--    Time_Error - Time errors
--
   function Image
            (  Value    : Time;
               Fraction : Second_Fraction := 0;
               Local    : Boolean := False
            )  return String;
   function Image
            (  Value    : Duration;
               Fraction : Second_Fraction := 0
            )  return String;
--
-- Put -- Time into string
--
--    Destination - The string to put object identifier into
--    Pointer     - The first element to write
--    Value       - The value
--    Fraction    - Of the second output
--  [ Local ]     - If true, local time is used otherwise it is UTC
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- The parameter Pointer is advanced beyond the value output. The object
-- is put in ISO 8601 format
--
-- Exceptions :
--
--    Layout_Error - Pointer is outside bounds or no room for output
--    Time_Error   - Time errors
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Time;
                Fraction    : Second_Fraction := 0;
                Local       : Boolean         := False;
                Field       : Natural         := 0;
                Justify     : Alignment       := Left;
                Fill        : Character       := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Duration;
                Fraction    : Second_Fraction := 0;
                Field       : Natural         := 0;
                Justify     : Alignment       := Left;
                Fill        : Character       := ' '
             );
--
-- Value -- Get time or duration from string
--
--    Source - The source string
--
-- Exceptions :
--
--    Constraint_Error   - Too large values
--    End_Error          - No time found
--    Data_Error         - Syntax error or not all string parsed
--    Time_Error         - Time errors
--    Unknown_Zone_Error - Time zone error
--
   function Value (Source : String) return Time;
   function Value (Source : String) return Duration;

end Strings_Edit.ISO_8601;
