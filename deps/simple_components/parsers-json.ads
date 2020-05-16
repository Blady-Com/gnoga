--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.JSON                                Luebeck            --
--  Implementation                                 Autumn, 2019       --
--                                                                    --
--                                Last revision :  19:57 14 Sep 2019  --
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

with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Strings_Edit;      use Strings_Edit;

package Parsers.JSON is
--
-- JSON_Value_Type -- The JSON value type
--
   type JSON_Value_Type is
        (  JSON_Boolean, -- True / False
           JSON_Null,    -- Null
           JSON_Number,  -- Numeric value
           JSON_String,  -- Text
           JSON_Array,   -- Array of any values, a sequence
           JSON_Object   -- Name to value mapping
        );
--
-- JSON_Value -- The JSON value
--
   type JSON_String_Ptr is access constant String;
   type JSON_Value;
   type JSON_Value_Ptr is access constant JSON_Value;
   type JSON_Sequence;
   type JSON_Sequence_Ptr is access constant JSON_Sequence;
   type JSON_Pair_Array;
   type JSON_Pair_Array_Ptr is access constant JSON_Pair_Array;
   type JSON_Value
        (  JSON_Type : JSON_Value_Type := JSON_Boolean
        )  is
   record
      case JSON_Type is
         when JSON_Boolean =>
            Condition : Boolean := False;
         when JSON_Null =>
            null;
         when JSON_Number =>
            Value     : Long_Float := 0.0;
         when JSON_String =>
            Text      : JSON_String_Ptr;
         when JSON_Array =>
            Sequence  : JSON_Sequence_Ptr;
         when JSON_Object =>
            Map       : JSON_Pair_Array_Ptr;
      end case;
   end record;
   type JSON_Sequence is array (Positive range <>) of JSON_Value;
   type JSON_Pair is record
      Name  : JSON_String_Ptr;
      Value : JSON_Value;
   end record;
   type JSON_Pair_Array is array (Positive range <>) of JSON_Pair;
--
-- Put -- Put a value in JSON format
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The value to be put
--    Escape      - Escape any non-printable or non-ASCII characters
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- Exceptions:
--
--    Data_Error   - Invalid UTF-8 encoding of a string or tag
--    Layout_Error - Pointer is  not in Destination'Range or there is no
--                   room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : JSON_Sequence;
                Escape      : Boolean   := False;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : JSON_Pair_Array;
                Escape      : Boolean   := False;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : JSON_Value;
                Escape      : Boolean   := False;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : String;
                Escape      : Boolean   := False;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Image -- String in JSON format
--
--    Value  - The value to be converted
--    Escape - Escape any non-printable or non-ASCII characters
--
-- Returns :
--
--    The result string
--
-- Exceptions:
--
--    Data_Error - Invalid UTF-8 encoding of a string or tag
--
   function Image
            (  Value  : JSON_Sequence;
               Escape : Boolean := False
            )  return String;
   function Image
            (  Value  : JSON_Pair_Array;
               Escape : Boolean := False
            ) return String;
   function Image
            (  Value  : JSON_Value;
               Escape : Boolean := False
            )  return String;
   function Image
            (  Value  : String;
               Escape : Boolean := False
            )  return String;

private
   WS : constant Character_Set :=
                    To_Set
                    (  ' '
                    &  Character'Val (9)
                    &  Character'Val (10)
                    &  Character'Val (13)
                    );
--
-- Check_Spelling -- Of a name, no checks
--
   procedure Check_Spelling (Name : String);
--
-- Check_Matched -- Check if no broken keyword matched
--
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;
--
-- Operations -- All the operations supported
--
   type Operations is
        (  Comma,        Colon,        -- Commas , and :
           Left_Brace,   Right_Brace,  -- Braces {}
           Left_Bracket, Right_Bracket -- Brackets []
        );
--
-- "and" -- Checks operation associations, always True (Ok)
--
   function "and" (Left, Right : Operations) return Boolean;
--
-- Is_Commutative -- No commutative operations, always False
--
   function Is_Commutative (Left, Right : Operations) return Boolean;
--
-- Is_Inverse -- No commutative operations, always False
--
   function Is_Inverse (Operation : Operations) return Boolean;
--
-- Group_Inverse -- No commutative operations, never called
--
   function Group_Inverse (Operation : Operations) return Operations;
--
-- Priorities -- The levels of association
--
   type Priorities is mod 2;
--
-- Tokens -- The lexical tokens
--
   type JSON_Argument is record
      Name  : JSON_String_Ptr;
      Value : JSON_Value;
   end record;

end Parsers.JSON;
