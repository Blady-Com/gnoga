--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Distinguished_Names            Luebeck            --
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
--  RFC 4514 distinguished names, e.g.
--
--  CN=Steve Hardcastle-Kille, OU=Computer Science, O=University
--      College London, C=GB
--
with Strings_Edit.Object_Identifiers;
use  Strings_Edit.Object_Identifiers;

with Strings_Edit.Lexicographical_Order;
use  Strings_Edit.Lexicographical_Order;

package Strings_Edit.Distinguished_Names is
--
-- Distinguished_Name -- RFC 1485 object
--
-- A distinguished name  is a sequence of components.  Each component is
-- a set of attributes. Each attribute is a pair key-value. A key can be
-- either text or OID.
--
   type Distinguished_Name (<>) is private;
   Null_Name : constant Distinguished_Name;
--
-- Attribute_Mode -- The type of the attribute key
--
   type Attribute_Mode is (OID_Keyed, Text_Keyed);
--
-- Attribute_Key -- The key of an attribute
--
--    Mode   - The mode of the attribute indicated by the key
--    Length - Of the key
--
-- The keys in the textual  form and given as  an object identifier  are
-- considered distinct. Equivalence in the sense of RFC 4519 is ignored.
-- Textual keys  are case-insensitive  and  may  contain  only  letters,
-- digits and '-'.
--
   type Attribute_Key
        (  Mode   : Attribute_Mode;
           Length : Natural
        )  is
   record
      case Mode is
         when OID_Keyed =>
            Identifier : Object_Identifier (1..Length);
         when Text_Keyed =>
            Text : String (1..Length);
      end case;
   end record;
   function "="  (Left, Right : Attribute_Key) return Boolean;
   function "<"  (Left, Right : Attribute_Key) return Boolean;
   function "<=" (Left, Right : Attribute_Key) return Boolean;
   function ">"  (Left, Right : Attribute_Key) return Boolean;
   function ">=" (Left, Right : Attribute_Key) return Boolean;
--
-- Name_Attribute -- A key-value pair
--
--    Mode         - The mode of the attribute indicated by the key
--    Key_Length   - The key length
--    Value_Length - The value length
--
   type Name_Attribute
        (  Mode         : Attribute_Mode;
           Key_Length   : Natural;
           Value_Length : Natural
        )  is
   record
      Key   : Attribute_Key (Mode, Key_Length);
      Value : String (1..Value_Length);
   end record;
--
-- Compare -- Compare attribute names (case-insensitive)
--
--    Left, Right - Operands to compare
--
-- Returns :
--
--    Comparison result
--
   function Compare (Left, Right : String) return Precedence;

--
-- Find_Attribute - Attribute by key
--
--    Name      - The distinguished name
--    Component - The component number 1..Get_Length
--    Attribute - The attribute key
--
-- Returns :
--
--    The attribute position if positive
--
-- Exceptions :
--
--    Constraint_Error - Component is out of range
--
   function Find_Attribute
            (  Name      : Distinguished_Name;
               Component : Positive;
               Attribute : Attribute_Key
            )  return Integer;
--
-- Get -- Distinguished name
--
--    Source  - The source string
--    Pointer - The location in the string to start with, advanced
--
-- The sequence of components  is delimited by commas.  Attributes in  a
-- component are separated by plus ('+').
--    Textual keys can  contain letters,  digits, '-'.  A key must start
-- with a letter. Keys are case insensitive. OID keys start with digits.
--    The value  can  contain any  UTF-8 encoded  characters  except for
-- ',', '=', '"', '+', '<', '>', '#', ';', '\'.  These characters can be
-- escaped using '\'.  Escape can also use  hexadecimal pair  of digits.
-- The value  cannot start or  end with spaces.  They must be escaped at
-- the value ends. Within the value space need not to be escaped.
--    Another value format begins with '#' and contains  two hexadecimal
-- digits per octet.
--
-- Returns :
--
--    The distinguished name
--
-- Exceptions :
--
--    End_Error    - No distinguished name
--    Data_Error   - Syntax error
--    Layout_Error - Pointer is outside Source'First..Source'Last + 1
--
   function Get
            (  Source  : String;
               Pointer : access Integer
            )  return Distinguished_Name;
--
-- Get_Attribute - The key/value pair
--
--    Name      - The distinguished name
--    Component - The component number 1..Get_Length
--    Attribute - The attribute of the component 1..Get_Component_Length
--
-- Returns :
--
--    The attribute
--
-- Exceptions :
--
--    Constraint_Error - Component or Attribute is out of range
--
   function Get_Attribute
            (  Name      : Distinguished_Name;
               Component : Positive;
               Attribute : Positive := 1
            )  return Name_Attribute;
--
-- Get_Component_Length -- Length of a named component
--
--    Name      - The distinguished name
--    Component - The component number 1..Get_Length
--
-- Returns :
--
--    The number of attributes in the component of the name
--
-- Exceptions :
--
--    Constraint_Error - Component not in 1..Get_Length
--
   function Get_Component_Length
            (  Name      : Distinguished_Name;
               Component : Positive
            )  return Positive;
--
-- Get_Key - The key
--
--    Name      - The distinguished name
--    Component - The component number 1..Get_Length
--    Attribute - The attribute of the component 1..Get_Component_Length
--
-- Returns :
--
--    The attribute key
--
-- Exceptions :
--
--    Constraint_Error - Component or Attribute is out of range
--
   function Get_Key
            (  Name      : Distinguished_Name;
               Component : Positive;
               Attribute : Positive := 1
            )  return Attribute_Key;
--
-- Get_Length -- Length of a name
--
--    Name - The distinguished name
--
-- Returns :
--
--    The number of name components
--
   function Get_Length (Name : Distinguished_Name) return Natural;
--
-- Get_Value - The attribute value
--
--    Name      - The distinguished name
--    Component - The component number 1..Get_Length
--    Attribute - The attribute of the component 1..Get_Component_Length
--
-- Returns :
--
--    The attribute value
--
-- Exceptions :
--
--    Constraint_Error - Component or Attribute is out of range
--
   function Get_Value
            (  Name      : Distinguished_Name;
               Component : Positive;
               Attribute : Positive := 1
            )  return String;
--
-- Image -- A string representation of distinguished name
--
--    Name - The identifier
--
-- Returns :
--
--    The result string
--
   function Image (Name : Distinguished_Name) return String;
--
-- Put -- Distinguished name into string
--
--    Destination - The string to put object identifier into
--    Pointer     - The first element to write
--    Value       - The value
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- The parameter  Pointer is advanced beyond the value output.  The name
-- is put in dotted notation
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds or no room for output
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Name        : Distinguished_Name;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Skip -- Distinguished name
--
--    Source  - The source string
--    Pointer - The location in the string to start with, advanced
--
-- This procedure parses  and skip distinguished name.  It is similar to
-- Get but returns no result.
--
-- Exceptions :
--
--    End_Error    - No distinguished name
--    Data_Error   - Syntax error
--    Layout_Error - Pointer is outside Source'First..Source'Last + 1
--
   procedure Skip (Source : String; Pointer : in out Integer);
--
-- Value -- Object identifier in dotted notation for string
--
--    Source - The source string
--
-- Returns :
--
--    The distinguished name
--
-- Exceptions :
--
--    End_Error  - No distinguished name
--    Data_Error - Syntax error or not all string parsed
--
   function Value (Source : String) return Distinguished_Name;
--
-- = -- Comparing two names
--
--    Left, Right - Names to compare
--
-- The equivalence of two names ignores the order of attributes
--
-- Returns :
--
--    True if names are equivalent
--
   function "="  (Left, Right : Distinguished_Name) return Boolean;
   function "<"  (Left, Right : Distinguished_Name) return Boolean;
   function "<=" (Left, Right : Distinguished_Name) return Boolean;
   function ">"  (Left, Right : Distinguished_Name) return Boolean;
   function ">=" (Left, Right : Distinguished_Name) return Boolean;
------------------------------------------------------------------------
--
-- Composition of distinguished names.  A distinguished name is composed
-- this way. Considering a name
--
--    CN=James Hacker,
--    L=Basingstoke,
--    O=Widget Inc,
--    CN=GB
--
-- It can be composed as:
--
--    "CN"="James Hacker" and "L"="Basingstoke" and "O"="Widget Inc" and
--    "CN"="GB"
--
-- = -- Create textually keyed attribute
--
--    Key   - The key (letter, digit, '-', starting with letter)
--    Value - The value
--
-- Returns :
--
--    The pair
--
-- Exceptions :
--
--    Constraint_Error - Invalid key
--
   function "="
            (  Key   : String;
               Value : String
            )  return Distinguished_Name;
   function "="
            (  Key   : Object_Identifier;
               Value : String
            )  return Distinguished_Name;
   function "="
            (  Key   : String;
               Value : String
            )  return Name_Attribute;
   function "="
            (  Key   : Object_Identifier;
               Value : String
            )  return Name_Attribute;
--
-- or -- Add attribute to the last component
--
--    Left  - The distinguished name
--    Right - The attribute
--
-- The attribute is added to the last component of the name.
--
-- Returns :
--
--    The new name
--
-- Exceptions :
--
--    Constraint_Error - Name is empty
--    Name_Error       - The attribute's key is already in use
--
   function "or"
            (  Left  : Distinguished_Name;
               Right : Name_Attribute
            )  return Distinguished_Name;
--
-- add -- Add new component
--
--    Left  - The distinguished name
--    Right - The attribute
--
-- Returns :
--
--    The new name
--
   function "and"
            (  Left  : Distinguished_Name;
               Right : Name_Attribute
            )  return Distinguished_Name;
--
-- & -- Concatenate two names
--
--    Left  - The prefix distinguished name
--    Right - The suffix
--
-- Returns :
--
--    Left followed by Right
--
   function "&"
            (  Left  : Distinguished_Name;
               Right : Distinguished_Name
            )  return Distinguished_Name;
--
-- Subname -- Get relative distinguished name
--
--    Name - The prefix distinguished name
--    From - The first component to extract
--    To   - The last component
--
-- Returns :
--
--    The relative distinguished name From..To
--
   function Subname
            (  Name : Distinguished_Name;
               From : Positive;
               To   : Positive
            )  return Distinguished_Name;
private
   type Slice is record
      Start  : Integer;
      Length : Natural;
   end record;
   type Pair is record
      Mode  : Attribute_Mode;
      Key   : Slice;
      Value : Slice;
   end record;
   type Components_Array is array (Positive range <>) of Slice;
   type Attributes_Array is array (Positive range <>) of Pair;
   type Distinguished_Name
        (  Depth : Natural;
           Pairs : Natural;
           Items : Natural;
           Size  : Natural
        )  is
   record
      Components : Components_Array  (1..Depth);
      Attributes : Attributes_Array  (1..Pairs);
      SubIDs     : Object_Identifier (1..Items);
      Buffer     : String            (1..Size);
   end record;
   function Get_Key
            (  Name : Distinguished_Name;
               Item : Pair
            )  return Attribute_Key;
   function Get_Value
            (  Name : Distinguished_Name;
               Item : Pair
            )  return String;

   Null_Name : constant Distinguished_Name :=
               (  Depth      => 0,
                  Pairs      => 0,
                  Items      => 0,
                  Size       => 0,
                  Components => (1..0 => (1, 0)),
                  Attributes => (1..0 => (Text_Keyed, (1, 0), (1, 0))),
                  SubIDs     => (1..0 => 0),
                  Buffer     => ""
              );

end Strings_Edit.Distinguished_Names;
