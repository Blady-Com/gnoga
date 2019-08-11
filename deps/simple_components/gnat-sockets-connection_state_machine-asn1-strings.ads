--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--     ASN1.Strings                                Spring, 2019       --
--  Interface                                                         --
--                                Last revision :  13:37 03 Aug 2019  --
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
--  ASN.1  encoding  of strings.  The  implementation  of Feed  supports
--  constructed  strings.  The  ASN.1  strings  tags  are  processed  as
--  following:
--
--     BMP_String_Tag       Converted from UCS-2 big endian
--     Character_String_Tag Converted from Latin-1
--     IA5_String_Tag       Checked to be 7-bit ASCII
--     ISO646_String_Tag    Checked to be code points from space to ~
--     Numeric_String_Tag   Checked to contain 0..9 or space
--     Octet_String_Tag     Octet string. The content is not checked and
--                          passed as-is
--     Printable_String_Tag Checked to contain  A..Z, a..z, 0..9, space,
--                          ', (, ), +, -, comma, dot, /, semicolon,  =,
--                          ?
--     Teletext_String_Tag  Converted from ITU T.61 to UTF-8
--     Universal_String_Tag Converted from UCS-4 big endian to UTF-8
--     UTF8_String_Tag      UTF-8 encoded  string.  The content  is  not
--                          verified and passed as-is
--     Videotext_String_Tag Passed as-is
--     Graphic_String_Tag   Passed as-is
--     General_String_Tag   Passed as-is
--
--  The value stored  in the object  is always  UTF-8 encoded.  When the
--  input is  in any  other  encoding  it is recoded  as necessary  into
--  UTF-8.
--
with Strings_Edit.UTF8;  use Strings_Edit.UTF8;

package GNAT.Sockets.Connection_State_Machine.ASN1.Strings is
--
-- Public_String_Data_Item -- ASN.1 encoded string, public view
--
--    Size - The maximum string length
--
   type Public_String_Data_Item
        (  Size : Natural
        )  is abstract new ASN1_Data_Item with
   record
      Length : Natural := 0; -- The actual length of the value
      Value  : String (1..Size);
   end record;
--
-- Get_ASN1_Type -- Overrides ...Connection_State_Machine.ASN1...
--
   function Get_ASN1_Type
            (  Item : Public_String_Data_Item
            )  return ASN1_Type;
--
-- Get_Length -- The current string value length
--
--    Item - The item
--
-- Returns :
--
--    The current value length
--
   function Get_Length (Item : Public_String_Data_Item) return Natural;
--
-- Get_Value -- The current string value
--
--    Item - The item
--
-- Returns :
--
--    The current value
--
   function Get_Value (Item : Public_String_Data_Item) return String;
--
-- Set_Value -- Set the string value
--
--    Item  - The item
--    Value - The new value to set
--
-- Exceptions :
--
--    Constraint_Error - The value is too large to store
--
   procedure Set_Value
             (  Item  : in out Public_String_Data_Item;
                Value : String
             );
------------------------------------------------------------------------
--
-- Get -- From stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to read
--    Value   - The result
--
-- Note that  the data type is  not included.  The string  is  always an
-- octet/UTF-8 string.  No other encodings are supported.  The parameter
-- Pointer is advanced beyond the value obtained
--
-- Exceptions :
--
--    Constraint_Error - No room in Value to store
--    End_Error        - Not enough data
--    Layout_Error     - Pointer is outside bounds
--
   procedure Get
             (  Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : out String
             );
--
-- Put -- Into stream element array
--
--    Data    - The stream element array
--    Pointer - The first element to write
--    Value   - The value to encode
--
-- The parameter Pointer  is advanced beyond the value output.  The data
-- type  and length  are not output,  octet/UTF-8 encoding  is  used for
-- String, UCS-2 for BMP and UCS-4 for Universal.
--
-- Exceptions :
--
--    Constraint_Error - Wrong UTF-8 encoding or unsupported code point
--    End_Error        - No room for output
--    Layout_Error     - Pointer is outside bounds
--
   procedure Put
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             );
   procedure Put_BMP
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             );
   procedure Put_ITU_T61
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             );
   procedure Put_Latin1
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             );
   procedure Put_Universal
             (  Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Value   : String
             );
------------------------------------------------------------------------
   type String_Data is abstract tagged record
      Primitive   : Boolean               := True;
      Definite    : Boolean               := True;
      Nested      : Boolean               := True;
      Long_Tag    : Boolean               := True;
      Tag         : ASN1_Type             := UTF8_String_Tag;
      Constraint  : ASN1_Type             := 0;
      Accumulator : Code_Point            := 0;
      Last        : Stream_Element_Offset := 0;
   end record;
   type String_Data_Ptr is access all String_Data'Class;
   procedure Check_Value
             (  Parent : ASN1_Data_Item'Class;
                Item   : String_Data;
                Tag    : ASN1_Type
             )  is abstract;
   function Get_Available
            (  Item  : String_Data
            )  return Natural is abstract;
   procedure Reset (Item : in out String_Data) is abstract;
   procedure Store
             (  Item    : in out String_Data;
                Element : Stream_Element
             )  is abstract;
   procedure Store
             (  Item     : in out String_Data;
                Sequence : String
             )  is abstract;
   procedure Feed
             (  Parent  : ASN1_Data_Item'Class;
                Item    : in out String_Data'Class;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset;
                Client  : in out State_Machine'Class;
                State   : in out Stream_Element_Offset
             );
   procedure Do_Feed
             (  Item     : in out String_Data'Class;
                Data     : Stream_Element_Array;
                Pointer  : in out Stream_Element_Offset;
                Expected : ASN1_Type_Array;
                State    : in out Stream_Element_Offset
             );
private
   Unsupported : constant String := "Unsupported code point";

   String_Tags : constant ASN1_Type_Array :=
                          (  BMP_String_Tag,
                             Character_String_Tag,
                             Date_Tag,
                             Date_Time_Tag,
                             Duration_Tag,
                             Embedded_Tag,
                             General_String_Tag,
                             Generalized_Time_Tag,
                             Graphic_String_Tag,
                             IA5_String_Tag,
                             ISO646_String_Tag,
                             Numeric_String_Tag,
                             Object_Descriptor_Tag,
                             Octet_String_Tag,
                             Printable_String_Tag,
                             Teletext_String_Tag,
                             Time_Tag,
                             Time_Of_Day_Tag,
                             Universal_String_Tag,
                             UTF8_String_Tag,
                             UTC_Time_Tag,
                             Videotext_String_Tag
                          );

   function Check_Code_Point
            (  Code : UTF8_Code_Point;
               Tag  : ASN1_Type
            )  return Boolean;

   procedure Check_Value (Value : String; Tag : ASN1_Type);
------------------------------------------------------------------------
   type Embedded_String_Data
        (  Parent : access Public_String_Data_Item'Class
        )  is new String_Data with null record;
   procedure Check_Value
             (  Parent : ASN1_Data_Item'Class;
                Item   : Embedded_String_Data;
                Tag    : ASN1_Type
             );
   function Get_Available
            (  Item  : Embedded_String_Data
            )  return Natural;
   procedure Reset (Item : in out Embedded_String_Data);
   procedure Store
             (  Item    : in out Embedded_String_Data;
                Element : Stream_Element
             );
   procedure Store
             (  Item     : in out Embedded_String_Data;
                Sequence : String
             );
------------------------------------------------------------------------
   type External_String_Data is new String_Data with record
      Buffer : External_String_Buffer_Ptr;
      Start  : Positive := 1;
      Length : Natural  := 0;
   end record;
   procedure Check_Value
             (  Parent : ASN1_Data_Item'Class;
                Item   : External_String_Data;
                Tag    : ASN1_Type
             );
   function Get_Available
            (  Item  : External_String_Data
            )  return Natural;
   procedure Reset (Item : in out External_String_Data);
   procedure Store
             (  Item    : in out External_String_Data;
                Element : Stream_Element
             );
   procedure Store
             (  Item     : in out External_String_Data;
                Sequence : String
             );

   procedure Uninitialized (Item : ASN1_Data_Item'Class);

end GNAT.Sockets.Connection_State_Machine.ASN1.Strings;
