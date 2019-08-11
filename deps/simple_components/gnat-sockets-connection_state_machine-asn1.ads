--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNAT.Sockets.Connection_State_Machine.      Luebeck            --
--        ASN1                                     Spring, 2019       --
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
--  This package  is  a root package  of ASN.1  encoding  implementation
--  based on X.690 by ITU-T standard. See
--
--     https://www.itu.int/rec/T-REC-X.690
--
package GNAT.Sockets.Connection_State_Machine.ASN1 is
--
-- ASN.1 universal tags
--
   type ASN1_Tag is new Stream_Element;
   type ASN1_Type is new Integer;
   Boolean_Tag            : constant ASN1_Type :=  1;
   Integer_Tag            : constant ASN1_Type :=  2;
   Bit_String_Tag         : constant ASN1_Type :=  3;
   Octet_String_Tag       : constant ASN1_Type :=  4;
   Null_Tag               : constant ASN1_Type :=  5;
   Object_Identifier_Tag  : constant ASN1_Type :=  6;
   Object_Descriptor_Tag  : constant ASN1_Type :=  7;
   Instance_Tag           : constant ASN1_Type :=  8; -- External
   Real_Tag               : constant ASN1_Type :=  9;
   Enumerated_Tag         : constant ASN1_Type := 10;
   Embedded_Tag           : constant ASN1_Type := 11;
   UTF8_String_Tag        : constant ASN1_Type := 12;
   Relative_OID_Tag       : constant ASN1_Type := 13;
   Time_Tag               : constant ASN1_Type := 14;
   Sequence_Tag           : constant ASN1_Type := 16;
   Set_Tag                : constant ASN1_Type := 17;
   Numeric_String_Tag     : constant ASN1_Type := 18;
   Printable_String_Tag   : constant ASN1_Type := 19;
   Teletext_String_Tag    : constant ASN1_Type := 20; -- T61String
   Videotext_String_Tag   : constant ASN1_Type := 21;
   IA5_String_Tag         : constant ASN1_Type := 22;
   UTC_Time_Tag           : constant ASN1_Type := 23;
   Generalized_Time_Tag   : constant ASN1_Type := 24;
   Graphic_String_Tag     : constant ASN1_Type := 25;
   ISO646_String_Tag      : constant ASN1_Type := 26; -- VisibleString
   General_String_Tag     : constant ASN1_Type := 27;
   Universal_String_Tag   : constant ASN1_Type := 28;
   Character_String_Tag   : constant ASN1_Type := 29;
   BMP_String_Tag         : constant ASN1_Type := 30;
   Date_Tag               : constant ASN1_Type := 31;
   Time_Of_Day_Tag        : constant ASN1_Type := 32;
   Date_Time_Tag          : constant ASN1_Type := 33;
   Duration_Tag           : constant ASN1_Type := 34;
--
-- Image -- ASN.1 data type representation in human-readable form
--
--    Tag - The predefined data type
--
-- Returns :
--
--    The data type string representation
--
   function Image (Tag : ASN1_Type) return String;

   type ASN1_Type_Array is array (Positive range <>) of ASN1_Type;
   function Image (Tags : ASN1_Type_Array) return String;

   Constructed            : constant ASN1_Tag := 2#0010_0000#;

   Universal_Class        : constant ASN1_Tag := 2#0000_0000#;
   Application_Class      : constant ASN1_Tag := 2#0100_0000#;
   Context_Specific_Class : constant ASN1_Tag := 2#1000_0000#;
   Private_Class          : constant ASN1_Tag := 2#1100_0000#;
------------------------------------------------------------------------
--
-- Tagging_Mode -- The tagging method
--
   type Tagging_Type is
        (  Application_Tag,
           Context_Specific_Tag,
           Private_Tag,
           Universal_Tag
        );
--
-- Tag_Type -- Tag of an ASN.1 object
--
-- When class is universal tag, the tag's value is the ASN.1 type
--
   type Tag_Type is record
      Class    : Tagging_Type := Universal_Tag;
      Value    : ASN1_Type    := 0;
      Optional : Boolean      := False;
   end record;
   function "=" (Left, Right : Tag_Type) return Boolean;
   function "<" (Left, Right : Tag_Type) return Boolean;
   function Image (Value : Tag_Type) return String;
------------------------------------------------------------------------
--
-- Abstract_ASN1_Data_Item -- The base type of all ASN.1 encoded objects
--
   type Abstract_ASN1_Data_Item is
      abstract new Data_Item with null record;
   type Abstract_ASN1_Data_Item_Ptr is
      access all Abstract_ASN1_Data_Item'Class;
--
-- Always_Constructed -- Check if instances are always constructed
--
--    Item - The object
--
-- Returns :
--
--    True if the objects of the type are always constructed
--
   function Always_Constructed
            (  Item : Abstract_ASN1_Data_Item
            )  return Boolean is abstract;
--
-- Encode -- ASN.1 object
--
--    Item    - The object
--    Data    - The buffer to place the encoding into
--    Pointer - Location in the buffer to start, advanced after output
--
-- In all cases the definite length form is used as recommended by ASN.1
-- distinguished encoding rules (DER)
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - The pointer is not in Data'First..Data'Last + 1
--    Status_Error - The object is not initialized yet
--
   procedure Encode
             (  Item    : Abstract_ASN1_Data_Item;
                Data    : in out Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             )  is abstract;
--
-- Is_Implicit -- If the object has implicit type encoding
--
--    Item - The object
--
-- Returns :
--
--    True if implicit
--
   function Is_Implicit
            (  Item : Abstract_ASN1_Data_Item
            )  return Boolean is abstract;
--
-- Set_Implicit_Tag -- Set ASN.1 tag of an implicit object
--
--    Item   - The object
--    Tag    - The tag
--    Length - The encoding length, negative if indefinite
--
-- This procedure  must  be called  before using  Feed  on  an  implicit
-- object.  It supplies it  the missing  information about the  encoding
-- length and the tag value.  The latter  is normally  not used  because
-- the type of the object is known beforehand. Though it can be checked.
-- When  Length  is zero  the callee's  Feed  is never called so if zero
-- length  is allowed  the object must set  itself  as appropriate.  The
-- default implementation checks the length greater than zero.
--
   procedure Set_Implicit_Tag
             (  Item   : in out Abstract_ASN1_Data_Item;
                Tag    : Tag_Type;
                Length : Stream_Element_Offset
             );
--
-- Set_Untagged -- Set that an implicit object has no tag
--
--    Item - The object
--
-- This procedure is called before using Feed to inform that there is no
-- tag. Some implicit objects  cannot be input without tag.  The default
-- implementation fails if the object is implicit.
--
   procedure Set_Untagged (Item : in out Abstract_ASN1_Data_Item);
--
-- ASN1_Data_Item -- The base type of typed ASN.1 objects
--
   type ASN1_Data_Item is
      abstract new Abstract_ASN1_Data_Item with null record;
--
-- Always_Constructed -- Overrides ...Connection_State_Machine.ASN1...
--
   function Always_Constructed
            (  Item : ASN1_Data_Item
            )  return Boolean;
--
-- Get_ASN1_Type -- Get ASN.1 type of the object
--
--    Item - The object
--
-- Returns :
--
--    The ASN.1 tag
--
   function Get_ASN1_Type (Item : ASN1_Data_Item)
      return ASN1_Type is abstract;
------------------------------------------------------------------------
--
-- ASN1_List_Data_Item -- The abstract base type  of sequences  and sets
--                        with elements
--
   type ASN1_List_Data_Item is
      abstract new ASN1_Data_Item with null record;
--
-- Get_Length -- The number of elements contained by the list
--
--    Item - The ASN.1 list object
--
-- Returns :
--
--    The number of list elements
--
-- Exceptions :
--
--    Use_Error - The sequence was not initialized yet
--
   function Get_Length
            (  Item : ASN1_List_Data_Item
            )  return Natural is abstract;
------------------------------------------------------------------------
--
-- ASN1_Tagged_List_Data_Item -- The abstract base type of sequences and
--                               sets with tagged elements
--
   type ASN1_Tagged_List_Data_Item is
      abstract new ASN1_List_Data_Item with null record;
--
-- Get_Tag -- Get tag of an element
--
--    Item  - The sequence
--    Index - The number of the element 1..Get_Length (Item)
--
-- Returns :
--
--    The element's tag
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range or element is untagged
--    Use_Error        - The list was not initialized yet
--
   function Get_Tag
            (  Item  : ASN1_Tagged_List_Data_Item;
               Index : Positive
            )  return Tag_Type is abstract;
--
-- Is_Set -- Check if an optional element is set
--
--    Item  - The sequence
--    Index - The number of the element 1..Get_Length (Item)
--
-- Returns :
--
--    The element was set
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Use_Error        - The list was not initialized yet
--
   function Is_Set
            (  Item  : ASN1_Tagged_List_Data_Item;
               Index : Positive
            )  return Boolean is abstract;
--
-- Is_Untagged -- Check if an element is untagged
--
--    Item  - The sequence
--    Index - The number of the element 1..Get_Length (Item)
--
-- Returns :
--
--    True if the element is untagged
--
-- Exceptions :
--
--    Constraint_Error - Index is out of range
--    Use_Error        - The list was not initialized yet
--
   function Is_Untagged
            (  Item  : ASN1_Tagged_List_Data_Item;
               Index : Positive
            )  return Boolean is abstract;
------------------------------------------------------------------------
--
-- Get -- From stream element array
--
--    Data        - The stream element array
--    Pointer     - The first element to read
--    Tag         - The tag
--    Constructed - The value of the tagged object is constructed
--
-- The parameter Pointer is advanced beyond the tag obtained.  The field
-- Optional is set to false.
--
-- Exceptions :
--
--    Constraint_Error - The tag is too large
--    Data_Error       - Invalid tag
--    End_Error        - Not enough data
--    Layout_Error     - Pointer is outside bounds
--
   procedure Get
             (  Data        : Stream_Element_Array;
                Pointer     : in out Stream_Element_Offset;
                Tag         : out Tag_Type;
                Constructed : out Boolean
             );
--
-- Put -- Into stream element array
--
--    Data        - The stream element array
--    Pointer     - The first element to write
--    Constructed - The value of the tagged object is constructed
--    Value       - The value to encode
--
-- The parameter Pointer  is advanced beyond the value output.  The data
-- type is not output, octet/UTF-8 encoding is used.
--
-- Exceptions :
--
--    End_Error    - No room for output
--    Layout_Error - Pointer is outside bounds
--
   procedure Put
             (  Data        : in out Stream_Element_Array;
                Pointer     : in out Stream_Element_Offset;
                Tag         : Tag_Type;
                Constructed : Boolean
             );
private
--
-- Check -- Type tag of an object
--
--    Tag      - The tag
--    Expected - The list of expected types
--    Primitve - True if the encoding must be primitive
--
-- Exceptions :
--
--    Data_Error - Check failed
--
   procedure Check
             (  Tag       : Stream_Element;
                Expected  : ASN1_Type_Array;
                Primitive : Boolean := False
             );
--
-- Always_Constructed -- Types that must be constructed
--
--    Tag - The ASN.1 type
--
-- Returns :
--
--    True if the object is always constructed
--
   function Always_Constructed (Tag : ASN1_Type) return Boolean;

   Bit_String_Invalid_EOC  : constant String :=
                                 "Invalid ASN.1 bit string constructor";
   Wrong_Length : constant String :=
                         "Primitive ASN.1 object has indefinite length";
   Invalid_Length          : constant String :=
                                          "Invalid ASN.1 object length";
   Nested                  : constant String :=
                                             "Nested ASN.1 constructor";
   No_Container            : constant String :=
                     "Missing container for externally managed objects";
   Non_Terminated          : constant String :=
                                          "Non-terminated ASN.1 object";
   Nothing_Selected        : constant String := "No choice selected";
   Real_Empty_Decimal      : constant String :=
                                  "Empty decimal number representation";
   Real_Invalid_Decimal    : constant String :=
                                "Invalid decimal number representation";
   Real_Not_A_Number       : constant String :=
                                               "Non-numeric ASN.1 real";
   Real_Format_Reserved    : constant String :=
                            "ASN.1 format reserved for future editions";
   Real_Too_Large_Decimal  : constant String :=
                           "Decimal number representation is too large";
   String_Invalid_EOC      : constant String :=
                                     "Invalid ASN.1 string constructor";
   String_Type_Error       : constant String :=
               "ASN.1 constructed string does not match the constraint";
   Tag_Too_Large           : constant String :=
                                               "ASN.1 tag is too large";
   Too_Large               : constant String :=
                                      "ASN.1 object value is too large";
   Wrong_Choice            : constant String :=
                            "ASN.1 choice contains a non-ASN.1 object ";
   Wrong_Index             : constant String := "Invalid element index";
   Wrong_Unused_Count      : constant String :=
                                              "Wrong unused bits count";
end GNAT.Sockets.Connection_State_Machine.ASN1;
